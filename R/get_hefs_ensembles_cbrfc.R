#' Retrieves Colorado Basin River Forecast Center Hydro-Ensemble Forecast
#' System (HEFS) Enseble Forecast Data.
#' https://www.cbrfc.noaa.gov/lmap/lmap.php
#'
#' This function queries HEFS for ensemble discharge forecasts for a specific
#' NWPS gage ID. It handles API communication, parses the complex JSON
#' structure, and flattens the quantile array into separate, named columns.
#'
#' @param ID A character string representing a single NWPS gauge ID (e.g., "COLOC070"),
#' In the return from get_hefs_gauges(), the NWPS gauge id is `lid` from this output.
#' @param date Date (in YYYY-MM-DD format) for which a quantile forecast is desired
#' Defaults to NULL, which will pull the latest forecast.
#'
#' @return A tibble containing the forecast date_time, value, and and ensemble
#' member for each of the 30-member ensemble
#'
get_hefs_ensembles_cbrfc <- function(ID, date = NULL) {

  # --- 1. Input Validation ---
  if (is.null(ID) || !is.character(ID) || length(ID) != 1) {
    warning("Input 'ID' must be a single character string representing the gauge ID.")
    return(NULL)
  }

  if (is.null(date)) {
    warning("No date input provided, defaulting to today's date.")
    date <- Sys.Date()
  }

  # --- 2. Request json data from site using httr
  full_request <- paste0("https://www.cbrfc.noaa.gov/dbdata/station/ensgraph/ensgraph_data.py?id=",
                         ID,
                         "&linearflow=1&fdate=",
                         date)

  message(sprintf("Requesting forecast data for ID %s", ID))

  #Perform the GET request
  forecast_raw <- httr::GET(url = full_request)

  # Extract content as text
  unpacked_data <- httr::content(forecast_raw, as = "text", encoding = "UTF-8")

  if (httr::status_code(forecast_raw) != 200) {
    warning(sprintf("API request failed for ID %s with message %s",
                    ID,
                    str_match(unpacked_data, '"error":"([^"]+)"')[,2]))
    return(NULL)
  }

  # --- 3. JSON Parsing and Structure Check ---
  tryCatch({
    # Parse the text content into an R list/data frame
    forecast_full <- jsonlite::fromJSON(unpacked_data, simplifyVector = TRUE)
  }, error = function(e) {
    warning(sprintf("JSON Parsing Error for ID %s. Message: %s", ID, e$message))
    return(NULL)
  })

  # Check for the primary expected data element
  if (is.null(forecast_full) || !("traces" %in% names(forecast_full))) {
    warning(sprintf("Forecast API response for ID %s is missing the expected 'traces' element.", ID))
    return(NULL)
  }

  # Check to see that the forecast originated on the correct date

  # --- 4. Data Transformation and Cleanup ---
  # forecast_data$traces contains individual ensemble members
  forecast_data <- forecast_full$traces %>%
    purrr::map2(names(.), ~ .x %>%
          dplyr::as_tibble() %>%
          # The columns in the new tibble are automatically named V1 and V2
            dplyr::rename(date_time = V1) %>%
            dplyr::rename(!!paste0("ens_", .y) := V2)) %>%
    purrr::reduce(full_join, by = "date_time") %>%
    dplyr::mutate(date_time = as.POSIXct(date_time / 1000, origin = "1970-01-01", tz = "UTC")) %>%
    dplyr::rowwise() %>%
    # Calculate ensemble stats
    dplyr::mutate(
      min_ens = min(c_across(starts_with("ens_")), na.rm = TRUE),
      q_25 = quantile(c_across(starts_with("ens_")), probs = 0.25, na.rm = TRUE) %>% as.numeric(),
      q_50 = quantile(c_across(starts_with("ens_")), probs = 0.5, na.rm = TRUE) %>% as.numeric(),
      q_75 = quantile(c_across(starts_with("ens_")), probs = 0.75, na.rm = TRUE) %>% as.numeric(),
      max_ens = max(c_across(starts_with("ens_")), na.rm = TRUE)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(forecast_date = lubridate::date(forecast_full$ensinfo$fcstdate))

  # Ensure we have quantile names before proceeding with unnesting/renaming
  if (is.null(forecast_data) || length(forecast_data) == 0) {
    warning(sprintf("No ensemble data returned for: ", ID))
    return(NULL)
  }

  message(sprintf("Successfully retrieved %s forecast records for ID %s",
                  nrow(forecast_data), ID))


  if (lubridate::date(forecast_full$ensinfo$fcstdate) != lubridate::date(Sys.Date())) {
    message(sprintf("Note, the forecast retrieved is not current. The most recent forecast is dated %s",
                    lubridate::date(forecast_full$ensinfo$fcstdate)))
  }

  return(forecast_data)

}
