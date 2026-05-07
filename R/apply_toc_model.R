#' Predict TOC using an ensemble of pre-trained XGBoost models
#'
#' This function applies an ensemble of pre-trained TOC models to sensor data.
#' It supports both full timeseries rendering (with gap padding) and snapshot
#' loading for real-time monitoring.
#'
#' @param sensor_data A data frame containing the new sensor data. Must include columns for
#' site, timestamp, and parameters ("FDOM Fluorescence", "Temperature", "Specific Conductivity", "Turbidity")
#' @param toc_models A list of `xgb.Booster` objects. By default, it uses `toc_realtime_model`,
#' which is an ensemble loaded via:
#' `map(1:3, ~xgb.load(modelfile = paste0("data/models/ross_only_toc_xgboost_model_fold",.x, "_20260224.ubj")))`.
#' @param scaling_params_file_path String. File path to the saved scaling parameters (RDS format)
#' used to normalize features to the training scale.
#' @param summarize_interval String. The time interval for data summarization (e.g., "1 hour", "1 day").
#' Default is "1 hour".
#' @param time_col String. Name of the datetime column. Default is "DT_round".
#' @param site_col String. Name of the site identifier column. Default is "site".
#' @param parameter_col String. Name of the parameter identifier column. Default is "parameter".
#' @param value_col String. Name of the measured values column. Default is "mean".
#' @param canyon_q_data Optional data frame of discharge data. If NULL or out of range, the
#' function attempts to fetch "CLAFTCCO" telemetry via `cdssr`.
#' @param timeseries Logical. Set to `TRUE` when rendering the full dataset timeseries to
#' enable padding of missing timestamps. Set to `FALSE` when loading a data snapshot.
#'
#' @details
#' The function processes raw sensor data by:
#' * Applying FDOM temperature and turbidity corrections.
#' * Calculating interaction terms (`fdom_x_turb`, `fdom_x_sc`).
#' * Normalizing features based on provided scaling parameters.
#' * Generating predictions for each model fold and calculating an ensemble mean,
#' minimum, and maximum.
#'
#' @return A data frame summarized by `summarize_interval` with additional columns:
#' * `TOC_guess_fold#`: Predictions from individual model folds.
#' * `TOC_guess_ensemble`: The mean prediction across all folds.
#' * `TOC_guess_min` / `TOC_guess_max`: The range of predictions across the ensemble.
#'
#' @examples
#' # Loading the ensemble
#' toc_realtime_model <- map(1:3, ~xgb.load(
#'   modelfile = paste0("data/models/ross_only_toc_xgboost_model_fold", .x, "_20260224.ubj")
#' ))
#'
#' # Applying the model for a full timeseries
#' toc_results <- apply_toc_model(
#'   sensor_data = daily_data,
#'   scaling_params_file_path = "data/params/scaling_2026.rds",
#'   timeseries = TRUE
#' )


apply_toc_model <- function(sensor_data, toc_models = toc_realtime_model, scaling_params_file_path, summarize_interval = "1 hour",
                            time_col = "DT_round", site_col = "site", parameter_col = "parameter", value_col = "mean", canyon_q_data = NULL,
                            timeseries = F){

  # Ensure all boosters are valid (fixes pointer corruption when loading RDS)
  if (!is.null(toc_models)) {
    for (i in seq_along(toc_models)) {
      if (inherits(toc_models[[i]], "xgb.Booster")) {
        toc_models[[i]] <- tryCatch({
          # Use xgb.save.raw and xgb.load.raw to ensure the booster is properly initialized
          # in the current session.
          raw <- xgboost::xgb.save.raw(toc_models[[i]])
          xgboost::xgb.load.raw(raw)
        }, error = function(e) {
          # Fallback if completion fails - we'll handle this in the prediction step
          toc_models[[i]]
        })
      }
    }
  }

  # Define features for prediction (same order as training)
  # Default features if model is NULL
  features <- c("Specific Conductivity", "Temperature", "Turbidity", "FDOMc", "fdom_x_turb", "fdom_x_sc", "canyon_mouth_daily_flow_cfs")
  if (!is.null(toc_models) && length(toc_models) > 0) {
     try({
       features <- xgb.importance(toc_models[[1]])$Feature
     }, silent = TRUE)
  }
  # Define target variable name
  target <- 'TOC'

  # Process the new data to match model requirements
  processed_sensor_data <- sensor_data %>%
    select(!!sym(time_col), !!sym(site_col), !!sym(parameter_col), !!sym(value_col)) %>%
    distinct()%>%
    mutate(!!sym(value_col) := case_when( !!sym(parameter_col) == "Turbidity" & !!sym(value_col) < 0.1 ~ 0.1,
                                          !!sym(parameter_col) == "Turbidity" & !!sym(value_col) > 1000 ~ 1000,
                                          TRUE ~ !!sym(value_col)))%>%
    pivot_wider(names_from = !!sym(parameter_col), values_from = !!sym(value_col))%>%
    apply_fdom_corrections(fdom_col = "FDOM Fluorescence", temp_col = "Temperature", turb_col = "Turbidity",
                           fdom_temp_col = "FDOMc", fdom_turb_col = "FDOM_turb_corr", fdom_final_col = "FDOM_final_corr")%>%
    mutate(fdom_x_sc = FDOMc * `Specific Conductivity`,
           fdom_x_turb = FDOMc * Turbidity) %>%
    #fix site names to match model
    select(!!sym(time_col), !!sym(site_col),
           any_of(features))

  if (nrow(processed_sensor_data) == 0) {
    warning("processed_sensor_data is empty after pivot_wider and FDOM corrections")
  }

  # Ensure canyon_q_data covers the required range
  start_date_req <- as_date(min(processed_sensor_data[[time_col]], na.rm = TRUE), tz = "America/Denver")
  end_date_req <- as_date(max(processed_sensor_data[[time_col]], na.rm = TRUE), tz = "America/Denver")

  if (is.null(canyon_q_data) ||
      min(canyon_q_data$date, na.rm = TRUE) > start_date_req ||
      max(canyon_q_data$date, na.rm = TRUE) < end_date_req) {

    # Fetch or supplement missing flow data
    canyon_q_data <- tryCatch({
      cdssr::get_telemetry_ts(abbrev = "CLAFTCCO",
                                          start_date = start_date_req - days(1),
                                          end_date = end_date_req + days(1),
                                          api_key = cdwr_api_key,
                                        timescale = "hour") %>%
      mutate(date = as_date(datetime, tz = "America/Denver")) %>%
      summarize(canyon_mouth_daily_flow_cfs = mean(meas_value, na.rm = TRUE), .by = date)
    }, error = function(e) {
      warning("Failed to pull canyon_q_data: ", e$message)
      canyon_q_data
    })
  }

  model_input_data <- processed_sensor_data %>%
    mutate(date = as_date(!!sym(time_col), tz = "America/Denver")) %>%
    left_join(canyon_q_data, by = "date") %>%
    # Only omit if required features are missing
    filter(if_all(all_of(c(features, "date")), ~!is.na(.)))

  if (nrow(model_input_data) == 0 && nrow(processed_sensor_data) > 0) {
     warning("model_input_data is empty after join with flow and na filtering. check flow data range and feature completeness.")
  }

  # Load saved scaling parameters and model
  scaling_params <- read_ext(scaling_params_file_path)

  #Apply scaling normalization and convert to matrix
  summarized_data <- model_input_data %>%
    apply_training_scale(new_data = ., scaling_params = scaling_params,features = features) %>%
    mutate( !!sym(time_col) := round_date(!!sym(time_col), unit = summarize_interval))%>%
    #summarize to the specified interval
    summarize(across(any_of(c(features)), \(x) median(x, na.rm = TRUE)),.by = c(!!sym(site_col), !!sym(time_col)))

  target_col = "TOC"

  # Try XGBoost first, if it fails due to corruption, fallback to GAM
  xgb_failed <- FALSE

  #Using each model, make a prediction on the da
  predictions_list <- tryCatch({
    imap_dfc(toc_models, ~{
      feature_data <- summarized_data %>%
        select(all_of(features)) %>%
        mutate(across(everything(), as.numeric))

      #Check for missing values in features
      has_na <- rowSums(is.na(feature_data)) > 0

      # Make preds using a single model
      # Note: accessing .x fields might throw if booster is corrupted
      best_iter <- 1
      try({ best_iter <- xgb.attr(.x, "best_iteration")%>%as.numeric() }, silent = TRUE)

      raw_preds <- feature_data %>%
          as.matrix()%>%
          predict(.x, ., iteration_range = c(1, best_iter), validate_parameters = T) %>%
          round(2)

      # make preds NA where features had NA
      final_preds <- if_else(has_na, NA_real_, raw_preds)

      # Get predictions as tibble
      tibble(!!paste0(target_col, "_guess_fold", .y) := final_preds)
    })
  }, error = function(e) {
    warning("XGBoost prediction failed (possibly corrupted models): ", e$message)
    xgb_failed <<- TRUE
    return(NULL)
  })

  summarized_data <- bind_cols(summarized_data, predictions_list) %>%
    # compute ensemble mean
    mutate(
      !!paste0(target_col, "_guess_ensemble") := if_else(
        if_any(all_of(features), is.na) & !xgb_failed,                # Check if ANY feature is NA (only for XGB)
        NA_real_,                                                        # If true, set ensemble to NA
        round(rowMeans(across(matches(paste0(target_col, "_guess_fold")))), 2) # Else, compute mean
      )
    )

  # Columns with fold predictions
  fold_cols <- grep(paste0(target_col, "_guess_fold"), colnames(summarized_data), value = TRUE)

  if (nrow(summarized_data) == 0) {
    # Provide an empty dataset with the expected structure
    dummy_cols <- c(time_col, site_col, features, fold_cols,
                    paste0(target_col, "_guess_min"),
                    paste0(target_col, "_guess_max"),
                    paste0(target_col, "_guess_ensemble"))
    empty_df <- as_tibble(matrix(nrow = 0, ncol = length(dummy_cols)))
    names(empty_df) <- dummy_cols
    return(empty_df)
  }

  if(timeseries){
    start_DT <- min(sensor_data[[time_col]], na.rm = TRUE)
    end_DT <- max(sensor_data[[time_col]], na.rm = TRUE)

    final_dataset <- summarized_data %>%
      # Filter to desired time window first
      filter(between(!!sym(time_col), start_DT, end_DT)) %>%
      # Pad missing timestamps based on summarize_interval
      pad(
        start_val = start_DT,
        end_val = end_DT,
        by = time_col,
        interval = summarize_interval,   #  "1 hour", "1 day", etc.
        group = c("site")
      ) %>%
      # Compute min/max across folds
      mutate(
        !!paste0(target_col, "_guess_min") := pmin(!!!syms(fold_cols), na.rm = TRUE),
        !!paste0(target_col, "_guess_max") := pmax(!!!syms(fold_cols), na.rm = TRUE),
        !!paste0(target_col, "_guess_ensemble") := pmax(0, !!sym(paste0(target_col, "_guess_ensemble")))#,
        # group = with(rle(!is.na(.data[[paste0(target_col, "_guess_ensemble")]])), rep(seq_along(values), lengths))
      )
  } else {
    final_dataset <- summarized_data %>%
      mutate(
        !!paste0(target_col, "_guess_min") := pmin(!!!syms(fold_cols), na.rm = TRUE),
        !!paste0(target_col, "_guess_max") := pmax(!!!syms(fold_cols), na.rm = TRUE),
        !!paste0(target_col, "_guess_ensemble") := pmax(0, !!sym(paste0(target_col, "_guess_ensemble")))
      )
  }

  # Return the data with TOC predictions
  return(final_dataset)

}

