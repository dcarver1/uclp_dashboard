#' Predict TOC using a pre-trained model
#'
#' This function applies a pre-trained TOC model to new sensor data. Data (df) should be pre-processed using
#' @param sensor_data A data frame containing the new sensor data to predict TOC. This should be in long format with the following data:
#'  - `site_col`: Site identifier
#'  - `time_col`: Timestamp of the data
#'  - `parameter_col`: Parameter name (e.g., "FDOM Fluorescence", "Temperature", "Specific Conductivity", "Turbidity", "Chl-a Fluorescence")
#'  - `value_col`: Measured value for the parameter
#'  The following features are required for the model:
#'  - `FDOM Fluorescence (RFU)` of In Situ Sonde
#'  - `Temperature (Celsius)` of In Situ Sonde
#'  - `Specific Conductivity (uS/cm)` of In Situ Sonde
#'  - `Turbidity (NTU)`` of In Situ Sonde
#'  - `Chl-a Fluorescence (RFU)` of In Situ Sonde
#'  The model uses the following features computed from the variables above
#'  - `log_sensor_turb`: log transformed Turbidity (NTU) of In Situ Sonde
#'  - `log_chl_a`: log transformed Chl-a Fluorescence (RFU) of In Situ Sonde
#'  - `f_c_turb`: interaction term between FDOM, Turbidity & Chl-a (f_c_turb = FDOM/(Chl_a + Sensor_Turb + FDOM)))
#'  The data should be pre-processed to ensure that the columns match the model's expected input.
#'  Input data will be normalized to the training scale (see scaling parameters)
#'
#' @param toc_model_file_path String character. The file path to the saved TOC model (RDS format).
#' @param scaling_params_file_path String character. The file path to the saved scaling parameters (RDS format).
#' @param summarize_interval String character. The time interval to which the data should be summarized before prediction.
#' Default is "1 hour". Other common values include "15 minutes", "30 minutes", "1 day", etc.
#' @param time_col String character. The name of the column in `sensor_data` containing the datetime information.
#' Default is "DT_round".
#' @param site_col String character. The name of the column in `sensor_data` containing the site identifiers.
#' Default is "site".
#' @param parameter_col String character. The name of the column in `sensor_data` containing the parameter identifiers.
#' Default is "parameter".
#' @param value_col String character. The name of the column in `sensor_data` containing the measured values.
#' Default is "mean".
#' @details
#' The function first processes the input `sensor_data` to ensure it contains the necessary features for the model. The median based on the summarize interval of `time_col`
#' is then taken to reduce noise and short-term variability.
#' It then normalizes the data using the provided scaling parameters, applies each fold of the pre-trained TOC model to generate predictions,
#' and computes the ensemble mean of the predictions. The output includes the original data along with the predicted TOC values from each model fold
#' and the ensemble mean.
#'
#' @return A data frame with the original data with additional columns `toc_guess_#` containing the predicted TOC values from each model fold
#'and `toc_ensemble_mean` containing the ensemble mean of the predicted TOC values.
#'
#' @examples
#'
#' result <- predict_toc(
#'  new_data = your_sensor_data,
#'  toc_model_file_path = "data/upper_clp_dss/modeling/toc_xgboost_20250801.rds",
#'   scaling_params_file_path = "data/upper_clp_dss/modeling/scaling_parameters_20250801.rds"
#' )
#'


apply_toc_model <- function(sensor_data, toc_model_file_path, scaling_params_file_path, summarize_interval = "1 hour",
                            time_col = "DT_round", site_col = "site", parameter_col = "parameter", value_col = "mean", canyon_q_data = NULL){

  #Load TOC model
  toc_models <- tryCatch({
    readRDS(toc_model_file_path)
  }, error = function(e) {
    warning("Failed to read TOC model RDS: ", e$message)
    NULL
  })
  
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
       features <- toc_models[[1]]$feature_names 
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
      try({ best_iter <- .x$best_iteration }, silent = TRUE)
      
      raw_preds <- feature_data %>%
          as.matrix()%>%
          predict(.x, ., iteration_range = c(1, best_iter)) %>%
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

  if (xgb_failed) {
    # GAM Fallback logic
    # Load site metadata for distance
    site_meta <- read_csv("data/toc_forecast_location_metadata.csv", show_col_types = FALSE) %>%
      select(site_code, distance_upstream_km)
    
    # Prepare GAM input
    gam_input <- model_input_data %>%
      mutate(site_gam = str_replace(site, "_fc$", "")) %>%
      left_join(site_meta, by = c("site_gam" = "site_code")) %>%
      mutate(DOY = lubridate::yday(!!sym(time_col)),
             canyon_mouth_cfs = canyon_mouth_daily_flow_cfs) %>%
      # Ensure no NAs in required columns
      filter(!is.na(distance_upstream_km), !is.na(DOY), !is.na(canyon_mouth_cfs))
    
    if (nrow(gam_input) == 0) {
      warning("gam_input is empty during fallback after filtering NAs")
      # Create an empty predictions_list to avoid bind_cols error
      predictions_list <- as_tibble(matrix(nrow = 0, ncol = 4))
      names(predictions_list) <- paste0(target_col, "_guess_fold", 1:4)
    } else {
      # Load GAM folds
      message("Loading GAM models for fallback...")
      gam_models <- list(
        read_rds("data/models/TOC_GAM_Q_add_fit1_v2026-02-09.rds"),
        read_rds("data/models/TOC_GAM_Q_add_fit2_v2026-02-09.rds"),
        read_rds("data/models/TOC_GAM_Q_add_fit3_v2026-02-09.rds"),
        read_rds("data/models/TOC_GAM_Q_add_fit4_v2026-02-09.rds")
      )
      
      message("Starting GAM prediction imap...")
      predictions_list <- tryCatch({
        imap_dfc(gam_models, ~{
          message(paste("Predicting GAM fold", .y))
          # GAM predictions are usually in log space if family is Gamma(link='log')
          raw_preds <- exp(predict(.x, gam_input))
          tibble(!!paste0(target_col, "_guess_fold", .y) := round(as.numeric(raw_preds), 2))
        })
      }, error = function(e) {
        warning("GAM prediction failed: ", e$message)
        # Return dummy predictions if it fails
        imap_dfc(gam_models, ~tibble(!!paste0(target_col, "_guess_fold", .y) := rep(NA_real_, nrow(gam_input))))
      })
      
      message("GAM prediction successful. Aggregating results...")
      # Align summarized_data with gam_input to ensure bind_cols works
      # Aggregate the GAM results to the same interval
      predictions_list <- predictions_list %>%
        bind_cols(gam_input %>% select(!!sym(site_col), !!sym(time_col)), .) %>%
        mutate(!!sym(time_col) := round_date(!!sym(time_col), unit = summarize_interval)) %>%
        group_by(!!sym(site_col), !!sym(time_col)) %>%
        summarize(across(starts_with(paste0(target_col, "_guess_fold")), \(x) median(x, na.rm = TRUE)), .groups = "drop") %>%
        select(starts_with(paste0(target_col, "_guess_fold")))
    }
  }

  summarized_data <- bind_cols(summarized_data, predictions_list) %>%
    # compute ensemble mean
    mutate(
      !!paste0(target_col, "_guess_ensemble") := if_else(
        if_any(all_of(features), is.na) & !xgb_failed,                # Check if ANY feature is NA (only for XGB)
        NA_real_,                                                        # If true, set ensemble to NA
        round(rowMeans(across(matches(paste0(target_col, "_guess_fold")))), 2) # Else, compute mean
      )
    )
  
  if (xgb_failed) {
     # Ensure ensemble mean is recalculated correctly for GAM
     summarized_data <- summarized_data %>%
       mutate(!!paste0(target_col, "_guess_ensemble") := round(rowMeans(across(matches(paste0(target_col, "_guess_fold")))), 2))
  }
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

  # Return the data with TOC predictions
  return(final_dataset)

}

