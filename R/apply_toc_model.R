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
  toc_models <- read_ext(toc_model_file_path)
  # Define features for prediction (same order as training)
  features <- toc_models[[1]]$feature_names # extract feature names from the first model (assuming all models have the same features)
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
           fdom_x_turb = FDOMc * Turbidity)%>%
    #fix site names to match model
    select(!!sym(time_col), !!sym(site_col),
           any_of(features))%>%
    mutate(date = as_date(!!sym(time_col)))

  if (is.null(canyon_q_data)) {
    # Fallback if not provided
    canyon_q_data <- cdssr::get_telemetry_ts(abbrev = "CLAFTCCO",
                                          start_date = min(processed_sensor_data[[time_col]], na.rm = TRUE) - days(1),
                                          end_date = max(processed_sensor_data[[time_col]], na.rm = TRUE) + days(1),
                                          api_key = cdwr_api_key,
                                        timescale = "hour")%>%
      mutate(date = as_date(datetime))%>%
      summarize(canyon_mouth_daily_flow_cfs = mean(meas_value, na.rm = TRUE), .by = date)
  }

  model_input_data <-  processed_sensor_data %>%
    left_join(canyon_q_data, by = "date")%>%
    na.omit() # remove any rows missing needed values

  # Load saved scaling parameters and model
  scaling_params <- read_ext(scaling_params_file_path)

  #Apply scaling normalization and convert to matrix
  summarized_data <- model_input_data %>%
    apply_training_scale(new_data = ., scaling_params = scaling_params,features = features) %>%
    mutate( !!sym(time_col) := round_date(!!sym(time_col), unit = summarize_interval))%>%
    #summarize to the specified interval
    summarize(across(any_of(c(features)), \(x) median(x, na.rm = TRUE)),.by = c(!!sym(site_col), !!sym(time_col)))

  target_col = "TOC"
  #Using each model, make a prediction on the da
  summarized_data <- imap_dfc(toc_models, ~{

    feature_data <- summarized_data %>%
      select(all_of(features)) %>%
      mutate(across(everything(), as.numeric))


    #Check for missing values in features
    has_na <- rowSums(is.na(feature_data)) > 0

    # Make preds using a single model
    raw_preds <- tryCatch({
      feature_data %>%
        as.matrix()%>%
        predict(.x, ., iteration_range = c(1, .x$best_iteration)) %>%
        round(2)
    }, error = function(e) {
      warning("XGBoost prediction failed: ", e$message)
      rep(NA_real_, nrow(feature_data))
    })

    # make preds NA where features had NA
    final_preds <- if_else(has_na, NA_real_, raw_preds)

    # Get predictions as tibble
    tibble(!!paste0(target_col, "_guess_fold", .y) := final_preds)

  }) %>%
    bind_cols(summarized_data, .)%>%
    # compute ensemble mean
    mutate(
      !!paste0(target_col, "_guess_ensemble") := if_else(
        if_any(all_of(features), is.na),                # Check if ANY feature is NA
        NA_real_,                                                        # If true, set ensemble to NA
        round(rowMeans(across(matches(paste0(target_col, "_guess_fold")))), 2) # Else, compute mean
      )
    )
  # Columns with fold predictions
  fold_cols <- grep(paste0(target_col, "_guess_fold"), colnames(summarized_data), value = TRUE)

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

