#' Apply training data scaling to new data
#'
#' This function rescales selected feature columns in a new dataset to the 0–1 range
#' using min–max values derived from a training dataset or provided scaling parameters.
#' Either `full_dataset` or `scaling_params` must be supplied (but not both).
#' Optionally, computed scaling parameters can be saved as a parquet file.
#'
#' @param new_data A data frame or tibble containing new data to be scaled.
#' @param full_dataset Optional. A data frame or tibble used to compute scaling
#'   parameters if `scaling_params` is not provided.
#' @param scaling_params Optional. A named list or single-row tibble containing
#'   min and max values for each feature (e.g., `feature_min`, `feature_max`).
#' @param features A character vector of feature names to scale.
#' @param scaling_param_out_path Optional. File name path where scaling parameters
#'   will be saved as a `parquet` file if computed from `full_dataset`.
#'
#' @details
#' The function ensures that all features are present in `new_data`, and that the
#' necessary columns exist in either `full_dataset` or `scaling_params`. It applies
#' consistent min–max normalization so that model inputs remain on the same scale
#' across datasets. Only `full_dataset` or `scaling_params` should be provided, not both.
#'
#' @return A tibble identical to `new_data`, but with specified feature columns
#'   rescaled to the 0–1 range.
#'
#' @examples
#' # Example 1: Compute scaling parameters from a full dataset
#' scaled <- apply_training_scale(
#'   new_data = val_data,
#'   full_dataset = train_data,
#'   features = c("temp", "ph")
#' )
#'
#' # Example 2: Apply existing scaling parameters
#' scaled <- apply_training_scale(
#'   new_data = val_data,
#'   scaling_params = saved_params,
#'   features = c("temp", "ph")
#' )
#'
apply_training_scale <- function( new_data, full_dataset = NULL, scaling_params = NULL, features, scaling_param_out_path = NULL){
  `%nin%` <- Negate(`%in%`)
  #Check that new data  have all the features
  missing_in_new_data <- features[features %nin% colnames(new_data)]
  if(length(missing_in_new_data) > 0){
    stop(paste("The following features are missing in new_data:", paste(missing_in_new_data, collapse = ", ")))
  }
  #Only one of full_dataset or scaling_params should be provided and they both can't be NULL
  if(is.null(full_dataset) & is.null(scaling_params)){
    stop("Either full_dataset or scaling_params must be provided.")
  }

  #if no scaling params provided, compute from full dataset
  if(is_null(scaling_params)){
    #Check that full dataset has all the features
    missing_in_complete_data <- features[features %nin% colnames(full_dataset)]
    if(length(missing_in_complete_data) > 0){
      stop(paste("The following features are missing in full_dataset:", paste(missing_in_complete_data, collapse = ", ")))
    }

    scaling_params <- full_dataset %>%
      select(all_of(features)) %>% #only scale features
      #make sure all features are numeric
      mutate(across(everything(), ~ as.numeric(.x))) %>%
      #compute min and max for each feature
      summarise(across(
        everything(),
        list(min = ~min(.x, na.rm = TRUE), max = ~max(.x, na.rm = TRUE)), #get min and max
        .names = "{.col}_{.fn}"
      ))
  }else{
    #check that scaling parameters has all required columns
    required_cols <- unlist(lapply(features, function(x) c(paste0(x, "_min"), paste0(x, "_max"))))
    missing_in_scaling_params <- required_cols[required_cols %nin% colnames(scaling_params)]
    if(length(missing_in_scaling_params) > 0){
      stop(paste("The following scaling parameter columns are missing in scaling_params:", paste(missing_in_scaling_params, collapse = ", ")))
    }
  }

  #if scaling params output dir provided, save scaling params as parquet file
  if(!is.null(scaling_param_out_path)){
    # Get the directory
    scaling_param_out_dir <- dirname(scaling_param_out_path)
    #check to see if dir exists & create if not
    if(!dir.exists(scaling_param_out_dir)){
      dir.create(scaling_param_out_dir, recursive = TRUE)
    }

    scaling_params %>%
      write_parquet(paste0(scaling_param_out_path, ".parquet"))
  }

  #apply new scaling to new data
  scaled_data <- new_data %>%
    #make sure all features are numeric
    mutate(across(all_of(features), ~ as.numeric(.x))) %>%
    #apply scaling
    mutate(across(
      all_of(features),
      ~ {
        col <- cur_column()
        #get scaling from complete sensor data
        min_val <- scaling_params[[paste0(col, "_min")]]
        max_val <- scaling_params[[paste0(col, "_max")]]
        #rescale the original data to 0-1 based on training data min/max
        (.x - min_val) / (max_val - min_val)
      },
      .names = "{.col}" # overwrite with scaled values (keep same names)
    ))

  return(scaled_data)

}
