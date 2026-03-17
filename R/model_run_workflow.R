# model_run_workflow.R
#
# Daily TOC forecast workflow for the Upper Cache la Poudre watershed.
# Called by .github/workflows/model_run.yml (runs at 3 PM MDT / 21Z).
#
# Migrated and adapted from upper_clp_dss:
#   - modeling/external_forecasts/HEFS_canyon_mouth_23-25.Rmd
#   - modeling/toc/toc_forecast/distributed/apply_gam_doy_forecasted_Q.Rmd
#   - modeling/toc/toc_forecast/intake/apply_intake_toc_model.Rmd
#
# Output (append-only, one file per model type):
#   data/toc_forecast_distributed_backup.parquet — distributed GAM (all mainstem sites)
#   data/toc_forecast_intake_backup.parquet      — intake SWE GAM (FC treatment plant)
#   Each run appends new rows; today's rows are replaced if the script runs twice.

library(tidyverse)
library(arrow)
library(mgcv)
library(snotelr)
library(httr)
library(jsonlite)

source("R/get_hefs_ensembles_cbrfc.R")

# ==============================================================================
# 1. LOCATION METADATA
# ==============================================================================

location_meta <- read_csv("data/toc_forecast_location_metadata.csv",
                           show_col_types = FALSE)

dist_sites <- location_meta %>%
  filter(model_version == "Distributed")

intake_sites <- location_meta %>%
  filter(model_version == "FC Intake")

# ==============================================================================
# 2. HEFS FORECAST — pulled ONCE, used by both models
# ==============================================================================

message("Pulling HEFS forecast for FTDC2 (Canyon Mouth)...")

hefs_raw <- get_hefs_ensembles_cbrfc(ID = "FTDC2", date = Sys.Date())

if (is.null(hefs_raw) || nrow(hefs_raw) == 0) {
  stop("HEFS pull returned NULL or no rows. Aborting model run.")
}

# Each forecast starts at 12Z; subtract 12 hours to align to the "next day"
# mean (matching the convention established in the modeling Rmds)
forecasted_q <- hefs_raw %>%
  mutate(date_24h = as.Date(date_time - lubridate::hours(12))) %>%
  summarize(across(starts_with("ens_"), ~ mean(.x, na.rm = TRUE)),
            .by = c(forecast_date, date_24h))

forecasted_q_vert <- forecasted_q %>%
  pivot_longer(cols = starts_with("ens_"),
               names_to = "ensemble_member",
               values_to = "canyon_mouth_cfs") %>%
  rename(date = forecast_date)

# ==============================================================================
# 3. SNOTEL — current water year, for SWE PCA (intake model only)
# ==============================================================================

message("Downloading SNOTEL data for HUC 10190007...")

# Identify SNOTEL sites in the CLP HUC8
snotel_sites <- snotel_info() %>%
  filter(grepl("10190007", description))

# Download full record (no speed updates, we are pulling this data at set intervals)
snotel_data <- snotel_download(site_id = snotel_sites$site_id,
                               internal = TRUE,
                               metric = TRUE) %>%
  mutate(date = lubridate::ymd(date),
         year = lubridate::year(date))

# Pivot to wide for PCA
snotel_matrix <- snotel_data %>%
  select(date, site_id, snow_water_equivalent) %>%
  pivot_wider(names_from = site_id,
              names_prefix = "swe_",
              values_from = snow_water_equivalent) %>%
  filter(complete.cases(.))

# Load PCA model and project to PC1
swe_pca <- read_rds("data/models/SWE_PCA_v2026-02-18.rds")

snotel <- snotel_matrix %>%
  mutate(swe_pca = predict(swe_pca, .)[, "PC1"]) %>%
  select(date, swe_pca) %>%
  # Compute 14-day melt rate
  arrange(date) %>%
  mutate(date_m14 = date - lubridate::days(14)) %>%
  left_join(snotel %>% select(date, swe_pca) %>%
              rename(date_m14 = date, swe_pca_lag14 = swe_pca),
            by = "date_m14") %>%
  mutate(melt_rate_14d = (swe_pca_lag14 - swe_pca) / 14) %>%
  select(-c(date_m14, swe_pca_lag14))

# ==============================================================================
# 4. DISTRIBUTED GAM — all mainstem + upper watershed sites
# ==============================================================================

message("Applying distributed TOC GAM...")

dist_fit_1 <- read_rds("data/models/TOC_GAM_Q_add_fit1_v2026-02-09.rds")
dist_fit_2 <- read_rds("data/models/TOC_GAM_Q_add_fit2_v2026-02-09.rds")
dist_fit_3 <- read_rds("data/models/TOC_GAM_Q_add_fit3_v2026-02-09.rds")
dist_fit_4 <- read_rds("data/models/TOC_GAM_Q_add_fit4_v2026-02-09.rds")

# Cross join each site with all ensemble forecasts
dist_input <- dist_sites %>%
  select(site_code, site_name, distance_upstream_km) %>%
  cross_join(forecasted_q_vert) %>%
  mutate(DOY = lubridate::yday(date_24h))

dist_forecast <- dist_input %>%
  # Step 1: average 4 CV folds per row (one row = one HEFS ensemble member)
  mutate(
    pred_toc = (exp(predict(dist_fit_1, dist_input)) +
                exp(predict(dist_fit_2, dist_input)) +
                exp(predict(dist_fit_3, dist_input)) +
                exp(predict(dist_fit_4, dist_input))) / 4
  ) %>%
  # Step 2: summarize fold-averaged predictions across HEFS ensemble members
  summarize(
    dist_mean_pred_toc = mean(pred_toc),
    dist_min_pred_toc  = min(pred_toc),
    dist_max_pred_toc  = max(pred_toc),
    dist_q25_pred_toc  = quantile(pred_toc, 0.25),
    dist_q75_pred_toc  = quantile(pred_toc, 0.75),
    .by = c(date, site_code, site_name, date_24h)
  ) %>%
  mutate(model_version = "Distributed")

# ==============================================================================
# 5. INTAKE GAM — Fort Collins treatment plant intake (PRW)
# ==============================================================================

message("Applying intake TOC GAM...")

intake_q_swe <- read_rds("data/models/TOC_intake_GAM_Q_swe-melt14_v2026-02-18.rds")

# Join HEFS forecasts with SNOTEL on the forecast issue date
intake_input <- forecasted_q_vert %>%
  left_join(snotel, by = "date") %>%
  filter(!is.na(swe_pca)) %>%
  mutate(
    doy                = lubridate::yday(date_24h),
    distance_upstream_km = intake_sites$distance_upstream_km
  )

intake_forecast <- intake_input %>%
  mutate(
    intake_q_swe_pred = predict(intake_q_swe, intake_input)
  ) %>%
  # Summarize across HEFS ensemble members, matching distributed GAM convention
  summarize(
    intake_q_swe_pred     = mean(intake_q_swe_pred),
    intake_q_swe_pred_q25 = quantile(intake_q_swe_pred, 0.25),
    intake_q_swe_pred_q75 = quantile(intake_q_swe_pred, 0.75),
    .by = c(date, date_24h)
  ) %>%
  mutate(
    site_code          = intake_sites$site_code,
    site_name          = intake_sites$site_name,
    model_version      = "FC Intake"
  )

# ==============================================================================
# 6. SAVE (append, replacing today's rows if already present)
# ==============================================================================

# --- Distributed ---
message("Writing toc_forecast_distributed_backup.parquet...")

dist_forecast_new <- dist_forecast %>%
  select(date, site_code, site_name, model_version, date_24h,
         dist_mean_pred_toc, dist_min_pred_toc, dist_max_pred_toc,
         dist_q25_pred_toc, dist_q75_pred_toc)

dist_parquet_path <- "data/toc_forecast_distributed_backup.parquet"

if (file.exists(dist_parquet_path)) {
  dist_toc_forecast <- read_parquet(dist_parquet_path) %>%
    filter(date != Sys.Date()) %>%
    bind_rows(dist_forecast_new)
} else {
  dist_toc_forecast <- dist_forecast_new
}

write_parquet(dist_toc_forecast, dist_parquet_path)

# --- Intake ---
message("Writing toc_forecast_intake_backup.parquet...")

intake_forecast_new <- intake_forecast %>%
  select(date, site_code, site_name, model_version, date_24h,
         intake_q_swe_pred, intake_q_swe_pred_q25, intake_q_swe_pred_q75)

intake_parquet_path <- "data/toc_forecast_intake_backup.parquet"

if (file.exists(intake_parquet_path)) {
  intake_toc_forecast <- read_parquet(intake_parquet_path) %>%
    filter(date != Sys.Date()) %>%
    bind_rows(intake_forecast_new)
} else {
  intake_toc_forecast <- intake_forecast_new
}

write_parquet(intake_toc_forecast, intake_parquet_path)

message(sprintf("Done. %d distributed + %d intake forecast rows written.",
                nrow(dist_forecast_new),
                nrow(intake_forecast_new)))
