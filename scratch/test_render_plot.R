library(tidyverse)
library(lubridate)
library(arrow)
library(plotly)
library(mgcv)
library(padr)
library(xgboost)
library(htmlwidgets)

# 1. Setup Environment
# Source the same functions used by the app
source("R/read_ext.R")
source("R/apply_fdom_corrections.R")
source("R/apply_training_scale.R")
source("R/apply_toc_model.R")
source("R/apply_timestep_median.R")
source("R/apply_cleaning_filters.R")
source("R/apply_interpolation_missing_data.R")
source("R/apply_low_pass_binomial_filter.R")

# Mock global variables from global.R/server.R
cdwr_api_key <- NULL
site_table <- tibble(site_code = c("sfm", "chd", "pfal", "pbd", "pbr_fc", "pman_fc"),
                     site_name = c("South Fork CLP", "Chambers Lake Outflow", "CLP at Poudre Falls", "Canyon Mouth", "CLP at Indian Meadows", "CLP at Manners Bridge"),
                     color = c("#002EA3", "#E70870", "#256BF5", "#1E4D2B", "#56104E", "#FFCA3A"))

# 2. Load Data
message("Loading data from data_backup.parquet...")
raw_data <- arrow::read_parquet("data/data_backup.parquet")

# 3. Replicate base_filtered_data logic
message("Filtering base data...")
# User selections - using dates that EXIST in data/data_backup.parquet
input_date_range <- c(as.Date("2026-04-01"), as.Date("2026-04-07"))
input_sites_select <- c("Canyon Mouth", "CLP at Poudre Falls", "South Fork CLP")
input_parameters_select <- c("FDOM Fluorescence", "Temperature", "Specific Conductivity", "Turbidity", "Chl-a Fluorescence")

start_DT <- as.POSIXct(paste0(input_date_range[1], " 00:01"), tz = "America/Denver")
end_DT   <- as.POSIXct(paste0(input_date_range[2], " 23:55"), tz = "America/Denver")

sites_sel <- filter(site_table, site_name %in% input_sites_select) %>% pull(site_code)

base_data <- raw_data %>%
  mutate(DT_round = with_tz(DT_round, tzone = "America/Denver")) %>%
  filter(
    between(DT_round, start_DT, end_DT),
    site %in% sites_sel,
    parameter %in% input_parameters_select
  ) %>%
  mutate(
    mean = ifelse(!is.na(mal_flag), NA, mean),
    mean = if_else(units == "m", mean * 3.28084, mean),
    units = if_else(units == "m", "ft", units)
  )

message(sprintf("Base data filtered: %d rows", nrow(base_data)))

# 4. Replicate filtered_data logic (with QAQC filter ON)
message("Applying QAQC filters and aggregation...")
input_data_timestep <- "1 hour"

# Re-applying logic carefully
filtered_data_result <- base_data %>%
  apply_cleaning_filters(new_value_col = "mean_cleaned") %>%
  apply_interpolation_missing_data(value_col = "mean_cleaned", dt_col = "DT_round", method = "linear", max_gap = 4) %>%
  apply_low_pass_binomial_filter(value_col = "mean_filled", new_value_col = "mean_smoothed", dt_col = "DT_round") %>%
  # Ensure DT_round is POSIXct before aggregation
  mutate(DT_round = as.POSIXct(DT_round)) %>%
  apply_timestep_median(value_col = "mean_smoothed", new_value_col = "timestep_median", timestep = input_data_timestep, dt_col = "DT_round") %>%
  select(DT_round = DT_group, site, parameter, mean = timestep_median) %>%
  distinct(site, parameter, mean, DT_round, .keep_all = TRUE)

message(sprintf("Filtered/Aggregated data: %d rows", nrow(filtered_data_result)))

# 5. Replicate TOC model plot logic
message("Running TOC model (with expected fallback)...")
# remove FC sonde data
input_data_toc <- filtered_data_result %>%
  filter(!str_detect(site, "_fc"))

# Mock values$canyon_q (initialize with NULL to force API call or just use empty df)
# In local test, let's use what's in data if possible or fetch
canyon_q <- NULL

toc_plot_data <- apply_toc_model(
  sensor_data = input_data_toc,
  toc_model_file_path = "data/models/ross_only_toc_xgboost_models_light_20260224.rds",
  scaling_params_file_path = "data/models/scaling_params_toc_20260224.parquet",
  summarize_interval = input_data_timestep,
  time_col = "DT_round",
  value_col = "mean",
  canyon_q_data = canyon_q
) %>%
  left_join(site_table, by = c("site" = "site_code")) %>%
  mutate(across(contains("TOC_guess"), ~ round(.x, 2)))

message(sprintf("TOC plot data generated: %d rows", nrow(toc_plot_data)))
print("TOC summary:")
print(summary(toc_plot_data$TOC_guess_ensemble))

# 6. Render one plot (Canyon Mouth)
message("Rendering plot for Canyon Mouth...")
site_cd <- "Canyon Mouth"
plot_param <- "TOC_guess_ensemble"

site_toc_data <- toc_plot_data %>%
  filter(site_name == site_cd) %>%
  arrange(DT_round) %>%
  mutate(
    gap = is.na(TOC_guess_min) | is.na(TOC_guess_max) | is.na(.data[[plot_param]]),
    gid = cumsum(lag(gap, default = TRUE) != gap)
  ) %>%
  filter(!gap)

message(sprintf("Plot data for %s: %d rows", site_cd, nrow(site_toc_data)))

if (nrow(site_toc_data) > 0) {
  p <- plot_ly() %>%
    layout(
      title = list(text = paste0("Estimated TOC (mg/L) at: ", site_cd), font = list(size = 14)),
      shapes = list(
        list(type = "rect", xref = "paper", x0 = 0, x1 = 1, yref = "y", y0 = 0, y1 = 2, fillcolor = "#00FF00", opacity = 0.1, line = list(width = 0)),
        list(type = "rect", xref = "paper", x0 = 0, x1 = 1, yref = "y", y0 = 2, y1 = 4, fillcolor = "#FFFF00", opacity = 0.1, line = list(width = 0)),
        list(type = "rect", xref = "paper", x0 = 0, x1 = 1, yref = "y", y0 = 4, y1 = 15, fillcolor = "#FF0000", opacity = 0.1, line = list(width = 0))
      ),
      xaxis = list(title = "Date"),
      yaxis = list(title = "Model Estimated TOC (mg/L)", range = c(2, 5)),
      showlegend = TRUE,
      hovermode = "x unified"
    ) %>%
    add_ribbons(
      data = site_toc_data,
      x = ~DT_round,
      ymin = ~TOC_guess_min,
      ymax = ~TOC_guess_max,
      name = "Models Range of Estimates",
      line = list(color = 'rgba(7, 164, 181, 0.05)'),
      fillcolor = 'rgba(128, 128, 128, 0.4)',
      showlegend = TRUE
    ) %>%
    add_lines(
      data = site_toc_data,
      x = ~DT_round,
      y = ~TOC_guess_ensemble,
      name = "Mean Model Estimate",
      line = list(color = "#E70870", width = 2),
      showlegend = TRUE
    )
  
  saveWidget(p, "test_plot.html")
  p
  message("Plot saved to test_plot.html")
} else {
  message("NO DATA to plot for this site.")
}
