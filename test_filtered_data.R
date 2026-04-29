source('global.R')
source('R/apply_interpolation_missing_data.R')
source('R/apply_timestep_median.R')

base_filtered_data <- arrow::read_parquet(snapshot_url, as_data_frame = TRUE) %>%
  mutate(DT_round_MT = with_tz(DT_round, tzone = "America/Denver")) %>%
  filter(
    parameter %in% c("Temperature", "Turbidity")
  ) %>%
  mutate(
    mean = ifelse(!is.na(mal_flag), NA, mean),
    mean = if_else(units == "m", mean * 3.28084, mean),
    units = if_else(units == "m", "ft", units)
  )

# Simulate QAQC False path
df1 <- apply_interpolation_missing_data(df = base_filtered_data,  value_col = "mean", dt_col = "DT_round_MT", method = "linear", max_gap = 4)
df2 <- apply_timestep_median(df = df1, value_col = "mean_filled", new_value_col = "timestep_median", timestep = "1 hour", dt_col = "DT_round_MT")
filtered_data <- df2 %>%
        select(DT_round_MT = DT_group, site, parameter, mean = timestep_median)%>%
        distinct(site, parameter, mean, DT_round_MT, .keep_all = TRUE)

print(unique(filtered_data$site))
