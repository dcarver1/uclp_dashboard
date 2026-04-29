source('global.R')
source('R/apply_interpolation_missing_data.R')
source('R/apply_timestep_median.R')

df <- arrow::read_parquet(snapshot_url, as_data_frame = TRUE) %>%
  arrange(site, parameter, DT_round) %>%
  distinct(site, parameter, DT_round, .keep_all = TRUE) %>%
  mutate(DT_round_MT = with_tz(DT_round, tzone = "America/Denver")) %>%
  filter(
    parameter %in% c("Temperature", "Turbidity")
  )

df1 <- apply_interpolation_missing_data(df = df,  value_col = "mean", dt_col = "DT_round_MT", method = "linear", max_gap = 4)
print(nrow(df1))
