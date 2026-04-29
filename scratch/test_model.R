source('global.R')
source('R/apply_toc_model.R')
source('R/apply_fdom_corrections.R')
source('R/read_ext.R')
source('R/apply_training_scale.R')

df <- arrow::read_parquet(snapshot_url, as_data_frame = TRUE) %>%
  filter(site == "sfm") %>%
  filter(DT_round > max(DT_round, na.rm=TRUE) - days(2))
  
canyon_q <- cdssr::get_telemetry_ts(abbrev = "CLAFTCCO",
                                          start_date = min(df$DT_round, na.rm = TRUE) - days(1),
                                          end_date = max(df$DT_round, na.rm = TRUE) + days(1),
                                          api_key = cdwr_api_key,
                                        timescale = "hour")%>%
      mutate(date = as_date(datetime))%>%
      summarize(canyon_mouth_daily_flow_cfs = mean(meas_value, na.rm = TRUE), .by = date)

res <- apply_toc_model(df, 
  toc_model_file_path = "data/models/ross_only_toc_xgboost_models_light_20260224.rds",
  scaling_params_file_path = "data/models/scaling_params_toc_20260224.parquet",
  canyon_q_data = canyon_q)

print(head(res))
