#calibration_events <- use %>% select(model_run) %>% distinct() %>% unname()
calibration_events <- c("2019-11-12 16:40")

comment_options <- c(
  "-"
  , "New Development"
  , "Investigate - Pump"
  , "Investigate - Basin Flow"
  , "Investigate - As-Builts"
  , "Investigate - Valve Position"
  , "Field Verification Needed"
)

filter_options <- c(
  "All"
  , "Approved"
  , "Not Approved"
  , "Investigate"
)

wastewater_plants <- c(
  "I - Buckman"
  , "II - Cedar Bay"
  , "III - Southwest"
)