#calibration_events <- use %>% select(model_run) %>% distinct() %>% unname()
calibration_events <- c("2019-11-12 16:40")

comment_options <- c(
  "-"
  , "New Development"
  , "Investigate - Pump"
  , "Investigate - Basin Flow"
  , "Investigate - Excessive I&I"
  , "Investigate - As-Builts"
  , "Investigate - Valve Position"
  , "Field Verification Needed"
  , "Approved - Low Flow"
  , "Update - Model Info"
)

filter_options <- c(
  "All"
  , "Approved"
  , "Not Approved"
  , "Investigate"
)

file_info <- data.frame(date_created = "2019-11-12 4:40 PM", plant_basin="III - Southwest")

wastewater_plants <- c(
    "I - Buckman"
  , "II - Cedar Bay"
  , "III - Southwest"
  , "IV - Arlington-East"
  , "IV - Monterey"
  , "V - Mandarin"
  , "VI - Blacks Ford"
  , "VI - JCP"
  , "VII - Nassau"
  , "VIII - Ponte Vedra"
  , "VIII - PDL"
)

wastewater_abv <- c(
    "BM"
  , "D2"
  , "SW"
  , "AE"
  , "MO"
  , "MN"
  , "BF"
  , "JCP"
  , "NC"
  , "PV"
  , "PDL"
)

suggested_filename <- paste(
  wastewater_abv[match(file_info$plant_basin, wastewater_plants)]
  , format(ymd_hm(file_info$date_created), format="%F")
  , sep="_")

sketch = htmltools::withTags(table(
  class = 'display',
  
  thead(
    tr(
      th(rowspan = 2, 'CMMS'),
      th(rowspan = 2, 'Address'),
      th(rowspan = 2, 'SCADA\nRuntime'),
      th(colspan = 2, 'Time-Series'),
      th(colspan = 2, 'Z-Score'),
      th(colspan = 2, 'Double-Mass')
    ),
    
    tr(
      th('MPE'),
      th('NSE'),
      th('mean'),
      th('stdev'),
      th('beta'),
      th('RMSE')
    )
  )
))



summary_sketch = htmltools::withTags(table(
  class = 'display',
  
  thead(
    tr(
      th(rowspan = 2, 'SCADA\nRuntime'),
      th(colspan = 2, 'Time-Series'),
      th(colspan = 2, 'Z-Score'),
      th(colspan = 2, 'Double-Mass')
    ),
    
    tr(
      th('MPE'),
      th('NSE'),
      th('mean'),
      th('stdev'),
      th('beta'),
      th('RMSE')
    )
  )
))