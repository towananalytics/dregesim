
start_date_time <- as.POSIXct("2021-08-30 07:00:00")

dregde_vol <- 190000 # m3 material to be dredged

time_constraint <- c("07:00:00", "19:00:00")

hms::as_hms(time) >= hms::as_hms(min(hms::as_hms(time_constraint))) &
  hms::as_hms(time) <= hms::as_hms(max(hms::as_hms(time_constraint)))
