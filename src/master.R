
start_date_time <- as.POSIXct("2021-08-30 07:00:00")

# dregde_vol <- 190000 # m3 material to be dredged

time_constraint <- c("07:00:00", "19:00:00")

hms::as_hms(time) >= hms::as_hms(min(hms::as_hms(time_constraint))) &
  hms::as_hms(time) <= hms::as_hms(max(hms::as_hms(time_constraint)))

source(here::here('src', 'dredge.R'))
source(here::here('src', 'sim_bed.R'))
source(here::here('src', 'hopper.R'))
source(here::here('src', 'sim_water_level.R'))
source(here::here('src', 'surface_dredge_surface.R'))


seabed_surface <- simulate_seabed()
simuluate_dredge_surface <- seabed_surface - 10
water_level <- simulated_water_level()

hopper_details <- hopper_split(
                            number_of_hoppers = 2,
                            hopper_capacity = c(250, 130), # m3 NOTE - Vector lengths must match the number of hoppers 
                            hopper_efficiency = c(0.8, 0.8),
                            sailing_distance = 15, #km to disposal ground
                            discharge_time = c(15, 45), #seconds
                            sail_spd_full = c(6, 12), #knots
                            sail_spd_empty = c(6, 12), #knots
                            attach_line_time = c(15, 30)
                            )

dredge_backhoe( start_date_time = min(water_level$date_time),
                water_level = water_level,
                dredge_surface = simuluate_dredge_surface,
                bed_grid = seabed_surface,
                hopper_details = hopper_details,
               time_constraint = c("07:00:00", "19:00:00"),
               dregde_vol = 5000,
               easting_start = 665818,
               northing_start = 7753509
               )