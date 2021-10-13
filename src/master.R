

source(here::here('src', 'dredge.R'))
source(here::here('src', 'sim_bed.R'))
source(here::here('src', 'hopper.R'))
source(here::here('src', 'water_depth.R'))
source(here::here('src', 'sim_water_level.R'))
source(here::here('src', 'surface_dredge_surface.R'))


# Grid Parameters ---------------------------------------------------------

param.easting_start <- 665816.1774
param.northing_start <- 7753500.0084
param.grid_res <- 10
param.dist_x <- 100
param.dist_y <- 100
start_date_time <- as.POSIXct("2021-08-30 07:00:00")
time_constraint <- c("07:00:00", "19:00:00")


# Dredge Parameters -------------------------------------------------------

bucket_size <- 3 #m3
boom_length <- 8 #m
bucket_cut_depth <- 2 #m
bucket_fill_efficiency <- 0.8 #proportion
bucket_emptying_time <- #seconds
hopper_height_above_water <- 2 #m
slew_rate <- 2 # rpm
slew_angle <- 90 # degrees between dredge and hopper
bucket_speed <- 1 #m/s
bucket_speed_cutting <- 2 #m/s cutting/dredging speed once at depth
material_bulk_factor <- 1.15 # used for volume estimates

seabed_surface <- simulate_seabed(easting_start = param.easting_start,
                                  northing_start = param.northing_start,
                                  grid_res = param.grid_res,
                                  dist_x = param.dist_x,
                                  dist_y = param.dist_y
                                  )

simulate_dredge_surface <- seabed_surface

simulate_dredge_surface[, ] <- -2

volumes <- dredge_volumes(seabed_surface = seabed_surface, 
                          dredge_surface = simulate_dredge_surface)

total.dredge.vol <- sum(volumes)

water_level <- simulated_water_level(start_date_time = start_date_time,
                                     time_interval = 20, #minutes
                                     days_of_readings = 1)

# hms::as_hms(time) >= hms::as_hms(min(hms::as_hms(time_constraint))) &
#   hms::as_hms(time) <= hms::as_hms(max(hms::as_hms(time_constraint)))


# FOR LOOP START HERE -----------------------------------------------------

# Initialise the dredge position
northing <- 7753575
easting <- 665818

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

# tatools::save_xlsx(data = c("hopper_details", "temp"))


# TODO --------------------------------------------------------------------
# Set the length of matrix based on estimated number of timesteps/cycles.

  # temp <- matrix(ncol = 4, 
  #                nrow = total_buckets)

  temp <- matrix(ncol = 4,
                 nrow = 10)

  temp[1, 1] <- start_date_time

for(x in seq_along(hopper_details[, 1])){


# Dredge at cell position: ------------------------------------------------
# Calculate volume at cell location
  
  # print(start_date_time)
  depth <- water_depth( loc_north = northing,
                        loc_east = easting,
                        water_levels = water_level,
                        current_time = start_date_time,
                        bed_grid = seabed_surface
                       )
  
  time <- dredge_cycle_time(water_depth_at_location = depth, 
                            bucket_size = bucket_size,
                            boom_length = boom_length,
                            bucket_cut_depth = bucket_cut_depth,
                            bucket_fill_efficiency = bucket_fill_efficiency,
                            bucket_emptying_time = bucket_emptying_time,
                            hopper_height_above_water = hopper_height_above_water,
                            slew_rate = slew_rate,
                            slew_angle = slew_angle,
                            bucket_speed = bucket_speed,
                            bucket_speed_cutting = bucket_speed_cutting,
                            material_bulk_factor = material_bulk_factor
                            ) + start_date_time
  
  cut.vol <- bucket_size * bucket_fill_efficiency
  
  print(time)
  
  temp[1 + x, 1] <- time # 1 + x wont work - x is the hopper number therefore wont increase beyond number of hoppers and will overwrite. will need to loop over length of temp
  temp[1 + x, 2] <- cut.vol
  
  start_date_time <- time

}

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