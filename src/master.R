

source(here::here('src', 'dredge.R'))
source(here::here('src', 'sim_bed.R'))
source(here::here('src', 'hopper.R'))
source(here::here('src', 'water_depth.R'))
source(here::here('src', 'sim_water_level.R'))
# source(here::here('src', 'surface_dredge_surface.R'))
source(here::here('src', 'dredge_volumes.R'))


# Grid Parameters ---------------------------------------------------------

param.easting_start <- 665816.1774
param.northing_start <- 7753500.0084
param.grid_res <- 10
param.dist_x <- 100
param.dist_y <- 100
start_date_time <- lubridate::with_tz(as.POSIXct("2021-08-30 07:00:00"), "Australia/Perth")
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

# FOR LOOP START HERE -----------------------------------------------------

# Initialise the dredge position
# northing <- 7753575
# easting <- 665818

total.buckets <- ceiling(total.dredge.vol/ (bucket_size * bucket_fill_efficiency))

# Create a matrix to hold dredging data:
temp <- matrix(ncol = 4,
               nrow = total.buckets)

temp[1, 1] <- start_date_time
temp[1, 2] <- 0 #m3

dredge.buckets.complete <- 0 # Increments as buckets are extracted

# Start with the first hopper alongside and then fill with dredger --------

time <- start_date_time

for(j in 1:ncol(seabed_surface)){ # cells along the easting axis (columns) - start at top left and work down first column

  for(i in 1:nrow(seabed_surface)){ # Cells along northing axis (rows) - start at top left and work down first column
  
    northing <- as.numeric(rownames(seabed_surface)[i])
    easting <- as.numeric(colnames(seabed_surface)[j])
    
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
    
    # for(x in seq_along(hopper_details[, 1])){
      
      # Estimate total buckets to extract material at cell location:
      total.buckets.in.cell <- ceiling(dredge_volume_at_location(dredge.volumes = volumes,
                                                         loc_east = northing, 
                                                         loc_north = easting) / (bucket_size * bucket_fill_efficiency))
      
      
      
      if(hms::as_hms(t) >= hms::as_hms(min(hms::as_hms(time_constraint))) &
         hms::as_hms(t) <= hms::as_hms(max(hms::as_hms(time_constraint)))) { # Operating within operational hours
      
      for(y in 1:total.buckets.in.cell){ # Keep dredging up to total number of buckets for cell
        
        # Dredge at cell position: ------------------------------------------------
        # Calculate volume at cell location
        
        dredge.buckets.complete <- dredge.buckets.complete + 1
        
        if(sum(temp[n:dredge.buckets.complete, 2], na.rm = TRUE) < hopper_actual_capacity) { # Check if still space in the hopper dredge
          
          # TODO n not defined for above if statement.
          # TODO insert dredge functions here when there is space in the hopper
    
          depth <- water_depth( loc_north = northing,
                                loc_east = easting,
                                water_levels = water_level,
                                current_time = time,
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
          
          temp[dredge.buckets.complete, 1] <- time # 1 + y to offset from the initialised start point
          temp[dredge.buckets.complete, 2] <- cut.vol
          
        } else { 
          
          # TODO Iterate to next available hopper
          
        }
        
        # temp[1 + y, 1] <- time # 1 + y to offset from the initialised start point
        # temp[1 + y, 2] <- cut.vol
        
        start_date_time <- time
        
      # } # Then move dredge to next cell
        
      } # Cell complete - move dredge to next cell
      }
  }
  }

temp.df <- as.data.frame(temp)

temp.df[, 1] <- as.POSIXct(temp.df[,1], origin = "1970-01-01 00:00:00 ", tz = "Australia/Perth")
 # Dredge duration
duration <- as.numeric(max(temp.df$V1, na.rm = TRUE) - min(temp.df$V1, na.rm = TRUE))

m3_per_hr <- sum(temp.df$V2, na.rm = TRUE) / as.numeric(duration)

# tatools::save_xlsx(data = c("hopper_details", "temp.df"))
