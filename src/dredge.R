

#' Title
#'
#' @param bed_grid 
#' @param bucket_size 
#' @param boom_length 
#' @param bucket_cut_depth 
#' @param bucket_fill_efficiency 
#' @param bucket_emptying_time 
#' @param hopper_height_above_water 
#' @param slew_rate 
#' @param slew_angle 
#' @param dregde_vol 
#' @param bucket_speed 
#' @param bucket_speed_cutting 
#' @param material_bulk_factor 
#' @param northing_start 
#' @param start_date_time 
#'
#' @return
#' @export
#'
#' @examples
dredge_backhoe <- function(
                            bed_grid = NULL, # The existing seabed matrix
                            dredge_surface = NULL,
                            hopper_details = NULL,
                            water_level = NULL,
                            bucket_size = 3, #m3
                            boom_length = 8, #m
                            bucket_cut_depth = 2, #m
                            bucket_fill_efficiency = 0.8, #proportion
                            bucket_emptying_time = 3, #seconds
                            hopper_height_above_water = 2, #m
                            slew_rate = 2, # rpm
                            slew_angle = 90, # degrees between dredge and hopper
                            
                            dregde_vol = 2000, # m3 material to be dredged
                            
                            bucket_speed = 1, #m/s
                            bucket_speed_cutting = 2, #m/s cutting/dredging speed once at depth
                            
                            material_bulk_factor = 1.15, # used for volume estimates
                            
                            
                            # Time Constraints --------------------------------------------------------
                            
                            time_constraint = c("0:00:00", "24:00:00"),
                            
                            # Dredge start position ---------------------------------------------------
                            
                            easting_start = 665818,
                            northing_start = 7753509,
                            
                            start_date_time = as.POSIXct("2021-08-30 07:00:00")
                            
                            
                            # Hopper details ----------------------------------------------------------
                            
                            # hopper_capacity = 250, #m3
                            # hopper_efficiency = 0.8,
                            # hopper_actual_capacity = hopper_capacity * hopper_efficiency, 
                            # cycle_time_hopper = 9764.118 # seconds
                            
                            ){

    # Calculations ------------------------------------------------------------
  
    hopper_cycle_time <- hopper_details[, "cycle.time"]
    
    total_buckets <- ceiling(dregde_vol / (bucket_size * bucket_fill_efficiency))
    
    slew_angle_dec_degrees <- slew_angle / 360
    
    time_to_slew <- (slew_angle_dec_degrees * 60) /  slew_rate # seconds
    
    dredge_start_pos <- matrix(c(easting_start, northing_start))
    
    temp <- matrix(ncol = 4, 
                   nrow = total_buckets)
    
    t <- start_date_time #
    n <- 1
    j <- 1 # hopper cycle
    
    
    vol_cut_cum <- 0 # Cumulative volume dredged
    
    for(x in seq_along(hopper_details[, 1])){
    
    for (i in 1:total_buckets) {
      
    
    # Find row index for where dredge is located within seabed:
    loc_east <- which(abs(as.numeric(colnames(bed_grid))-dredge_start_pos[1]) == min(abs(as.numeric(colnames(bed_grid))-dredge_start_pos[1])))
    loc_north <- which(abs(as.numeric(row.names(bed_grid))-dredge_start_pos[2]) == min(abs(as.numeric(row.names(bed_grid))-dredge_start_pos[2])))
    
    
    bed_level_at_location <- bed_grid[loc_north, loc_east] # look up the surface level
    dredge_depth_at_location <- dredge_surface[loc_north, loc_east] # look up the required dredge level
    
    grid_resolution <- as.numeric(colnames(bed_grid)[2]) - as.numeric(colnames(bed_grid)[1])
    
    dredge_vol_at_location <- ((bed_level_at_location - dredge_depth_at_location)) * grid_resolution #m3 total volume at location (grid cell)
    
    total_buckets_at_grid_loc <- ceiling(dredge_vol_at_location / (bucket_size * bucket_fill_efficiency))
    
    
    # START LOOP HERE - TIME DREDGING STARTS AT SURFACE
    
    current_time <- 0
    
    average_cut_depth_at_grid_loc <- (bucket_size * bucket_fill_efficiency) / grid_resolution # i = bucket size

    water_level_at_time <- as.numeric(water_level %>% filter(date_time == current_time) %>% select(y)) # look up water level @ current time
        
    depth_of_water_at_location <- water_level_at_time - bed_level_at_location
    
    time_to_seabed <- depth_of_water_at_location / bucket_speed # seconds
    
    time_to_cut <- bucket_cut_depth * bucket_speed_cutting # seconds to make the cut
    
    (cycle_time_cut <-(hopper_height_above_water * bucket_speed) + # assumes bucket is @ hopper height above dredge position
                      time_to_seabed + # lower bucket to seabed
                      time_to_cut + # cut/dredge
                      time_to_seabed + # raise bucket to surface
                      (hopper_height_above_water * bucket_speed) + # lift to height
                      time_to_slew + # slew to hopper
                      bucket_emptying_time + # seconds
                      time_to_slew) # ready for next cut
    
    vol_cut <- bucket_size * bucket_fill_efficiency
    
    if(hms::as_hms(t) >= hms::as_hms(min(hms::as_hms(time_constraint))) &
      hms::as_hms(t) <= hms::as_hms(max(hms::as_hms(time_constraint)))) { # Operating within operational hours
      
    
    if(sum(temp[n:i, 2], na.rm = TRUE) < hopper_actual_capacity) { # Check if still space in the hopper dredge
    
      vol_cut_cum <- sum(temp[n:i, 2], na.rm = TRUE)
      
      t <- t + cycle_time_cut
      temp[i, 1] <- t
      temp[i, 2] <- vol_cut
      temp[i, 3] <- vol_cut_cum
      temp[i, 4] <- j
      
    } else {
      
      j <- j + 1 # Hopper Cycle number
      t <- t + cycle_time_hopper
      
      vol_cut_cum <- vol_cut 
      
      temp[i, 1] <- t
      temp[i, 2] <- vol_cut
      temp[i, 3] <- vol_cut_cum #sum(temp[n:i, 2], na.rm = TRUE)
      temp[i, 4] <- j # Add cycle number
      
      vol_cut_cum <- vol_cut_cum + vol_cut
      
      n <- i - 1
      
      }
      
      } else { # Operating outside of operational hours
      
      t <- t + (hms::as_hms(max(hms::as_hms(time_constraint))) - hms::as_hms(min(hms::as_hms(time_constraint))))
      temp[i, 1] <- t
      temp[i, 2] <- vol_cut
      temp[i, 3] <- vol_cut_cum #sum(temp[n:i, 2], na.rm = TRUE)
      temp[i, 4] <- j
      
      vol_cut_cum <- vol_cut_cum + vol_cut
      
      n <- i - 1
      
      }
    
    }
    
    return(temp)
    
    }
}

# dredge_vols <- as.data.frame(temp)
# 
# names(dredge_vols) <- c("time", "cut.vol", "cum.hopper.vol", "hopper.number")
# 
# dredge_vols$time <- as.POSIXct(dredge_vols$time, origin = "1970-01-01")
# 
# sum(temp[, 2], na.rm = TRUE)
# 
# completion_time <- as.POSIXct(max(temp[, 1]), origin = "1970-01-01")
# (duration <- as.POSIXct(max(temp[, 1]), origin = "1970-01-01") - start_date_time) # hours dredging time (multiply by 3600 to get seconds)

#' Dredge Cycle Time
#'
#' @param water_depth_at_location 
#' @param bucket_size 
#' @param boom_length 
#' @param bucket_cut_depth 
#' @param bucket_fill_efficiency 
#' @param bucket_emptying_time 
#' @param hopper_height_above_water 
#' @param slew_rate 
#' @param slew_angle 
#' @param bucket_speed 
#' @param bucket_speed_cutting 
#' @param material_bulk_factor 
#'
#' @return
#' @export
#'
#' @examples
dredge_cycle_time <- function(
                              water_depth_at_location = NULL, # Function of water level and seabed elevation
                              bucket_size = 3, #m3
                              boom_length = 8, #m
                              bucket_cut_depth = 2, #m
                              bucket_fill_efficiency = 0.8, #proportion
                              bucket_emptying_time = 3, #seconds
                              hopper_height_above_water = 2, #m
                              slew_rate = 2, # rpm
                              slew_angle = 90, # degrees between dredge and hopper
                              bucket_speed = 1, #m/s
                              bucket_speed_cutting = 2, #m/s cutting/dredging speed once at depth
                              material_bulk_factor = 1.15 # used for volume estimates
                              ){
  
  slew_angle_dec_degrees <- slew_angle / 360
  
  time_to_slew <- (slew_angle_dec_degrees * 60) /  slew_rate # seconds
  
  time_to_seabed <- water_depth_at_location / bucket_speed # seconds
  
  time_to_cut <- bucket_cut_depth * bucket_speed_cutting # seconds to make the cut
  
  (cycle_time_cut <-(hopper_height_above_water * bucket_speed) + # assumes bucket is @ hopper height above dredge position
      time_to_seabed + # lower bucket to seabed
      time_to_cut + # cut/dredge
      time_to_seabed + # raise bucket to surface
      (hopper_height_above_water * bucket_speed) + # lift to height
      time_to_slew + # slew to hopper
      bucket_emptying_time + # seconds
      time_to_slew) # ready for next cut
  
  
  
}
