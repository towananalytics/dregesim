bucket_size <- 3 #m3
boom_length <- 8 #m
bucket_cut_depth <- 2 #m
bucket_fill_efficiency <- 0.8 #proportion
bucket_emptying_time <- 3 #seconds
hopper_height_above_water <- 2 #m
slew_rate <- 2 # rpm
slew_angle <- 90 # degrees

dregde_vol <- 2000 # m3 material to be dredged

bucket_speed <- 1 #m/s
bucket_speed_cutting <- 2 #m/s cutting/dredging speed once at depth

material_bulk_factor <- 1.15 # used for volume estimates


# Time Constraints --------------------------------------------------------

time_constraint <- c("07:00:00", "19:00:00")

# Dredge start position ---------------------------------------------------

easting_start <- 665818
northing_start <- 7753509

start_date_time <- as.POSIXct("2021-08-30 07:00:00")


# Hopper details ----------------------------------------------------------

hopper_capacity <- 250 #m3
hopper_efficiency <- 0.8
hopper_actual_capacity <- hopper_capacity * hopper_efficiency 
cycle_time_hopper <- 9764.118 # seconds

# Calculations ------------------------------------------------------------

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

for (i in 1:total_buckets) {
  

# Find row index for where dredge is located within seabed:
loc_east <- which(abs(as.numeric(colnames(bed_grid))-dredge_start_pos[1]) == min(abs(as.numeric(colnames(bed_grid))-dredge_start_pos[1])))
loc_north <- which(abs(as.numeric(row.names(bed_grid))-dredge_start_pos[2]) == min(abs(as.numeric(row.names(bed_grid))-dredge_start_pos[2])))


bed_level_at_location <- bed_grid[loc_north, loc_east] # look up the surface level

water_level_at_time <- as.numeric(water_level %>% filter(date_time == start_date_time) %>% select(y)) # look up water level
    
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
  

if(sum(temp[n:i, 2], na.rm = TRUE) < hopper_actual_capacity) {

  vol_cut_cum <- sum(temp[n:i, 2], na.rm = TRUE)
  
  t <- t + cycle_time_cut
  temp[i, 1] <- t
  temp[i, 2] <- vol_cut
  temp[i, 3] <- vol_cut_cum
  temp[i, 4] <- j
  
} else {
  
  j <- j + 1 # Hopper Cycle
  t <- t + cycle_time_hopper
  
  vol_cut_cum <- vol_cut 
  
  temp[i, 1] <- t
  temp[i, 2] <- vol_cut
  temp[i, 3] <- vol_cut_cum #sum(temp[n:i, 2], na.rm = TRUE)
  temp[i, 4] <- j
  
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


dredge_vols <- as.data.frame(temp)

names(dredge_vols) <- c("time", "cut.vol", "cum.hopper.vol", "hopper.number")

dredge_vols$time <- as.POSIXct(dredge_vols$time, origin = "1970-01-01")

sum(temp[, 2], na.rm = TRUE)

completion_time <- as.POSIXct(max(temp[, 1]), origin = "1970-01-01")
(duration <- as.POSIXct(max(temp[, 1]), origin = "1970-01-01") - start_date_time) # hours dredging time (multiply by 3600 to get seconds)

