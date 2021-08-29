bucket_size <- 3 #m3
boom_length <- 8 #m
bucket_cut_depth <- 2 #m
bucket_fill_efficiency <- 0.8
hopper_height_above_water <- 2 #m
slew_rate <- 1 # rpm
slew_angle <- 90 # degrees

slew_angle_dec_degrees <- slew_angle / 360

time_to_slew <- slew_angle_dec_degrees * 60 # seconds

bucket_speed <- 1 #m/s


# Dredge start position ---------------------------------------------------


easting_start <- 665818
northing_start <- 7753509

start_date_time <- as.POSIXct("2021-08-30 07:00:00")

dredge_start_pos <- matrix(c(easting_start, northing_start))

# Find row index for where dredge is located within seabed:
loc_east <- which(abs(as.numeric(colnames(bed_grid))-dredge_start_pos[1])==min(abs(as.numeric(colnames(bed_grid))-dredge_start_pos[1])))
loc_north <- which(abs(as.numeric(row.names(bed_grid))-dredge_start_pos[2])==min(abs(as.numeric(row.names(bed_grid))-dredge_start_pos[2])))


bed_level_at_location <- bed_grid[loc_north, loc_east]

water_level_at_time <- as.numeric(water_level %>% filter(date_time == start_date_time) %>% select(y))
    
depth_of_water_at_location <- water_level_at_time - bed_level_at_location 

time_to_seabed <- depth_of_water_at_location / bucket_speed # seconds

time_to_cut <- bucket_cut_depth * bucket_speed # seconds

slew_time <- time_to_slew * 2

time_to_lower_cut_raise <- time_to_seabed * 2 + time_to_cut + (2 * hopper_height_above_water * bucket_speed) + time_to_slew# seconds

vol_cut <- bucket_size * bucket_fill_efficiency

