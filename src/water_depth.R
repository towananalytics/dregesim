#' Title
#'
#' @param water_levels 
#' @param current_time 
#' @param bed_grid 
#'
#' @return
#' @export
#'
#' @examples
#' 
water_depth <- function(
  loc_north = NULL,
  loc_east = NULL,
  water_levels = NULL,
  current_time = NULL,
  bed_grid = NULL # The existing seabed matrix
  ){
  
  water_level_at_time <- mean(water_levels[which(abs(water_levels$date_time - current_time) == min(abs(water_levels$date_time - current_time))), "y"])
  # water_level_at_time <- as.numeric(water_levels %>% filter(date_time == current_time) %>% select(y)) # look up water level @ current time
  
  # bed_level_at_location <- bed_grid[loc_north, loc_east] # look up the surface level
  # Look up the closest depth based on the reference position being used
  # Where the position is equidistant between the grid references two depth values are returned - the following function takes the mean in this instance
  bed_level_at_location <- mean(bed_grid[which(abs(as.numeric(rownames(seabed_surface)) - loc_north) == min(abs(as.numeric(rownames(seabed_surface)) - loc_north))), 
                                         which(abs(as.numeric(colnames(seabed_surface)) - loc_east) == min(abs(as.numeric(colnames(seabed_surface)) - loc_east)))]) 
  
  depth_of_water_at_location <- water_level_at_time - bed_level_at_location
  
  return(depth_of_water_at_location)
  
}
