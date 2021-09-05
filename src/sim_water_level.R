

#' Title
#'
#' @param start_date_time e.g. as.POSIXct("2021-08-30 07:00:00")
#' @param time_interval 
#' @param days_of_readings 
#'
#' @return
#' @export
#'
#' @examples
#' 
simulated_water_level <- function(
                                  start_date_time = as.POSIXct("2021-08-30 07:00:00"),
                                  time_interval = 20, #minutes
                                  days_of_readings = 1
                                  ){

  library(dplyr)
  
  t=seq(0, days_of_readings * 24 * 3600, (time_interval/60) * 3600) # time in seconds
  
  y=sin(t)+15
  plot(t, y, type="l", xlab="time", ylab="Water Level (m)")
  
  water_level <- as.data.frame(cbind(t, y))
  water_level <- water_level %>% mutate(date_time = start_date_time + t)
  
  return(water_level)

}


