# split hopper

#' Title
#'
#' @param hopper_capacity 
#' @param hopper_efficiency 
#' @param sailing_distance 
#' @param discharge_time 
#' @param sail_spd_full 
#' @param sail_spd_empty 
#' @param attach_line_time 
#' @param speed_conversion_kmh 
#' @param speed_conversion_ms 
#'
#' @return
#' @export
#'
#' @examples
  hopper_split <- function(
                          number_of_hoppers = 2,
                          hopper_capacity = c(250, 130), # m3 NOTE - Vector lengths must match the number of hoppers 
                          hopper_efficiency = c(0.8, 0.8),
                          sailing_distance = 15, #km to disposal ground
                          discharge_time = c(15, 45), #seconds
                          sail_spd_full = c(6, 12), #knots
                          sail_spd_empty = c(6, 12), #knots
                          attach_line_time = c(15, 30), # seconds - time to tie up to dredge before dredging can commence
                          speed_conversion_kmh = 1.852, # knots to km/hr knots * conversion = m/s
                          speed_conversion_ms = 0.51445 # knots to m/s knots * conversion = m/s
                          ){
    

# Build Matrix for Hopper Details -----------------------------------------
    
    hopper_details <- matrix(ncol = 7, nrow = number_of_hoppers)

      hopper_details[, 1] <- 1:number_of_hoppers # Hopper number #  rep_len(hopper_capacity, n)
      hopper_details[, 2] <- hopper_capacity #hopper_capacity[n]
      hopper_details[, 3] <- hopper_efficiency #hopper_efficiency[n]
      hopper_details[, 4] <- discharge_time
      hopper_details[, 5] <- sail_spd_full
      hopper_details[, 6] <- sail_spd_empty
      hopper_details[, 7] <- attach_line_time

    

# Calculate Cycle Times ---------------------------------------------------

  
    for(n in 1:number_of_hoppers){
    
      hopper_actual_capacity <- hopper_capacity * hopper_efficiency 
        
      sailing_time <- (sailing_distance * 1000) / (sail_spd_full * speed_conversion_ms) # sailing time to/from disposal grounds seconds
      
      (cycle_time_hopper <- sailing_time + #seconds outbound travel time
                            discharge_time + # seconds discharge time
                            sailing_time + #seconds inbound travel time
                            attach_line_time 
        ) # seconds depart dredge and return
      
      print(n)
      
    }
      
      return(cycle_time_hopper)
      
      }

(cycle_time_hopper / 3600) # hrs 