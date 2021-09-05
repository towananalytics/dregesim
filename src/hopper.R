# split hopper

#' Title
#'
#' @param hopper_capacity an integer specifying the number of split hoppers to be used
#' @param hopper_efficiency load efficiency as a vector of length `hopper_capacity` as a proportion (>0 hopper_efficiency <1)
#' @param sailing_distance integer value as distance to spoil ground in kilometers
#' @param discharge_time hopper discharge time in seconds as a vector with length `hopper_capacity`
#' @param sail_spd_full fully loaded sailing speed in knots as a vector with length `hopper_capacity` 
#' @param sail_spd_empty empty sailing speed in knots as a vector with length `hopper_capacity`
#' @param attach_line_time time to moor hopper to dredge in seconds as a vector with length `hopper_capacity`
#' @param speed_conversion_kmh knots to km/hr knots * conversion = m/s
#' @param speed_conversion_ms knots to m/s knots * conversion = m/s
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
    

# Comment this next section out -------------------------------------------

    # number_of_hoppers = 2
    # hopper_capacity = c(250, 130) # m3 NOTE - Vector lengths must match the number of hoppers 
    # hopper_efficiency = c(0.8, 0.8)
    # sailing_distance = 15 #km to disposal ground
    # discharge_time = c(15, 45) #seconds
    # sail_spd_full = c(6, 12) #knots
    # sail_spd_empty = c(6, 50) #knots
    # attach_line_time = c(15, 30) # seconds - time to tie up to dredge before dredging can commence
    # speed_conversion_kmh = 1.852 # knots to km/hr knots * conversion = m/s
    # speed_conversion_ms = 0.51445 # knots to m/s knots * conversion = m/s    
    

# Build Matrix for Hopper Details -----------------------------------------
    
    hopper_details <- matrix(ncol = 11, nrow = number_of_hoppers)

      hopper_details[, 1] <- 1:number_of_hoppers # Hopper number #  rep_len(hopper_capacity, n)
      hopper_details[, 2] <- hopper_capacity #hopper_capacity[n]
      hopper_details[, 3] <- hopper_efficiency #hopper_efficiency[n]
      hopper_details[, 4] <- discharge_time
      hopper_details[, 5] <- sail_spd_full
      hopper_details[, 6] <- sail_spd_empty
      hopper_details[, 7] <- attach_line_time
      hopper_details[, 8] <- hopper_details[, 2] * hopper_details[, 3]
      hopper_details[, 9] <- (sailing_distance * 1000) / (hopper_details[, 5] * speed_conversion_ms) # sail time full
      hopper_details[, 10] <- (sailing_distance * 1000) / (hopper_details[, 6] * speed_conversion_ms) # sail time empty
      hopper_details[, 11] <- hopper_details[, 9] + hopper_details[, 10] + hopper_details[, 4] + (hopper_details[, 7] * 2)
      
     colnames(hopper_details) <- c(
                                 "hopper.ID", 
                                 "hopper.capacity", 
                                 "capacity.efficiency", 
                                 "discharge.time",
                                 "speed.full",
                                 "speed.empty",
                                 "line.time",
                                 "actual.capacity",
                                 "sailing.time.full",
                                 "sailing.time.empty",
                                 "cycle.time"
                                 )
     
     

# Calculate Cycle Times ---------------------------------------------------

  
    # for(n in 1:number_of_hoppers){
    # 
    #   #hopper_actual_capacity <- hopper_capacity * hopper_efficiency 
    #     
    #   #sailing_time <- (sailing_distance * 1000) / (sail_spd_full * speed_conversion_ms) # sailing time to/from disposal grounds seconds
    #   
    #   # (cycle_time_hopper <- sailing_time + #seconds outbound travel time
    #   #                       discharge_time + # seconds discharge time
    #   #                       sailing_time + #seconds inbound travel time
    #   #                       attach_line_time 
    #   #  ) # seconds depart dredge and return
    #   
    #   # print(n)
    #   
    # }
      
      return(hopper_details[, c("hopper.ID", "actual.capacity", "cycle.time")])
      
      }

#(cycle_time_hopper / 3600) # hrs 
  