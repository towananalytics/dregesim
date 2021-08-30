# split hopper
hopper_capacity <- 250 # m3
sailing_distance <- 15 #km to disposal ground
discharge_time <- 15 #seconds
sail_spd_full <- 6 #knots
sail_spd_empty <- 6 #knots
attach_line_time <- 30 # seconds - time to tie up to dredge before dredging can commence

speed_conversion_kmh <- 1.852 # knots to km/hr knots * conversion = m/s
speed_conversion_ms <- 0.51445 # knots to m/s knots * conversion = m/s

sailing_time <- (sailing_distance * 1000) / (sail_spd_full * speed_conversion_ms) # sailing time to/from disposal grounds seconds

(cycle_time_hopper <- sailing_time + #seconds outbound travel time
                      discharge_time + # seconds discharge time
                      sailing_time + #seconds inbound travel time
                      attach_line_time 
  ) # seconds depart dredge and return

(cycle_time_hopper / 3600) # hrs
