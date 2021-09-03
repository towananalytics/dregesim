
easting_start <- 665816.1774
northing_start <- 7753500.0084
grid_res <- 10
dist_x <- 100
dist_y <- 100

easting_end <- floor(easting_start) + dist_x
northing_end <- floor(northing_start) + dist_y

bed_grid <- matrix(rnorm((dist_y / grid_res + 1) * (dist_x / grid_res + 1)) + 10,
                   nrow = dist_y / grid_res + 1, 
                   ncol = dist_x / grid_res + 1,
                   dimnames = list(
                       seq(from = floor(northing_start),
                           to = floor(northing_start) + dist_y,
                           by = grid_res),
                       seq(from = floor(easting_start),
                           to = floor(easting_start) + dist_x,
                           by = grid_res)
                   )
)


