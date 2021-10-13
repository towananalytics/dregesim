

#' dredge_volumes
#' 
#' Estimate the volume of material between two surfaces (matrices)
#'
#' @param seabed_surface 
#' @param dredge_surface 
#'
#' @return
#' @export
#'
#' @examples
dredge_volumes <- function(seabed_surface = NULL,
                           dredge_surface = NULL
                           ){
  
  return(seabed_surface - dredge_surface)
  
}

#' volume_at_location
#' 
#' Return the dredge volume value according to the cell location provided
#'
#' @param dredge.volumes 
#' @param loc_east 
#' @param loc_north 
#'
#' @return
#' @export
#'
#' @examples
dredge_volume_at_location <- function(dredge.volumes = NULL,
                                      loc_east = NULL,
                                      loc_north = NULL){
  
  return(mean(dredge.volumes[which(abs(as.numeric(rownames(dredge.volumes)) - loc_north) == min(abs(as.numeric(rownames(dredge.volumes)) - loc_north))), 
                                         which(abs(as.numeric(colnames(dredge.volumes)) - loc_east) == min(abs(as.numeric(colnames(dredge.volumes)) - loc_east)))])) 
  
}
