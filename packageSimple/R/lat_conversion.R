#' Latitudinal coordinate conversion
#' 
#' Converts a given latitude into the temperature grid format. 
#' 
#' @param coord_lat numerical latitude to input
#' 
#' @return numerical latitude in grid format
#' 
#' @examples 
#' lat_conversion(31.93774)
#' 
#' @export
#' 
lat_conversion = function(coord_lat){
  coord_lat_double = coord_lat * 2
  grid_lat = 180 - coord_lat_double
  return(grid_lat)
}
