#' Longitudinal coordinate conversion
#' 
#' Converts a given longitude into the temperature grid format. 
#' 
#' @param coord_lon numerical longitude to input
#' 
#' @return numerical longitude in grid format
#' 
#' @examples 
#' lon_conversion(-109.08013)
#' 
#' @export
#' 
lon_conversion = function(coord_lon){
  coord_lon_double = coord_lon * 2
  if(coord_lon_double < 0){
    grid_lon = 720 + coord_lon_double
  } else {
    grid_lon = coord_lon_double
  }
  return(grid_lon)
}
