lat_conversion = function(coord_lat){
  coord_lat_double = coord_lat * 2
  grid_lat = 180 - coord_lat_double
  return(grid_lat)
}
