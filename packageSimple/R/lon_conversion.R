lon_conversion = function(coord_lon){
  coord_lon_double = coord_lon * 2
  if(coord_lon_double < 0){
    grid_lon = 720 + coord_lon_double
  } else {
    grid_lon = coord_lon_double
  }
  return(grid_lon)
}
