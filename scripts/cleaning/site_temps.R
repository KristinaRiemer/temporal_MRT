library(dplyr)

# TODO: read in site locations data

#### Convert coordinates to grid format ####

lon_conversion = function(coord_lon){
  coord_lon_double = coord_lon * 2
  if(coord_lon_double < 0){
    grid_lon = 720 + coord_lon_double
  } else {
    grid_lon = coord_lon_double
  }
  return(grid_lon)
}

lat_conversion = function(coord_lat){
  coord_lat_double = coord_lat * 2
  grid_lat = 180 - coord_lat_double
  return(grid_lat)
}

site_locations = site_locations %>% 
  rowwise() %>% 
  mutate(grid_lon = lon_conversion(lon), 
         grid_lat = lat_conversion(lat))
site_locations

# TODO: remove this temporary fix
site_locations$grid_lon[2] = site_locations$grid_lon[2] + 2
site_locations

#### Plot all site locations on one month of temp data ####

if(!file.exists("../data/temperature/air.mon.mean.v401.nc")){
  download.file("ftp://ftp.cdc.noaa.gov/Datasets/udel.airt.precip/air.mon.mean.v401.nc", "../data/temperature/air.mon.mean.v401.nc")
}

temperature_df = nc_open("../data/temperature/air.mon.mean.v401.nc")

temp_variable_all = ncvar_get(temperature_df, attributes(temperature_df$var)$names)

if(!file.exists("../data/temperature/single_year_temp.csv")){
  single_year_temp = expand.grid(1:359, 1:719)
  names(single_year_temp) = c("lat", "lon")
  single_year_temp$temp = NA
  for(i in 1:nrow(single_year_temp)){
    single_year_temp[i, "temp"] = temp_variable_all[single_year_temp$lon[i], single_year_temp$lat[i], 1380]
  }
  write.csv(single_year_temp, "../data/temperature/single_year_temp.csv")
}

single_year_temp = read.csv("../data/temperature/single_year_temp.csv")

ggplot(single_year_temp, aes(lon, lat)) +
  geom_tile(aes(fill = temp)) +
  geom_point(data = site_locations, aes(x = grid_lon, y = grid_lat)) +
  scale_y_reverse()

#### Get mean annual temp for each year for all sites ####

months = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
years = c()
for(year in 1900:2014){
  year_repeats = rep(year, 12)
  years = c(years, year_repeats)
}
indices = data.frame(index = seq(1, 1380), month = rep(months, 115), year = years)
indices$month = factor(indices$month, levels = month.abb)
indices

monthly_temps = data.frame(site = character(), month = numeric(), temp = numeric())
for(site in site_locations$site_name){
  lon = site_locations$grid_lon[site_locations$site_name == site]
  lat = site_locations$grid_lat[site_locations$site_name == site]
  for(index in indices$index){
    temp = temp_variable_all[lon, lat, index]
    each_temp = data.frame(site = site, index = index, temp = temp)
    monthly_temps = rbind(monthly_temps, each_temp)
  }
}
monthly_temps

monthly_temps = monthly_temps %>% 
  left_join(indices, by = "index")
monthly_temps

annual_temps = monthly_temps %>%
  group_by(site, year) %>% 
  summarise(avg_temp = mean(temp), 
            avg_temp_sd = sd(temp))
annual_temps

