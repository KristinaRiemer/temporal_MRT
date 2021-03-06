library(ncdf4)
library(dplyr)
library(ggplot2)
library(temporalMRTfxs)

# Read in site locations data
site_locations = read.csv("data/site_locations.csv")

# Download and read in all temperature data
temperature_path = "data/temperature/air.mon.mean.v401.nc"
if(!file.exists(temperature_path)){
  download.file(url = "ftp://ftp.cdc.noaa.gov/Datasets/udel.airt.precip/air.mon.mean.v401.nc", 
                destfile = temperature_path)
}

temperature_nc = nc_open(temperature_path)
temperature_df = ncvar_get(nc = temperature_nc, 
                              varid = attributes(temperature_nc$var)$names)

# Convert coordinates to grid format
site_locations = site_locations %>% 
  rowwise() %>% 
  mutate(grid_lon = lon_conversion(lon), 
         grid_lat = lat_conversion(lat))

# TODO: remove this temporary fix once Fray Jorge location is more accurate
site_locations$grid_lon[2] = site_locations$grid_lon[2] + 2

# Create, save, and read in csv for a single month of temperature data
single_month_path = "data/temperature/single_month.csv"
if(!file.exists(single_month_path)){
  single_month_temp = expand.grid(1:359, 1:719)
  names(single_month_temp) = c("lat", "lon")
  single_month_temp$temp = NA
  for(i in 1:nrow(single_month_temp)){
    single_month_temp[i, "temp"] = temperature_df[single_month_temp$lon[i], 
                                                  single_month_temp$lat[i], 
                                                  1380]
  }
  write.csv(single_month_temp, single_month_path)
}
single_month_temp = read.csv(single_month_path)

# Plot site locations on top of single month of temperature
single_month_temp$Temperature = single_month_temp$temp
site_locations$Site = c("Portal", "Fray Jorge", "Sevilleta")
site_locations_map = ggplot(single_month_temp, aes(lon, lat)) +
  geom_tile(aes(fill = Temperature)) +
  geom_point(data = site_locations, aes(x = grid_lon, y = grid_lat, 
                                        shape = Site), size = 2) +
  theme(axis.title = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank()) +
  scale_y_reverse()

# Create temperature month index lookup table
months = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
years = c()
for(year in 1900:2014){
  year_repeats = rep(year, 12)
  years = c(years, year_repeats)
}
indices = data.frame(index = seq(1, 1380), month = rep(months, 115), year = years)
indices$month = factor(indices$month, levels = month.abb)

# Get all monthly temperatures for each site
monthly_temps = data.frame(site = character(), month = numeric(), temp = numeric())
for(site in site_locations$site_name){
  lon = site_locations$grid_lon[site_locations$site_name == site]
  lat = site_locations$grid_lat[site_locations$site_name == site]
  for(index in indices$index){
    temp = temperature_df[lon, lat, index]
    each_temp = data.frame(site = site, index = index, temp = temp)
    monthly_temps = rbind(monthly_temps, each_temp)
  }
}
monthly_temps = monthly_temps %>% 
  left_join(indices, by = "index")

# Get all annual temperatures for each site
annual_temps = monthly_temps %>%
  group_by(site, year) %>% 
  summarise(avg_temp = mean(temp), 
            avg_temp_sd = sd(temp))

# Save map of site locations & temperatures
site_locations_map_path = "plots/site_locations_map.png"
ggsave(site_locations_map_path, plot = site_locations_map, width = 10, height = 7)

# Save site annual temperatures
annual_temps_path = "data/site_annual_temps.csv"
write.csv(annual_temps, annual_temps_path)
