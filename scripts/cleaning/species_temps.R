library(dplyr)

# TODO: combine all site occurrences in separate script
# TODO: simplify this script

# Read in temperature and occurrence data
site_annual_temps = read.csv("data/site_annual_temps.csv")
occurrences_portal = read.csv("data/portal/clean/occurrences.csv")
occurrences_fray_jorge = read.csv("data/frayjorge/clean/occurrences.csv")

# Get annual temperature for each species
sites = c("portal", "fray_jorge")
occurrences_with_temp = data.frame(species = factor(), yr = numeric(), mass_mean = numeric(), avg_temp = numeric(), site = factor())
for(each_site in sites){
  annual_temps_by_site = site_annual_temps %>% 
    filter(site == each_site) %>% 
    data.frame() %>% 
    select(year, avg_temp)
  occurrences_by_site = eval(parse(text = paste("occurrences", each_site, sep = "_")))
  occurrences_by_site = occurrences_by_site %>%
    group_by(species) %>%
    add_tally() %>%
    filter(n > 5) %>%
    ungroup() %>%
    select(species, yr, mass_mean)
  occurrences_with_temp_by_site = occurrences_by_site %>%
    left_join(annual_temps_by_site, by = c("yr" = "year"))
  occurrences_with_temp_by_site$site = as.factor(each_site)
  occurrences_with_temp = rbind(occurrences_with_temp, occurrences_with_temp_by_site)
}

# Save combined occurrences/temperature data
occurrences_with_temp_path = "data/occurrences_with_temp.csv"
write.csv(occurrences_with_temp, occurrences_with_temp_path)
