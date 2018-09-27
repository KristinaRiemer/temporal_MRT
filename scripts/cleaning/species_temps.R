library(dplyr)

# TODO: read in temperature and mass dataframes

sites = c("portal", "fray_jorge")
all_annual_masses = data.frame(species = factor(), yr = numeric(), mass_mean = numeric(), avg_temp = numeric(), site = factor())
for(each_site in sites){
  site_annual_temps = annual_temps %>% 
    filter(site == each_site) %>% 
    data.frame() %>% 
    select(year, avg_temp)
  by_sp_yr_name = eval(parse(text = paste("by_sp_yr", each_site, sep = "_")))
  site_annual_masses = by_sp_yr_name %>%
    group_by(species) %>% 
    add_tally() %>% 
    filter(n > 5) %>%
    ungroup() %>% 
    select(species, yr, mass_mean)
  site_annual_masses = site_annual_masses %>%
    left_join(site_annual_temps, by = c("yr" = "year"))
  site_annual_masses$site = as.factor(each_site)
  all_annual_masses = rbind(all_annual_masses, site_annual_masses)
}
all_annual_masses