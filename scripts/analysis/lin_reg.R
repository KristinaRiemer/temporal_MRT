library(dplyr)
library(tidyr)
library(broom)
library(purrr)
library(temporalMRTfxs)

# Read in occurrences with temperature data
occurrences_with_temp_path = "data/occurrences_with_temp.csv"
occurrences_with_temp = read.csv(occurrences_with_temp_path)

# Create dataframe of slope for each species mass over time
sites = unique(occurrences_with_temp$site)
all_lm_mass = data.frame(species = factor(), slope = numeric(), site = factor(), scientific_name = factor())
for(each_site in sites){
  site_annual_masses = occurrences_with_temp[occurrences_with_temp$site == each_site,]
  site_lm_mass = regression_mass_slope(site_annual_masses)
  site_lm_mass$site = as.factor(each_site)
  species_codes = site_annual_masses %>% 
    select(species, scientific_name) %>% 
    distinct(species, scientific_name)
  site_lm_mass = left_join(site_lm_mass, species_codes, by = c("species" = "species"))
  all_lm_mass = rbind(all_lm_mass, site_lm_mass)
}

# Create dataframe of r and p values for each species temp-mass relationship
all_lm_mrt = data.frame(species = factor(), r.squared = numeric(), slope = numeric(), p.value = numeric(), r = numeric(), site = factor(), scientific_name = factor())
for(each_site in sites){
  site_annual_masses = occurrences_with_temp[occurrences_with_temp$site == each_site,]
  site_lm_tidy_mrt = regression_mrt_p(site_annual_masses)
  site_lm_glance_mrt = regression_mrt_r2(site_annual_masses)
  site_lm_mrt = site_lm_glance_mrt %>%
    full_join(site_lm_tidy_mrt, by = "species")
  site_lm_mrt = site_lm_mrt %>%
    mutate(r = case_when(slope > 0 ~ sqrt(r.squared),
                         slope < 0 ~ -sqrt(r.squared)))
  site_lm_mrt$site = as.factor(each_site)
  species_codes = site_annual_masses %>% 
    select(species, scientific_name) %>% 
    distinct(species, scientific_name)
  site_lm_mrt = left_join(site_lm_mrt, species_codes, by = c("species" = "species"))
  all_lm_mrt = rbind(all_lm_mrt, site_lm_mrt)
}

# Save linear regression results dataframe
lin_reg_mass_path = "data/lin_reg_mass.csv"
write.csv(all_lm_mass, lin_reg_mass_path)

lin_reg_mrt_path = "data/lin_reg_mrt.csv"
write.csv(all_lm_mrt, lin_reg_mrt_path)
