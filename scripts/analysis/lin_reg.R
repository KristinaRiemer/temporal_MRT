library(dplyr)
library(tidyr)
library(broom)

# Read in occurrences with temperature data
occurrences_with_temp_path = "data/occurrences_with_temp.csv"
occurrences_with_temp = read.csv(occurrences_with_temp_path)

# Create dataframe of r and p values for all species at each site
# TODO: break this code up into smaller chunks
sites = unique(occurrences_with_temp$site)
all_lm = data.frame(species = factor(), r.squared = numeric(), slope = numeric(), p.value = numeric(), r = numeric(), site = factor(), scientific_name = factor())
for(each_site in sites){
  site_annual_masses = occurrences_with_temp[occurrences_with_temp$site == each_site,]
  site_lm_tidy = site_annual_masses %>%
    nest(-species) %>% 
    mutate(fit = purrr::map(data, ~lm(mass_mean ~ avg_temp, data = .)),
           results = purrr::map(fit, tidy)) %>%
    unnest(results) %>%
    filter(term == "avg_temp") %>%
    select(species, slope = estimate, p.value)
  site_lm_glance = site_annual_masses %>%
    nest(-species) %>%
    mutate(fit = purrr::map(data, ~lm(mass_mean ~ avg_temp, data = .)),
           results = purrr::map(fit, glance)) %>%
    unnest(results) %>%
    select(species, r.squared)
  site_lm = site_lm_glance %>%
    full_join(site_lm_tidy, by = "species")
  site_lm = site_lm %>%
    mutate(r = case_when(slope > 0 ~ sqrt(r.squared),
                         slope < 0 ~ -sqrt(r.squared)))
  site_lm$site = as.factor(each_site)
  species_codes = site_annual_masses %>% 
    select(species, scientific_name) %>% 
    distinct(species, scientific_name)
  site_lm = left_join(site_lm, species_codes, by = c("species" = "species"))
  all_lm = rbind(all_lm, site_lm)
}

# Save linear regression results dataframe
lin_reg_stats_path = "data/lin_reg_stats.csv"
write.csv(all_lm, lin_reg_stats_path)
