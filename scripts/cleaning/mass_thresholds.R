library(dplyr)
library(tidyr)
library(broom)
library(ggplot2)
library(purrr)
library(cowplot)

# Read in temperature and occurrence data
site_annual_temps = read.csv("data/site_annual_temps.csv")
occurrences = read.csv("data/occurrences.csv")

# Get annual temperature for each species
occurrences_with_temp = data.frame(species = factor(), yr = numeric(), mass_mean = numeric(), avg_temp = numeric(), site = factor())
for(each_site in unique(occurrences$site)){
  annual_temps_by_site = site_annual_temps %>% 
    filter(site == each_site) %>% 
    data.frame() %>% 
    select(year, avg_temp)
  occurrences_by_site = occurrences %>% 
    filter(site == each_site) %>% 
    select(species, yr, inds, mass_mean, mass_sd, scientific_name)
  occurrences_with_temp_by_site = occurrences_by_site %>%
    left_join(annual_temps_by_site, by = c("yr" = "year"))
  occurrences_with_temp_by_site$site = as.factor(each_site)
  occurrences_with_temp = rbind(occurrences_with_temp, occurrences_with_temp_by_site)
}

# Retain only species that meet 3 sets of thresholds
occurrences_with_temp_actual = occurrences_with_temp %>% 
  filter(!is.na(avg_temp)) %>% 
  filter(inds >= 15) %>% 
  group_by(site, species) %>% 
  mutate(num_yrs = n_distinct(yr)) %>% 
  ungroup() %>% 
  filter(num_yrs >= 5)

occurrences_with_temp_moreinds = occurrences_with_temp %>% 
  filter(!is.na(avg_temp)) %>% 
  filter(inds >= 30) %>% 
  group_by(site, species) %>% 
  mutate(num_yrs = n_distinct(yr)) %>% 
  ungroup() %>% 
  filter(num_yrs >= 5)

occurrences_with_temp_moreyrs = occurrences_with_temp %>% 
  filter(!is.na(avg_temp)) %>% 
  filter(inds >= 15) %>% 
  group_by(site, species) %>% 
  mutate(num_yrs = n_distinct(yr)) %>% 
  ungroup() %>% 
  filter(num_yrs >= 10)

# Create dataframe of slope for each species mass over time for three threshold datasets
sites = unique(occurrences_with_temp$site)

all_lm_mass_actual = data.frame(species = factor(), slope = numeric(), site = factor(), scientific_name = factor())
for(each_site in sites){
  site_annual_masses = occurrences_with_temp_actual[occurrences_with_temp_actual$site == each_site,]
  site_lm_mass = site_annual_masses %>% 
    nest(-species) %>% 
    mutate(fit = purrr::map(data, ~lm(mass_mean ~ yr, data = .)), 
           results = purrr::map(fit, tidy)) %>% 
    unnest(results) %>% 
    filter(term == "yr") %>% 
    select(species, slope = estimate)
  site_lm_mass$site = as.factor(each_site)
  species_codes = site_annual_masses %>% 
    select(species, scientific_name) %>% 
    distinct(species, scientific_name)
  site_lm_mass = left_join(site_lm_mass, species_codes, by = c("species" = "species"))
  all_lm_mass_actual = rbind(all_lm_mass_actual, site_lm_mass)
}

all_lm_mass_moreinds = data.frame(species = factor(), slope = numeric(), site = factor(), scientific_name = factor())
for(each_site in sites){
  site_annual_masses = occurrences_with_temp_moreinds[occurrences_with_temp_moreinds$site == each_site,]
  site_lm_mass = site_annual_masses %>% 
    nest(-species) %>% 
    mutate(fit = purrr::map(data, ~lm(mass_mean ~ yr, data = .)), 
           results = purrr::map(fit, tidy)) %>% 
    unnest(results) %>% 
    filter(term == "yr") %>% 
    select(species, slope = estimate)
  site_lm_mass$site = as.factor(each_site)
  species_codes = site_annual_masses %>% 
    select(species, scientific_name) %>% 
    distinct(species, scientific_name)
  site_lm_mass = left_join(site_lm_mass, species_codes, by = c("species" = "species"))
  all_lm_mass_moreinds = rbind(all_lm_mass_moreinds, site_lm_mass)
}

all_lm_mass_moreyrs = data.frame(species = factor(), slope = numeric(), site = factor(), scientific_name = factor())
for(each_site in sites){
  site_annual_masses = occurrences_with_temp_moreyrs[occurrences_with_temp_moreyrs$site == each_site,]
  site_lm_mass = site_annual_masses %>% 
    nest(-species) %>% 
    mutate(fit = purrr::map(data, ~lm(mass_mean ~ yr, data = .)), 
           results = purrr::map(fit, tidy)) %>% 
    unnest(results) %>% 
    filter(term == "yr") %>% 
    select(species, slope = estimate)
  site_lm_mass$site = as.factor(each_site)
  species_codes = site_annual_masses %>% 
    select(species, scientific_name) %>% 
    distinct(species, scientific_name)
  site_lm_mass = left_join(site_lm_mass, species_codes, by = c("species" = "species"))
  all_lm_mass_moreyrs = rbind(all_lm_mass_moreyrs, site_lm_mass)
}

all_lm_mass_actual$threshold = c("Actual")
all_lm_mass_moreinds$threshold = c("More individuals")
all_lm_mass_moreyrs$threshold = c("More years")
all_lm_mass = bind_rows(all_lm_mass_actual, all_lm_mass_moreinds, all_lm_mass_moreyrs)

all_lm_mass = all_lm_mass %>% 
  mutate(site_fancy = case_when(site == "portal" ~ "Portal", 
                                site == "frayjorge" ~ "Fray Jorge", 
                                site == "sevilleta" ~ "Sevilleta"))

# Plot slope values for three thresholds at all sites
slopes_plot = ggplot(all_lm_mass, aes(x = threshold, y = slope)) +
  geom_boxplot(color = "grey", outlier.shape = NA) +
  geom_jitter(width = 0.1) +
  ylim(c(-2, 2)) +
  labs(x = "Threshold", y = "Mass time series slope") +
  facet_grid(~site_fancy)

# Create dataframe of r values for each species mass over time for three threshold datasets
all_lm_mrt_actual = data.frame(species = factor(), r.squared = numeric(), slope = numeric(), p.value = numeric(), r = numeric(), site = factor(), scientific_name = factor())
for(each_site in sites){
  site_annual_masses = occurrences_with_temp_actual[occurrences_with_temp_actual$site == each_site,]
  site_lm_tidy_mrt = site_annual_masses %>%
    nest(-species) %>% 
    mutate(fit = purrr::map(data, ~lm(mass_mean ~ avg_temp, data = .)),
           results = purrr::map(fit, tidy)) %>%
    unnest(results) %>%
    filter(term == "avg_temp") %>%
    select(species, slope = estimate, p.value)
  site_lm_glance_mrt = site_annual_masses %>%
    nest(-species) %>%
    mutate(fit = purrr::map(data, ~lm(mass_mean ~ avg_temp, data = .)),
           results = purrr::map(fit, glance)) %>%
    unnest(results) %>%
    select(species, r.squared)
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
  all_lm_mrt_actual = rbind(all_lm_mrt_actual, site_lm_mrt)
}

all_lm_mrt_moreinds = data.frame(species = factor(), r.squared = numeric(), slope = numeric(), p.value = numeric(), r = numeric(), site = factor(), scientific_name = factor())
for(each_site in sites){
  site_annual_masses = occurrences_with_temp_moreinds[occurrences_with_temp_moreinds$site == each_site,]
  site_lm_tidy_mrt = site_annual_masses %>%
    nest(-species) %>% 
    mutate(fit = purrr::map(data, ~lm(mass_mean ~ avg_temp, data = .)),
           results = purrr::map(fit, tidy)) %>%
    unnest(results) %>%
    filter(term == "avg_temp") %>%
    select(species, slope = estimate, p.value)
  site_lm_glance_mrt = site_annual_masses %>%
    nest(-species) %>%
    mutate(fit = purrr::map(data, ~lm(mass_mean ~ avg_temp, data = .)),
           results = purrr::map(fit, glance)) %>%
    unnest(results) %>%
    select(species, r.squared)
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
  all_lm_mrt_moreinds = rbind(all_lm_mrt_moreinds, site_lm_mrt)
}

all_lm_mrt_moreyrs = data.frame(species = factor(), r.squared = numeric(), slope = numeric(), p.value = numeric(), r = numeric(), site = factor(), scientific_name = factor())
for(each_site in sites){
  site_annual_masses = occurrences_with_temp_moreyrs[occurrences_with_temp_moreyrs$site == each_site,]
  site_lm_tidy_mrt = site_annual_masses %>%
    nest(-species) %>% 
    mutate(fit = purrr::map(data, ~lm(mass_mean ~ avg_temp, data = .)),
           results = purrr::map(fit, tidy)) %>%
    unnest(results) %>%
    filter(term == "avg_temp") %>%
    select(species, slope = estimate, p.value)
  site_lm_glance_mrt = site_annual_masses %>%
    nest(-species) %>%
    mutate(fit = purrr::map(data, ~lm(mass_mean ~ avg_temp, data = .)),
           results = purrr::map(fit, glance)) %>%
    unnest(results) %>%
    select(species, r.squared)
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
  all_lm_mrt_moreyrs = rbind(all_lm_mrt_moreyrs, site_lm_mrt)
}

all_lm_mrt_actual$threshold = c("Actual")
all_lm_mrt_moreinds$threshold = c("More individuals")
all_lm_mrt_moreyrs$threshold = c("More years")
all_lm_mrt = bind_rows(all_lm_mrt_actual, all_lm_mrt_moreinds, all_lm_mrt_moreyrs)

all_lm_mrt = all_lm_mrt %>% 
  mutate(site_fancy = case_when(site == "portal" ~ "Portal", 
                                site == "frayjorge" ~ "Fray Jorge", 
                                site == "sevilleta" ~ "Sevilleta"))

# Plot r values for three thresholds at all sites
rs_plot = ggplot(all_lm_mrt, aes(x = threshold, y = r)) +
  geom_boxplot(color = "grey", outlier.shape = NA) +
  geom_jitter(width = 0.1) +
  labs(x = "Threshold", y = "Mass-temp relationship R") +
  facet_grid(~site_fancy)

# Combine and save slope and r plots
both_plots = plot_grid(slopes_plot, rs_plot, ncol = 1, labels = c("A", "B"))
ggsave("plots/supp_mass_thresholds.png", plot = both_plots, width = 13, height = 11)
