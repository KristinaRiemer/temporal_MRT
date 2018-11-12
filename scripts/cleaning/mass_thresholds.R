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

occurrences_with_temp_lower = occurrences_with_temp %>% 
  filter(!is.na(avg_temp)) %>% 
  filter(inds >= 5) %>% 
  group_by(site, species) %>% 
  mutate(num_yrs = n_distinct(yr)) %>% 
  ungroup() %>% 
  filter(num_yrs >= 2)

occurrences_with_temp_higher = occurrences_with_temp %>% 
  filter(!is.na(avg_temp)) %>% 
  filter(inds >= 30) %>% 
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

all_lm_mass_lower = data.frame(species = factor(), slope = numeric(), site = factor(), scientific_name = factor())
for(each_site in sites){
  site_annual_masses = occurrences_with_temp_lower[occurrences_with_temp_lower$site == each_site,]
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
  all_lm_mass_lower = rbind(all_lm_mass_lower, site_lm_mass)
}

all_lm_mass_higher = data.frame(species = factor(), slope = numeric(), site = factor(), scientific_name = factor())
for(each_site in sites){
  site_annual_masses = occurrences_with_temp_higher[occurrences_with_temp_higher$site == each_site,]
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
  all_lm_mass_higher = rbind(all_lm_mass_higher, site_lm_mass)
}

# Create slope distribution plot for all threshold datasets for each site
mass_plot_df_actual = all_lm_mass_actual %>% 
  group_by(site) %>% 
  nest() %>% 
  mutate(slope_dist_actual = purrr::map(data, ~ggplot(., aes(x = slope)) +
                                   geom_histogram() +
                                   xlim(c(-1, 1)) +
                                   geom_vline(xintercept = 0, color = "red") +
                                   labs(x = "Yearly mass slope", y = "Number of species"))
  )

mass_plot_df_lower = all_lm_mass_lower %>% 
  group_by(site) %>% 
  nest() %>% 
  mutate(slope_dist_lower = purrr::map(data, ~ggplot(., aes(x = slope)) +
                                          geom_histogram() +
                                          xlim(c(-1, 1)) +
                                          geom_vline(xintercept = 0, color = "red") +
                                          labs(x = "Yearly mass slope", y = "Number of species"))
  )

mass_plot_df_higher = all_lm_mass_higher %>% 
  group_by(site) %>% 
  nest() %>% 
  mutate(slope_dist_higher = purrr::map(data, ~ggplot(., aes(x = slope)) +
                                          geom_histogram() +
                                          xlim(c(-1, 1)) +
                                          geom_vline(xintercept = 0, color = "red") +
                                          labs(x = "Yearly mass slope", y = "Number of species"))
  )

mass_plot_df = left_join(mass_plot_df_actual, mass_plot_df_lower, by = c("site" = "site"))
mass_plot_df = left_join(mass_plot_df, mass_plot_df_higher, by = c("site" = "site"))

mass_plot_df$combined = purrr::pmap(list(mass_plot_df$slope_dist_actual, mass_plot_df$slope_dist_lower, 
                                         mass_plot_df$slope_dist_higher), 
                                    ~ cowplot::plot_grid(plot_grid(..1), plot_grid(..2), 
                                                         plot_grid(..3), 
                                                         ncol = 1))

combined_sites_mass = purrr::pmap(list(mass_plot_df$combined[1], mass_plot_df$combined[2], 
                                      mass_plot_df$combined[3]), 
                                 ~ cowplot::plot_grid(plot_grid(..1), plot_grid(..2), 
                                                      plot_grid(..3), nrow = 1))

ggsave_args_mass = list(filename = "plots/supp_mass_thresholds_1.png", 
                    plot = combined_sites_mass, 
                    width = 14, 
                    height = 12)
pmap(ggsave_args_mass, ggsave)

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

all_lm_mrt_lower = data.frame(species = factor(), r.squared = numeric(), slope = numeric(), p.value = numeric(), r = numeric(), site = factor(), scientific_name = factor())
for(each_site in sites){
  site_annual_masses = occurrences_with_temp_lower[occurrences_with_temp_lower$site == each_site,]
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
  all_lm_mrt_lower = rbind(all_lm_mrt_lower, site_lm_mrt)
}

all_lm_mrt_higher = data.frame(species = factor(), r.squared = numeric(), slope = numeric(), p.value = numeric(), r = numeric(), site = factor(), scientific_name = factor())
for(each_site in sites){
  site_annual_masses = occurrences_with_temp_higher[occurrences_with_temp_higher$site == each_site,]
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
  all_lm_mrt_higher = rbind(all_lm_mrt_higher, site_lm_mrt)
}

# Create r distribution plot for all threshold datasets for each site
mrt_plot_df_actual = all_lm_mrt_actual %>% 
  group_by(site) %>% 
  nest() %>% 
  mutate(r_dist_actual = purrr::map(data, ~ggplot(., aes(x = r)) +
                                          geom_histogram() +
                                          xlim(c(-1, 1)) +
                                          geom_vline(xintercept = 0, color = "red") +
                                          labs(x = "R", y = "Number of species"))
  )

mrt_plot_df_lower = all_lm_mrt_lower %>% 
  group_by(site) %>% 
  nest() %>% 
  mutate(r_dist_lower = purrr::map(data, ~ggplot(., aes(x = r)) +
                                      geom_histogram() +
                                      xlim(c(-1, 1)) +
                                      geom_vline(xintercept = 0, color = "red") +
                                      labs(x = "R", y = "Number of species"))
  )

mrt_plot_df_higher = all_lm_mrt_higher %>% 
  group_by(site) %>% 
  nest() %>% 
  mutate(r_dist_higher = purrr::map(data, ~ggplot(., aes(x = r)) +
                                      geom_histogram() +
                                      xlim(c(-1, 1)) +
                                      geom_vline(xintercept = 0, color = "red") +
                                      labs(x = "R", y = "Number of species"))
  )

mrt_plot_df = left_join(mrt_plot_df_actual, mrt_plot_df_lower, by = c("site" = "site"))
mrt_plot_df = left_join(mrt_plot_df, mrt_plot_df_higher, by = c("site" = "site"))

mrt_plot_df$combined = purrr::pmap(list(mrt_plot_df$r_dist_actual, mrt_plot_df$r_dist_lower, 
                                         mrt_plot_df$r_dist_higher), 
                                    ~ cowplot::plot_grid(plot_grid(..1), plot_grid(..2), 
                                                         plot_grid(..3), 
                                                         ncol = 1))

combined_sites_mrt = purrr::pmap(list(mrt_plot_df$combined[1], mrt_plot_df$combined[2], 
                                       mrt_plot_df$combined[3]), 
                                  ~ cowplot::plot_grid(plot_grid(..1), plot_grid(..2), 
                                                       plot_grid(..3), nrow = 1))

ggsave_args_mrt = list(filename = "plots/supp_mass_thresholds_2.png", 
                        plot = combined_sites_mrt, 
                        width = 14, 
                        height = 12)
pmap(ggsave_args_mrt, ggsave)