library(dplyr)
library(ggplot2)
library(cowplot)

# Read in occurrences with temperature data
occurrences_with_temp_path = "data/occurrences_with_temp.csv"
occurrences_with_temp = read.csv(occurrences_with_temp_path)

# Read in model results data with statistical significance
model_stats_path = "data/model_stats.csv"
model_stats = read.csv(model_stats_path)

# Calculate absolute change in temperature for each species at each site
temp_changes = data.frame(site = factor(), temp_change = numeric())
for(site in unique(occurrences_with_temp$site)){
  site_occurrences = occurrences_with_temp[occurrences_with_temp$site == site,]
  for(species in unique(site_occurrences$species)){
    species_occurrences = site_occurrences[site_occurrences$species == species,]
    species_endpoints = data.frame(yr = c(min(species_occurrences$yr), max(species_occurrences$yr)))
    temp_occurrences = site_occurrences %>% 
      filter(yr >= species_endpoints$yr[1] & yr <= species_endpoints$yr[2])
    temp_model = lm(avg_temp ~ yr, data = temp_occurrences)
    species_endpoints$avg_temp = c(predict(temp_model, species_endpoints))
    temp_change = species_endpoints$avg_temp[2] - species_endpoints$avg_temp[1]
    temp_change_df = data.frame(site, species, temp_change)
    temp_changes = rbind(temp_changes, temp_change_df)
  }
}

# Calculate percent change in mass for each species at each site
mass_changes = data.frame(site = factor(), species = factor(), mass_change = numeric())
for(site in unique(occurrences_with_temp$site)){
  site_occurrences = occurrences_with_temp[occurrences_with_temp$site == site,]
  for(species in unique(site_occurrences$species)){
    species_occurrences = site_occurrences[site_occurrences$species == species,]
    species_model = lm(mass_mean ~ yr, data = species_occurrences)
    species_endpoints = data.frame(yr = c(min(species_occurrences$yr), max(species_occurrences$yr)))
    species_endpoints$mass_mean = c(predict(species_model, species_endpoints))
    mass_change = ((species_endpoints$mass_mean[2] - species_endpoints$mass_mean[1]) / species_endpoints$mass_mean[1]) * 100
    species_mass_change = data.frame(site, species, mass_change)
    mass_changes = rbind(mass_changes, species_mass_change)
  }
}

# Combine change dataframes with model significance data and plot
temp_mass_changes = left_join(mass_changes, temp_changes, by = c("site", "species"))

temp_mass_changes = left_join(temp_mass_changes, model_stats, by = c("site", "species"))
temp_mass_changes = temp_mass_changes %>% 
  mutate(Significant = case_when(exog_pvalue_sig == "yes" ~ "Yes", 
                                 exog_pvalue_sig == "no" ~ "No"))

temp_mass_changes = temp_mass_changes %>% 
  mutate(Site = case_when(site == "portal" ~ "Portal", 
                                site == "frayjorge" ~ "Fray Jorge", 
                                site == "sevilleta" ~ "Sevilleta"))

temp_mass_changes_plot = ggplot(temp_mass_changes, aes(x = temp_change, y = mass_change, 
                                                       color = Site, shape = Significant)) +
  geom_point() +
  lims(x = c(-1, 1), y = c(-35, 35)) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  scale_shape_manual(values = c(1, 16)) +
  labs(x = "Change in temperature (*C)", y = "Change in mass (%)") +
  theme_cowplot()

# Save figure
ggsave("plots/synth_fig.png", temp_mass_changes_plot, height = 6, width = 7)
