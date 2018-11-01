library(dplyr)
library(ggplot2)
library(cowplot)

# Read in occurrences with temperature data
occurrences_with_temp_path = "data/occurrences_with_temp.csv"
occurrences_with_temp = read.csv(occurrences_with_temp_path)

# test case
all_sev = occurrences_with_temp[occurrences_with_temp$site == "sevilleta",]
sev_model = lm(avg_temp ~ yr, data = all_sev)
sev_ends = data.frame(yr = c(min(all_sev$yr), max(all_sev$yr)))
sev_ends$avg_temp = c(predict(sev_model, sev_ends))
temp_change = sev_ends$avg_temp[2] - sev_ends$avg_temp[1]

one_sev = occurrences_with_temp[occurrences_with_temp$site == "sevilleta" & occurrences_with_temp$species == "onar",]
#1 includes all temps, 2 includes only temps from years with masses
one_sev_ends1 = 

one_sev_ends2 = 


ggplot(all_sev, aes(x = yr, y = avg_temp)) +
  geom_point() +
  stat_smooth(method = "lm")
ggplot(one_sev, aes(x = yr, y = avg_temp)) +
  geom_point() +
  stat_smooth(method = "lm")

# Calculate absolute change in temperature at each site
temp_changes = data.frame(site = factor(), temp_change = numeric())
for(site in unique(occurrences_with_temp$site)){
  site_occurrences = occurrences_with_temp[occurrences_with_temp$site == site,]
  site_model = lm(avg_temp ~ yr, data = site_occurrences)
  site_endpoints = data.frame(yr = c(min(site_occurrences$yr), max(site_occurrences$yr)))
  site_endpoints$avg_temp = c(predict(site_model, site_endpoints))
  temp_change = site_endpoints$avg_temp[2] - site_endpoints$avg_temp[1]
  site_temp_change = data.frame(site, temp_change)
  temp_changes = rbind(temp_changes, site_temp_change)
}

# Calculate absolute change in temperature for each species at each site
temp_changes2 = data.frame(site = factor(), temp_change = numeric())
for(site in unique(occurrences_with_temp$site)){
  site_occurrences = occurrences_with_temp[occurrences_with_temp$site == site,]
  for(species in unique(site_occurrences$species)){
    species_occurrences = site_occurrences[site_occurrences$species == species,]
    temp_model = lm(avg_temp ~ yr, data = species_occurrences)
    temp_endpoints = data.frame(yr = c(min(species_occurrences$yr), max(species_occurrences$yr)))
    temp_endpoints$avg_temp = c(predict(temp_model, temp_endpoints))
    temp_change2 = temp_endpoints$avg_temp[2] - temp_endpoints$avg_temp[1]
    temp_change_df = data.frame(site, species, temp_change2)
    temp_changes2 = rbind(temp_changes2, temp_change_df)
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

# Combine change dataframes and plot
temp_mass_changes = left_join(mass_changes, temp_changes, by = "site")

temp_mass_changes = temp_mass_changes %>% 
  mutate(Site = case_when(site == "portal" ~ "Portal", 
                                site == "frayjorge" ~ "Fray Jorge", 
                                site == "sevilleta" ~ "Sevilleta"))
temp_mass_changes_plot = ggplot(temp_mass_changes, aes(x = temp_change, y = mass_change, color = Site)) +
  geom_point() +
  lims(x = c(-1, 1), y = c(-35, 35)) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  labs(x = "Change in temperature (*C)", y = "Change in mass (%)") +
  theme_cowplot()

# Save figure
ggsave("plots/synth_fig.png", temp_mass_changes_plot, height = 6, width = 7)
