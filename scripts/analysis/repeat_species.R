library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(cowplot)

# Read in occurrences with temperature data
occurrences_with_temp_path = "data/occurrences_with_temp.csv"
occurrences_with_temp = read.csv(occurrences_with_temp_path)

# Get occurrences for species that occur in more than one site
occurrences_repeat_with_temp = occurrences_with_temp %>% 
  group_by(scientific_name) %>% 
  mutate(num_sites = n_distinct(site)) %>% 
  ungroup() %>% 
  filter(num_sites > 1)

# Create two plots for each species
two_plots_df = occurrences_repeat_with_temp %>% 
  group_by(scientific_name) %>% 
  nest() %>% 
  mutate(
    mass_by_year = purrr::map(data, ~ ggplot(., aes(yr, mass_mean, color = site)) +
                                geom_point() +
                                stat_smooth(method = "lm", se = FALSE) +
                                theme(legend.position = "bottom") +
                                labs(x = "Year", y = "Mean Mass (g)")),
    mass_by_temp = purrr::map(data, ~ ggplot(., aes(avg_temp, mass_mean, color = site)) +
                                geom_point() +
                                stat_smooth(method = "lm", se = FALSE) +
                                theme(legend.position = "bottom") +
                                labs(x = "Mean Temp (C*)", y = "Mean Mass (g)"))
  )

# Combine all plots for each species
two_plots_df$combined = purrr::pmap(list(two_plots_df$mass_by_year, two_plots_df$mass_by_temp), 
                                    ~ cowplot::plot_grid(plot_grid(..1), plot_grid(..2), 
                                                         ncol = 1))

combined_species = purrr::pmap(list(two_plots_df$combined[1], two_plots_df$combined[2], 
                                    two_plots_df$combined[3], two_plots_df$combined[4], 
                                    two_plots_df$combined[5], two_plots_df$combined[6], 
                                    two_plots_df$combined[7]), 
                               ~cowplot::plot_grid(plot_grid(..1), plot_grid(..2), 
                                                   plot_grid(..3), plot_grid(..4), 
                                                   plot_grid(..5), plot_grid(..6), 
                                                   plot_grid(..7), nrow = 1))

# Save figure
ggsave_args = list(filename = "plots/repeat_species.png", 
                    plot = combined_species, 
                    width = 28, 
                    height = 8)
pmap(ggsave_args, ggsave)
