library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(cowplot)

# Read in occurrences with temperature data
occurrences_with_temp_path = "data/occurrences_with_temp.csv"
occurrences_with_temp = read.csv(occurrences_with_temp_path)

# Read in linear regression results data
lin_reg_stats_path = "data/lin_reg_stats.csv"
lin_reg_stats = read.csv(lin_reg_stats_path)

# Create four plots for each site
occurrences_with_temp = occurrences_with_temp %>% 
  mutate(label1 = case_when(site == "portal" ~ "A", 
                            site == "frayjorge" ~ "B", 
                            site == "sevilleta" ~ "C"), 
         label2 = case_when(site == "portal" ~ "D", 
                            site == "frayjorge" ~ "E", 
                            site == "sevilleta" ~ "F"), 
         label3 = case_when(site == "portal" ~ "G", 
                            site == "frayjorge" ~ "H", 
                            site == "sevilleta" ~ "I"))
lin_reg_stats = lin_reg_stats %>% 
  mutate(label4 = case_when(site == "portal" ~ "J", 
                            site == "frayjorge" ~ "K", 
                            site == "sevilleta" ~ "L"))

three_plots_df = occurrences_with_temp %>% 
  group_by(site) %>% 
  nest() %>% 
  mutate(
    temp_by_year = purrr::map(data, ~ggplot(., aes(yr, avg_temp)) +
                                geom_point() +
                                stat_smooth(method = "lm") +
                                labs(x = "Year", y = "Mean Temp (C*)") +
                                geom_text(aes(x = min(yr), y = max(avg_temp), label = label1), 
                                          fontface = "bold")), 
    mass_by_year = purrr::map(data, ~ ggplot(., aes(yr, mass_mean, color = species)) +
                                geom_point() +
                                stat_smooth(method = "lm", se = FALSE) +
                                theme(legend.position = "none") +
                                labs(x = "Year", y = "Mean Mass (g)") +
                                geom_text(aes(x = min(yr), y = max(mass_mean), label = label2), 
                                          fontface = "bold", color = "black")),
    mass_by_temp = purrr::map(data, ~ ggplot(., aes(avg_temp, mass_mean, color = species)) +
                                geom_point() +
                                stat_smooth(method = "lm", se = FALSE) +
                                theme(legend.position = "bottom") +
                                labs(x = "Mean Temp (C*)", y = "Mean Mass (g)") +
                                geom_text(aes(x = min(avg_temp), y = max(mass_mean), label = label3), 
                                          fontface = "bold", color = "black"))
  )

last_plot_df = lin_reg_stats %>% 
  group_by(site) %>% 
  nest() %>% 
  mutate(r_dist = purrr::map(data, ~ggplot(., aes(x = r)) +
                               geom_histogram() +
                               xlim(c(-1, 1)) +
                               labs(x = "R", y = "Number of species") +
                               geom_text(aes(x = -1, y = 2, label = label4), 
                                         fontface = "bold"))
  )

# Combine four plots for each site, then combine all site plots
# TODO: create the 12-panel plot in a better way
plots_df = left_join(three_plots_df, last_plot_df, by = "site")
plots_df$combined = purrr::pmap(list(plots_df$temp_by_year, plots_df$mass_by_year, 
                                     plots_df$mass_by_temp, plots_df$r_dist), 
                                ~ cowplot::plot_grid(plot_grid(..1), plot_grid(..2), 
                                                     plot_grid(..3), plot_grid(..4), 
                                                     ncol = 1))

combined_sites = purrr::pmap(list(plots_df$combined[1], plots_df$combined[2], 
                                  plots_df$combined[3]), 
                             ~ cowplot::plot_grid(plot_grid(..1), plot_grid(..2),
                                                  plot_grid(..3), nrow = 1))

# Save figure
ggsave_args = list(filename = "plots/main_fig.png", 
                   plot = combined_sites, 
                   width = 15, 
                   height = 20)
pmap(ggsave_args, ggsave)
