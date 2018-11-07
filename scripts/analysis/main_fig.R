library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(cowplot)

# Read in occurrences with temperature data
occurrences_with_temp_path = "data/occurrences_with_temp.csv"
occurrences_with_temp = read.csv(occurrences_with_temp_path)

# Read in linear regression results for both yearly mass and mass-temp relationships
lin_reg_mass_path = "data/lin_reg_mass.csv"
lin_reg_mass = read.csv(lin_reg_mass_path)
lin_reg_mrt_path = "data/lin_reg_mrt.csv"
lin_reg_mrt = read.csv(lin_reg_mrt_path)

# Read in model results data with statistical significance and combine with r dist dataframe
model_stats_path = "data/model_stats.csv"
model_stats = read.csv(model_stats_path)
lin_reg_mrt = left_join(lin_reg_mrt, model_stats, by = c("site", "species"))

# Create four plots for each site
occurrences_with_temp = occurrences_with_temp %>% 
  mutate(temp_by_year_label = case_when(site == "portal" ~ "A", 
                            site == "frayjorge" ~ "B", 
                            site == "sevilleta" ~ "C"), 
         mass_by_year_label = case_when(site == "portal" ~ "D", 
                            site == "frayjorge" ~ "E", 
                            site == "sevilleta" ~ "F"), 
         mass_by_temp_label = case_when(site == "portal" ~ "A", 
                            site == "frayjorge" ~ "B", 
                            site == "sevilleta" ~ "C"))
lin_reg_mrt = lin_reg_mrt %>% 
  mutate(r_dist_label = case_when(site == "portal" ~ "D", 
                            site == "frayjorge" ~ "E", 
                            site == "sevilleta" ~ "F"))
lin_reg_mass = lin_reg_mass %>% 
  mutate(slope_dist_label = case_when(site == "portal" ~ "G", 
                            site == "frayjorge" ~ "H", 
                            site == "sevilleta" ~ "I"))

three_plots_df = occurrences_with_temp %>% 
  group_by(site) %>% 
  nest() %>% 
  mutate(
    temp_by_year = purrr::map(data, ~ggplot(., aes(yr, avg_temp)) +
                                geom_point() +
                                stat_smooth(method = "lm") +
                                labs(x = "Year", y = "Mean Temp (C*)") +
                                geom_text(aes(x = min(yr), y = max(avg_temp), label = temp_by_year_label), 
                                          fontface = "bold")), 
    mass_by_year = purrr::map(data, ~ ggplot(., aes(yr, mass_mean, color = species)) +
                                geom_point() +
                                stat_smooth(method = "lm", se = FALSE) +
                                theme(legend.position = "none") +
                                labs(x = "Year", y = "Mean Mass (g)") +
                                geom_text(aes(x = min(yr), y = max(mass_mean), label = mass_by_year_label), 
                                          fontface = "bold", color = "black")),
    mass_by_temp = purrr::map(data, ~ ggplot(., aes(avg_temp, mass_mean, color = species)) +
                                geom_point() +
                                stat_smooth(method = "lm", se = FALSE) +
                                theme(legend.position = "bottom") +
                                labs(x = "Mean Temp (C*)", y = "Mean Mass (g)") +
                                geom_text(aes(x = min(avg_temp), y = max(mass_mean), label = mass_by_temp_label), 
                                          fontface = "bold", color = "black"))
  )

mrt_plot_df = lin_reg_mrt %>% 
  group_by(site) %>% 
  nest() %>% 
  mutate(r_dist = purrr::map(data, ~ggplot(., aes(x = r, fill = pvalue_sig)) +
                               geom_histogram() +
                               xlim(c(-1, 1)) +
                               geom_vline(xintercept = 0, color = "red") +
                               labs(x = "R", y = "Number of species") +
                               scale_fill_manual(values = c("purple", "darkgreen")) +
                               theme(legend.position = "none") +
                               geom_text(aes(x = -1, y = 2, label = r_dist_label), 
                                         fontface = "bold"))
  )

mass_plot_df = lin_reg_mass %>% 
  group_by(site) %>% 
  nest() %>% 
  mutate(slope_dist = purrr::map(data, ~ggplot(., aes(x = slope)) +
                                   geom_histogram() +
                                   xlim(c(-1, 1)) +
                                   geom_vline(xintercept = 0, color = "red") +
                                   labs(x = "Yearly mass slope", y = "Number of species") +
                                   geom_text(aes(x = -1, y = 3, label = slope_dist_label), 
                                             fontface = "bold"))
  )

# Combine four plots for each site, then combine all site plots
# TODO: create the 12-panel plot in a better way
plots_df = left_join(three_plots_df, mrt_plot_df, by = "site")
plots_df = left_join(plots_df, mass_plot_df, by = "site")

plots_df$combined1 = purrr::pmap(list(plots_df$temp_by_year, plots_df$mass_by_year, 
                                     plots_df$slope_dist), 
                                ~ cowplot::plot_grid(plot_grid(..1), plot_grid(..2), 
                                                     plot_grid(..3), 
                                                     ncol = 1))

plots_df$combined2 = purrr::pmap(list(plots_df$mass_by_temp, plots_df$r_dist), 
                                ~ cowplot::plot_grid(plot_grid(..1), plot_grid(..2), 
                                                     ncol = 1))




combined_sites1 = purrr::pmap(list(plots_df$combined1[1], plots_df$combined1[2], 
                                  plots_df$combined1[3]), 
                             ~ cowplot::plot_grid(plot_grid(..1), plot_grid(..2),
                                                  plot_grid(..3), nrow = 1))

combined_sites2 = purrr::pmap(list(plots_df$combined2[1], plots_df$combined2[2], 
                                   plots_df$combined2[3]), 
                              ~ cowplot::plot_grid(plot_grid(..1), plot_grid(..2),
                                                   plot_grid(..3), nrow = 1))

# Save figures
ggsave_args1 = list(filename = "plots/main_fig1.png", 
                   plot = combined_sites1, 
                   width = 14, 
                   height = 12)
pmap(ggsave_args1, ggsave)

ggsave_args2 = list(filename = "plots/main_fig2.png", 
                    plot = combined_sites2, 
                    width = 16, 
                    height = 10)
pmap(ggsave_args2, ggsave)
