library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(cowplot)

# Read in occurrences with temperature data
occurrences_with_temp_path = "data/occurrences_with_temp.csv"
occurrences_with_temp = read.csv(occurrences_with_temp_path)

# Generate three-panel figure for each site
# TODO: double check that range of temperature years match occurrences years
plots_df = occurrences_with_temp %>% 
  group_by(site) %>% 
  nest() %>% 
  mutate(
    temp_by_year = purrr::map(data, ~ggplot(., aes(yr, avg_temp)) +
                                geom_point() +
                                stat_smooth(method = "lm") +
                                labs(x = "Year", y = "Mean Temperature (C*)")), 
    mass_by_year = purrr::map(data, ~ ggplot(., aes(yr, mass_mean, color = species)) +
                                geom_point() +
                                geom_line() +
                                theme(legend.position = "none") +
                                labs(x = "Year", y = "Mean Mass (g?)")),
    mass_by_temp = purrr::map(data, ~ ggplot(., aes(avg_temp, mass_mean, color = species)) +
                                geom_point() +
                                geom_line() +
                                theme(legend.position = "bottom") +
                                labs(x = "Mean Temperature (C*)", y = "Mean Mass (g?)")), 
    combined = purrr::pmap(list(temp_by_year, mass_by_year, mass_by_temp), 
                           ~ cowplot::plot_grid(plot_grid(..1), plot_grid(..2), 
                                                plot_grid(..3), ncol = 1))
  )
plots_df$combined[1]

# Save final figure for each site
# TODO: fix heights and widths of plots
sites = unique(occurrences_with_temp$site)
site_file_names = paste0("plots/", sites, "/", sites, "_main.png")
ggsave_args = list(filename = site_file_names, 
                   plot = plots_df$combined)
pmap(ggsave_args, ggsave)
