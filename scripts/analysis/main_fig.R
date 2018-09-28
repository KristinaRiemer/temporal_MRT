library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(cowplot)

# Read in occurrences with temperature data
occurrences_with_temp_path = "data/occurrences_with_temp.csv"
occurrences_with_temp = read.csv(occurrences_with_temp_path)

# TODO: double check that range of temperature years match occurrences years
# Generate three-panel figure for each site
plots_df = occurrences_with_temp %>% 
  group_by(site) %>% 
  nest() %>% 
  mutate(
    temp_yearly = purrr::map(data, ~ ggplot(., aes(yr, avg_temp)) +
                               geom_point() +
                               stat_smooth(method = "lm")),
    mass_yearly = purrr::map(data, ~ ggplot(., aes(yr, mass_mean)) + 
                               geom_point() +
                               stat_smooth(method = "lm") +
                               facet_wrap(~species, scales = "free", ncol = 1)),
    mrt = purrr::map(data, ~ ggplot(., aes(avg_temp, mass_mean)) +
                       geom_point() +
                       stat_smooth(method = "lm") +
                       facet_wrap(~species, scales = "free", ncol = 1)),
    combined = purrr::pmap(list(temp_yearly, mass_yearly, mrt), ~ cowplot::plot_grid(plot_grid(..1), plot_grid(..2, ..3, nrow = 1), ncol = 1))
  )

# TODO: automatically determine height of plot from number of species
# Save final figure for each site
sites = unique(occurrences_with_temp$site)
site_file_names = paste0("plots/", sites, "/", sites, ".png")
site_file_names
map2(site_file_names, plots_df$combined, ggsave)
