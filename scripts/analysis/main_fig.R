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
    # combined = purrr::pmap(list(temp_yearly, mass_yearly, mrt), 
    #                        ~ cowplot::plot_grid(plot_grid(..1), plot_grid(..2, ..3, nrow = 1, rel_widths = c(.25, .25)), 
    #                                             ncol = 1, rel_heights = c(1, 5), rel_widths = c(1, 5))), 
    combined2 = purrr::pmap(list(temp_yearly, mass_yearly, mrt), 
                           ~ ggdraw(plot = NULL, xlim = c(0, 1), ylim = c(0, 10)) + 
                             draw_plot(..1, 0, 7.5, width = 1, height = 1) + 
                             draw_plot(..2, 0, 0, width = 0.5, height = 7.5) + 
                             draw_plot(..3, 0.5, 0, width = 0.5, height = 7.5))
    #plot_name = purrr::map(data, paste0("plots/", site))
    #ggsave()
  )
plots_df$combined2[2]

# ggdraw() +
#   draw_plot(plots_df$temp_yearly[2]) +
#   draw_plot(plots_df$mass_yearly[2]) +
#   draw_plot(plots_df$mrt[2])

# TODO: automatically determine height of plot from number of species
# Save final figure for each site
sites = unique(occurrences_with_temp$site)
site_file_names = paste0("plots/", sites, "/", sites, "_main.png")
plots_df$height = c(20, 10)
ggsave_args = list(filename = site_file_names, 
                   plot = plots_df$combined2, 
                   height = plots_df$height)
pmap(ggsave_args, ggsave)
