library(purrr)
library(ggplot2)
library(cowplot)

plots = all_annual_masses %>% 
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
                       facet_wrap(~species, scales = "free", ncol = 1))
  )
plots

plots$combined_plots = purrr::pmap(plots, ~cowplot::plot_grid(..4, ..5, align = "h"))
plots$combined_plots
plots$more_combined_plots = purrr::pmap(plots, ~cowplot::plot_grid(..3, ..6, nrow = 2, align = "v"))
plots$more_combined_plots

plots$combined = plots %>% 
  purrr::pmap(., ~cowplot::plot_grid(..4, ..5, align = "h")) %>% 
  purrr::pmap(., ~cowplot::plot_grid(..3, ..6, nrow = 2, align = "v"))
plots
