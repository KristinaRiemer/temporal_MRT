library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(cowplot)

# Read in occurrences with temperature data
occurrences_with_temp_path = "data/occurrences_with_temp.csv"
occurrences_with_temp = read.csv(occurrences_with_temp_path)

# Create mass-temp plots, with lin reg shown, for each species for all sites
plots_df = occurrences_with_temp %>% 
  group_by(site) %>% 
  nest() %>% 
  mutate(
    mass_by_year = purrr::map(data, ~ ggplot(., aes(yr, mass_mean)) + 
                                geom_point() +
                                stat_smooth(method = "lm") +
                                facet_wrap(~species, scales = "free") +
                                labs(x = "Year", y = "Mean Mass (g)")),
    mass_by_temp = purrr::map(data, ~ ggplot(., aes(avg_temp, mass_mean)) +
                       geom_point() +
                       stat_smooth(method = "lm") +
                       facet_wrap(~species, scales = "free") + 
                       labs(x = "Mean Temp (C*)", y = "Mean Mass (g)"))
    )

mass_by_year_combined = purrr::pmap(list(plots_df$mass_by_year[1], plots_df$mass_by_year[2], 
                                        plots_df$mass_by_year[3]), 
                                   ~ cowplot::plot_grid(plot_grid(..1), plot_grid(..2), 
                                                        plot_grid(..3), 
                                                        labels = c("A", "B", "C")))

mass_by_temp_combined = purrr::pmap(list(plots_df$mass_by_temp[1], plots_df$mass_by_temp[2], 
                                        plots_df$mass_by_temp[3]), 
                                   ~ cowplot::plot_grid(plot_grid(..1), plot_grid(..2), 
                                                        plot_grid(..3), 
                                                        labels = c("A", "B", "C")))

# Save both all species plots for each site
mass_by_year_args = list("plots/supp_mass_by_year.png", plot = mass_by_year_combined, 
                         width = 15, height = 12)
pmap(mass_by_year_args, ggsave)

mass_by_temp_args = list("plots/supp_mass_by_temp.png", plot = mass_by_temp_combined, 
                         width = 15, height = 12)
pmap(mass_by_temp_args, ggsave)




