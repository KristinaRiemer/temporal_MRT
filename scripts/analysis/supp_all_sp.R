library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)

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
                                labs(x = "Year", y = "Mean Mass (g?)")),
    mass_by_temp = purrr::map(data, ~ ggplot(., aes(avg_temp, mass_mean)) +
                       geom_point() +
                       stat_smooth(method = "lm") +
                       facet_wrap(~species, scales = "free") + 
                       labs(x = "Mean Temperature (C*)", y = "Mean Mass (g?)"))
    )

# Save both all species plots for each site
sites = unique(occurrences_with_temp$site)
site_file_names_mass = paste0("plots/", sites, "/", sites, "_mass_supp.png")
site_file_names_mrt = paste0("plots/", sites, "/", sites, "_mrt_supp.png")
map2(site_file_names_mass, plots_df$mass_by_year, ggsave)
map2(site_file_names_mrt, plots_df$mass_by_temp, ggsave)
