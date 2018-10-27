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
                               facet_wrap(~species, scales = "free")),
    mass_by_temp = purrr::map(data, ~ ggplot(., aes(avg_temp, mass_mean)) +
                       geom_point() +
                       stat_smooth(method = "lm") +
                       facet_wrap(~species, scales = "free"))
    )
plots_df$mass_by_year[1]

# Save plots as [site]_supp.png