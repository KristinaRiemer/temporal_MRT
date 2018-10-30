library(dplyr)

# Read in occurrences with temperature data
occurrences_with_temp_path = "data/occurrences_with_temp.csv"
occurrences_with_temp = read.csv(occurrences_with_temp_path)

# Create dataframe with summary values and states across sites
across_site_summary = occurrences_with_temp %>% 
  summarize(num_inds = sum(inds), 
            num_species = n_distinct(scientific_name))

# Create dataframe with summary values and stats for each site
by_site_summary = occurrences_with_temp %>% 
  group_by(site) %>% 
  summarize(num_inds = sum(inds),
            num_species = n_distinct(scientific_name), 
            num_yr = n_distinct(yr), 
            min_inds = min(inds), 
            max_inds = max(inds), 
            min_yr = min(num_yrs), 
            max_yr = max(num_yrs))
