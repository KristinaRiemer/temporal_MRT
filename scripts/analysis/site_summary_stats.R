library(dplyr)
library(WordR)
library(flextable)

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

# Save both dataframes as tables in .docx
ft_across_site_summary = flextable(across_site_summary)
ft_by_site_summary = flextable(by_site_summary)
FT = list(ft_across_site_summary = ft_across_site_summary, 
          ft_by_site_summary = ft_by_site_summary)
body_add_flextables("plots/summary_tables_initial.docx", "plots/summary_tables.docx", FT)
