library(dplyr)
library(WordR)
library(flextable)

# Read in occurrences with temperature data
occurrences_with_temp_path = "data/occurrences_with_temp.csv"
occurrences_with_temp = read.csv(occurrences_with_temp_path)
occurrences_with_temp = occurrences_with_temp %>% 
  mutate(Site = case_when(site == "portal" ~ "Portal", 
                          site == "frayjorge" ~ "Fray Jorge", 
                          site == "sevilleta" ~ "Sevilleta"))
  

# Create dataframe with summary values and stats for each site
by_site_summary = occurrences_with_temp %>% 
  group_by(Site) %>% 
  summarize(num_inds = sum(inds),
            num_species = n_distinct(species), 
            num_yr = n_distinct(yr), 
            min_inds = min(inds), 
            max_inds = max(inds), 
            min_yr = min(num_yrs), 
            max_yr = max(num_yrs)) %>% 
  mutate(min_inds = as.integer(min_inds), 
         max_inds = as.integer(max_inds), 
         min_yr = as.integer(min_yr), 
         max_yr = as.integer(max_yr))

# Create dataframe with summary values and states across sites
across_site_summary = by_site_summary %>% 
  summarize(num_inds = sum(num_inds), 
            num_species = sum(num_species))

# Save both dataframes as flextables
ft_across_site_summary = flextable(across_site_summary)
ft_across_site_summary = set_header_labels(ft_across_site_summary, 
                                           num_inds = "Individuals", 
                                           num_species = "Species")

ft_by_site_summary = flextable(by_site_summary)
ft_by_site_summary = set_header_labels(ft_by_site_summary,
                                       num_inds = "Individuals", 
                                       num_species = "Species", 
                                       num_yr = "Years", 
                                       min_inds = "Individuals (min)", 
                                       max_inds = "Individuals (max)", 
                                       min_yr = "Years (min)", 
                                       max_yr = "Years (max)")

# Put flextables into .docs
FT = list(ft_across_site_summary = ft_across_site_summary, 
          ft_by_site_summary = ft_by_site_summary)
body_add_flextables("plots/summary_tables_initial.docx", "plots/summary_tables.docx", FT)
