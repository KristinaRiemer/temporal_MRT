library(dplyr)

# Read in and combine occurrences data for all sites
sites = c("portal", "frayjorge")
occurrences = data.frame()
for(site in sites){
  site_occurrences_path = paste0("data/", site, "/clean/occurrences.csv")
  site_occurrences_df = read.csv(site_occurrences_path)
  site_occurrences_df$site = c(site)
  occurrences = bind_rows(occurrences, site_occurrences_df)
}

# Save all site occurrences
occurrences_path = "data/occurrences.csv"
write.csv(occurrences, occurrences_path)
