library(rdataretriever)
library(dplyr)

# Download and read in occurrences data
occurrences_path = "data/portal/raw/portal_dev_rodent.csv"
if(!file.exists(occurrences_path)){
  rdataretriever::install(dataset = "portal-dev", connection = "csv", data_dir = "data/portal/raw/")
}
occurrences = read.csv(occurrences_path)

# Download and read in plots data
plots_path = "data/portal/raw/plots.csv"
if(!file.exists(plots_path)){
  download.file(url = "https://raw.githubusercontent.com/weecology/PortalData/master/SiteandMethods/Portal_plots.csv", 
                destfile = plots_path)
}
plots = read.csv(plots_path)

# List of rodent species
species_path = "data/portal/raw/portal_dev_rodent_species.csv"
species = read.csv(species_path)
species_rodents = species %>% 
  filter(rodent == 1)

# Remove occurrences and get annual species masses
clean_occurrences = occurrences %>% 
  left_join(plots, by = c("yr" = "year", "mo" = "month", "plot" = "plot")) %>% 
  filter(treatment == "control")
clean_occurrences = clean_occurrences %>% 
  filter(!is.na(wgt), 
         !is.na(species), 
         species %in% species_rodents$species_code, 
         reprod != "J") %>% 
  group_by(species, yr) %>% 
  summarise(inds = n(), 
            mass_mean = mean(wgt), 
            mass_sd = sd(wgt))
clean_occurrences = left_join(clean_occurrences, species_rodents, by = c("species" = "species_code")) %>% 
  select(species, yr, inds, mass_mean, mass_sd, scientific_name)

# Save cleaned occurrences data
clean_occurrences_path = "data/portal/clean/occurrences.csv"
write.csv(clean_occurrences, clean_occurrences_path)
