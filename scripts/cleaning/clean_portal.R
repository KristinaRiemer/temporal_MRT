library(dplyr)

if(!file.exists("../data/raw_portal/portal_dev_rodent_trapping.csv")){
  rdataretriever::install(dataset = "portal-dev", connection = "csv", data_dir = "../data/raw_portal/")
}

portal_occurrences = read.csv("../data/raw_portal/portal_dev_rodent.csv")
dim(portal_occurrences)
head(portal_occurrences)

colnames(portal_occurrences)
unique(portal_occurrences$age)
head(unique(portal_occurrences$tag), n = 20)
head(unique(portal_occurrences$ltag), n = 20)

portal_species = read.csv("../data/raw_portal/portal_dev_rodent_species.csv")
portal_species

table(portal_species$rodent)
rodent_list = portal_species %>% 
  filter(rodent == 1)
rodent_list

by_sp_yr_portal = portal_occurrences %>% 
  filter(!is.na(wgt), !is.na(species), species %in% rodent_list$species_code) %>% 
  group_by(species, yr) %>% 
  summarize(mass_mean = mean(wgt), 
            mass_sd = sd(wgt))
by_sp_yr_portal
