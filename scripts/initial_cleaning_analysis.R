library(rdataretriever)
library(dplyr)
library(ggplot2)

if(!file.exists("data/raw/portal_dev_rodent_trapping.csv")){
  rdataretriever::install(dataset = "portal-dev", connection = "csv", data_dir = "data/raw/")
}

portal_df = read.csv("data/raw/portal_dev_rodent.csv")

by_sp_yr = portal_df %>% 
  filter(!is.na(wgt), !is.na(species)) %>% 
  group_by(species, yr) %>% 
  summarize(mass_mean = mean(wgt), 
            mass_sd = sd(wgt))

ggplot(by_sp_yr, aes(x = yr, y = mass_mean)) +
  geom_errorbar(aes(ymin = mass_mean - mass_sd, ymax = mass_mean + mass_sd)) +
  geom_point() +
  facet_wrap(~species, scale = "free_y")
