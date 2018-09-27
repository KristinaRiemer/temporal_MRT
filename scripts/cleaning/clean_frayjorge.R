

if(!file.exists("../data/raw_frayjorge/fray_jorge_ecology_mammals.csv")){
  rdataretriever::install(dataset = "fray-jorge-ecology", connection = "csv", data_dir = "../data/raw_frayjorge/")
}

fray_occurrences = read.csv("../data/raw_frayjorge/fray_jorge_ecology_mammals.csv", na.strings = ".")
dim(fray_occurrences)
head(fray_occurrences)

fray_occurrences = fray_occurrences %>% 
  separate(mo, into = c("year", "month"), sep = 4, remove = FALSE, convert = TRUE)

by_sp_yr_fray_jorge = fray_occurrences %>% 
  filter(!is.na(wt), !is.na(sp)) %>% 
  group_by(sp, year) %>% 
  summarize(mass_mean = mean(wt), 
            mass_sd = sd(wt)) %>% 
  mutate(species = sp, 
         yr = year)
by_sp_yr_fray_jorge
