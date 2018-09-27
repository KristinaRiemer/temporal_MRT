library(rdataretriever)
library(tidyr)
library(dplyr)

# Download and read in occurrences data
occurrences_path = "data/frayjorge/raw/fray_jorge_ecology_mammals.csv"
if(!file.exists(occurrences_path)){
  rdataretriever::install(dataset = "fray-jorge-ecology", connection = "csv", data_dir = "../data/raw_frayjorge/")
}
occurrences = read.csv(occurrences_path, na.strings = ".")

# Split date column into month and year columns
# TODO: add test for correct year and month ranges
occurrences = occurrences %>% 
  separate(mo, into = c("year", "month"), sep = 4, remove = FALSE, convert = TRUE)

# Remove occurrences and get annual species masses
# TODO: add test to make sure there are no NA values in weight or species columns
clean_occurrences = occurrences %>% 
  filter(!is.na(wt), 
         !is.na(sp)) %>% 
  group_by(sp, year) %>% 
  summarise(mass_mean = mean(wt), 
            mass_sd = sd(wt)) %>% 
  rename(species = sp, 
         yr = year)
  
# Save cleaned occurrences data
clean_occurrences_path = "data/frayjorge/clean/occurrences.csv"
write.csv(clean_occurrences, clean_occurrences_path)
