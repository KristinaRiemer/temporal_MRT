library(dplyr)

# Read in occurrences data
# TODO: replace download with popler package method
occurrences_path = "data/sevilleta/raw/sev008_rodentpopns_20151022.txt"
if(!file.exists(occurrences_path)){
  download.file(url = "https://digitalrepository.unm.edu/context/lter_sev_data/article/1280/type/native/viewcontent", 
                destfile = "data/sevilleta/raw/knb_lter_sev.8.297972.zip")
  unzip("data/sevilleta/raw/knb_lter_sev.8.297972.zip", exdir = "data/sevilleta/raw/")
}
occurrences = read.csv(occurrences_path, na.strings = c("na", "-999"))
occurrences$mass = as.numeric(as.character(occurrences$mass))

# Remove occurrences and get annual species masses
clean_occurrences = occurrences %>% 
  filter(!is.na(mass), 
         !is.na(species)) %>% 
  group_by(species, year) %>% 
  summarise(mass_mean = mean(mass), 
            mass_sd = sd(mass)) %>% 
  rename(yr = year)

# Save cleaned occurrences data
clean_occurrences_path = "data/sevilleta/clean/occurrences.csv"
write.csv(clean_occurrences, clean_occurrences_path)
