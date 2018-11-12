library(XML)
library(dplyr)
library(textreadr)
library(WordR)
library(flextable)

# Create species codes dataframe for Fray Jorge
frayjorge_url = "http://www.esapubs.org/archive/ecol/E094/084/Fray_Jorge_Small_Mammal_Metadata.php"
frayjorge_list = readHTMLTable(frayjorge_url)
frayjorge_df = data.frame(frayjorge_list[4])
frayjorge_df = frayjorge_df %>% 
  select(NULL.Code, NULL.Species) %>% 
  rename(code = NULL.Code, 
         scientific_name = NULL.Species)
frayjorge_df$site = c("frayjorge")

# Create species codes dataframe for Portal
portal_path = "data/portal/raw/portal_dev_rodent_species.csv"
portal_list = read.csv(portal_path)
portal_df = portal_list %>% 
  filter(rodent == 1) %>% 
  select(species_code, scientific_name) %>% 
  rename(code = species_code)
portal_df$site = c("portal")

# Create species codes dataframe for Sevilleta
sevilleta_url = "https://digitalrepository.unm.edu/cgi/viewcontent.cgi?filename=0&article=1280&context=lter_sev_data&type=additional"
sevilleta_list = sevilleta_url %>% 
  read_html()
sevilleta_list = sevilleta_list[597:759]
sevilleta_df = data.frame(code = sevilleta_list[seq(1, 161, by = 5)], 
                      scientific_name = sevilleta_list[seq(3, 163, by = 5)])
add_codes_df = data.frame(code = c("pmbo", "pmdi", "pmer", "pmle", "pmma", 
                                   "pmtr", "pmsp"), 
                          scientific_name = c("Peromyscus boylii", "Peromyscus difficilis", 
                                              "Peromyscus eremicus", "Peromyscus leucopus", 
                                              "Peromyscus maniculatus", "Peromyscus truei", 
                                              "Peromyscus sp."))
sevilleta_df = bind_rows(sevilleta_df, add_codes_df)
sevilleta_df$site = c("sevilleta")

# Combine species code dataframes for all sites
sites_df = bind_rows(frayjorge_df, portal_df, sevilleta_df)

# Keep only codes for species in final occurrences dataset
occurrences_with_temp_path = "data/occurrences_with_temp.csv"
occurrences_with_temp = read.csv(occurrences_with_temp_path)
species_actual = occurrences_with_temp %>% 
  group_by(site, species) %>% 
  summarize(site_by_species = n_distinct(species)) %>% 
  ungroup() %>% 
  select(site, species)
species_codes = left_join(species_actual, sites_df, by = c("site" = "site", "species" = "code"))
species_codes = species_codes %>% 
  mutate(Site = case_when(site == "frayjorge" ~ "Fray Jorge", 
                          site == "portal" ~ "Portal", 
                          site == "sevilleta" ~ "Sevilleta")) %>% 
  select(Site, species, scientific_name)


# Put dataframe into .docx
ft_species_codes = flextable(species_codes)
ft_species_codes = italic(ft_species_codes, j = ~ scientific_name)
ft_species_codes = set_header_labels(ft_species_codes, 
                                     species = "Species Code", 
                                     scientific_name = "Scientific Name")
ft_species_codes = align(ft_species_codes, align = "left", part = "all")
ft_species_codes = autofit(ft_species_codes)

ft_list = list(ft_species_codes = ft_species_codes)
body_add_flextables("plots/species_codes_initial.docx", "plots/species_codes.docx", ft_list)
