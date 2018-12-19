library(dplyr)
library(textreadr)
library(tidyr)
library(purrr)
library(ggplot2)
library(cowplot)
library(broom)
library(forecast)
library(lmtest)
library(temporalMRTfxs)

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
occurrences$species = trimws(occurrences$species)

# Read in temperature data
site_annual_temps = read.csv("data/site_annual_temps.csv")

# Create species codes list
codes_url = "https://digitalrepository.unm.edu/cgi/viewcontent.cgi?filename=0&article=1280&context=lter_sev_data&type=additional"
codes_list = codes_url %>% 
  read_html()
codes_list = codes_list[597:759]
codes_df = data.frame(code = codes_list[seq(1, 161, by = 5)], 
                      scientific_name = codes_list[seq(3, 163, by = 5)])
add_codes_df = data.frame(code = c("pmbo", "pmdi", "pmer", "pmle", "pmma", 
                                   "pmtr", "pmsp"), 
                          scientific_name = c("Peromyscus boylii", "Peromyscus difficilis", 
                                              "Peromyscus eremicus", "Peromyscus leucopus", 
                                              "Peromyscus maniculatus", "Peromyscus truei", 
                                              "Peromyscus sp."))
codes_df = bind_rows(codes_df, add_codes_df)

# Remove occurrences and get annual species masses by subsites
clean_occurrences_subsites = occurrences %>% 
  filter(!is.na(mass), 
         !is.na(species), 
         age == "a") %>% 
  group_by(location, species, year) %>% 
  summarise(inds = n(), 
            mass_mean = mean(mass), 
            mass_sd = sd(mass)) %>% 
  rename(yr = year)
clean_occurrences_subsites = left_join(clean_occurrences_subsites, codes_df, by = c("species" = "code"))

# Get annual temperature for each species at all subsites
sevilleta_annual_temps = site_annual_temps %>% 
  filter(site == "sevilleta")
occurrences_subsites_with_temp = left_join(clean_occurrences_subsites, sevilleta_annual_temps, by = c("yr" = "year"))

# Retain only species with five years of data and 15 individuals per each year
occurrences_subsites_with_temp = occurrences_subsites_with_temp %>% 
  filter(!is.na(avg_temp)) %>% 
  filter(inds >= 15) %>% 
  group_by(location, species) %>% 
  mutate(num_yrs = n_distinct(yr)) %>% 
  ungroup() %>% 
  filter(num_yrs >= 5)

# Create dataframe of mass and mrt time series models for each species mass over time
model_stats = data.frame()
for(subsite in unique(occurrences_subsites_with_temp$location)){
  subsite_occurrences = occurrences_subsites_with_temp[occurrences_subsites_with_temp$location == subsite,]
  min_year = min(subsite_occurrences$yr)
  max_year = max(subsite_occurrences$yr)
  for(species in unique(subsite_occurrences$species)){
    species_occurrences = subsite_occurrences[subsite_occurrences$species == species,]
    species_occurrences = complete(species_occurrences, yr = min_year:max_year)
    mass_ts = ts(species_occurrences$mass_mean)
    temp_ts = ts(species_occurrences$avg_temp)
    exog_model = auto.arima(mass_ts, xreg = temp_ts)
    exog_pvalue = coeftest(exog_model)[dim(coeftest(exog_model))[1], dim(coeftest(exog_model))[2]]
    mass_model = Arima(mass_ts, order = arimaorder(exog_model), include.drift = TRUE)
    mass_pvalue = coeftest(mass_model)[dim(coeftest(mass_model))[1], dim(coeftest(mass_model))[2]]
    mass_dir = coeftest(mass_model)[dim(coeftest(mass_model))[1], 1]
    pvalues = data.frame(subsite = subsite, species = species, exog_pvalue = exog_pvalue, 
                         mass_pvalue = mass_pvalue, mass_dir = mass_dir)
    model_stats = rbind(model_stats, pvalues)
    
  }
}

model_stats$exog_pvalue_adjust = p.adjust(model_stats$exog_pvalue)
model_stats$mass_pvalue_adjust = p.adjust(model_stats$mass_pvalue)
model_stats = model_stats %>% 
  mutate(exog_pvalue_sig = case_when(exog_pvalue_adjust <= 0.05 ~ "yes", 
                                     exog_pvalue_adjust > 0.05 ~ "no"), 
         mass_pvalue_sig = case_when(mass_pvalue_adjust <= 0.05 ~ "yes", 
                                     mass_pvalue_adjust > 0.05 ~ "no"))

# Create dataframe of r and p values for each species temp-mass relationship
subsites = unique(occurrences_subsites_with_temp$location)
all_lm_mrt = data.frame(species = factor(), r.squared = numeric(), slope = numeric(), p.value = numeric(), r = numeric(), site = factor(), scientific_name = factor())
for(each_subsite in subsites){
  subsite_annual_masses = occurrences_subsites_with_temp[occurrences_subsites_with_temp$location == each_subsite,]
  subsite_lm_tidy_mrt = regression_mrt_p(subsite_annual_masses)
  subsite_lm_glance_mrt = regression_mrt_r2(subsite_annual_masses)
  subsite_lm_mrt = subsite_lm_glance_mrt %>% 
    full_join(subsite_lm_tidy_mrt, by = "species")
  subsite_lm_mrt = subsite_lm_mrt %>% 
    mutate(r = case_when(slope > 0 ~ sqrt(r.squared), 
                         slope < 0 ~ -sqrt(r.squared)))
  subsite_lm_mrt$subsite = as.factor(each_subsite)
  species_codes = subsite_annual_masses %>% 
    select(species, scientific_name) %>% 
    distinct(species, scientific_name)
  subsite_lm_mrt = left_join(subsite_lm_mrt, species_codes, by = c("species" = "species"))
  all_lm_mrt = rbind(all_lm_mrt, subsite_lm_mrt)
}
all_lm_mrt = left_join(all_lm_mrt, model_stats, by = c("subsite", "species"))

# Create plots for each subsite
three_plots_df = occurrences_subsites_with_temp %>% 
  group_by(location) %>% 
  nest() %>% 
  mutate(
    temp_by_year = purrr::map(data, ~ggplot(., aes(yr, avg_temp)) +
                                geom_point() +
                                stat_smooth(method = "lm") +
                                labs(x = "Year", y = "Mean Temp (C*)")), 
    mass_by_year = purrr::map(data, ~ ggplot(., aes(yr, mass_mean, color = species)) +
                                geom_point() +
                                stat_smooth(method = "lm", se = FALSE) +
                                theme(legend.position = "bottom") +
                                labs(x = "Year", y = "Mean Mass (g)")),
    mass_by_temp = purrr::map(data, ~ ggplot(., aes(avg_temp, mass_mean, color = species)) +
                                geom_point() +
                                stat_smooth(method = "lm", se = FALSE) +
                                theme(legend.position = "bottom") +
                                labs(x = "Mean Temp (C*)", y = "Mean Mass (g)"))
  )

mrt_plot_df = all_lm_mrt %>% 
  group_by(subsite) %>% 
  nest() %>% 
  mutate(r_dist = purrr::map(data, ~ggplot(., aes(x = r, fill = exog_pvalue_sig)) +
                               geom_histogram() +
                               xlim(c(-1, 1)) +
                               geom_vline(xintercept = 0, color = "red") +
                               labs(x = "R", y = "Number of species") +
                               scale_fill_manual(values = c("gray", "black")) +
                               theme(legend.position = "none"))
  )

mass_plot_df = model_stats %>% 
  group_by(subsite) %>% 
  nest() %>% 
  mutate(slope_dist = purrr::map(data, ~ggplot(., aes(x = mass_dir, fill = mass_pvalue_sig)) +
                                   geom_histogram() +
                                   xlim(c(-1, 1)) +
                                   geom_vline(xintercept = 0, color = "red") +
                                   labs(x = "Mass time series trend", y = "Number of species") +
                                   scale_fill_manual(values = c("gray", "black")) +
                                   theme(legend.position = "none"))
  )

# Combine plots for each subsite, then combine all subsite plots
plots_df = left_join(three_plots_df, mrt_plot_df, by = c("location" = "subsite"))
plots_df = left_join(plots_df, mass_plot_df, by = c("location" = "subsite"))

plots_df$combined1 = purrr::pmap(list(plots_df$temp_by_year, plots_df$mass_by_year, 
                                      plots_df$slope_dist), 
                                 ~ cowplot::plot_grid(plot_grid(..1), plot_grid(..2), 
                                                      plot_grid(..3), 
                                                      ncol = 1))

plots_df$combined2 = purrr::pmap(list(plots_df$mass_by_temp, plots_df$r_dist), 
                                 ~ cowplot::plot_grid(plot_grid(..1), plot_grid(..2), 
                                                      ncol = 1))


combined_sites1 = purrr::pmap(list(plots_df$combined1[1], plots_df$combined1[2], 
                                   plots_df$combined1[3], plots_df$combined1[4], 
                                   plots_df$combined1[5]), 
                              ~ cowplot::plot_grid(plot_grid(..1), plot_grid(..2),
                                                   plot_grid(..3), plot_grid(..4), 
                                                   plot_grid(..5), nrow = 1))

combined_sites2 = purrr::pmap(list(plots_df$combined2[1], plots_df$combined2[2], 
                                   plots_df$combined2[3], plots_df$combined2[4], 
                                   plots_df$combined2[5]), 
                              ~ cowplot::plot_grid(plot_grid(..1), plot_grid(..2),
                                                   plot_grid(..3), plot_grid(..4), 
                                                   plot_grid(..5), nrow = 1))

combined_sites = purrr::pmap(list(combined_sites1, combined_sites2), 
                             ~ cowplot::plot_grid(plot_grid(..1), plot_grid(..2), 
                                                  ncol = 1))

# Save figures
ggsave_args = list(filename = "plots/sev_subsites.png", 
                    plot = combined_sites, 
                    width = 18, 
                    height = 18)
pmap(ggsave_args, ggsave)
