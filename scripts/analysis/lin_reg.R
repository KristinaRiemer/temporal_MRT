library(dplyr)

all_lm = data.frame(species = factor(), r.squared = numeric(), slope = numeric(), p.value = numeric(), r = numeric(), site = factor())
for(each_site in sites){
  site_annual_masses = all_annual_masses[all_annual_masses$site == each_site,]
  site_lm_tidy = site_annual_masses %>%
    nest(-species) %>% 
    mutate(fit = map(data, ~lm(mass_mean ~ avg_temp, data = .)), 
           results = map(fit, tidy)) %>% 
    unnest(results) %>% 
    filter(term == "avg_temp") %>% 
    select(species, slope = estimate, p.value)
  site_lm_glance = site_annual_masses %>% 
    nest(-species) %>% 
    mutate(fit = map(data, ~lm(mass_mean ~ avg_temp, data = .)), 
           results = map(fit, glance)) %>% 
    unnest(results) %>% 
    select(species, r.squared)
  site_lm = site_lm_glance %>% 
    full_join(site_lm_tidy, by = "species")
  site_lm = site_lm %>% 
    mutate(r = case_when(slope > 0 ~ sqrt(r.squared), 
                         slope < 0 ~ -sqrt(r.squared)))
  site_lm$site = as.factor(each_site)
  all_lm = rbind(all_lm, site_lm)
}
all_lm

for(each_site in unique(all_lm$site)){
  print(ggplot(all_lm[all_lm$site == each_site,], aes(x = r)) +
          geom_histogram() +
          xlim(c(-1, 1)) +
          ggtitle(each_site))
  print(ggplot(all_lm[all_lm$site == each_site,], aes(x = r)) +
          geom_density() +
          xlim(c(-1, 1)) +
          ggtitle(each_site))
}
