library(dplyr)
library(tidyr)
library(ggplot2)
library(forecast)
library(lmtest)

# Read in occurrences with temperature data
occurrences_with_temp_path = "data/occurrences_with_temp.csv"
occurrences_with_temp = read.csv(occurrences_with_temp_path)

# Run model on all species, creating diagnostics plots and saving p-values
pdf(file = "plots/time_series_model_diagnostics.pdf")
model_stats = data.frame(site = factor(), species = factor(), pvalue = numeric())
for(site in unique(occurrences_with_temp$site)){
  site_occurrences = occurrences_with_temp[occurrences_with_temp$site == site,]
  min_year = min(site_occurrences$yr)
  max_year = max(site_occurrences$yr)
  for(species in unique(site_occurrences$species)){
    species_occurrences = site_occurrences[site_occurrences$species == species,]
    species_occurrences = complete(species_occurrences, yr = min_year:max_year)
    mass_ts = ts(species_occurrences$mass_mean)
    par(mfrow = c(2, 1))
    title_pg1 = paste0("Mass time series diagnostics for ", species, " (", site, ")")
    Acf(mass_ts, main = title_pg1)
    Pacf(mass_ts, main = "")
    # TODO: test some autos against customs
    temp_ts = ts(species_occurrences$avg_temp)
    species_model = auto.arima(mass_ts, xreg = temp_ts)
    title_pg2 = paste0("Model residuals for ", species, " (", site, ")")
    tsdisplay(residuals(species_model), main = title_pg2)
    species_pvalue = data.frame(site = site, species = species, 
                                pvalue = coeftest(species_model)[dim(coeftest(species_model))[1], dim(coeftest(species_model))[2]])
    model_stats = rbind(model_stats, species_pvalue)
  }
}
dev.off()

# Adjust p-values and add significance column
model_stats$pvalue_adjust = p.adjust(model_stats$pvalue)
model_stats = model_stats %>% 
  mutate(pvalue_sig = case_when(pvalue_adjust <= 0.05 ~ "yes", 
                                pvalue_adjust > 0.05 ~ "no"))

# Save dataframe as csv
write.csv(model_stats, "data/model_stats.csv")
