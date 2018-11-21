library(dplyr)
library(tidyr)
library(ggplot2)
library(forecast)
library(lmtest)
library(purrr)
library(cowplot)

# Read in occurrences with temperature data
occurrences_with_temp_path = "data/occurrences_with_temp.csv"
occurrences_with_temp = read.csv(occurrences_with_temp_path)

# Run models on all species, creating diagnostics plots and saving p-values
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
    folder = "plots/time_series_model_diagnostics/"
    png_title1 = paste0(folder, site, species, "1", ".png")
    dev.copy(png, png_title1)
    dev.off()
    temp_ts = ts(species_occurrences$avg_temp)
    exog_model = auto.arima(mass_ts, xreg = temp_ts)
    exog_pvalue = coeftest(exog_model)[dim(coeftest(exog_model))[1], dim(coeftest(exog_model))[2]]
    title_pg2 = paste0("Model residuals for ", species, " (", site, ")")
    tsdisplay(residuals(exog_model), main = title_pg2)
    png_title2 = paste0(folder, site, species, "2", ".png")
    dev.copy(png, png_title2)
    dev.off()
    mass_model = Arima(mass_ts, order = arimaorder(exog_model), include.drift = TRUE)
    mass_pvalue = coeftest(mass_model)[dim(coeftest(mass_model))[1], dim(coeftest(mass_model))[2]]
    mass_dir = coeftest(mass_model)[dim(coeftest(mass_model))[1], 1]
    pvalues = data.frame(site = site, species = species, exog_pvalue = exog_pvalue, 
                         mass_pvalue = mass_pvalue, mass_dir = mass_dir)
    model_stats = rbind(model_stats, pvalues)
  }
}

# Adjust p-values and add significance column
model_stats$exog_pvalue_adjust = p.adjust(model_stats$exog_pvalue)
model_stats$mass_pvalue_adjust = p.adjust(model_stats$mass_pvalue)
model_stats = model_stats %>% 
  mutate(exog_pvalue_sig = case_when(exog_pvalue_adjust <= 0.05 ~ "yes", 
                                     exog_pvalue_adjust > 0.05 ~ "no"), 
         mass_pvalue_sig = case_when(mass_pvalue_adjust <= 0.05 ~ "yes", 
                                     mass_pvalue_adjust > 0.05 ~ "no"))

# Combine occurrences and model results
occurrences_with_temp = left_join(occurrences_with_temp, model_stats, by = c("species" = "species", 
                                                             "site" = "site"))

# Plot mass and temp time series with model p-values
occurrences_with_temp = occurrences_with_temp %>% 
  mutate(pvalue_star = case_when(exog_pvalue_sig == "no" ~ "", 
                                 exog_pvalue_sig == "yes" ~ "*"),
         exog_pvalue_adjust_plot = round(exog_pvalue_adjust, digits = 3), 
         plot_label = paste0(species, " (p = ", exog_pvalue_adjust_plot, ") ", pvalue_star))

plots_df = occurrences_with_temp %>% 
  group_by(site) %>% 
  nest() %>% 
  mutate(
    mass_temp_by_year = purrr::map(data, ~ ggplot(.) +
                                     geom_line(aes(x = yr, y = mass_mean)) +
                                     geom_point(aes(x = yr, y = mass_mean)) +
                                     geom_line(aes(x = yr, y = avg_temp, color = "red")) +
                                     geom_point(aes(x = yr, y = avg_temp, color = "red")) +
                                     labs(x = "Year", y = "Mean Temp (C*)/Mean Mass (g)") +
                                     theme(legend.position = "none") +
                                     facet_wrap(~plot_label, scales = "free"))
  )

combined = purrr::pmap(list(plots_df$mass_temp_by_year[1], plots_df$mass_temp_by_year[2], 
                            plots_df$mass_temp_by_year[3]), 
                       ~ cowplot::plot_grid(plot_grid(..1), plot_grid(..2), 
                                            plot_grid(..3), 
                                            labels = c("A", "B", "C")))

# Save dataframe as csv
write.csv(model_stats, "data/model_stats.csv")

# Save plot
ggsave_args = list("plots/time_series_plot.png", plot = combined, 
                         width = 17, height = 12)
pmap(ggsave_args, ggsave)
