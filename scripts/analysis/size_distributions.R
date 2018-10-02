library(dplyr)
library(ggplot2)
library(cowplot)

# Functions
plot_size_distributions = function(site){
  size_hist = ggplot(occurrences[occurrences$site == site,], aes(x = mass_mean)) +
    geom_histogram() +
    scale_x_log10() +
    facet_wrap(~yr)
  size_density = ggplot(occurrences[occurrences$site == site,], aes(x = mass_mean)) +
    geom_density() +
    scale_x_log10() +
    facet_wrap(~yr)
  size_both = plot_grid(size_hist, size_density)
  site_file_name = paste0("plots/", site, "/", site, "_size.png")
  ggsave(site_file_name, plot = size_both)
}

# Read in occurrences data
sites = c("portal", "frayjorge")
occurrences = data.frame()
for(site in sites){
  site_occurrences_path = paste0("data/", site, "/clean/occurrences.csv")
  site_occurrences_df = read.csv(site_occurrences_path)
  site_occurrences_df$site = c(site)
  occurrences = bind_rows(occurrences, site_occurrences_df)
}

# Plot size distributions as histograms and density plots
for(site in sites){
  plot_size_distributions(site)
}
