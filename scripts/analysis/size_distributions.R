library(ggplot2)
library(cowplot)

# Read in occurrences data
occurrences_path = "data/occurrences.csv"
occurrences = read.csv(occurrences_path)

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
  print(size_both)
  ggsave(site_file_name, plot = size_both)
}

# Plot size distributions as histograms and density plots
for(site in unique(occurrences$site)){
  plot_size_distributions(site)
}
