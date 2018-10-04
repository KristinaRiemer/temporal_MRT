library(ggplot2)
library(cowplot)
source("temporal_MRT/R/plot_size_distributions.R")

# Read in occurrences data
occurrences_path = "data/occurrences.csv"
occurrences = read.csv(occurrences_path)

# Plot size distributions as histograms and density plots
# TODO: scale size of plot to number of species for site
for(site in unique(occurrences$site)){
  site_data = occurrences[occurrences$site == site,]
  size_plot = plot_size_distributions(site_data)
  size_plot_path = paste0("plots/", site, "/", site, "_size.png")
  ggsave(size_plot_path, plot = size_plot)
}
