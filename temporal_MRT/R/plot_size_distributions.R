plot_size_distributions = function(site_data){
  size_hist = ggplot(site_data, aes(x = mass_mean)) +
    geom_histogram() +
    scale_x_log10() +
    labs(x = "Mean species size", y = "Number of species") +
    facet_wrap(~yr)
  size_density = ggplot(site_data, aes(x = mass_mean)) +
    geom_density() +
    scale_x_log10() +
    labs(x = "Mean species size", y = "Density of species") +
    facet_wrap(~yr)
  size_both = plot_grid(size_hist, size_density)
  return(size_both)
}
