plot_masses = function(mass_data){
  ggplot(data = mass_data, aes(x = yr, y = mass_mean)) +
    geom_point() +
    geom_errorbar(aes(ymin = mass_mean - mass_sd, ymax = mass_mean + mass_sd)) +
    facet_wrap(~species, scales = "free_y")
}
