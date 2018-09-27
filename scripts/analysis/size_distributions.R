library(ggplot2)

ggplot(by_sp_yr_portal, aes(x = mass_mean)) +
  geom_histogram() +
  scale_x_log10() +
  facet_wrap(~yr)
ggplot(by_sp_yr_portal, aes(x = mass_mean)) +
  geom_density() +
  scale_x_log10() +
  facet_wrap(~yr)

ggplot(by_sp_yr_fray_jorge, aes(x = mass_mean)) +
  geom_histogram() +
  scale_x_log10() +
  facet_wrap(~yr)
ggplot(by_sp_yr_fray_jorge, aes(x = mass_mean)) +
  geom_density() +
  scale_x_log10() +
  facet_wrap(~yr)
