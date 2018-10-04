#run entire project pipeline

#download and clean data
mkdir data
mkdir data/temperature
mkdir data/portal
mkdir data/portal/raw
mkdir data/portal/clean
mkdir data/frayjorge
mkdir data/frayjorge/raw
mkdir data/frayjorge/clean

Rscript scripts/cleaning/clean_portal.R
Rscript scripts/cleaning/clean_frayjorge.R
Rscript scripts/cleaning/combine_occurrences.R

Rscript scripts/cleaning/site_locations.Rmd
Rscript scripts/cleaning/site_temps.R
Rscript scripts/cleaning/species_temps.R

#run analyses and produce plots
mkdir plots
mkdir plots/portal
mkdir plots/frayjorge

Rscript scripts/analysis/main_fig.R
Rscript scripts/analysis/lin_reg.R
Rscript scripts/analysis/size_distributions.R
