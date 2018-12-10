#run entire project pipeline

mkdir plots

#download and clean data
mkdir data
mkdir data/temperature
mkdir data/portal
mkdir data/portal/raw
mkdir data/portal/clean
mkdir data/frayjorge
mkdir data/frayjorge/raw
mkdir data/frayjorge/clean
mkdir data/sevilleta
mkdir data/sevilleta/raw
mkdir data/sevilleta/clean

Rscript scripts/cleaning/clean_portal.R
Rscript scripts/cleaning/clean_frayjorge.R
Rscript scripts/cleaning/clean_sevilleta.R
Rscript scripts/cleaning/combine_occurrences.R

Rscript -e 'rmarkdown::render("scripts/cleaning/site_locations.Rmd")'
Rscript scripts/cleaning/site_temps.R
Rscript scripts/cleaning/species_temps.R

Rscript scripts/cleaning/mass_thresholds.R
Rscript scripts/cleaning/species_codes.R

#run analyses and produce plots
mkdir plots/time_series_model_diagnostics

Rscript scripts/analysis/time_series_models.R
Rscript scripts/analysis/lin_reg.R
Rscript scripts/analysis/main_fig.R
Rscript scripts/analysis/synth_fig.R

Rscript scripts/analysis/site_summary_stats.R
Rscript scripts/analysis/supp_all_sp.R
Rscript scripts/analysis/repeat_species.R
Rscript scripts/analysis/sevilleta_subsites.R
