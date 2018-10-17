# Size response to temperature across time

Using at least a dozen time series datasets of individual-level small mammal body size to determine how size is affected by temporal shifts in temperature. The null hypothesis is that there is no change in size that corresponds to temperature shifts, while the alternative hypothesis is that there is a negative relationship between size and temperature, in accordance with Bergmann's rule. 

## Packages

Install with `Rscript scripts/r_packages.R`

Versions: 

* rdataretriever_1.0.0
* tidyr_0.8.0
* dplyr_0.7.4
* raster_2.7-15
* ncdf4_1.16
* ggplot2_3.0.0
* broom_0.4.3
* cowplot_0.9.1
* purrr_0.2.4

## Data

* Temperature raster in `data/temperature`
* Raw mammal size datasets for each site in `data/[site]/raw`
* Cleaned mammal size datasets (by `clean_[site].R`) in `data/[site]/raw`

## Scripts

* Scripts for cleaning, organizing, and combining datasets in `scripts/cleaning` listed in order they need to be run
	* `clean_[site].R` is for cleaning each size dataset, one unique script per site
	* `combine_occurrences.R` is for combining clean size datasets for all sites
	* `site_locations.Rmd` compiles the coordinates for each site, including documentation for sources
	* `site_temps.R` converts site coordinates to temperature raster format, plots them on temperature data from a single month, and extracts all annual temperatures for all sites
	* `species_temps.R` combines mass and temperature datasets for each site together
* Scripts for statistical analysis and visualization in `scripts/analysis`, should be able to run them in any order because they are standalone
	* `main_fig.R` plots the main combined figure for each site; this includes annual temperature, annual mean mass per species, and temperature-mass relationship
	* `lin_reg.R` runs linear regression on each species temperature-mass relationship, extracts r and p-values, and plots these values
	* `size_distributions.R` plots size distribution of community across time, both at the species and individual levels
* Entire rough pipeline, including end figures, in `scripts/initial_cleaning_analysis.Rmd`; get html rendering of R Notebook by running all of the code chunks and selecting `Preview`
* Very simple first data and figures in `scripts/initial_cleaning_analysis.R`

## Other files

* Actual Shiny app to dynamically display project outputs in `shiny`, which currently displays only the size distributions plots by site
* Initial example of a Shiny app in `shiny_simple`, which just displays each species mean mass across year for each site. 
* Functions go in the `R` folder for the not-yet-completely-formed package `temporal_MRT` 
* Initial example of turning code into an R script in `packageSimple`, which only contains the two functions for converting coordinates to the temperature grid format. 
