# Size response to temperature across time

Using at least a dozen time series datasets of individual-level small mammal body size to determine how size is affected by temporal shifts in temperature. The null hypothesis is that there is no change in size that corresponds to temperature shifts, while the alternative hypothesis is that there is a negative relationship between size and temperature, in accordance with Bergmann's rule. 

## Data

* Raw mammal size datasets for each site in `data/raw_[site]`
* Temperature raster in `data/temperature`

## Scripts
_all in_ `scripts`

* Very simple first data and figures in `initial_cleaning_analysis.R`
* Entire rough pipeline, including end figures, in `initial_cleaning_analysis.Rmd`; get html rendering of R Notebook by running all of the code chunks and selecting `Preview`

## Other files

* Initial example of a Shiny app in `shiny_simple`, which just displays each species mean mass across year for each site. 
* Initial example of turning code into an R script in `packageSimple`, which only contains the two functions for converting coordinates to the temperature grid format. 
