# Install pacman
if("pacman" %in% rownames(installed.packages()) == FALSE){
  install.packages("pacman")
}

# TODO: create list of all packages for installing and version checking

# Install required packages
# TODO: include instructions or automation for installing Data Retriever
# TODO: install specific versions of packages used
pacman::p_load(rdataretriever, 
               tidyr, 
               dplyr, 
               raster, 
               ncdf4, 
               ggplot2, 
               broom, 
               cowplot, 
               purrr)

# TODO: use packageVersion() to check installations worked and their versions
