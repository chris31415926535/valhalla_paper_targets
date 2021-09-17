# targets file for Valhalla paper

# to get a network dependency diagram:
# targets::tar_visnetwork()

# to build the pipeline:
# targets::tar_make()

library(targets)
library(tarchetypes)

options(tidyverse.quiet = TRUE)

tar_option_set(packages = c("tidyverse",
                            "sf",
                            "leaflet",
                            "onsr",
                            "htmltools",
                            "valhallr"))


## source() any function .R files here


# list of targets

list(
  ######### INPUT FILES ######################
  
  
  ######### ANALYSIS ######################
  
  
  ######### FIGURES ######################
  
  
  ######### REPORTS ######################
  
)