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
                            "ggspatial",
                            "leaflet",
                            "onsr",
                            "htmltools",
                            "valhallr"))


## source() any function .R files here


# list of targets

list(
  ######### INPUT FILES ######################
  
  # load the pseudo-household dataset, filter it down to DBs in Ottawa & 
  # Renfrew county (by the first 4 digits of the DBUID), unselect hex IDs (not
  # used in our analysis) and province IDs (not needed--all in Ontario),
  # remove columns for dwelling counts (not used in this analysis),
  # rename for convenience, and finally remove any with population zero
  # then save to csv in data folder
  # result is a 5.5MB file, instead of a 120MB file
  # tar_target(generate_phh_ott_ren,
  #            readr::read_csv("../../large_shapefiles/psuedo_household_demographic_distribution/PHH-ON.csv") %>%
  #              mutate(DBUID_Ididu = as.character(DBUID_Ididu)) %>%
  #              filter(stringr::str_detect(DBUID_Ididu, "^3547|^3506")) %>%
  #              select(-HEXUID_IdUHEX, -Pruid_Pridu, TDwell2016_TLog2016,
  #                     -URDwell2016_RH2016) %>%
  #              rename(dbuid = DBUID_Ididu,
  #                     lat = "Latitude",
  #                     lon = "Longitude",
  #                     phh_id = PHH_ID,
  #                     type = Type,
  #                     pop2016 = Pop2016) %>%
  #              filter(pop2016 > 0) %>%
  #              write_csv("data/phh_ott_renfrew.csv")
  #            ),
  
  # specifying the PHH input file
  # we include it like this so that it detects if the file changes
  tar_target(phh_ott_renfrew_file,
             "data/phh_ott_renfrew.csv",
             format = "file"),
  
  # loading the PHH input file
  tar_target(phh_ott_renfrew,
             readr::read_csv(phh_ott_renfrew_file)
  ),
  
  
  ######### ANALYSIS ######################
  
  
  ######### FIGURES ######################
  tar_target(simple_phh_plot,
             phh_ott_renfrew %>%
               sf::st_as_sf(coords = c("lon", "lat"), crs = "WGS84") %>%
             ggplot() +
               ggspatial::annotation_map_tile() +
               geom_sf() +
               labs(title = "ISE Pseudo-Households in Ottawa and Renfrew County",
                    subtitle = "Any Type, Population > 0")
             ),
  
  ######### REPORTS ######################
  
  NULL
)
