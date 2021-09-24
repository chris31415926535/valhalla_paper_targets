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
  tar_target(phh_ottawa,
             readr::read_csv(phh_ott_renfrew_file) %>%
               filter(stringr::str_detect(as.character(dbuid), "^3506"))
  ),
  
  tar_target(phh_renfrew,
             readr::read_csv(phh_ott_renfrew_file) %>%
               filter(stringr::str_detect(as.character(dbuid), "^3547"))
  ),
  ## Physician Data
  
  tar_target(docs_ottawa_file,
             "data/docs_ottawa_mapclean_2021-08-17.csv",
             format = "file"),
  
  tar_target(docs_renfrew_file,
             "data/docs_renfrew_2021-08-17.csv",
             format = "file"),
  tar_target(docs_renfrew,
             readr::read_csv(docs_renfrew_file) %>%
               select(cpso, lat, lon = lng) 
               ),
  tar_target(docs_ottawa,
             readr::read_csv(docs_ottawa_file) %>%
               select(cpso, lat, lon = lng)
             ),
  
  ######### ANALYSIS ######################
  
  ## TEST ANALYSIS
  # run the analysis  with a small subset of data to make sure it's working
  # commented out once confirmed
  # tar_target(test_od_valhalla,
  #            {
  #              doc_small <- head(docs_ottawa, 10)
  #              phh_small <- head(phh_ottawa, 1000)
  #              
  #              valhallr::od_table(froms = phh_small,
  #                                 from_id_col = "phh_id",
  #                                 tos = doc_small,
  #                                 to_id = "cpso",
  #                                 hostname = "192.168.2.34"
  #                                 ) %>%
  #                write_csv("outputs/csv/test_od_valhalla.csv")
  #            }),
  
  ## OTTAWA ANALYSIS
  
  # get OD table from ottawa PHHs to ottawa docs
  tar_target(od_valhalla_phh_ottawa,
             valhallr::od_table(froms = phh_ottawa,
                                from_id_col = "phh_id",
                                tos = docs_ottawa,
                                to_id = "cpso",
                                hostname = "192.168.2.34",
                                verbose = TRUE,
                                batch_size = 100
             ) %>%
               write_csv("outputs/csv/od_valhalla_phh_ottawa.csv")
             ),
  
  # now group and summarise to get the PHH distances/times to mean DB-levels
  # PHHs split DB populations evenly, so we don't need to pop-weight
  tar_target(od_valhalla_db_ottawa,
             od_valhalla_phh_ottawa %>% 
               left_join(select(phh_ottawa, 
                                phh_id, pop2016, dbuid), 
                         by = "phh_id") %>%
               group_by(dbuid, cpso) %>%
               summarise(dist = mean(distance),
                         time = mean(time)) %>%
               write_csv("outputs/csv/od_valhalla_db_ottawa.csv")
  ),
  
  ## RENFREW ANALYSIS
  
  # get OD table from renfrew PHHs to renfrew docs
  tar_target(od_valhalla_phh_renfrew,
             valhallr::od_table(froms = phh_renfrew,
                                from_id_col = "phh_id",
                                tos = docs_renfrew,
                                to_id = "cpso",
                                hostname = "192.168.2.34",
                                verbose = TRUE,
                                batch_size = 100
             ) %>%
               write_csv("outputs/csv/od_valhalla_phh_renfrew.csv")
  ),
  
  # now group and summarise to get the PHH distances/times to mean DB-levels
  # PHHs split DB populations evenly, so we don't need to pop-weight
  tar_target(od_valhalla_db_renfrew,
             od_valhalla_phh_renfrew %>% 
               left_join(select(phh_renfrew, 
                                phh_id, pop2016, dbuid), 
                         by = "phh_id") %>%
               group_by(dbuid, cpso) %>%
               summarise(dist = mean(distance),
                         time = mean(time)) %>%
               write_csv("outputs/csv/od_valhalla_db_renfrew.csv")
  ),
  ######### FIGURES ######################
  tar_target(simple_phh_plot,
            { phh_ottawa %>%
                bind_rows(phh_renfrew) %>%
               sf::st_as_sf(coords = c("lon", "lat"), crs = "WGS84") %>%
             ggplot() +
               ggspatial::annotation_map_tile() +
               geom_sf() +
               labs(title = "ISED Pseudo-Households in Ottawa and Renfrew County",
                    subtitle = "Any Type, Population > 0") } %>%
               ggsave(plot = .,
                      filename = "outputs/plots/simple_phh_plot.png")
             ),
  
  # test plot to make sure that we have the ottawa PHH and doc data loaded
  # commented out after confirming
  # tar_target(test_od_valhalla_plot,
  #            {
  #              {head(docs_ottawa, 10) %>%
  #                bind_rows(head(phh_ottawa, 1000)) %>%
  #                mutate(type = if_else(is.na(cpso), "phh", "cpso")) %>%
  #                select(lat, lon, type) %>%
  #                sf::st_as_sf(coords = c("lon", "lat"), crs = "WGS84") %>%
  #                ggplot() +
  #                ggspatial::annotation_map_tile() +
  #                geom_sf(aes(colour = type)) +
  #                  labs("10 Physicians and 1000 PHHs in Ottawa")} %>%
  #                ggsave(plot= .,
  #                       filename = "outputs/plots/test_od_valhalla_plot.png")
  #            }
  # ),
  ######### REPORTS ######################
  
  NULL
)
