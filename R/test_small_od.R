# getting some random start and end PHH points for a test OD matrix in ArcGIS Pro

library(tidyverse)
library(targets)
library(valhallr)


######## FUCTIONS

#FUNCTION TO RUN THE ANALYSIS AND SAVE RESULTS
# https://stackoverflow.com/questions/24309910/how-to-get-name-of-variable-in-r-substitute
do_valhalla_od_analysis <- function(start_points, end_points){
  
  # get the variable names in character format
  start_name <- rlang::enexpr(start_points) %>% 
    as.character() 
  
  end_name <- rlang::enexpr(end_points) %>% 
    as.character()
  
  # update to console
  message(sprintf("Running Valhalla analysis from %s to %s.", start_name, end_name))
  
  # create output filename
  filename <- sprintf("od_val_%s_to_%s.csv", start_name, end_name) %>%
    stringr::str_remove_all("phhs_")
  
  # create stripped-down input versions
  starts <- sf::st_set_geometry(start_points, NULL) %>%
    select(rowid, lat, lon)
  
  ends <- sf::st_set_geometry(end_points, NULL) %>%
    select(rowid,lat,lon)
  
  # run valhalla analysis
  od_valhalla <- valhallr::od_table(froms = starts,
                                    from_id_col = "rowid",
                                    tos = ends,
                                    to_id_col = "rowid",
                                    hostname = "192.168.2.39",
                                    verbose = TRUE,
                                    batch_size = 100)
  
  # save to appropriately named csv
  od_valhalla %>%
    write_csv(paste0("outputs/od_tables/valhalla/", filename))
  
  
  return()
  #stop("make this run the actual analysis")
}

# function to join valhalla to  DMTI predictions and create comp table
compare_tables <- function(od_val, od_dmti){
  
  # get from and to from variable names. not pretty but this works
  fullname <- rlang::enexpr(od_val) %>% 
    as.character()
  
  from <- stringr::str_split(fullname, "_") %>%
    unlist() %>%
    pluck(3)
  
  to <- stringr::str_split(fullname, "_") %>%
    unlist() %>%
    pluck(4)
  
  # sort the indices out--arcgis can make them weird
  dmti_origin_adjust <- min(od_dmti$OriginID) - 1
  dmti_dest_adjust <- min(od_dmti$DestinationID) - 1
  
  comp_table <- od_val %>% 
    mutate(OriginID = rowid_from + dmti_origin_adjust, DestinationID = rowid_to + dmti_dest_adjust) %>% 
    left_join(od_dmti) %>%
    rename(time_dmti = Total_Minutes,
           time_val = time) %>%
    mutate(time_val = time_val/60,
           time_diff = time_dmti - time_val,
           time_diff_pct = time_diff / time_dmti,
           from = from,
           to = to)
  
  return(comp_table)
}

# function to summarise diffs from comp tables
summarise_diffs <- function(comp_table, verbose = TRUE){
  
  comp_table %>%
    summarise(time_diff_mean= mean(time_diff, na.rm = TRUE),
              time_diff_sd =  sd(time_diff, na.rm = TRUE),
              time_diff_pct_mean= mean(time_diff_pct, na.rm = TRUE),
              time_diff_pct_sd =  sd(time_diff_pct, na.rm = TRUE),
              from = unique(from),
              to = unique(to))
  
}

############## ANALYSIS

# Number of PHHs from each rurality class. Please use an even number!
num_phhs <- 1000

# load the ottawa pseudo households
targets::tar_load(phh_ottawa)

# Standardise our random selections
set.seed(1)

# Load the rurality boundaries, based on ONS neighbourhoods
rurality <- sf::read_sf("data/shp/ruralities.geojson") 

## GET THE PHH POINTS
# Rural PHHs
phhs_rural <- phh_ottawa %>%
  sf::st_as_sf(coords = c("lon", "lat"), crs = "WGS84", remove = FALSE) %>%
  sf::st_filter(filter(rurality, rurality == "Rural")) %>%
  dplyr::slice_sample(n=num_phhs) %>%
  rowid_to_column()

phhs_rural1 <- phhs_rural %>%
  dplyr::slice_head(prop = .5)

phhs_rural2 <- phhs_rural %>%
  dplyr::slice_tail(prop = .5)

# Suburban PHHs
phhs_suburban <- phh_ottawa %>%
  sf::st_as_sf(coords = c("lon", "lat"), crs = "WGS84", remove = FALSE) %>%
  sf::st_filter(filter(rurality, rurality == "Suburban")) %>%
  dplyr::slice_sample(n=num_phhs) %>%
  rowid_to_column()

phhs_suburban1 <- phhs_suburban %>%
  dplyr::slice_head(prop = .5)

phhs_suburban2 <- phhs_suburban %>%
  dplyr::slice_tail(prop = .5)

# Urban PHHs
phhs_urban <- phh_ottawa %>%
  sf::st_as_sf(coords = c("lon", "lat"), crs = "WGS84", remove = FALSE) %>%
  sf::st_filter(filter(rurality, rurality == "Urban")) %>%
  dplyr::slice_sample(n=num_phhs) %>%
  rowid_to_column()

phhs_urban1 <- phhs_urban %>%
  dplyr::slice_head(prop = .5)

phhs_urban2 <- phhs_urban %>%
  dplyr::slice_tail(prop = .5)

## MAKE A PLOT

ggplot()  + geom_sf(data=rurality)+ 
  stat_sf_coordinates(data=phhs_urban1, colour = "yellow", shape = "square") +
  stat_sf_coordinates(data=phhs_urban2, colour = "yellow", shape = "circle") +
  stat_sf_coordinates(data=phhs_suburban1, colour = "purple", shape = "square") +
  stat_sf_coordinates(data=phhs_suburban2, colour = "purple", shape = "circle") +
  stat_sf_coordinates(data=phhs_rural1, colour = "blue", shape = "square") +
  stat_sf_coordinates(data=phhs_rural2, colour = "blue", shape = "circle") 

## WRITE THEM ALL TO FILE
# Urban
phhs_urban %>%
  sf::write_sf("data/shp/urban/phhs_urban.shp")

phhs_urban1 %>%
  sf::write_sf("data/shp/urban/phhs_urban1.shp")

phhs_urban2 %>%
  sf::write_sf("data/shp/urban/phhs_urban2.shp")

# Suburban
phhs_suburban %>%
  sf::write_sf("data/shp/suburban/phhs_suburban.shp")

phhs_suburban1 %>%
  sf::write_sf("data/shp/suburban/phhs_suburban1.shp")

phhs_suburban2 %>%
  sf::write_sf("data/shp/suburban/phhs_suburban2.shp")

# Rural
phhs_rural %>%
  sf::write_sf("data/shp/rural/phhs_rural.shp")

phhs_rural1 %>%
  sf::write_sf("data/shp/rural/phhs_rural1.shp")

phhs_rural2 %>%
  sf::write_sf("data/shp/rural/phhs_rural2.shp")



#########################
# CALL THE FUNCTION TO RUN THE ANALYSES
# could this be done with a functional? probably, but whatever
do_valhalla_od_analysis(phhs_urban,phhs_rural)
do_valhalla_od_analysis(phhs_suburban,phhs_rural)
do_valhalla_od_analysis(phhs_suburban,phhs_urban)
do_valhalla_od_analysis(phhs_rural, phhs_urban)
do_valhalla_od_analysis(phhs_urban, phhs_suburban)
do_valhalla_od_analysis(phhs_rural, phhs_suburban)
do_valhalla_od_analysis(phhs_urban1, phhs_urban2)
do_valhalla_od_analysis(phhs_suburban1, phhs_suburban2)
do_valhalla_od_analysis(phhs_rural1, phhs_rural2)



####################################################
## COMPARISON ANALYSIS

####### LOAD COMPUTED ORIGIN DESTINATION TABLES
od_dmti_urban_rural <- read_csv("outputs/od_tables/arcgis/od_dmti_urban_to_rural.csv")
od_dmti_urban_suburban <- read_csv("outputs/od_tables/arcgis/od_dmti_urban_to_suburban.csv")
od_dmti_suburban_urban <- read_csv("outputs/od_tables/arcgis/od_dmti_suburban_to_urban.csv")
od_dmti_suburban_rural <- read_csv("outputs/od_tables/arcgis/od_dmti_suburban_to_rural.csv")
od_dmti_rural_urban <- read_csv("outputs/od_tables/arcgis/od_dmti_rural_to_urban.csv")
od_dmti_rural_suburban <- read_csv("outputs/od_tables/arcgis/od_dmti_rural_to_suburban.csv")

od_val_urban_suburban <- read_csv("outputs/od_tables/valhalla/od_val_urban_to_suburban.csv")
od_val_urban_rural <- read_csv("outputs/od_tables/valhalla/od_val_urban_to_rural.csv")
od_val_suburban_urban <- read_csv("outputs/od_tables/valhalla/od_val_suburban_to_urban.csv")
od_val_suburban_rural <- read_csv("outputs/od_tables/valhalla/od_val_suburban_to_rural.csv")
od_val_rural_urban <- read_csv("outputs/od_tables/valhalla/od_val_rural_to_urban.csv")
od_val_rural_suburban <- read_csv("outputs/od_tables/valhalla/od_val_rural_to_suburban.csv")

####### CREATE COMPARISON TABLES (transitory) and SUMMARISE DIFFS

comp_urban_rural <- compare_tables(od_val_urban_rural, od_dmti_urban_rural)
comp_urban_suburban <- compare_tables(od_val_urban_suburban, od_dmti_urban_suburban)
comp_suburban_urban <- compare_tables(od_val_suburban_urban, od_dmti_suburban_urban)
comp_suburban_rural <- compare_tables(od_val_suburban_rural, od_dmti_suburban_rural)
comp_rural_urban <- compare_tables(od_val_rural_urban, od_dmti_rural_urban)
comp_rural_suburban <- compare_tables(od_val_rural_suburban, od_dmti_rural_suburban)


### SUMMARISE DIFFS

diff_summaries <- summarise_diffs(comp_urban_rural) %>%
  bind_rows(summarise_diffs(comp_urban_suburban)) %>%
  bind_rows(summarise_diffs(comp_suburban_urban)) %>%
  bind_rows(summarise_diffs(comp_suburban_rural)) %>%
  bind_rows(summarise_diffs(comp_rural_urban)) %>%
  bind_rows(summarise_diffs(comp_rural_suburban)) %>%
  select(from, to, everything())

diff_summaries %>% knitr::kable() %>% kableExtra::kable_styling()

#################

all_comp <- bind_rows(comp_urban_rural,
          comp_urban_suburban,
          comp_suburban_rural,
          comp_suburban_urban,
          comp_rural_suburban,
          comp_rural_urban)

all_comp %>% filter(abs(time_diff_pct) > .5) %>%
  group_by(rowid_to, to) %>% count() %>% arrange(desc(n))


# a small number of predictions are several multiples in V cmp DMTI
# should look more closely
comp_table %>% ggplot() + geom_histogram(aes(x=time_diff_pct))
comp_table %>% filter(time_diff_pct > -.5) %>%ggplot() + geom_histogram(aes(x=time_diff_pct))

comp_table %>% filter(abs(time_diff_pct) > .5) %>%
  group_by(rowid_from) %>% count() %>% arrange(desc(n))

comp_table %>% filter(abs(time_diff_pct) > .5) %>%
  group_by(rowid_to) %>% count() %>% arrange(desc(n))


############# EXAMINING WEIRD POINTS
# suburban row 266 is the worst "to", It's in the middle of an ON RAMP
# suburban row 515 is also bad, it's on the middle of the vanie parkway bridge over the 417!
# So trips need to come towrds it on a major road, take the off ramp, do a 
# detour, turn around, then get on the on ramp
# same with row 515, DMTI does a u-turn at riverside and tremblay, valhalla is
# a lot more conservative and does a u-turn later
bad_to_rowid <- 515#266
bad_to <- phhs_suburban %>% filter(rowid == bad_to_rowid) 


comp_urban_suburban %>%
  filter(rowid_to == bad_to_rowid) %>%
  pull(time_diff_pct) %>% hist()

iso <- bad_to %>%
  valhallr::isochrone(costing = "auto", hostname = "192.168.2.39")

valhallr::map_isochrone(iso)

bad_from_rowid <- 8
bad_from <- filter(phhs_urban, rowid == bad_from_rowid)

bad_trip <- valhallr::route(from = bad_from, 
                            to = bad_to, 
                            hostname = "192.168.2.39")

valhallr::map_trip(bad_trip)%>%
  addMarkers(data = bad_from, label = paste0("From: ",bad_from$phh_id)) %>%
  addMarkers(data = bad_to, label = paste0("To: ",bad_to$phh_id))


# # old version
# summarise_diffs_old <- function(od_val, od_dmti){
#   dmti_origin_adjust <- min(dmti$OriginID) - 1
#   dmti_dest_adjust <- min(dmti$DestinationID) - 1
#   
# comp_table <- od_val %>% 
#   mutate(OriginID = rowid_from + dmti_origin_adjust, DestinationID = rowid_to + dmti_dest_adjust) %>% 
#   left_join(od_dmti) %>%
#   rename(time_dmti = Total_Minutes,
#          time_val = time) %>%
#   mutate(time_val = time_val/60,
#          time_diff = time_dmti - time_val,
#          time_diff_pct = time_diff / time_dmti)
# 
# comp_table %>%
#   summarise(time_diff_mean= mean(time_diff, na.rm = TRUE),
#             time_diff_sd =  sd(time_diff, na.rm = TRUE),
#             time_diff_pct_mean= mean(time_diff_pct, na.rm = TRUE),
#             time_diff_pct_sd =  sd(time_diff_pct, na.rm = TRUE))
# 
# }

##############
# test <- read_csv("outputs/od_tables/valhalla/odmatrix_val_urban_to_rural.csv")
# test2 <- read_csv("outputs/od_tables/valhalla/od_val_urban_to_rural_old.csv")
# 
# testthat::expect_equal(test, test2)


##### TSTING URBAN TO RURAL

od_dmti_urban_rural <- read_csv("outputs/arcgis/od_dmti_urban_to_rural.csv")

starts <- sf::st_set_geometry(phhs_urban, NULL) %>%
  select(rowid, lat, lon)
ends <- sf::st_set_geometry(phhs_rural, NULL) %>%
  select(rowid,lat,lon)

od_valhalla_urban_rural <- valhallr::od_table(froms = starts,
                                              from_id_col = "rowid",
                                              tos = ends,
                                              to_id_col = "rowid",
                                              hostname = "192.168.2.39",
                                              verbose = TRUE,
                                              batch_size = 100
)

od_valhalla_urban_rural %>%
  write_csv("outputs/csv/od_val_urban_to_rural.csv")


## URBAN TO RURAL
# join valhalla to  DMTI predictions
comp_table <- od_valhalla_urban_rural %>% 
  mutate(OriginID = rowid_from + 10, DestinationID = rowid_to + 10) %>% 
  left_join(od_dmti_urban_rural) %>%
  rename(time_dmti = Total_Minutes,
         time_val = time) %>%
  mutate(time_val = time_val/60,
         time_diff = time_dmti - time_val,
         time_diff_pct = time_diff / time_dmti)

comp_table %>%
  summarise(time_diff_mean= mean(time_diff, na.rm = TRUE),
            time_diff_sd =  sd(time_diff, na.rm = TRUE),
            time_diff_pct_mean= mean(time_diff_pct, na.rm = TRUE),
            time_diff_pct_sd =  sd(time_diff_pct, na.rm = TRUE)
  ) %>%
  knitr::kable(col.names = c("Diff Mean (min)", "Diff SD (min)", "% Diff Mean", "% Diff SD")) %>%
  kableExtra::kable_styling()

# and a heatmap
comp_table %>% 
  slice_head(n=1000000) %>% 
  ggplot() + 
  geom_density2d_filled(aes(x=time_dmti, y = time_diff))


phh_start <- phhs[1:10,]

phh_start <- rowid_to_column(phh_start)

phh_end <- phhs[11:20,]
phh_end  <- rowid_to_column(phh_end)

phh_start %>%
  sf::st_as_sf(coords = c("lon", "lat"), crs = "WGS84") %>%
  sf::write_sf("data/phh_start_10.shp")

phh_end %>%
  sf::st_as_sf(coords = c("lon", "lat"), crs = "WGS84") %>%
  sf::write_sf("data/phh_end_10.shp")


## start here though
phh_start <- sf::read_sf("data/phh_start_10.shp") %>%
  valhallr::sf_to_latlon() %>%
  rowid_to_column()

phh_end <- sf::read_sf("data/phh_end_10.shp") %>%
  valhallr::sf_to_latlon() %>%
  rowid_to_column()

od_valhalla <- valhallr::od_table(froms = phh_start,
                                  from_id_col = "rowid",
                                  tos = phh_end,
                                  to_id_col = "rowid",
                                  hostname = "192.168.2.39",
)

# from arcgis's black-box network solver
od_arcgis <- read_csv("outputs/arcgis/naresults_20211202_162745_aa148319b3f4410f9ccb9148f3b40dec/ODLines.csv") %>%
  select(rowid_from = OriginOID, rowid_to = DestinationOID, time_arc = Total_Time, dist_arc = Total_Distance)

# from DMTI arcgispro network analyst
od_dmti <- read_csv("data/odmatrix_arcgis_phh10_dmti.csv") %>%
  select(rowid_from = OriginID, rowid_to = DestinationID, time_dmti = Total_Minutes)

comp_table <- od_valhalla %>%
  rename(dist_v = distance,
         time_v = time) %>%
  mutate(time_v = time_v/60) %>%
  left_join(od_arcgis) %>%
  left_join(od_dmti)
#mutate(diff_dist = dist_arc - dist_v,
#       diff_time = time_arc - (time_v/60))


comp_table_diff <- comp_table %>%
  mutate(diff_pct = (time_dmti - time_v)/time_v,
         diff_time = (time_dmti - time_v))

comp_table_diff %>%
  summarise(mean_diff_pct = mean(diff_pct),
            sd_diff_pct = sd(diff_pct),
            mean_diff_time = mean(diff_time),
            sd_diff_time = sd(diff_time))
ggplot(aes(x=))


hist(comp_table$diff_dist)
hist(comp_table$diff_time)

badfrom = c(45.31361, -75.91519)
badto = c(45.02590,    -75.76437)

badtrip <- valhallr::route(from = tibble(lat = badfrom[[1]], lon = badfrom[[2]]), 
                           to = tibble(lat = badto[[1]], lon = badto[[2]]),
                           hostname = "192.168.2.39")

valhallr::map_trip(badtrip)

comp_table %>% 
  ggplot() + 
  geom_histogram(aes(x=diff_time), bins = 11) + 
  theme_minimal() + 
  labs(title="Difference in Time (Arc - Valhalla)", subtitle = "10x10 OD Matrix, random pseudo-households in and around Ottawa ON")



################# PHYSICIAN ANALYSIS

# set up the shapefiles
# docs_ottawa <- read_csv("data/shp/physicians/docs_ottawa_mapclean_2021-08-17.csv") %>%
#   rename(lon = lng) %>%
#   rowid_to_column()
# docs_ottawa %>% sf::st_as_sf(coords = c("lon", "lat"), crs = "WGS84", remove = FALSE) %>%
#   sf::write_sf("data/shp/physicians/docs_ottawa.shp")


## read the data
docs_ottawa <- sf::read_sf("data/shp/physicians/docs_ottawa.shp") 
phhs_urban <- sf::read_sf("data/shp/urban/phhs_urban.shp")
phhs_suburban <- sf::read_sf("data/shp/suburban/phhs_suburban.shp")
phhs_rural <- sf::read_sf("data/shp/rural/phhs_rural.shp")

#########################
# CALL THE FUNCTION TO RUN THE ANALYSES
# could this be done with a functional? probably, but whatever
do_valhalla_od_analysis(phhs_urban,docs_ottawa)
do_valhalla_od_analysis(phhs_suburban,docs_ottawa)
do_valhalla_od_analysis(phhs_rural,docs_ottawa)


# TEST AGAINST DMTI FOR URBAN

od_val <- read_csv("outputs/od_tables/valhalla/od_val_urban_to_docs_ottawa.csv") %>%
  left_join(docs_ottawa %>%
              sf::st_set_geometry(NULL) %>%
              select(rowid_to = rowid, cpso) ) %>%
  left_join(phhs_urban %>%
              sf::st_set_geometry(NULL) %>%
              select(rowid_from = rowid, phh_id, dbuid, pop2016)) %>%
  mutate(cpso = as.character(cpso),
         phh_id = as.character(phh_id)) %>%
  select(-starts_with("rowid"))

od_dmti3 <- read_csv("outputs/od_tables/arcgis/od_dmti_urban_to_docs_ottawa3.csv") %>%
  separate(col = Name, into = c("phh_id", "cpso"), remove = FALSE)

test <- od_val %>%
  left_join(od_dmti3) %>%
  mutate(dist_val = distance,
         time_val = time/60,
         time_dmti = Total_Minutes) %>%
  select(phh_id, cpso, dist_val, time_val, time_dmti, dbuid, pop2016) %>%
  mutate(time_diff = time_dmti - time_val,
         time_diff_pct = time_diff/time_dmti)

test %>%
  summarise(time_diff_mean = mean(time_diff, na.rm= TRUE),
            time_diff_sd = sd(time_diff, na.rm = TRUE),
            time_diff_pct_mean= mean(time_diff_pct, na.rm = TRUE),
            time_diff_pct_sd = sd(time_diff_pct, na.rm = TRUE))


test$time_diff_pct %>% hist()
test$time_diff_pct %>% summary()
test$time_diff_pct %>% boxplot()

##################
# Check for DBUID level

dbuid_comp <- test %>%
  group_by(dbuid) %>%
  summarise(db_time_val = mean(time_val, na.rm = TRUE),
            db_time_dmti = mean(time_dmti, na.rm = TRUE),
            db_time_diff = db_time_dmti - db_time_val,
            db_time_diff_pct = db_time_diff/db_time_dmti,
            pop2016 = pop2016 * n()
            ) %>%
  distinct()

dbuid_comp %>%
  summarise(db_time_diff_mean = mean(db_time_diff, na.rm= TRUE),
            db_time_diff_sd = sd(db_time_diff, na.rm = TRUE),
            db_time_diff_pct_mean= mean(db_time_diff_pct, na.rm = TRUE),
            db_time_diff_pct_sd = sd(db_time_diff_pct, na.rm = TRUE))

# DB-level diffs are smaller: mean -0.59%, sd 3.96%


##################
# Check for DA level?? just straight aveaging, not pop-weighted yet

dbuid_comp %>%
  mutate(dauid = as.character(dbuid) %>% 
           stringr::str_trunc(width = 8, ellipsis ="")) %>%
  group_by(dauid) %>%
  summarise(da_time_val = mean(db_time_val, na.rm = TRUE),
            da_time_dmti = mean(db_time_dmti, na.rm  = TRUE))

dbuid_comp$pop2016 %>% sum()

summarise_diffs(test)




##### STEP 1: Comparing urban/suburban/rural pseudo-household trips in Ottawa
#               Looking at high-level averages, abs and pct
#               Looking at specific cases where they disagree strongly
#               Find they agree on average; they disagree in specific cases
#               based on e.g. permissiveness of U-turns to reach unrealistic
#               goals (e.g. points on the middle of an overpass)

##### STEP 2: Comparing DA-level population-weighted averages for trips to 
#             family physicians in Ottawa and Renfrew County
#               Looking again at PHH-level averages, abs and pct
#               Then looking at DB-level averages
#               Then population-weighting up to DA-level averages
#               Looking at agreement, mean, sd

fun1 <- function (some_variable) {
  var_name <- rlang::enexpr(some_variable) %>% 
    as.character()
  
  sprintf("%s = %s", var_name, some_variable)
}


fun1(var)
fun2(var)

