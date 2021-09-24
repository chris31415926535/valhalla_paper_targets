library(tidyverse)
library(targets)
library(valhallr)

# load the ottawa pseudo households
targets::tar_load(phh_ottawa)

# get 250 of them randomly
phh_o <- phh_ottawa %>%
  sample_n(250)

# get service areas around each of them
result_o <- phh_o %>%
  nest(data = c(lat, lon)) %>%
  mutate(service_areas = purrr::map(data, 
                                    valhallr::isochrone,
                                    hostname = "192.168.2.35",
                                    contours = "15",
                                    costing = "auto"))

# unnest and ditch the extra columns
result_o <- result_o %>% 
  unnest() %>%
  sf::st_as_sf() %>%
  select(phh_id) 
  
# calculate areas
result_o %>%
  mutate(area = sf::st_area(geometry))

# make a map if you like
result_o %>%
  ggplot() + geom_sf(aes(fill = as.factor(phh_id))) + theme(legend.position = "none")

result_o
