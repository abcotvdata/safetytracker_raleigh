library(tidyverse)
library(tidycensus)
library(leaflet)
library(leaflet.extras)
library(leaflet.providers)
library(sf)

raleigh_districts_map <- leaflet(raleigh_crime_last12) %>%
  setView(-78.65, 35.79, zoom = 10) %>% 
  addProviderTiles(provider = "CartoDB.Positron") %>%
  addCircleMarkers(clusterOptions = 2)
raleigh_districts_map
