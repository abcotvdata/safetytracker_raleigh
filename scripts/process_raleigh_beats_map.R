library(tidyverse)
library(tidycensus)
library(leaflet)
library(leaflet.extras)
library(leaflet.providers)
library(sp)
library(sf)

# GEOGRAPHY
# Get Durham police beats
download.file("https://live-durhamnc.opendata.arcgis.com/datasets/police-beats/explore?location=36.001650%2C-78.883300%2C11.63",
              "data/source/geo/durham_police_beats.geojson")

# Read in geojson and then transform to sf format
beats_geo <- st_read("data/source/geo/durham_police_beats.geojson") %>% st_transform(3857)

# Get demographic data for Census block groups to aggregate/apportion to precinct geography
# Also transforming to match the planar projection of NYPD's beats spatial file
# This also reduces us down to just the numeric population est and geometry
blocks <- get_decennial(geography = "block", 
                        year = 2020,
                        output = 'wide',
                        variables = "P1_001N", 
                        state = "CA",
                        county = c("Alameda"),
                        geometry = TRUE) %>%
  rename("population"="P1_001N") %>% 
  select(3) %>%
  janitor::clean_names() %>%
  st_transform(3857)

# Calculate the estimated population of police BEATS geographies/interpolate with tidycensus bgs
# Reminder: ext=true SUMS the population during interpolation
beats_withpop <- st_interpolate_aw(blocks, beats_geo, ext = TRUE)
# Drops geometry so it's not duplicated in the merge
beats_withpop <- st_drop_geometry(beats_withpop)
# Binds that new population column to the table
beats_geo <- cbind(beats_geo,beats_withpop)
# Cleans up unneeded calculation file
# rm(beats_withpop, blocks)

# Check total population assigned/estimated across all precincts
sum(beats_geo$population) # tally is 453321 

# Round the population figure; rounded to nearest thousand
beats_geo$population <- round(beats_geo$population,-3)
# Prep for tracker use
beats_geo <- beats_geo %>% st_transform(4326)
beats_geo <- st_make_valid(beats_geo)
beats_geo <- beats_geo %>% select(4,6,10,17,18)
names(beats_geo) <- c("beat","district","beat_number","population","geometry")

# saving a clean geojson and separate RDS for use in tracker
file.remove("data/output/geo/oakland_beats.geojson")
st_write(beats_geo,"data/output/geo/oakland_beats.geojson")
# saveRDS(beats_geo,"scripts/rds/oakland_beats.rds")

# BEAT MAP JUST FOR TESTING PURPOSES
# CAN COMMENT OUT ONCE FINALIZED
# Set bins for beats pop map
popbins <- c(0,1000, 10000,25000,50000,100000, Inf)
poppal <- colorBin("YlOrRd", beats_geo$population, bins = popbins)
poplabel <- paste(sep = "<br>", beats_geo$beat,prettyNum(beats_geo$population, big.mark = ","))

oakland_beats_map <- leaflet(beats_geo) %>%
  setView(-122.25, 37.8, zoom = 11.5) %>% 
  addProviderTiles(provider = "Esri.WorldImagery") %>%
  addProviderTiles(provider = "Stamen.TonerLabels") %>%
  addPolygons(color = "white", popup = poplabel, weight = 1, smoothFactor = 0.5,
              opacity = 0.5, fillOpacity = 0.3,
              fillColor = ~poppal(`population`))
oakland_beats_map