library(tidyverse)
library(sf)
library(readxl)
library(zoo)
library(lubridate)

# starting to lay out crime data sourcing

download.file("https://opendata.arcgis.com/api/v3/datasets/24c0b37fa9bb4e16ba8bcaa7e806c615_0/downloads/data?format=csv&spatialRefId=4326",
              "raleigh_crime.csv")

raleigh_crime <- read_csv("raleigh_crime.csv") %>% select(5:22)

# Rebuild date fields in formats we need
raleigh_crime$date <- ymd(substr(raleigh_crime$reported_date,1,10))
raleigh_crime$hour <- raleigh_crime$reported_hour
raleigh_crime$year <- raleigh_crime$reported_year
raleigh_crime$month <- lubridate::floor_date(as.Date(raleigh_crime$date),"month")

raleigh_crime$category <- case_when(raleigh_crime$crime_code=="11" ~ "Murder",
                                    raleigh_crime$crime_code %in% 
                                      c("25A","25B","25C","25D","25G","25M") ~ "Aggravated Assault",
                                    raleigh_crime$crime_code %in% 
                                      c("20A","20B") ~ "Robbery",
                                    raleigh_crime$crime_code %in% 
                                      c("30A","30B") ~ "Burglary",
                                    raleigh_crime$crime_code %in% 
                                      c("17A","17B","17C","17D") ~ "Sexual Assault",
                                    raleigh_crime$crime_code %in% 
                                      c("35A","35B","35C","35D","35E","35F","35G","35H") ~ "Theft",
                                    raleigh_crime$crime_code=="40A" ~ "Motor Vehicle Theft",
                                    TRUE ~ "Other/Unknown")

raleigh_crime$type <- case_when(raleigh_crime$crime_code=="11" ~ "Violent",
                                raleigh_crime$crime_code %in% 
                                  c("25A","25B","25C","25D","25G","25M") ~ "Violent",
                                raleigh_crime$crime_code %in% 
                                  c("20A","20B") ~ "Violent",
                                raleigh_crime$crime_code %in% 
                                  c("30A","30B") ~ "Property",
                                raleigh_crime$crime_code %in% 
                                  c("17A","17B","17C","17D") ~ "Violent",
                                raleigh_crime$crime_code %in% 
                                  c("35A","35B","35C","35D","35E","35F","35G","35H") ~ "Property",
                                raleigh_crime$crime_code=="40A" ~ "Property",
                                TRUE ~ "Other/Unknown")

#remove weird anomaly date 12/03/2025
raleigh_crime <- raleigh_crime %>% filter(date<max(raleigh_crime$date))

# select and rename columns
raleigh_crime <- raleigh_crime %>% select(1,3,4,6:9,16:24)
raleigh_crime <- raleigh_crime %>% rename("description"="crime_description")

# create separate table of crimes from the last full 12 months
raleigh_crime_last12 <- raleigh_crime %>% filter(raleigh_crime$date > max(raleigh_crime$date)-365)

### CITYWIDE CRIME 
### TOTALS AND OUTPUT

# Set variable of city population
# likely needs added to the tracker itself
raleigh_population <- 469124

# Calculate of each detailed offense type CITYWIDE
citywide_detailed <- raleigh_crime %>%
  group_by(category,description,year) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from=year, values_from=count)
# rename the year columns
citywide_detailed <- citywide_detailed %>% 
  rename("total15" = "2015",
         "total16" = "2016",
         "total17" = "2017",
         "total18" = "2018",
         "total19" = "2019",
         "total20" = "2020",
         "total21" = "2021",
         "total22" = "2022",
         "total23" = "2023",
         "total24" = "2024",
         "total25" = "2025")
# add last 12 months
citywide_detailed_last12 <- raleigh_crime_last12 %>%
  group_by(category,description) %>%
  summarise(last12mos = n())
citywide_detailed <- left_join(citywide_detailed,citywide_detailed_last12,by=c("category","description"))
# add zeros where there were no crimes tallied that year
citywide_detailed[is.na(citywide_detailed)] <- 0
rm(citywide_detailed_last12)
# Calculate a total across the 3 prior years
citywide_detailed$total_prior3years <- citywide_detailed$total22+citywide_detailed$total23+citywide_detailed$total24
citywide_detailed$avg_prior3years <- round(citywide_detailed$total_prior3years/3,1)
# calculate increases
citywide_detailed$inc_19to24 <- round(citywide_detailed$total24/citywide_detailed$total19*100-100,1)
citywide_detailed$inc_19tolast12 <- round(citywide_detailed$last12mos/citywide_detailed$total19*100-100,1)
citywide_detailed$inc_22tolast12 <- round(citywide_detailed$last12mos/citywide_detailed$total24*100-100,1)
citywide_detailed$inc_prior3yearavgtolast12 <- round((citywide_detailed$last12mos/citywide_detailed$avg_prior3years)*100-100,0)
# calculate the citywide rates
citywide_detailed$rate19 <- round(citywide_detailed$total19/raleigh_population*100000,1)
citywide_detailed$rate20 <- round(citywide_detailed$total20/raleigh_population*100000,1)
citywide_detailed$rate21 <- round(citywide_detailed$total21/raleigh_population*100000,1)
citywide_detailed$rate22 <- round(citywide_detailed$total22/raleigh_population*100000,1)
citywide_detailed$rate23 <- round(citywide_detailed$total23/raleigh_population*100000,1)
citywide_detailed$rate24 <- round(citywide_detailed$total24/raleigh_population*100000,1)
citywide_detailed$rate_last12 <- round(citywide_detailed$last12mos/raleigh_population*100000,1)
# calculate a multiyear rate
citywide_detailed$rate_prior3years <- round(citywide_detailed$avg_prior3years/raleigh_population*100000,1)
# for map/table making purposes, changing Inf and NaN in calc fields to NA
citywide_detailed <- citywide_detailed %>%
  mutate_if(is.numeric, ~ifelse(. == Inf, NA, .))
citywide_detailed <- citywide_detailed %>%
  mutate_if(is.numeric, ~ifelse(. == "NaN", NA, .))

# Calculate of each detailed offense type CITYWIDE
citywide_detailed_monthly <- raleigh_crime %>%
  group_by(category,description,month) %>%
  summarise(count = n())
# add rolling average of 3 months for chart trend line & round to clean
citywide_detailed_monthly <- citywide_detailed_monthly %>%
  dplyr::mutate(rollavg_3month = rollsum(count, k = 3, fill = NA, align = "right")/3)
citywide_detailed_monthly$rollavg_3month <- round(citywide_detailed_monthly$rollavg_3month,0)
# write to save for charts for detailed monthly
write_csv(citywide_detailed_monthly,"data/output/monthly/citywide_detailed_monthly.csv")

# Calculate of each category of offense CITYWIDE
citywide_category <- raleigh_crime %>%
  group_by(category,year) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from=year, values_from=count)
# rename the year columns
citywide_category <- citywide_category %>% 
  rename("total15" = "2015",
         "total16" = "2016",
         "total17" = "2017",
         "total18" = "2018",
         "total19" = "2019",
         "total20" = "2020",
         "total21" = "2021",
         "total22" = "2022",
         "total23" = "2023",
         "total24" = "2024",
         "total25" = "2025")
# add last 12 months
citywide_category_last12 <- raleigh_crime_last12 %>%
  group_by(category) %>%
  summarise(last12mos = n())
citywide_category <- left_join(citywide_category,citywide_category_last12,by=c("category"))
# add zeros where there were no crimes tallied that year
citywide_category[is.na(citywide_category)] <- 0
# Calculate a total across the 3 prior years
citywide_category$total_prior3years <- citywide_category$total22+citywide_category$total23+citywide_category$total24
citywide_category$avg_prior3years <- round(citywide_category$total_prior3years/3,1)
# calculate increases
citywide_category$inc_19to24 <- round(citywide_category$total24/citywide_category$total19*100-100,1)
citywide_category$inc_19tolast12 <- round(citywide_category$last12mos/citywide_category$total19*100-100,1)
citywide_category$inc_24tolast12 <- round(citywide_category$last12mos/citywide_category$total24*100-100,1)
citywide_category$inc_prior3yearavgtolast12 <- round((citywide_category$last12mos/citywide_category$avg_prior3years)*100-100,0)
# calculate the citywide rates
citywide_category$rate19 <- round(citywide_category$total19/raleigh_population*100000,1)
citywide_category$rate20 <- round(citywide_category$total20/raleigh_population*100000,1)
citywide_category$rate21 <- round(citywide_category$total21/raleigh_population*100000,1)
citywide_category$rate22 <- round(citywide_category$total22/raleigh_population*100000,1)
citywide_category$rate23 <- round(citywide_category$total23/raleigh_population*100000,1)
citywide_category$rate24 <- round(citywide_category$total24/raleigh_population*100000,1)
citywide_category$rate_last12 <- round(citywide_category$last12mos/raleigh_population*100000,1)
# calculate a multiyear rate
citywide_category$rate_prior3years <- round(citywide_category$avg_prior3years/raleigh_population*100000,1)

# Calculate monthly totals for categories of crimes CITYWIDE
citywide_category_monthly <- raleigh_crime %>%
  group_by(category,month) %>%
  summarise(count = n())
# add rolling average of 3 months for chart trend line & round to clean
citywide_category_monthly <- citywide_category_monthly %>%
  arrange(category,month) %>%
  dplyr::mutate(rollavg_3month = rollsum(count, k = 3, fill = NA, align = "right")/3)
citywide_category_monthly$rollavg_3month <- round(citywide_category_monthly$rollavg_3month,0)

# write series of monthly files for charts (NOTE murder is written above in detailed section)
write_csv(citywide_category_monthly,"data/output/monthly/citywide_category_monthly.csv")
citywide_category_monthly %>% filter(category=="Sexual Assault") %>% write_csv("data/output/monthly/sexassaults_monthly.csv")
citywide_category_monthly %>% filter(category=="Motor Vehicle Theft") %>% write_csv("data/output/monthly/autothefts_monthly.csv")
citywide_category_monthly %>% filter(category=="Theft") %>% write_csv("data/output/monthly/thefts_monthly.csv")
citywide_category_monthly %>% filter(category=="Burglary") %>% write_csv("data/output/monthly/burglaries_monthly.csv")
citywide_category_monthly %>% filter(category=="Robbery") %>% write_csv("data/output/monthly/robberies_monthly.csv")
citywide_category_monthly %>% filter(category=="Aggravated Assault") %>% write_csv("data/output/monthly/assaults_monthly.csv")
citywide_category_monthly %>% filter(category=="Murder") %>% write_csv("data/output/monthly/murders_monthly.csv")


### Some YEARLY tables for charts for our pages
citywide_category %>% select(1,3:10,12) %>% filter(category=="Murder") %>% write_csv("data/output/yearly/murders_city.csv")
citywide_category %>% select(1,3:10,12) %>% filter(category=="Sexual Assault") %>%  write_csv("data/output/yearly/sexassaults_city.csv")
citywide_category %>% select(1,3:10,12) %>% filter(category=="Motor Vehicle Theft") %>%  write_csv("data/output/yearly/autothefts_city.csv")
citywide_category %>% select(1,3:10,12) %>% filter(category=="Theft") %>%  write_csv("data/output/yearly/thefts_city.csv")
citywide_category %>% select(1,3:10,12) %>% filter(category=="Burglary") %>%  write_csv("data/output/yearly/burglaries_city.csv")
citywide_category %>% select(1,3:10,12) %>% filter(category=="Robbery") %>%  write_csv("data/output/yearly/robberies_city.csv")
citywide_category %>% select(1,3:10,12) %>% filter(category=="Aggravated Assault") %>%  write_csv("data/output/yearly/assaults_city.csv")


# Calculate of each type of crime CITYWIDE
citywide_type <- raleigh_crime %>%
  group_by(type,year) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from=year, values_from=count)
# rename the year columns
citywide_type <- citywide_type %>% 
  rename("total15" = "2015",
         "total16" = "2016",
         "total17" = "2017",
         "total18" = "2018",
         "total19" = "2019",
         "total20" = "2020",
         "total21" = "2021",
         "total22" = "2022",
         "total23" = "2023",
         "total24" = "2024",
         "total25" = "2025")
# add last 12 months
citywide_type_last12 <- raleigh_crime_last12 %>%
  group_by(type) %>%
  summarise(last12mos = n())
citywide_type <- left_join(citywide_type,citywide_type_last12,by=c("type"))
# Calculate a total across the 3 prior years
citywide_type$total_prior3years <- citywide_type$total22+citywide_type$total23+citywide_type$total24
citywide_type$avg_prior3years <- round(citywide_type$total_prior3years/3,1)
# add zeros where there were no crimes tallied that year
citywide_type[is.na(citywide_type)] <- 0
# calculate increases
citywide_type$inc_19to24 <- round(citywide_type$total24/citywide_type$total19*100-100,1)
citywide_type$inc_19tolast12 <- round(citywide_type$last12mos/citywide_type$total19*100-100,1)
citywide_type$inc_24tolast12 <- round(citywide_type$last12mos/citywide_type$total24*100-100,1)
citywide_type$inc_prior3yearavgtolast12 <- round((citywide_type$last12mos/citywide_type$avg_prior3years)*100-100,0)
# calculate the citywide rates
citywide_type$rate19 <- round(citywide_type$total19/raleigh_population*100000,1)
citywide_type$rate20 <- round(citywide_type$total20/raleigh_population*100000,1)
citywide_type$rate21 <- round(citywide_type$total21/raleigh_population*100000,1)
citywide_type$rate22 <- round(citywide_type$total22/raleigh_population*100000,1)
citywide_type$rate23 <- round(citywide_type$total23/raleigh_population*100000,1)
citywide_type$rate24 <- round(citywide_type$total24/raleigh_population*100000,1)
citywide_type$rate_last12 <- round(citywide_type$last12mos/raleigh_population*100000,1)
# calculate a multiyear rate
citywide_type$rate_prior3years <- round(citywide_type$avg_prior3years/raleigh_population*100000,1)

### RALEIGH PD DISTRICT CRIME TOTALS AND OUTPUT

# MERGE WITH BEATS GEOGRAPHY AND POPULATION
# Geography and populations processed separately in 
districts <- st_read("data/output/geo/raleigh_districts.geojson")

# we need these unique lists for making the beat tables below
# this ensures that we get crime details for beats even with zero
# incidents of certain types over the entirety of the time period
list_district_category <- crossing(district = unique(raleigh_crime$district), description = unique(raleigh_crime$description))
list_district_category <- crossing(district = unique(raleigh_crime$district), category = unique(raleigh_crime$category))
list_district_type <- crossing(district = unique(raleigh_crime$district), type = unique(raleigh_crime$type))

# Calculate total of each detailed offense type by community area
district_detailed <- raleigh_crime %>%
  group_by(district,category,description,year) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from=year, values_from=count)
# rename the year columns
district_detailed <- district_detailed %>% 
  rename("total15" = "2015",
         "total16" = "2016",
         "total17" = "2017",
         "total18" = "2018",
         "total19" = "2019",
         "total20" = "2020",
         "total21" = "2021",
         "total22" = "2022",
         "total23" = "2023",
         "total24" = "2024",
         "total25" = "2025")
# add last 12 months
district_detailed_last12 <- raleigh_crime_last12 %>%
  group_by(district,category,description) %>%
  summarise(last12mos = n())
district_detailed <- left_join(district_detailed,district_detailed_last12,by=c("district","category","description"))
rm(district_detailed_last12)
# add zeros where there were no crimes tallied that year
district_detailed[is.na(district_detailed)] <- 0
# Calculate a total across the 3 prior years
district_detailed$total_prior3years <- district_detailed$total22+district_detailed$total23+district_detailed$total24
district_detailed$avg_prior3years <- round(district_detailed$total_prior3years/3,1)
# calculate increases
district_detailed$inc_19to24 <- round(district_detailed$total24/district_detailed$total19*100-100,1)
district_detailed$inc_19tolast12 <- round(district_detailed$last12mos/district_detailed$total19*100-100,1)
district_detailed$inc_24tolast12 <- round(district_detailed$last12mos/district_detailed$total24*100-100,1)
district_detailed$inc_prior3yearavgtolast12 <- round((district_detailed$last12mos/district_detailed$avg_prior3years)*100-100,0)
# add population for beats
district_detailed <- full_join(districts,district_detailed,by=c("district_name"="district"))
# calculate the beat by beat rates PER 1K people
district_detailed$rate19 <- round(district_detailed$total19/district_detailed$population*100000,1)
district_detailed$rate20 <- round(district_detailed$total20/district_detailed$population*100000,1)
district_detailed$rate21 <- round(district_detailed$total21/district_detailed$population*100000,1)
district_detailed$rate22 <- round(district_detailed$total22/district_detailed$population*100000,1)
district_detailed$rate23 <- round(district_detailed$total23/district_detailed$population*100000,1)
district_detailed$rate24 <- round(district_detailed$total24/district_detailed$population*100000,1)
district_detailed$rate_last12 <- round(district_detailed$last12mos/district_detailed$population*100000,1)
# calculate a multiyear rate
district_detailed$rate_prior3years <- round(district_detailed$avg_prior3years/district_detailed$population*100000,1)
# for map/table making purposes, changing Inf and NaN in calc fields to NA
district_detailed <- district_detailed %>%
  mutate_if(is.numeric, ~ifelse(. == Inf, NA, .))
district_detailed <- district_detailed %>%
  mutate_if(is.numeric, ~ifelse(. == "NaN", NA, .))

# Calculate total of each category of offense BY POLICE BEAT
district_category <- raleigh_crime %>%
  group_by(district,category,year) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from=year, values_from=count)
# merging with full list so we have data for every beat, every category_name
district_category <- left_join(list_district_category,district_category,by=c("district"="district","category"="category"))
# rename the year columns
district_category <- district_category %>% 
  rename("total19" = "2019",
         "total20" = "2020",
         "total21" = "2021",
         "total22" = "2022",
         "total23" = "2023",
         "total24" = "2024",
         "total25" = "2025")
# add last 12 months
district_category_last12 <- raleigh_crime_last12 %>%
  group_by(district,category) %>%
  summarise(last12mos = n())
district_category <- left_join(district_category,district_category_last12,by=c("district","category"))
rm(district_category_last12)
# add zeros where there were no crimes tallied that year
district_category[is.na(district_category)] <- 0
# Calculate a total across the 3 prior years
district_category$total_prior3years <- district_category$total22+district_category$total23+district_category$total24
district_category$avg_prior3years <- round(district_category$total_prior3years/3,1)
# calculate increases
district_category$inc_19to24 <- round(district_category$total24/district_category$total19*100-100,1)
district_category$inc_19tolast12 <- round(district_category$last12mos/district_category$total19*100-100,1)
district_category$inc_24tolast12 <- round(district_category$last12mos/district_category$total24*100-100,1)
district_category$inc_prior3yearavgtolast12 <- round((district_category$last12mos/district_category$avg_prior3years)*100-100,0)
# add population for beats
district_category <- full_join(districts,district_category,by=c("district_name"="district"))
# calculate the beat by beat rates PER 1K people
district_category$rate19 <- round(district_category$total19/district_category$population*100000,1)
district_category$rate20 <- round(district_category$total20/district_category$population*100000,1)
district_category$rate21 <- round(district_category$total21/district_category$population*100000,1)
district_category$rate22 <- round(district_category$total22/district_category$population*100000,1)
district_category$rate23 <- round(district_category$total23/district_category$population*100000,1)
district_category$rate24 <- round(district_category$total24/district_category$population*100000,1)
district_category$rate_last12 <- round(district_category$last12mos/district_category$population*100000,1)
# calculate a multiyear rate
district_category$rate_prior3years <- round(district_category$avg_prior3years/district_category$population*100000,1)
# for map/table making purposes, changing Inf and NaN in calc fields to NA
district_category <- district_category %>%
  mutate_if(is.numeric, ~ifelse(. == Inf, NA, .))
district_category <- district_category %>%
  mutate_if(is.numeric, ~ifelse(. == "NaN", NA, .))

# Calculate total of each type of crime BY POLICE BEAT
district_type <- raleigh_crime %>%
  group_by(district,type,year) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from=year, values_from=count)
# merging with full list so we have data for every beat, every type
district_type <- left_join(list_district_type,district_type,by=c("district"="district","type"="type"))
# rename the year columns
district_type <- district_type %>% 
  rename("total19" = "2019",
         "total20" = "2020",
         "total21" = "2021",
         "total22" = "2022",
         "total23" = "2023",
         "total24" = "2024",
         "total25" = "2025")
# add last 12 months
district_type_last12 <- raleigh_crime_last12 %>%
  group_by(district,type) %>%
  summarise(last12mos = n())
district_type <- left_join(district_type,district_type_last12,by=c("district","type"))
rm(district_type_last12)
# add zeros where there were no crimes tallied that year
district_type[is.na(district_type)] <- 0
# Calculate a total across the 3 prior years
district_type$total_prior3years <- district_type$total22+district_type$total23+district_type$total24
district_type$avg_prior3years <- round(district_type$total_prior3years/3,1)
# calculate increases
district_type$inc_19to24 <- round(district_type$total24/district_type$total19*100-100,1)
district_type$inc_19tolast12 <- round(district_type$last12mos/district_type$total19*100-100,1)
district_type$inc_24tolast12 <- round(district_type$last12mos/district_type$total24*100-100,1)
district_type$inc_prior3yearavgtolast12 <- round((district_type$last12mos/district_type$avg_prior3years)*100-100,0)
# add population for beats
district_type <- full_join(districts,district_type,by=c("district_name"="district"))
# calculate the beat by beat rates PER 1K people
district_type$rate19 <- round(district_type$total19/district_type$population*100000,1)
district_type$rate20 <- round(district_type$total20/district_type$population*100000,1)
district_type$rate21 <- round(district_type$total21/district_type$population*100000,1)
district_type$rate22 <- round(district_type$total22/district_type$population*100000,1)
district_type$rate23 <- round(district_type$total23/district_type$population*100000,1)
district_type$rate24 <- round(district_type$total24/district_type$population*100000,1)
district_type$rate_last12 <- round(district_type$last12mos/district_type$population*100000,1)
# calculate a multiyear rate
district_type$rate_prior3years <- round(district_type$avg_prior3years/district_type$population*100000,1)
# for map/table making purposes, changing Inf and NaN in calc fields to NA
district_type <- district_type %>%
  mutate_if(is.numeric, ~ifelse(. == Inf, NA, .))
district_type <- district_type %>%
  mutate_if(is.numeric, ~ifelse(. == "NaN", NA, .))

# output various csvs for basic tables to be made with crime totals
# we are dropping geometry for beats here because this is just for tables
district_detailed %>% st_drop_geometry() %>% write_csv("data/output/districts/district_detailed.csv")
district_category %>% st_drop_geometry() %>% write_csv("data/output/districts/district_category.csv")
district_type %>% st_drop_geometry() %>% write_csv("data/output/districts/district_type.csv")
citywide_detailed %>% write_csv("data/output/city/citywide_detailed.csv")
citywide_category %>% write_csv("data/output/city/citywide_category.csv")
citywide_type %>% write_csv("data/output/city/citywide_type.csv")

# Create individual spatial tables of crimes by major categories and types
murders_district <- district_category %>% filter(category=="Murder")
sexassaults_district <- district_category %>% filter(category=="Sexual Assault")
autothefts_district <- district_category %>% filter(category=="Motor Vehicle Theft")
thefts_district <- district_category %>% filter(category=="Theft")
burglaries_district <- district_category %>% filter(category=="Burglary")
robberies_district <- district_category %>% filter(category=="Robbery")
assaults_district <- district_category %>% filter(category=="Aggravated Assault")
violence_district <- district_type %>% filter(type=="Violent")
property_district <- district_type %>% filter(type=="Property")

# Create same set of tables for citywide figures
murders_city <- citywide_category %>% filter(category=="Murder")
sexassaults_city <- citywide_category %>% filter(category=="Sexual Assault")
autothefts_city <- citywide_category %>% filter(category=="Motor Vehicle Theft")
thefts_city <- citywide_category %>% filter(category=="Theft")
burglaries_city <- citywide_category %>% filter(category=="Burglary")
robberies_city <- citywide_category %>% filter(category=="Robbery")
assaults_city <- citywide_category %>% filter(category=="Aggravated Assault")
violence_city <- citywide_type %>% filter(type=="Violent")
property_city <- citywide_type %>% filter(type=="Property")

# Using hour to identify the hours of day when murders happen
when_murders_happen <- raleigh_crime %>%
  filter(category=="Murder") %>%
  group_by(hour) %>%
  summarise(count=n()) %>% 
  arrange(hour)
when_murders_happen$time <- case_when(when_murders_happen$hour == "0" ~ "12 a.m.",
                                      when_murders_happen$hour %in% c("1","2","3","4","5","6","7","8","9","10","11") ~ paste0(when_murders_happen$hour," a.m."),
                                      when_murders_happen$hour %in% c("12") ~ paste0(when_murders_happen$hour," p.m."),
                                      when_murders_happen$hour %in% c("13","14","15","16","17","18","19","20","21","22","23") ~ paste0((as.numeric(when_murders_happen$hour)-12)," p.m."),
                                      TRUE ~ "Other")
when_murders_happen$timeframe <- case_when(when_murders_happen$hour %in% c("0","1","2","3","4","21","22","23") ~ "Overnight from 9 p.m. to 5 a.m.",
                                           when_murders_happen$hour %in% c("5","6","7","8","9","10","11") ~ "Morning from 5 a.m. to 12 p.m.",
                                           when_murders_happen$hour %in% c("12","13","14","15","16","17","18","19","20")  ~ "Afternoon/Evening from 12 p.m. to 9 p.m.",
                                           TRUE ~ "Other")
when_murders_happen <- when_murders_happen %>%
  group_by(timeframe) %>%
  summarise(total=sum(count))

# Create individual spatial tables of crimes by major categories and types
murders_district %>% st_drop_geometry() %>% write_csv("data/output/districts/murders_district.csv")
sexassaults_district %>% st_drop_geometry() %>% write_csv("data/output/districts/sexassaults_district.csv")
autothefts_district %>% st_drop_geometry() %>% write_csv("data/output/districts/autothefts_district.csv")
thefts_district %>% st_drop_geometry() %>% write_csv("data/output/districts/thefts_district.csv")
burglaries_district %>% st_drop_geometry() %>% write_csv("data/output/districts/burglaries_district.csv")
robberies_district %>% st_drop_geometry() %>% write_csv("data/output/districts/robberies_district.csv")
assaults_district %>% st_drop_geometry() %>% write_csv("data/output/districts/assaults_district.csv")
violence_district %>% st_drop_geometry() %>% write_csv("data/output/districts/violence_district.csv")
property_district %>% st_drop_geometry() %>% write_csv("data/output/districts/property_district.csv")

# TEST TEST TEST OF WHETHER RDS WILL WORK FOR TRACKERS IN AUTOMATION
saveRDS(murders_city,"scripts/rds/murders_city.rds")
saveRDS(assaults_city,"scripts/rds/assaults_city.rds")
saveRDS(sexassaults_city,"scripts/rds/sexassaults_city.rds")
saveRDS(autothefts_city,"scripts/rds/autothefts_city.rds")
saveRDS(thefts_city,"scripts/rds/thefts_city.rds")
saveRDS(burglaries_city,"scripts/rds/burglaries_city.rds")
saveRDS(robberies_city,"scripts/rds/robberies_city.rds")
saveRDS(robberies_city,"scripts/rds/retailthefts_city.rds")

saveRDS(murders_district,"scripts/rds/murders_district.rds")
saveRDS(assaults_district,"scripts/rds/assaults_district.rds")
saveRDS(sexassaults_district,"scripts/rds/sexassaults_district.rds")
saveRDS(autothefts_district,"scripts/rds/autothefts_district.rds")
saveRDS(thefts_district,"scripts/rds/thefts_district.rds")
saveRDS(burglaries_district,"scripts/rds/burglaries_district.rds")
saveRDS(robberies_district,"scripts/rds/robberies_district.rds")

# Get latest date in our file and save for
# automating the updated date text in building tracker
asofdate <- max(raleigh_crime$date)
saveRDS(asofdate,"scripts/rds/asofdate.rds")

# additional table exports for specific charts
when_murders_happen %>% write_csv("data/output/city/when_murders_happen.csv")

# deaths cause data update for TX specific table
deaths <- read_excel("data/source/health/deaths.xlsx") 
deaths <- deaths %>% filter(state=="NC")
deaths$Homicide <- murders_city$rate_last12
write_csv(deaths,"data/source/health/death_rates.csv")

