library(tidyverse)
library(sf)
library(readxl)
library(zoo)
library(lubridate)

# starting to lay out crime data sourcing

download.file("https://opendata.arcgis.com/api/v3/datasets/24c0b37fa9bb4e16ba8bcaa7e806c615_0/downloads/data?format=csv&spatialRefId=4326",
              "raleigh_crime.csv")

raleigh_crime <- read_csv("raleigh_crime.csv")
raleigh_crime$newdate <- ymd_hms(raleigh_crime$reported_date)


## combine two processed files
durham_crime <- left_join(past_crime_durham,recent_crime_durham,by=c("district"="district","category"="category","type"="type"))
# add last 12 months figure
durham_crime$last12mos <- (durham_crime$total21-durham_crime$ytd21)+durham_crime$ytd22
# narrow to just the columns we need in the proper order
durham_crime <- durham_crime %>% 
  select(3,1,2,5,6,8,9,4)

### CITYWIDE CRIME 
### TOTALS AND OUTPUT

# Set variable of city population
# likely needs added to the tracker itself
raleigh_population <- 469124

# OPEN WORK
# Get latest date in our file and save for
# automating the updated date text in building tracker
asofdate <- str_replace(asofdate,"Actual Offenses - Period Ending ","")
asofdate <- mdy(asofdate)
saveRDS(asofdate,"scripts/rds/asofdate.rds")

# Merge the precincts file with geography and populations
district_crime <- full_join(districts_geo, durham_crime, by="district") %>% filter(district!="DSO")
# add zeros where there were no crimes tallied that year
district_crime$population <- ifelse(district_crime$district=="Citywide", durham_population,district_crime$population)

# add 3-year totals and annualized averages
district_crime$total_prior3years <- district_crime$total19+
  district_crime$total20+
  district_crime$total21
district_crime$avg_prior3years <- round(((district_crime$total19+
                                            district_crime$total20+
                                            district_crime$total21)/3),1)
# now add the increases or change percentages
district_crime$inc_19to21 <- round(district_crime$total21/district_crime$total19*100-100,1)
district_crime$inc_19tolast12 <- round(district_crime$last12mos/district_crime$total19*100-100,1)
district_crime$inc_21tolast12 <- round(district_crime$last12mos/district_crime$total21*100-100,1)
district_crime$inc_prior3yearavgtolast12 <- round((district_crime$last12mos/district_crime$avg_prior3years)*100-100,0)
# add crime rates for each year
district_crime$rate19 <- round((district_crime$total19/district_crime$population)*100000,1)
district_crime$rate20 <- round((district_crime$total20/district_crime$population)*100000,1)
district_crime$rate21 <- round((district_crime$total21/district_crime$population)*100000,1)
district_crime$rate_last12 <- round((district_crime$last12mos/district_crime$population)*100000,1)
district_crime$rate_prior3years <- 
  round((district_crime$avg_prior3years/district_crime$population)*100000,1)

# Now reduce the precinct down to just the columns we likely need for the tracker pages
# district_crime <- district_crime %>% select(1,4,5,6,26:28,36:40,44:55,29,42)
# for map/table making purposes, changing Inf and NaN in calc fields to NA
district_crime <- district_crime %>%
  mutate(across(where(is.numeric), ~na_if(., Inf)))
district_crime <- district_crime %>%
  mutate(across(where(is.numeric), ~na_if(., "NaN")))

# Divide into citywide_crime and district_crime files
citywide_crime <- district_crime %>% filter(district=="Citywide")
district_crime <- district_crime %>% filter(district!="Citywide")

# create a quick long-term annual table
district_yearly <- district_crime %>% select(1,4:7,9) %>% st_drop_geometry()
write_csv(district_yearly,"data/output/yearly/district_yearly.csv")

# create a quick long-term annual table
citywide_yearly <- citywide_crime %>% select(1,4:7,9) %>% st_drop_geometry()
citywide_yearly$total10 <- c(24,70,666,874,3691,7043,738)
citywide_yearly$total11 <- c(26,66,702,922,3886,6766,619)
citywide_yearly$total12 <- c(21,73,622,1006,3298,6302,690)
citywide_yearly$total13 <- c(30,103,608,886,3365,6799,716)
citywide_yearly$total14 <- c(22,102,658,1090,3658,6847,562)
citywide_yearly$total15 <- c(37,101,736,1336,3187,6810,592)
citywide_yearly$total16 <- c(42,103,862,1250,2576,6757,680)
citywide_yearly$total17 <- c(21,132,855,1256,2337,7197,746)
citywide_yearly$total18 <- c(32,138,718,1073,2226,6671,801)
citywide_yearly <- citywide_yearly %>% select(1,2,7:15,3:6) 
write_csv(district_yearly,"data/output/yearly/citywide_yearly.csv")

# add additional years from state archive of reported ucr crimes back to 2000
# yearly_archive <- read_csv("data/source/annual/sf_annual_state.csv")
# citywide_yearly <- right_join(citywide_yearly,yearly_archive %>% select(1:18,23),by="category") %>% select(1,7:24,2:6)

# Now make individual crime files for trackers
# filter precinct versions - using beat for code consistency
murders_district <- district_crime %>% filter(category=="Murder")
sexassaults_district <- district_crime %>% filter(category=="Sexual Assault")
robberies_district <- district_crime %>% filter(category=="Robbery")
assaults_district <- district_crime %>% filter(category=="Aggravated Assault")
burglaries_district <- district_crime %>% filter(category=="Burglary")
thefts_district <- district_crime %>% filter(category=="Theft")
autothefts_district <- district_crime %>% filter(category=="Vehicle Theft")
# filter citywide versions
murders_city <- citywide_crime %>% filter(category=="Murder")
sexassaults_city <- citywide_crime %>% filter(category=="Sexual Assault")
robberies_city <- citywide_crime %>% filter(category=="Robbery")
assaults_city <- citywide_crime %>% filter(category=="Aggravated Assault")
burglaries_city <- citywide_crime %>% filter(category=="Burglary")
thefts_city <- citywide_crime %>% filter(category=="Theft")
autothefts_city <- citywide_crime %>% filter(category=="Vehicle Theft")

#### 
# Archive latest files as csv and rds store for use in trackers
# First save the weekly files as output csvs for others to use
write_csv(district_crime,"data/output/weekly/district_crime.csv")
write_csv(citywide_crime,"data/output/weekly/citywide_crime.csv")
# Archive a year's worth of week-numbered files from the weekly updates
write_csv(district_crime,paste0("data/output/archive/district_crime_week",asofdate,".csv"))
write_csv(citywide_crime,paste0("data/output/archive/citywide_crime_week",asofdate,".csv"))
# Now save the files needed for trackers into RDS store in scripts for GH Actions
# precinct versions
saveRDS(district_crime,"scripts/rds/district_crime.rds")
saveRDS(murders_district,"scripts/rds/murders_district.rds")
saveRDS(sexassaults_district,"scripts/rds/sexassaults_district.rds")
saveRDS(robberies_district,"scripts/rds/robberies_district.rds")
saveRDS(assaults_district,"scripts/rds/assaults_district.rds")
saveRDS(burglaries_district,"scripts/rds/burglaries_district.rds")
saveRDS(thefts_district,"scripts/rds/thefts_district.rds")
saveRDS(autothefts_district,"scripts/rds/autothefts_district.rds")
# city versions
saveRDS(citywide_crime,"scripts/rds/citywide_crime.rds")
saveRDS(murders_city,"scripts/rds/murders_city.rds")
saveRDS(sexassaults_city,"scripts/rds/sexassaults_city.rds")
saveRDS(robberies_city,"scripts/rds/robberies_city.rds")
saveRDS(assaults_city,"scripts/rds/assaults_city.rds")
saveRDS(burglaries_city,"scripts/rds/burglaries_city.rds")
saveRDS(thefts_city,"scripts/rds/thefts_city.rds")
saveRDS(autothefts_city,"scripts/rds/autothefts_city.rds")

### Some tables for charts for our pages
citywide_yearly %>% filter(category=="Murder") %>% write_csv("data/output/yearly/murders_city.csv")
citywide_yearly %>% filter(category=="Sexual Assault") %>%  write_csv("data/output/yearly/sexassaults_city.csv")
citywide_yearly %>% filter(category=="Vehicle Theft") %>%  write_csv("data/output/yearly/autothefts_city.csv")
citywide_yearly %>% filter(category=="Theft") %>%  write_csv("data/output/yearly/thefts_city.csv")
citywide_yearly %>% filter(category=="Burglary") %>%  write_csv("data/output/yearly/burglaries_city.csv")
citywide_yearly %>% filter(category=="Robbery") %>%  write_csv("data/output/yearly/robberies_city.csv")
citywide_yearly %>% filter(category=="Aggravated Assault") %>%  write_csv("data/output/yearly/assaults_city.csv")

# deaths cause data update for North Carolina specific table
deaths <- read_excel("data/source/health/deaths.xlsx") 
deaths <- deaths %>% filter(state=="NC")
deaths$Homicide <- murders_city$rate_last12
write_csv(deaths,"data/source/health/death_rates.csv")
