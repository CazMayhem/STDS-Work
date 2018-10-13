#--------------------------------------------------------------------------------------------
# SPATIAL MAPPING - attempting to link DSS LGA codes, to GIS Shape-file codes (kind of)
# Oct 2018 Carol Paipa
#--------------------------------------------------------------------------------------------
# Files are saved online https://github.com/CazMayhem/STDS-Work

library(tidyverse)
library(ggplot2)
library(dplyr)
library(readxl)
library(stringr)
library(httr)
library(ggmap)
library(rgdal)
library(tmap)

#getwd()
#setwd("/Users/paipac/Documents/Data/")
options.stringsasfactors = FALSE

# Load Postcode, State, Locality
#--------------------------------------------------------------------------------------------
urlPC <- "https://github.com/CazMayhem/welfare-proj/blob/master/LOCALITY_LGA_2016.xlsx?raw=true"
p01 <- tempfile()
GET(urlPC, write_disk(p01<- tempfile(fileext = ".xlsx")))
pc <- as.data.frame(read_excel(p01, sheet = 1, col_names = TRUE))
#View(pc)

# Shrink MANY LOCALITY_ID's into 1 per LGA_CODE, STATE
# filter on SA for now
pcLGA <- pc %>% filter(substr(LOCALITY_ID,1,2)=="SA" & STATE=="SA") %>%
  mutate(LGA_CODE=as.character(LGA_CODE)) %>% 
  select(LOCALITY_ID, LGA_CODE, STATE) %>% 
  group_by(LGA_CODE, STATE) %>% summarise(min(LOCALITY_ID), n()) %>%
  rename(LOCALITY_ID = `min(LOCALITY_ID)`) 
# remove count column
pcLGA[4] <- NULL


# DSS Summary Statistics
#--------------------------------------------------------------------------------------------
urlXL <- "https://github.com/CazMayhem/STDS-Work/blob/master/ds0004_lga_summary_statistics_2017.xls?raw=true" 
p02 <- tempfile()
GET(urlXL, write_disk(p02<- tempfile(fileext = ".xls")))
dss <- as.data.frame(read_xls(p02, sheet=2, col_names=TRUE, skip=0))
str(dss)
#View(dss)

# Filter on South Australia only
dssLGA <- dss %>% mutate(LGA_CODE = `LGA code`) %>% filter(`S/T name` == "South Australia") %>%
  mutate(LGA_CODE=as.character(LGA_CODE)) 
#View(dssLGA)


# Combine DSS data with LGA data - filtered by State=SA above
#--------------------------------------------------------------------------------------------
dssData <- left_join(dssLGA, pcLGA, by = "LGA_CODE")
#View(dssData)
str(dssData)

# split Male ~ Female ~ Person data (tall not wide)
dssFil  <- dssData # %>% filter(`S/T name` == "South Australia")
dMale   <- dssFil %>% select(`LGA code`, `S/T name`, Males)   %>% rename(Population=Males) %>% mutate(Gender=rep("male",length(`S/T name`)))
dFemale <- dssFil %>% select(`LGA code`, `S/T name`, Females) %>% rename(Population=Females) %>% mutate(Gender=rep("female",length(`S/T name`)))
dPerson <- dssFil %>% select(`LGA code`, `S/T name`, Persons) %>% rename(Population=Persons) %>% mutate(Gender=rep("person",length(`S/T name`)))

dssPlot <- rbind(dMale, dFemale, dPerson)

# For Shape files only - read from local folder
setwd("/Users/paipac/Documents/Data/")

# Read in Shape file for LGA11aAust from Data.gov.au, saved to my local drive
# https://data.gov.au/dataset/1d139f3a-fc76-4717-863c-3689413ee9c0 
# /Users/paipac/Documents/Data/ASGC_LGA2011/ ... file LGA11aAust.shp
Output.Areas <- readOGR(dsn="ASGC_LGA2011",layer="LGA11aAust")

#View(Output.Areas@data)

# Combine LGA shape file data, with DSS data "dPerson", set all.x=FALSE so only SA rows return
dssAreas <- merge(Output.Areas, dPerson, by.x="LGA_CODE11", by.y="LGA code", all.x = FALSE)
str(dssAreas@data)

# Plot SA boundaries and DSS Population
tm_shape(dssAreas) + 
  tm_fill("Population", palette = "Reds", style = "quantile", title = "Population") + 
  tm_borders(alpha=.4)



# ---------------------------------------------------------------
# Following code not required - here for reference only
# ---------------------------------------------------------------

# Google Maps
# ---------------------------------------------------------------
# Middle long, lat values per state to center map
# TAS 41.4545 S, 145.9707
# NSW 33.3332 S, 146.9211 E
# SA 31.9002 S, 135.1092 E
lat_long <- c(lon = 135.1092, lat = -31.9002)

# Get map at zoom level 5: map_5
# this step will need repeating until no more HTTP error 403 (intermittent only, it will work eventually)
map_lga <- get_map(lat_long, zoom = 6, scale = 1)
map_lga <- get_map(lat_long, zoom = 6, scale = 1)
map_lga <- get_map(lat_long, zoom = 6, scale = 1)
map_lga <- get_map(lat_long, zoom = 6, scale = 1)
map_lga <- get_map(lat_long, zoom = 6, scale = 1)

# Plot map at zoom level 6
#ggmap(map_lga)

# Use ggmap() instead of ggplot()
# ggmap(map_lga) +
#   geom_tile(aes(lon, lat, fill = Population), data = d3, alpha = 0.8)

# Australiaboundary SHP mappings - use SA for testing
#------------------------------------------------------------------------
# Output.Areas <- readOGR(dsn="Camden",layer="Camden_oa11")
# NSW_LGA_POLYGON_shp 26 Mb
# QLD_LGA_POLYGON_shp 4.8Mb
# NT_LGA_POLYGON_shp 1.8Mb
# SA_LGA_POLYGON_shp 4.3 Mb 
# TAS_LGA_POLYGON_shp 9.7 Mb
# VIC_LGA_POLYGON_shp 10.8Mb
# WA_LGA_POLYGON_shp 5 Mb
#library(rgdal)

# Extract lat / lon co-ordinates from Shape file data by LGA_PID
# https://gis.stackexchange.com/questions/169599/extract-all-the-polygon-coordinates-from-a-spatialpolygonsdataframe
p <- Output.Areas

## start here for polys
lin <- as(p, "SpatialLinesDataFrame")  
## start here for lines
pts <- as.data.frame(as(lin, "SpatialPointsDataFrame"))

#View(pts)
#str(pts)

# extract out lat & long - use mean() as there are many values per LGA_PID (266227 rows)
ptsCoord <- pts %>% select(LGA_PID, coords.x1, coords.x2) %>% group_by(LGA_PID) %>% 
  summarise(mean(coords.x1), mean(coords.x2), n()) %>% mutate(LGA_PID = as.character(LGA_PID)) %>% 
  rename(LOCALITY_ID = LGA_PID, lon = `mean(coords.x1)`, lat = `mean(coords.x2)`)

# view extract lat/long coordinates from Shape file
View(ptsCoord)
str(ptsCoord)

# Plot our Shape file LGA_PID lat/long values
ptsCoord %>% ggplot() +
  geom_point(aes(x=lon, y=lat))

# Use ggmap() to plot our LGA_PID lat/long values
ggmap(map_lga) +
  geom_point(data=ptsCoord, aes(x=lon, y=lat))

# Use ggmap() + geom_tile() for a tile map (yeah, doesn't do alot yet)
ggmap(map_lga) +
  geom_tile(aes(x=lon, y=lat, fill = `n()`*3), 
            data = ptsCoord, alpha = 0.8)
