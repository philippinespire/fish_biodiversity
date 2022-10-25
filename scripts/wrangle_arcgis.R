#### Initialize ####
# find distances between locations and geopoltical units
# associate human population size with locations
# https://stackoverflow.com/questions/26308426/how-do-i-find-the-polygon-nearest-to-a-point-in-r

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tidyverse)
library(janitor)
library(magrittr)
# library(measurements)
library(lubridate)
library(rgdal)
library(raster)
# library(sf)
library(rgeos)
library(maptools)
# library(tmap)

#### USER DEFINED VARIABLES ####
#inFilePath = "../data/SU-SI_Duplicates(1).xlsx"
# inZipFilePath = "../data/PhL_Province_Pop.zip"
inFileScript = "wrangle_cas_si_su_data.R"
inDirPath = "../data/gis/PhL_Province_Pop/"

#### READ IN DATA ####
# get survey data
# source(inFileScript)

# manually unzip your arcgis map, and then read it in like this
arcgis <- readOGR(dsn = inDirPath)

#### SET UP DATA TO FIND NeAREST PolyGon #### 

# Extract your survey locations from your data (data_cas_si_su). Make sure that the order is in lon/lat.
# 1 row per location per time (in this case defined by the `study` column)
station_pts <-
  data_cas_si_su %>%
  dplyr::select(study,
                station_code,
                longitude,
                latitude) %>%
  distinct() %>%
  dplyr::select(-study,
                -station_code) %>%
  drop_na() %>%
  as.data.frame()

# This saves characteristics of the stations to their own tibble so they can be joined to the data later, in this case, just the study is saved
# must be the same number of rows as station_pts
station_data <-
  data_cas_si_su %>%
  dplyr::select(study,
                station_code,
                longitude,
                latitude) %>%
  distinct() %>%
  drop_na() %>%
  dplyr::select(-longitude,
                -latitude) %>%
  as.data.frame()

# convert your station gis points to a SpatialPoints data structure
# the statement inside of CRS is a description of the projection used to define the lat and long of the stations.  see PROJ.4 and CRS-class {rgdal}
station_pts_longlat <- 
  SpatialPoints(station_pts,
                CRS("+proj=longlat"))

# the distance calculator we are using requires a planar coordinate system, like utm
# convert the station location data into UTM. Need the UTM Zone and the PROJ.4 projection describing UTM
utm_zone <- 51
utm_proj <- "+proj=utm +zone=%d +datum=NAD83 +units=m +no_defs +ellps=GRS80"

station_pts_utm <- 
  spTransform(station_pts_longlat,
              CRS(sprintf(utm_proj, 
                          utm_zone)))

# convert the arc gis data into UTM. Need the UTM Zone and the PROJ.4 projection describing UTM
arcgis_utm <- 
  spTransform(arcgis,
              CRS(sprintf(utm_proj, 
                          utm_zone)))


#### FIND NEAREST POLYGON ####
## For each point, find name of nearest polygon (in this case, Philippine provinces)
# arrange results of for loop into a dataframe and pull in additional arcgis data
data_distance_to_province <-
  # this makes a table of distances from each waypoint to each province
  sapply(1:nrow(station_pts),
         function(x){rgeos::gDistance(station_pts_utm[x,], 
                                      arcgis_utm, 
                                      byid=TRUE)}) %>%
  #transpose col and rows
  t() %>%
  as_tibble() %>%
  # name the cols by the unique province id, some provinces have same ISO, so can't use that
  rename_all(~ arcgis_utm@data$ID) %>%
  # add columns with station ids
  bind_cols(station_data) %>%
  # rearrange the col order
  dplyr::select(study,
                station_code,
                everything()) %>%
  # stack the data so that all distances are in 1 col
  pivot_longer(cols = !starts_with("st"),
               names_to = "ID",
               values_to = "distance_m")

data_human_pop <-
  data_distance_to_province %>%
  # keep rows containing closest province for each waypoint
  group_by(study,
           station_code) %>%
  filter(distance_m == min(distance_m)) %>%
  ungroup() %>%
  # use province id to get province name
  left_join(arcgis_utm@data,
            by = "ID") %>%
  # get lat and long used
  bind_cols(as_tibble(station_pts_utm@coords) %>%
              dplyr::rename(latitude_utm = latitude,
                            longitude_utm = longitude),
            as_tibble(station_pts_longlat@coords) %>%
              dplyr::rename(long = longitude,
                            lat = latitude)) %>%
  # housekeeping
  clean_names() %>%
  mutate(totpop_cy = as.numeric(totpop_cy),
         population = as.numeric(population),
         utm_zone = utm_zone,
         pop_dens_province = totpop_cy/area ) %>%
  dplyr::select(-aggregatio,
                -has_data) 

#### ADD HUMAN POP DATA TO SURVEY DATA ####
data_cas_si_su_pop <-
  data_cas_si_su %>%
  left_join(data_human_pop)
  
#### CONVERT ARCGIS DATA TO TIBBLE ####
# convert arcgis data to tibble for plotting
# additionally pull in data from arcgis_utm@data dataframe,
arcgis_tibble <- 
  spTransform(arcgis,
              CRS("+proj=longlat")) %>%
  fortify(region='ISO_SUB') %>%
  mutate(ISO_SUB = str_remove(group,
                              '\\.[0-9]*')) %>%
  left_join(arcgis@data,
            by = "ISO_SUB") %>%
  mutate(TOTPOP_CY = as.numeric(TOTPOP_CY),
         population = as.numeric(population)) %>%
  dplyr::select(-aggregatio)



#### REMOVE UNNEDED TIBBLES ####
rm(arcgis,
   arcgis_utm,
   station_data,
   station_pts,
   station_pts_longlat,
   station_pts_utm)
