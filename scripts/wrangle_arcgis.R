#### Initialize ####
# find distances between locations and geopoltical units
# associate human population size with locations
# https://stackoverflow.com/questions/26308426/how-do-i-find-the-polygon-nearest-to-a-point-in-r

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tidyverse)
library(janitor)
library(magrittr)
library(measurements)
library(lubridate)
library(rgdal)
library(raster)
library(sf)
library(rgeos)
library(maptools)
library(tmap)

#### USER DEFINED VARIABLES ####
#inFilePath = "../data/SU-SI_Duplicates(1).xlsx"
inZipFilePath = "../data/PhL_Province_Pop.zip"
inDirPath = "../data/PhL_Province_Pop/"
inFilePath = "../data/PhL_Province_Pop.zip"


#### READ IN DATA ####

# unzip your arcgis map, and then read it in like this
arcgis <- readOGR(dsn = inDirPath)

#### FIND NeAREST PolyGon #### 

# Extract your survey locations from your data (data_cas_si_su). Make sure that the order is in lon/lat.
# 1 row per location per time (in this case defined by the `study` column)
station_pts <-
  data_cas_si_su %>%
  dplyr::select(study,
                station_code,
                adjusted_longitude,
                adjusted_latitude) %>%
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
                adjusted_longitude,
                adjusted_latitude) %>%
  distinct() %>%
  drop_na() %>%
  dplyr::select(-adjusted_longitude,
                -adjusted_latitude) %>%
  as.data.frame()

# read in your station gis points that are in decimal degrees
# the statement inside of CRS is a description of the projection used to define the lat and long of the stations.  see PROJ.4 and CRS-class {rgdal}
station_pts_longlat <- 
  SpatialPoints(station_pts,
                CRS("+proj=longlat"))

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

# set up a for loop to find nearest polygon defined in arcgis data to your survey station sites
n <- length(station_pts_utm)
nearest_polygon <- character(n)
dist_nearest_polygon <- numeric(n)

## For each point, find name of nearest polygon (in this case, Philippine provinces)
for (i in seq_along(nearest_polygon)) {
  gDists <- gDistance(station_pts_utm[i,], 
                      arcgis_utm, 
                      byid=TRUE)
  nearest_polygon[i] <- arcgis_utm$ISO_SUB[which.min(gDists)]
  dist_nearest_polygon[i] <- min(gDists)
}

# arrange results of for loop into a dataframe and pull in additional arcgis data
data_nearest_province <- 
  station_data %>%
  bind_cols(tibble(nearest_polygon),
            tibble(dist_nearest_polygon),
            as_tibble(station_pts_utm@coords) %>%
              rename(latitude_utm = adjusted_latitude,
                     longitude_utm = adjusted_longitude),
            as_tibble(station_pts_longlat@coords) %>%
              rename(long = adjusted_longitude,
                     lat = adjusted_latitude)) %>%
  left_join(arcgis@data,
            by = c("nearest_polygon" = "ISO_SUB")) %>%
  clean_names() %>%
  mutate(totpop_cy = as.numeric(totpop_cy),
         population = as.numeric(population),
         utm_zone = utm_zone) %>%
  dplyr::select(-aggregatio,
                -has_data) 

as_tibble(station_pts_longlat@coords) %>%
  rename(long = adjusted_longitude,
         lat = adjusted_latitude)
# convert arcgis_utm data to tibble for plotting
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

# visualize survey sites on heatmap of human pop in each province

make_map <- function(map_shape_data = arcgis_tibble,
                     waypoint_data = data_nearest_province,
                     min_long = 120.5,
                     max_long = 124.3,
                     min_lat = 8.5,
                     max_lat = 14){
  map_out <-
    map_shape_data %>%
      ggplot() + 
      aes(x = long, 
          y = lat, 
          group = group,
          fill = population) +
      geom_polygon(color='black') +
      scale_fill_gradient(low = "white",
                          high = "black") +
      # here is where we pull in the station by station data
      geom_point(data = waypoint_data,
                 aes(x=long,
                     y=lat,
                     color = nearest_polygon,
                     # size = dist_nearest_polygon,
                     shape = study),
                 inherit.aes = FALSE,
                 size = 4,
                 stroke = 2) +
      scale_shape_manual(values = c(0,1,2)) +
      coord_quickmap(xlim = c(min_long,
                              max_long),
                     ylim = c(min_lat,
                              max_lat))
  
  return(map_out)
}

#default map
make_map()

#north sulu sea visayas
make_map(min_long = 120.8,
         max_long = 124,
         min_lat = 8.8,
         max_lat = 11)

#visayas
make_map(min_long = 122.8,
         max_long = 124,
         min_lat = 8.8,
         max_lat = 10)

# north sulu sea
make_map(min_long = 120.8,
         max_long = 121.4,
         min_lat = 10.8,
         max_lat = 11)

# northern most cluster
make_map(min_long = 120.8,
         max_long = 120.95,
         min_lat = 13.625,
         max_lat = 13.825)

# whole map
make_map(min_long = NA,
         max_long = NA,
         min_lat = NA,
         max_lat = NA)



#### PLOTS ####

arcgis@data %>%
  tibble() %>%
  ggplot() +
  aes(x=as.numeric(population)) +
  geom_histogram()

arcgis@data %>%
  tibble() %>%
  ggplot() +
  aes(y=as.numeric(population),
      x=Shape__Are) +
  geom_point()

bubble(trees['HEIGHT'], col=rgb(0.5,0.5,1,0.5))
bubble(arcgis['ID'], col=rgb(0.5,0.5,1,0.5))

