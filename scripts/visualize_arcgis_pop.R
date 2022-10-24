#### Initialize ####

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(janitor)
library(magrittr)
library(lubridate)
library(rgdal)
library(raster)
library(rgeos)
library(maptools)

#### USER DEFINED VARIABLES ####
source("wrangle_arcgis.R")

# visualize survey sites on heatmap of human pop in each province

make_map <- function(map_shape_data = arcgis_tibble,
                     waypoint_data = data_nearest_province_pop,
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
    labs(x="Longitude",
         y="Latitude") +
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


