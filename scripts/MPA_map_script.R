#### INITIALIZATION ####
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tidyverse)
library(janitor)
library(readxl)
#install.packages("maps")
#install.packages("viridis")
library(maps)
library(viridis)
require(maps)
require(viridis)
theme_set(
  theme_void()
)
# install.packages("sf")


#### USER DEFINED VARIABLES ####

inFilePath = "../data/MPA_coordinates.xlsx"
#inFilePath2 = 
  # inFilePath2 = "./PHIRES_MetaData.xlsx"
  
  # outFilePath = "./data_combined.tsv"
  
  #### READ IN DATA & CURATE ####

data <-
  read_excel(inFilePath,
           na="NA") %>%
  clean_names()

data_mpa <-
  data %>%
  clean_names() %>%
  dplyr::rename(size = area_ha) %>%
  select(lat,
         long,
         size,
         year)

#### COMBINE DATA ####

# rearrange order of columns, metadata then data


#### WRITE LONG FORMAT FILE ####

# data_all %>%
#   write_tsv(outFilePath)

#### VISuALIZE METADATA ####

data_mpa %>%
  ggplot(aes(y=lat,
             x=long,
             color = "black")) +
  geom_point(size = 5) +
  theme_classic() 

#### VISUALIZE ALL DATA ####

#### MAP DATA ####
#https://www.datanovia.com/en/blog/how-to-create-a-map-using-ggplot2/


world_map <- map_data("world")

world_map %>%
  ggplot(aes(x = long, 
             y = lat, 
             group = group)) +
  geom_polygon(fill="lightgray", 
               color = "blue")

map_data("world",
         region = "Philippines") %>%
  ggplot(aes(x = long, 
             y = lat,
             group = group)) +
  geom_polygon(fill="lightgray",
               colour = "blue") 

subregion_label_data <-
  map_data("world",
           region = "Philippines") %>%
  dplyr::group_by(subregion,
                  group) %>%
  dplyr::summarize(long = mean(long),
                   lat = mean(lat)) %>%
  filter(subregion == "Negros" |
           subregion == "Cebu")

region_label_data <-
  map_data("world",
           region = "Philippines") %>%
  dplyr::group_by(region) %>%
  dplyr::summarize(long = mean(long),
                   lat = mean(lat))

map_data("world",
         region = "Philippines") %>%
  filter(long>122 & long<125,
         lat>7.5 & lat<12) %>%
  ggplot(aes(long,
             lat,
             group=group)) +
  geom_polygon(fill="lightgray",
               color = "white") +
  geom_text(data = subregion_label_data,
            aes(label = subregion),
            size = 6,
            hjust = 0.5) +
  # geom_text(data = region_label_data,
  # aes(x = long,
  #   y= lat,
  # label = region),
  #  size = 10,
  # hjust = 0.5,
  # inherit.aes = FALSE) +
  geom_point(data = data_mpa, #tibble with mpa lat/long I need to create
             aes(x = long,
                 y = lat,
                 color = size,
                 shape = year,
                   inherit.aes = FALSE))
             #very last tutorial map should be used
             
             
             
      