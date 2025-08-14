#### INITIALIZATION ####
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#### INSTALL PACKAGES ####
packages_used <- 
  c("tidyverse",
    "janitor",
    "readxl",
    "maps",
    "viridis"
  )

packages_to_install <- 
  packages_used[!packages_used %in% installed.packages()[,1]]

if (length(packages_to_install) > 0) {
  install.packages(packages_to_install, 
                   Ncpus = Sys.getenv("NUMBER_OF_PROCESSORS") - 1)
}

lapply(packages_used, 
       require, 
       character.only = TRUE)

theme_set(
  theme_void()
)


#### USER DEFINED VARIABLES ####

inFilePath1 = "../../data/station_info.csv"
inFilePath2 = 
  # inFilePath2 = "./PHIRES_MetaData.xlsx"
  
  # outFilePath = "./data_combined.tsv"

    
#### READ IN DATA & CURATE ####

data <-
  read_csv(inFilePath1,
           na="NA") %>%
  clean_names()


#### COMBINE DATA ####

# rearrange order of columns, metadata then data


#### WRITE LONG FORMAT FILE ####

# data_all %>%
#   write_tsv(outFilePath)

#### VISuALIZE METADATA ####

data %>%
  ggplot(aes(y=centroid_latitude,
             x=centroid_longitude,
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
  geom_point(data = data_gis,
             aes(x = longitude,
                 y = latitude,
                 color = province_code),
             inherit.aes = FALSE)
