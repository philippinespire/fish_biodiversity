#### INITIALIZE ####

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# install.packages("tidyverse")
# install.packages("readxl")

library(tidyverse)
library(readxl)
library(janitor)
library(purrr)

#### USER DEFINED VARIABLES ####
querydataDir = "../SI/Collections_Data"
querydataPattern = "*.csv"
gisDataFile = "../SI/Coordinates/Coordinate_Conversions.xlsx"


#### READ IN COUNT DATA ####

data_si <-
  list.files(querydataDir,
             querydataPattern,
             full.names = TRUE) %>%
  map(.,
      ~ read_csv(.x)) %>%
  bind_rows() %>%
  clean_names() %>%
  distinct(catalog_number_usnm, # if we don't do this, get 2 more records
           .keep_all = TRUE) %>%
  rename(prep_loc_count = preparation_details_preparation_location_count,
         field_number = field_number_s,
         collectors = collector_s) %>%
  mutate(odu_station_code = str_replace(field_number,
                                        " ",
                                        "_"),
         collection_method = str_to_lower(collection_method),
         depth_m_min = str_remove(depth_m,
                                  " .*$"),
         depth_m_max = str_remove(depth_m,
                                  "^.* "))
data_si %>% filter(is.na(depth_m_min)) %>% view()
data_si %>% filter(is.na(depth_m_max)) %>% view()
data_si %>% filter(is.na(depth_m_min), is.na(depth_m_max)) %>% view()

# 9 out of 9 "min/max depth = NA" were due to the cells being empty

#### READ IN GIS DATA ####
data_gis <-
  read_excel(gisDataFile) %>%
  clean_names() %>%
  select(odu_station_code:smithsonian_station_code,
         starts_with("adjusted_"),
         -starts_with("x"))

#### JOIN DATA ####
data_si_gis <-
  data_si %>%
  left_join(data_gis,
            by = "odu_station_code")

#### DATA VISUALIZE ####
data_si %>%
  select(odu_station_code,
         starts_with("depth_")) %>%
  pivot_longer(cols = depth_m:depth_m_max,
               names_to = "depth_cat",
               values_to = "meters") %>%
  filter(depth_cat != "depth_m") %>%
  distinct() %>%
  ggplot(aes(x=odu_station_code,
             y=meters,
             color = depth_cat)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90,
                             hjust = 1,
                             vjust = 0.5))

# #### DATA CHECKING ####
# data_si %>% 
#   select(field_number_s) %>%
#   unique() %>%
#   view()
#   
# data_si %>% 
#   filter(is.na(field_number_s)) %>%
#   view()
# 
# data_si %>% 
#   select(field_number_s,
#          date_collected) %>%
#   distinct() %>%
#   arrange(date_collected,
#           field_number_s) %>%
#   view()
# 
# data_si %>% 
#   select(field_number_s,
#          date_collected) %>%
#   distinct() %>%
#   arrange(field_number_s,
#           date_collected) %>%
#   view()
# 
