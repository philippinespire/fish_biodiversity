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
                                        "_"))

#### READ IN GIS DATA ####
data_si_gis <-
  data_si %>%
  left_join(data_gis,
            by = "odu_station_code")

data_gis <-
  read_excel(gisDataFile) %>%
  clean_names() %>%
  select(odu_station_code:smithsonian_station_code,
         starts_with("adjusted_"),
         -starts_with("x"))

#### JOIN DATA ####


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
