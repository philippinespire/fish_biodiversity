#### INITIALIZE ####

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# install.packages("tidyverse")
# install.packages("readxl")

library(tidyverse)
library(readxl)
library(janitor)
library(purrr)
library(magrittr)
library(lubridate)

#### USER DEFINED VARIABLES ####
querydataDir = "../SI/Collections_Data"
querydataPattern = "*.csv"
gisDataFile = "../SI/Coordinates/Coordinate_Conversions.xlsx"
siteMetaDataFile = "../data/station_info.xlsx"
dataDir = "../data"

dataDir = str_replace(dataDir,
                      "\\/$",
                      "")

CAS_verified_names = "../data/All_confirmed_names.xlsx"

#### READ IN COUNT DATA ####

data_si <-
  list.files(querydataDir,
             querydataPattern,
             full.names = TRUE) %>%
  purrr::map(.,
      ~ read_csv(.x)) %>%
  bind_rows() %>%
  clean_names() %>%
  remove_empty(which = c("cols")) %>%
  distinct(catalog_number_usnm, # if we don't do this, get 2 more records
           .keep_all = TRUE) %>%
  dplyr::rename(prep_loc_count = preparation_details_preparation_location_count,
         field_number = field_number_s,
         collectors = collector_s) %>%
  mutate(station_code = str_replace(field_number,
                                        " ",
                                        "_"),
         collection_method = str_to_lower(collection_method),
         depth_m_min = as.numeric(str_remove(depth_m,
                                             " .*$")),
         depth_m_max = as.numeric(str_remove(depth_m,
                                             "^.* ")),
         depth_cat = case_when(depth_m_max < 2 ~ "<2m",
                               depth_m_max <= 15 ~ "2-15m",
                               depth_m_max >15 ~ ">15m",
                               TRUE ~ NA_character_),
         depth_cat = factor(depth_cat,
                            levels = c("<2m",
                                       "2-15m",
                                       ">15m")),
         date_collected = stringr::str_remove(date_collected,
                                     " \\(.*$"),
         date_collected = stringr::str_remove(date_collected,
                                     " to.*$"),
         date_collected = stringr::str_replace_all(date_collected,
                                          "\\-",
                                          " "),
         date_collected = dmy(date_collected)) %>%
  # select(catalog_number_usnm,
  #        date_collected,
  #        date_collected_2)
  filter(kind_of_object != "Image",
         #date_collected != "4 Dec 1967 (1967 Dec 04 - 0000 00 00; 14:15 - 15:15)",
         date_collected != "1967-12-04",
         !is.na(field_number))

# data_si %>% filter(is.na(depth_m_min)) %>% view()
# data_si %>% filter(is.na(depth_m_max)) %>% view()
# data_si %>% filter(is.na(depth_m_min), is.na(depth_m_max)) %>% view()

# 9 out of 9 "min/max depth = NA" were due to the cells being empty
#### CREATE STATION METADATA ####
# data_si %>%
#   select(date_collected,
#          ocean,
#          sea_gulf,
#          archipelago,
#          island_grouping,
#          island_name,
#          country,
#          province_state,
#          district_county,
#          precise_locality,
#          starts_with("centroid"),
#          collectors,
#          field_number,
#          vessel,
#          cruise,
#          station,
#          expedition,
#          collection_method,
#          depth_m,
#          station_code,
#          depth_m_min,
#          depth_m_max) %>%
#   distinct() %>%
  # write_csv(str_c(dataDir,
  #                 "station_info.csv",
  #                 sep = "/"))

#### JOIN FIELD DATA RECORDS WITH data_si ####
data_si_station <- 
  data_si %>%
    left_join(read_excel(siteMetaDataFile,
                         na = c("NA",
                                "na")) %>%
                clean_names() %>%
                dplyr::select(-date_collected) ) %>%
    mutate(dist_shore_m_min = case_when(str_detect(dist_shore,
                                                   "\\'") ~ as.numeric(str_remove(dist_shore,
                                                                                  "[ \\'].*$")) * 0.3048,
                                        str_detect(dist_shore,
                                                   "m *$") ~ as.numeric(str_remove(dist_shore,
                                                                                   " .*$")) * 1,
                                        str_detect(dist_shore,
                                                   "yds") ~ as.numeric(str_remove(dist_shore,
                                                                                  " .*$")) * 0.9144,
                                        str_detect(dist_shore,
                                                   "1/4") ~ as.numeric(str_replace(dist_shore,
                                                                                   "1/4",
                                                                                 "0.25")),
                                        str_detect(dist_shore,
                                                   "mi") ~ as.numeric(str_remove(dist_shore,
                                                                                 " .*$")) * 1609.344,
                                        str_detect(dist_shore,
                                                   "km") ~ as.numeric(str_remove(dist_shore,
                                                                                 "km.*$")),
                                        str_detect(dist_shore,
                                                   "~") ~ as.numeric(str_remove(dist_shore,
                                                                                 "~ ")) * 1000),
           ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
           ## figure out how to isolate max dist
           ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
           # dist_shore_m_max = case_when(str_detect(dist_shore,
           #                                         "\\'") ~ as.numeric(str_replace(dist_shore,
           #                                                                        "^.* \\- *")) * 0.3048,
           #                              str_detect(dist_shore,
           #                                         "m *$") ~ as.numeric(str_remove(dist_shore,
           #                                                                         " .*$")) * 1,
           #                              str_detect(dist_shore,
           #                                         "yds") ~ as.numeric(str_remove(dist_shore,
           #                                                                        " .*$")) * 0.9144,
           #                              str_detect(dist_shore,
           #                                         "mi") ~ as.numeric(str_remove(dist_shore,
           #                                                                       " .*$")) * 1609.344)
           
           collection_method_manual = str_to_lower(collection_method_manual),
           chemical_euthanasia = case_when(str_detect(collection_method_type,
                                                      "rotenone") ~ "yes",
                                           str_detect(collection_method_manual,  #remove if field data record overrules the si database
                                                      "rotenone") ~ "yes",
                                           !is.na(collection_method_type) ~ "no",
                                           TRUE ~ NA_character_)) %>%
  
  filter(chemical_euthanasia == "yes") %>%
  dplyr::select(-odu_station_code)


#### READ IN GIS DATA ####
data_gis <-
  read_excel(gisDataFile) %>%
  clean_names() %>%
  # dplyr::rename(station_code = odu_station_code) %>%
  mutate(station_code = str_replace(odu_station_code,
                                    "-0",
                                    "-"),
         station_code = str_replace(station_code,
                                    "JL_..",
                                    "JL")) %>%
  dplyr::select(station_code,
         odu_station_code:smithsonian_station_code,
         starts_with("adjusted_"),
         -starts_with("x"))
  

#### JOIN DATA ####
data_si_station_gis <-
  data_si_station %>%
  left_join(data_gis,
            by = "station_code") %>% 
  left_join(read_excel(CAS_verified_names) %>%
              dplyr::select(-family),
            by = c("identification" = "original_id")) %>%
  mutate(verified_identification = case_when(is.na(verified_identification) ~ identification,
                                TRUE ~ verified_identification),
         province_state = case_when(is.na(province_state) ~ province,
                                TRUE ~ province_state),
         # this adds zero padded station codeS
         station_code = odu_station_code) %>%
  dplyr::rename(notes = notes.x,
         notes_cas_verification = notes.y,
         island = island_name,
         locality = precise_locality) %>%
  dplyr::select(-kind_of_object,
         -special_collections,
         -type_status,
         -type_citations,
         -subfamily,
         -other_identifications,
         -centroid_latitude,
         -centroid_longitude,
         -collectors,
         -vessel,
         -prep_loc_count,
         -measurements,
         -accession_number,
         -gen_bank_numbers,
         -ezid,
         -other_numbers_type_value,
         -record_last_modified,
         -catalog_number_usnm,
         -name_hierarchy,
         -ocean,
         -sea_gulf,
         -archipelago,
         -country,
         -district_county,
         -cruise:-collection_method,
         -depth_m_min,
         -depth_m_max,
         -depth_cat,
         -odu_station_code,
         -collection_method_manual,
         -method_capture:-chemical_euthanasia,
         -smithsonian_station_code,
         -island_grouping,
         -province,
         -province_code)
 
rm(data_si,
   data_si_station,
   data_gis)
