#### INITIALIZE ####

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

install.packages("taxize")

library(tidyverse)
library(readxl)
library(janitor)
library(purrr)
library(magrittr)
library(lubridate)
library(readr)
library(taxize)

#### USER DEFINED VARIABLES ####

wrangle_si_data_path = "./wrangleStationData_SI.R"
wrangle_su_si_data_path = "./wrangle_SU-SI_DuplicatesNewData.R"
wrangle_cas_data_path = "./wrangle_cas_data.R"
calculate_mpa_distances = "./distance_calculations_mpa.R"

#### READ IN DATA ####
source(wrangle_si_data_path)
source(wrangle_su_si_data_path)
source(wrangle_cas_data_path)

#### FIX MISSING LAT LONG 2022 WITH 1970S COORDS ####
# data_su_all <-
#   data_su_all %>%
#   left_join(data_si_station_gis %>%
#               distinct(station_code,
#                        .keep_all = TRUE) %>%
#               dplyr::rename(station_code_7879 = station_code) %>%
#               dplyr::select(station_code_7879,
#                      latitude,
#                      longitude) %>%
#               dplyr::rename(latitude_7879 = latitude,
#                             longitude_7879 = longitude)) %>%
#   mutate(adjusted_latitude = case_when(is.na(latitude) ~ latitude_7879,
#                                        TRUE ~ latitude),
#          adjusted_longitude = case_when(is.na(longitude) ~ longitude_7879,
#                                        TRUE ~ longitude)) %>%
#   dplyr::select(-contains("7879"))
  

#### BIND DATA AMONG STUDIES ####
data_cas_si_su <-
  bind_rows(data_cas_all %>%
              mutate(study = "cas_2016"), 
            data_si_station_gis %>%
              mutate(study = "si_1978"), 
            data_su_all %>%
              mutate(study = "su_2022")) %>%
  dplyr::select(-family,
                -identification,
                -order,
                -ecol_habitat:-catalog_number)
  # drop_na() %>%
  # group_by(station_code,
  #          verified_identification:study) %>%
  # summarize(specimen_count = sum(specimen_count))

rm(data_si_station_gis,
   data_su_all,
   data_cas_all)

#### ADD MPA DISTANCE DATA ####
source(calculate_mpa_distances)

data_cas_si_su_mpa <-
  data_cas_si_su %>%
  left_join(data_closest_mpa) %>%
  dplyr::select(station_code:station_code_7879,
                mpa_name,
                mpa_year_established_earliest,
                mpa_area_ha,
                station_mpa_distance_km,
                closest_mpa_age_during_study_yrs)

#write_excel_csv(data_cas_si_su, "data_cas_si_su.csv")
  
#### ADD HUMAN POP DATA TO SURVEY DATA ####
data_cas_si_su_mpa_pop <-
  data_cas_si_su_mpa %>%
  left_join(data_human_pop)

#### GET FAMILY NAMES ####
get_ids(c("Acanthurus mata", "Cheilinus oxycephalus"), db="ncbi")
# vector to retrieve all names did not work, 2 names found only