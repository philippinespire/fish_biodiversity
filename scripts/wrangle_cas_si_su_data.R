#### INITIALIZE ####

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tidyverse)
library(readxl)
library(janitor)
library(purrr)
library(magrittr)
library(lubridate)
library(readr)

#### USER DEFINED VARIABLES ####

wrangle_si_data_path = "./wrangleStationData_SI.R"
wrangle_su_si_data_path = "./wrangle_SU-SI_DuplicatesNewData.R"
wrangle_cas_data_path = "./wrangle_cas_data.R"

#### READ IN DATA ####
source(wrangle_si_data_path)
source(wrangle_su_si_data_path)
source(wrangle_cas_data_path)

#### FIX MISSING LAT LONG 2022 WITH 1970S COORDS ####
data_su <-
  data_su %>%
  left_join(data_si_station_gis %>%
              distinct(station_code,
                       .keep_all = TRUE) %>%
              dplyr::rename(station_code_7879 = station_code) %>%
              dplyr::select(station_code_7879,
                     latitude,
                     longitude) %>%
              dplyr::rename(latitude_7879 = latitude,
                     longitude_7879 = longitude)) %>%
  mutate(adjusted_latitude = case_when(is.na(adjusted_latitude) ~ latitude_7879,
                                       TRUE ~ adjusted_latitude),
         adjusted_longitude = case_when(is.na(adjusted_longitude) ~ longitude_7879,
                                       TRUE ~ adjusted_longitude)) %>%
  dplyr::select(-contains("7879"))
  

#### BIND DATA ####
data_cas_si_su <-
  bind_rows(data_cas_all %>%
              mutate(study = "cas_2016"), 
            data_si_station_gis %>%
              mutate(study = "si_1978") %>%
              rename(adjusted_latitude = latitude,
                     adjusted_longitude = longitude), 
            data_su %>%
              mutate(study = "su_2022")) %>%
  dplyr::select(-family,
         -identification,
         -notes,
         -notes_cas_verification,
         # -catalog_number,
         -order,
         -ecol_habitat:-barangay)
  # drop_na() %>%
  # group_by(station_code,
  #          verified_identification:study) %>%
  # summarize(specimen_count = sum(specimen_count))

rm(data_si_station_gis,
   data_su,
   data_cas_all)

rm(data_closest_mpa,
   data_mpa,
   data_mpa_area_xkm,
   data_mpa_study_stationcode_distances,
   list_mpa_latlong,
   list_station_latlong,
   pca_mpa_influence)

#write_excel_csv(data_cas_si_su, "data_cas_si_su.csv")
  
