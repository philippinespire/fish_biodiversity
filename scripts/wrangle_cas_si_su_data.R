#### INITIALIZE ####

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#### PACKAGES ####
packages_used <- 
  c("tidyverse",
    "readxl",
    "janitor",
    "purrr",
    "magrittr",
    "measurements",
    "lubridate",
    "readr",
    "devtools",
    "maptools",
    #"tidyimpute",
    "taxize")

packages_to_install <- 
  packages_used[!packages_used %in% installed.packages()[,1]]

if (length(packages_to_install) > 0) {
  install.packages(packages_to_install, 
                   Ncpus = Sys.getenv("NUMBER_OF_PROCESSORS") - 1)
}

lapply(packages_used, 
       require, 
       character.only = TRUE)

#### USER DEFINED VARIABLES ####

wrangle_si_data_path = "./wrangleStationData_SI.R"
wrangle_su_si_data_path = "./wrangle_SU-SI_DuplicatesNewData.R"
wrangle_cas_data_path = "./wrangle_cas_data.R"
calculate_mpa_distances = "./distance_calculations_mpa.R"
visualize_pca_path = "./visualize_pca_mpa_influence.R"
wrangle_arcgis_path = "./wrangle_arcgis.R"

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
#   dplyr::mutate(adjusted_latitude = case_when(is.na(latitude) ~ latitude_7879,
#                                        TRUE ~ latitude),
#          adjusted_longitude = case_when(is.na(longitude) ~ longitude_7879,
#                                        TRUE ~ longitude)) %>%
#   dplyr::select(-contains("7879"))
  

#### BIND DATA AMONG STUDIES ####
data_cas_si_su <-
  bind_rows(data_cas_all %>%
              dplyr::mutate(study = "cas_2016"), 
            data_si_station_gis %>%
              dplyr::mutate(study = "si_1978"), 
            data_su_all %>%
              dplyr::mutate(study = "su_2022")) %>%
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
source(visualize_pca_path)

data_cas_si_su_mpa <-
  data_cas_si_su %>%
  left_join(data_mpa_stations_pc) %>%
  dplyr::select(station_code:station_code_7879,
                mpa_name,
                mpa_year_established_earliest,
                area_closest_mpa_ha,
                distance_closest_mpa_km,
                age_closest_mpa_y,
                mpa_area_within_xkm_ha:pc3_mpa_infl) 

#write_excel_csv(data_cas_si_su, "data_cas_si_su.csv")
  
#### ADD ARCGIS HUMAN POP DATA TO SURVEY DATA ####
source(wrangle_arcgis_path)

data_cas_si_su_mpa_pop <-
  data_cas_si_su_mpa %>%
  left_join(data_human_pop)

