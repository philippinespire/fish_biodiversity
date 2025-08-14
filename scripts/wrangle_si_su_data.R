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
    "tidyimpute",
    "taxize",
    "dplyr")

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
wrangle_arcgis_path = "./wrangle_arcgis_si_su.R"
calculate_mpa_distances = "./distance_calculations_mpa.R"
visualize_pca_path = "./visualize_pca_mpa_influence.R"


#### READ IN DATA ####
source(wrangle_si_data_path)
source(wrangle_su_si_data_path)

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
data_si_su <-
  bind_rows(data_si_station_gis %>%
              dplyr::mutate(study = "si_1978"), 
            data_su_all %>%
              dplyr::mutate(study = "su_2022")) %>%
  dplyr::select(-identification,
                -order,
                -ecol_habitat:-catalog_number)
  # drop_na() %>%
  # group_by(station_code,
  #          verified_identification:study) %>%
  # summarize(specimen_count = sum(specimen_count))

# si_su_unique_station_codes <- sort(unique(data_si_su$station_code))
# si_su_n_unique_station_codes <- n_distinct(data_si_su$station_code)
# print(si_su_unique_station_codes)
# print(si_su_n_unique_station_codes)
# 79 unique stations in data_si_su. 58 SI stations and 21 SU stations. 

rm(data_si_station_gis,
   data_su_all)


#### ADD MUNICIPALITY/PROVINCE/REGION INFO ####
# create a tibble with the municipality information
municipality_key <- tribble(
  ~municipality,             ~municipality_code, ~province,            ~region,              ~sea,
  "Siquijor",                "SIQ",              "Siquijor",           "Central Visayas",    "Bohol Sea",
  "San_Juan",                "JUA",              "Siquijor",           "Central Visayas",    "Bohol Sea",
  "San Juan",                "JUA",              "Siquijor",           "Central Visayas",    "Bohol Sea",
  "Siaton",                  "SIA",              "Negros_Oriental",    "Central Visayas",    "Bohol Sea",
  "Siaton/Zamboanguita",     "SIA",              "Negros_Oriental",    "Central Visayas",    "Bohol Sea",
  "Zamboanguita",            "ZAM",              "Negros_Oriental",    "Central Visayas",    "Bohol Sea",
  "Zamboangita",             "ZAM",              "Negros_Oriental",    "Central Visayas",    "Bohol Sea",
  "Magsaysay",               "MAG",              "Palawan",            "Palawan",            "Sulu Sea",
  "Cuyo",                    "CUY",              "Palawan",            "Palawan",            "Sulu Sea",
  "Larena",                  "LAR",              "Siquijor",           "Central Visayas",    "Bohol Sea"
)

# fill in missing info for municipality_code and province
data_si_su <- data_si_su %>%
  dplyr::left_join(
    municipality_key %>%
      dplyr::select(municipality, municipality_code, province),
    by     = "municipality",
    suffix = c("", "_key")
  ) %>%
  dplyr::mutate(
    municipality_code = coalesce(municipality_code, municipality_code_key),
    province          = coalesce(province,          province_key)
  ) %>%
  dplyr::select(-municipality_code_key, -province_key)

# add the columns region and sea by municipality
data_si_su <- data_si_su %>%
  dplyr::left_join(
    municipality_key %>% 
      dplyr::select(municipality, region, sea),
    by = "municipality"
  )

#### WRANGLE SU SI DUPLICATE STATIONS ####
# duplicate filter: Filter out non-duplicated si_1978 stations. 
# 24 su_2022 stations duplicated 24 si_1978 stations.

# length(unique(data_si_su$station_code))
# 79 unique stations

# Define the list of the 48 station codes from the si_1978 and su_2022 survey that were duplicated.
# Already filtered out in wrangle_SU-SI_DuplicatesNewData.R: SU-22-10 was a lagoon. SU-22-17 was a mangrove. SU-22-21 experienced too much waves/current.
# Already filtered out in wrangleStationData_SI.R: SP_78-19 was a lagoon. SP_78-22 was a mangrove.
# Have not filtered out SP-78-05, which was a duplicated of SU-22-21. 
duplicated_stations <- c(
  "LK_79-02", "LK_79-03", "LK_79-04", "LK_79-13", "LK_79-15", "LK_79-16", "SP_78-03", "SP_78-04", 
  "SP_78-05", "SP_78-07", "SP_78-09", "SP_78-10", "SP_78-11", "SP_78-17", "SP_78-18", "SP_78-19",  
  "SP_78-20", "SP_78-21", "SP_78-22", "SP_78-24", "SP_78-25", "SP_78-26", "SP_78-27", "SP_78-44",
  
  "SU-19-01", "SU-19-02", "SU-19-03", "SU-22-04", "SU-22-05", "SU-22-06", "SU-22-07", "SU-22-08", 
  "SU-22-09", "SU-22-10", "SU-22-11", "SU-22-12", "SU-22-13", "SU-22-14", "SU-22-15", "SU-22-16", 
  "SU-22-17", "SU-22-18", "SU-22-19", "SU-22-20", "SU-22-21", "SU-22-22", "SU-22-23", "SU-22-24" 
)

# Identify which duplicated_stations are present in data_cas_si_su
# present_stations <- duplicated_stations %in% data_si_su$station_code

# Combine the results into a data frame for clarity
# station_check <- data.frame(
#   station_code = duplicated_stations,
#   is_present = present_stations
# )

# View the result
# print(station_check)

# Filter the dataframe so that just the 48 duplicated stations are present (total of 48)
data_si_su <- data_si_su %>%
  filter(station_code %in% duplicated_stations)

# length(unique(data_si_su$station_code))
# 43 unique stations

# habitat filter: 
# Stations SU-22-10_SP-78-19 sampled a lagoon.
data_si_su <- data_si_su %>%
  filter(!station_code %in% c("SU-22-10", "SP_78-19"))

# Stations SU-22-17_SP_78-22 sampled a mangrove.
data_si_su <- data_si_su %>%
  filter(!station_code %in% c("SU-22-17", "SP_78-22"))

# sampling filter: 
# SU station SU-22-21_SP-78-05 experienced a rough sampling environment (current, waves), so should be filtered out.
data_si_su <- data_si_su %>%
  filter(!station_code %in% c("SU-22-21", "SP_78-05"))

# depth filter: 
# Depth of SU-22-23_LK-79-16 & SU-19-01_LK-79-13 is 30 meters. 
# The next deepest site is SU-22-12_SP-78-27 at 21.2 meters

# proxy filter: 
# SU stations SU-22-16_SP-78-25, SU-22-18_SP-78-17, & SU-22-24_SP-78-03 were a nearby proxy for their SI stations

# length(unique(data_si_su$station_code))
# 42 unique stations. 21 1970's stations and 21 contemporary duplicates. 


#### ADD MPA DISTANCE DATA ####
source(calculate_mpa_distances)
source(visualize_pca_path)

data_si_su_mpa <-
  data_si_su %>%
  left_join(data_mpa_stations_pc) %>%
  dplyr::select(station_code:study, # station_code_7879,
                mpa_name,
                mpa_year_established_earliest,
                area_closest_mpa_ha,
                distance_closest_mpa_km,
                age_closest_mpa_y,
                mpa_area_within_xkm_ha:pc3_mpa_infl) 

#write_excel_csv(data_cas_si_su, "data_cas_si_su.csv")
  
#### ADD ARCGIS HUMAN POP DATA TO SURVEY DATA ####
source(wrangle_arcgis_path)

data_si_su_mpa_pop <-
  data_si_su_mpa %>%
  left_join(data_human_pop)

