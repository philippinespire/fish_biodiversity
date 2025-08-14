#### NTOES ####
# This script takes raw input files of the 1978 and 1979 rotenone survey from https://collections.nmnh.si.edu/search/fishes/
# It was updated to filter out stations that did not use rotenone or did not sample coral habitat.
# Other changes, updates were also incorporated.


#### INITIALIZE ####

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#### PACKAGES ####
packages_used <- 
  c("tidyverse",
    "readxl",
    "janitor",
    "purrr",
    "magrittr",
    "lubridate",
    "stringr",
    "dplyr",
    "stringdist",
    "fuzzyjoin")

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
# data directory
querydataDir = "../SI/Collections_Data"
# data file pattern
querydataPattern = "*.csv"
# https://collections.nmnh.si.edu/search/fishes/ searches using 'Sp-78' and 'Sp-79' in the Field 'Expedition' on May 20, 2025
# SmithsonianCollections_Expedition_Sp-78_nmnhsearch-20250520.csv
# SmithsonianCollections_Expedition_Sp-79_nmnhsearch-20250520.csv
gisDataFile = "../SI/Coordinates/Coordinate_Conversions.xlsx"
siteMetaDataFile = "../data/station_info.xlsx"
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
  dplyr::mutate(station_code = str_replace(field_number,
                                           " ",
                                           "_"),
                collection_method = str_to_lower(collection_method),
                #technically don't need these depth min and max bc they are read in later from metadata
                depth_m_min = as.numeric(str_remove(depth_m,
                                                    " .*$")),
                depth_m_max = as.numeric(str_remove(depth_m,
                                                    "^.* ")),
                depth_cat = case_when(depth_m_max < 2 ~ "< 2m",
                                      depth_m_max <= 15 ~ "2 - 15m",
                                      depth_m_max >15 ~ "> 15m",
                                      TRUE ~ NA_character_),
                depth_cat = factor(depth_cat,
                                   levels = c("< 2m",
                                              "2 - 15m",
                                              "> 15m")),
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
  # filter out Image from column kind_of_object
  
  filter(kind_of_object != "Image",
         #date_collected != "4 Dec 1967 (1967 Dec 04 - 0000 00 00; 14:15 - 15:15)",
         date_collected != "1967-12-04",
         !is.na(field_number),
         specimen_count != 0 ) # filter out those with 0 in column specimens

# should have 4604 observations

data_si <-  data_si %>%
  mutate(station_code = str_remove(station_code, ";.*$"), # remove the ; and anything after in column field_number
         station_code = str_remove_all(station_code, " ") # remove any whitespaces 
  ) 


# si_unique_station_codes <- sort(unique(data_si$station_code))
# si_n_unique_station_codes <- n_distinct(data_si$station_code)
# print(si_unique_station_codes)
# print(si_n_unique_station_codes)
# 78 unique si field numbers. Does not include SP 78-14 because it only had "one Scorpaenid". 
# This station does not even show up on collections.nmnh.si.edu.

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

#### READ IN METADATA ####
# edit dist_shore
metadata_si <- read_excel(siteMetaDataFile,
                          na = c("NA",
                                 "na")) %>%
  clean_names() %>%
  dplyr::select(-date_collected) %>% 
  dplyr::rename(station_code = odu_station_code) %>%
  dplyr::mutate(dist_shore_m_min = case_when(str_detect(dist_shore,
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
                                                                                     "~ ")) * 1000)) #,
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


# collection_method_manual = str_to_lower(collection_method_manual) %>% # ,
# chemical_euthanasia = case_when(str_detect(collection_method_type,
#                                            "rotenone") ~ "yes",
#                                 str_detect(collection_method_manual,  #remove if field data record overrules the si database
#                                            "rotenone") ~ "yes")) %>% # ,
# !is.na(collection_method_type) ~ "no",
# TRUE ~ NA_character_)) %>%

# filter(chemical_euthanasia == "yes") %>%
# dplyr::select(-odu_station_code))

# meta_si_unique_station_codes <- sort(unique(metadata_si$station_code))
# meta_si_n_unique_station_codes <- n_distinct(metadata_si$station_code)
# print(meta_si_unique_station_codes)
# print(meta_si_n_unique_station_codes)
# 78. Should be the same as data_si


#### JOIN FIELD DATA RECORDS WITH data_si ####
# change names of depth columns in data_si. use metadata_si depth columns moving forward.
data_si <- data_si %>%
  dplyr::rename(
    depth_m_si = depth_m,
    depth_m_min_si = depth_m_min,
    depth_m_max_si = depth_m_max
  )

# clean dataframes of duplicated columns before joining
# collection_method, collectors, expedition, field_number, island_name, ocean, precise_locality, province_state
# from metadata_si, deselect columns collection_method, ocean 
metadata_si <- metadata_si %>%
  dplyr::select(-collection_method, -ocean, -sea_gulf, -archipelago, -vessel, -cruise, -station)

# from data_si, deselect columns collectors, expedition, field_number, island_name, ocean, precise_locality, province_state
data_si <- data_si %>%
  dplyr::select(-collectors, -expedition, -field_number,
         -island_name, -ocean, -precise_locality, -province_state,
         -sea_gulf, -archipelago, -vessel, -cruise, -station, 
         -island_grouping, -country, -district_county,
         -centroid_latitude, -centroid_longitude
  )

# join data_si and metadata_si by the column station_code
# If station_code is not identified as the column to join by, then it causes issues.
# Most notably there was a large decrease in the number of observations. 
data_si_station <- 
  data_si %>%
  left_join(metadata_si, by = "station_code")
# there should be 49 variables


#### FILTER OUT STATIONS THAT DIDN'T USE ROTENONE ####
# filter out stations that do not have rotenone in the column collection_method_manual
# it is assumed that each station used rotenone, unless specifically stated otherwise in the field data record
# 5, just spear, 1 just dipnet, 1 fish trap
data_si_station <- data_si_station %>%
  filter(str_detect(str_to_lower(collection_method_manual), "rotenone"))

# si_unique_station_codes <- sort(unique(data_si_station$station_code))
# si_n_unique_station_codes <- n_distinct(data_si_station$station_code)
# print(si_unique_station_codes)
# print(si_n_unique_station_codes)
# 71 stations used rotenone


#### FILTER OUT STATIONS THAT DIDN'T SAMPLE CORAL HABITAT ####
# 14 stations did not sample coral/reef/rock/rubble (estuary, sand, mangrove, lagoon, tidal inlet)
# this should further reduce the number of rotenone stations that sampled coral habitat to a total of 58 stations
data_si_station <- data_si_station %>%
  filter(str_detect(str_to_lower(habitat), "coral"))

# si_unique_station_codes <- sort(unique(data_si_station$station_code))
# si_n_unique_station_codes <- n_distinct(data_si_station$station_code)
# print(si_unique_station_codes)
# print(si_n_unique_station_codes)
# 58 stations used rotenone and sampled coral habitat


#### COMPARE TO SU STATIONS ####
# 24 su_2022 stations duplicated 24 si_1978 stations.
# Define the list of si_1978 station codes to keep that were duplicated.
# duplicated_stations <- c(
#   "LK_79-13", "SP_78-7", "SP_78-4", "LK_79-2", "SP_78-44", "LK_79-3", "SP_78-9", "LK_79-4",
#   "SP_78-18", "SP_78-19", "SP_78-24", "SP_78-27", "SP_78-21", "SP_78-20", "SP_78-26", "SP_78-25",
#   "SP_78-22", "SP_78-17", "SP_78-10", "SP_78-11", "SP_78-5", "LK_79-15", "LK_79-16", "SP_78-3"
# )

# Identify which duplicated_stations are NOT in data_si_station$field_number
# missing_stations <- duplicated_stations[!duplicated_stations %in% data_si_station$station_code]

# View the result
# print(missing_stations)
# n_distinct(missing_stations)
# should be missing 2. SP_78-19 was a lagoon. SP_78-22 was a mangrove.


#### SPECIES NAMES ####
# the only rows that did not have anything in the column family were those that had Trimma trioculatum in the column identification
data_si_station <- data_si_station %>%
  mutate(family = case_when(
    str_detect(identification, "Trimma trioculatum") ~ "Gobiidae",
    TRUE ~ family
  ))

# clean whitespaces in identification column 
data_si_station <- data_si_station %>%
  mutate(identification = str_trim(str_squish(identification)))


#### READ IN CAS VERIFIED NAMES ####
CAS_verified <- read_excel(CAS_verified_names) %>%
  janitor::clean_names() %>%
  mutate(original_id = str_trim(str_squish(original_id))) %>%
  dplyr::select(-family) %>%
  dplyr::rename(notes_cas_verification = notes)


#### JOIN DATA_SI_STATION WITH CAS_VERIFIED_NAMES
data_si_station <- 
  data_si_station %>%
  left_join(CAS_verified,
            by = c("identification" = "original_id")) %>%
  dplyr::mutate(verified_identification = case_when(
    is.na(verified_identification) ~ identification,
    TRUE ~ verified_identification))


#### CREATE GENUS COLUMN ####
# create genus column from contents of the lowest_tax_cat column
data_si_station <- data_si_station %>%
  mutate(genus = case_when(
    lowest_tax_cat == "genus"    ~ verified_identification,
    lowest_tax_cat == "family"   ~ "family",
    lowest_tax_cat == "subfamily"~ "subfamily",
    lowest_tax_cat == "species"  ~ str_extract(verified_identification, "^[^\\s]+"),
    TRUE                         ~ NA_character_
  ))


#### READ IN GIS DATA ####
## CONFIRM COORDINATES ##
data_gis <-
  read_excel(gisDataFile) %>%
  clean_names() %>%
  # dplyr::rename(station_code = odu_station_code) %>%
  dplyr::mutate(station_code = str_replace(odu_station_code,
                                           "-0",
                                           "-"),
                station_code = str_replace(station_code,
                                           "JL_..",
                                           "JL")) %>%
  dplyr::select(station_code,
                odu_station_code:smithsonian_station_code,
                starts_with("dd_"),
                -starts_with("x")) %>%
  dplyr::rename(latitude = dd_latitude,
                longitude = dd_longitude)


#### JOIN DATA GIS DATA TO data_si_station ####
data_si_station_gis <-
  data_si_station %>%
  # we decided that max depth is generally where rotenone was deployed
  dplyr::mutate(depth_m = depth_m_max) %>%
  left_join(data_gis,
            by = "station_code") %>% 
  dplyr::mutate(
    province_state = case_when(
      is.na(province_state) ~ province,
      TRUE ~ province_state),
    # this adds zero padded station codeS
    station_code = odu_station_code) %>%
  dplyr::rename(island = island_name,
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
                -prep_loc_count,
                -accession_number,
                -ezid,
                -other_numbers_type_value,
                -record_last_modified,
                -country,
                -district_county,
                -collection_method,
                -depth_m_min,
                -depth_m_max,
                -depth_cat,
                -odu_station_code,
                -collection_method_manual,
                -smithsonian_station_code,
                -island_grouping,
                -province_code,
                -field_number,
                -island,
                -notes_cas_verification,
                -notes,
                -vegetation,
                -ecol_habitat,
                -catalog_number_usnm,
                -depth_m_si,
                -depth_m_min_si,
                -depth_m_max_si,
                -province_state)





# rm(data_si,
#    data_si_station,
#    data_gis)
