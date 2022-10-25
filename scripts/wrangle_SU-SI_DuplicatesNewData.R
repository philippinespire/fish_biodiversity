#### Initialize ####

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tidyverse)
library(readxl)
library(janitor)
library(purrr)
library(magrittr)
library(measurements)
library(lubridate)

#### USER DEFINED VARIABLES ####
#inFilePath = "../data/SU-SI_Duplicates(1).xlsx"
inFilePath = "../data/SU-SI_Duplicates_20220808.xlsx"
CAS_verified_names = "../data/All_confirmed_names.xlsx"
stationMetaDataFilePath = "../SU/su_station_database_20221024.xlsx"
stationMetaDataSheet = "All_Stations"
numberStations = 24

#### Read in Data ####


data_su <- 
  read_excel(inFilePath,
             na = "NA") %>%
  clean_names() %>%
  mutate(
         # samples_retained = case_when(!is.na(x53) ~ x53,
         #                              TRUE ~ samples_retained),
        specimen_count = case_when(str_detect(specimen_count,
                                              "http") ~ NA_character_,
                                       TRUE ~ specimen_count),
        specimen_count = as.numeric(specimen_count),
        date_collected = ymd(date_collected)) %>%
  # select(-x53) %>%
  remove_empty(which = c("cols")) %>%
  # group_by(catalog_number) %>%
  # filter(n()>1) 
  # distinct(catalog_number, # if we don't do this, get 2 more records. CEB: not anymore
  #          .keep_all = TRUE) %>%
  dplyr::rename(station_code = odu_field_number_s,
                station_code_7879 = usnm_field_number_s) %>% #identification in Smithsonian is updated compared to "other identification"
         # look up Changed to Pleurosicya mossambica in SU-SI - problem
         # Should run column G against E to ensure first words are contained in the other column
         # Combine catalog num and field num in smithsonian?
  # fix the names to verified names
  left_join(read_excel(CAS_verified_names),
            by = c("identification" = "original_id")) %>%
  mutate(verified_identification = case_when(is.na(verified_identification) ~ identification,
                                TRUE ~ verified_identification)) %>%
  dplyr::rename(notes = notes.x,
         notes_cas_verification = notes.y,
         collectors = collector_s,
         ecol_habitat = ecological_habitat,
         dist_shore = distance_from_shore,
         # depth_m = depth_water,
         locality = precise_locality) %>%
  mutate(station_code_7879 = str_replace(station_code_7879,
                                         "-",
                                         "_"),
         # don't need, getting from metadata
         # adjusted_latitude = case_when(is.na(centroid_latitude) ~ as.numeric(conv_unit(dms_latitude,
         #                                                                               from = "deg_min_sec",
         #                                                                               to = "dec_deg")),
         #                               TRUE ~ centroid_latitude),
         # adjusted_longitude = case_when(is.na(centroid_longitude) ~ as.numeric(conv_unit(dms_longitude,
         #                                                                                 from = "deg_min_sec",
         #                                                                                 to = "dec_deg")),
         #                                TRUE ~ centroid_longitude),
         depth_m = str_remove(depth_water,
                              " *ft *"),
         depth_m = str_remove(depth_m,
                              "^[0-9]+\\-"),
         depth_m = as.numeric(depth_m) * 12 * 2.54 / 100) %>%
  dplyr::select(-i_dcheck_2nd,
               -i_dcheck_3rd,
               -i_dcheck_1st,
               -collectors,
               -dms_latitude,
               -dms_longitude,
               -centroid_latitude,
               -centroid_longitude,
               -time_start,
               -time_end,
               -preservation_method,
               -samples_retained,
               -i_dnotes,
               -i_dchange,
               -i_dprevious,
               -country,
               -expedition,
               -collection_method,
               # -station_code_7879,
               -depth_water,
               -lot_id,
               -province_state,
               -municipality,
               -barangay)

# all_spec_ids <- 
#   unique(c(data_su$identification, data_si$identification))
# view(all_spec_ids)
# capture.output(all_spec_ids, file = "all_spec_ids.tsv")
# 
# all_spec_ids <- data.frame(all_spec_ids)
# write_tsv(all_spec_ids, file = "all_spec_ids.tsv")
# filter(x!=y)
# 
# all_spec_ids <- tibble(all_spec_ids) %>%
#   filter(all_spec_ids != "NA") %>%
#   write_tsv(file = "all_spec_ids.tsv")


#### READ IN METADATA ####
data_su_metadata <-
  read_excel(stationMetaDataFilePath,
             sheet = stationMetaDataSheet,
             n_max = numberStations) %>%
  clean_names() %>%
  # remove cols with no data
  dplyr::select(!(where(~ all(is.na(.))))) %>%
  #match formatting from other studies
  mutate(usnm_field_number_s = str_replace(usnm_field_number_s,
                                           "(..)\\-",
                                           "\\1_"),
         #set depth to max
         depth_m = depth_to_m,
         date_collected = ymd(date)) %>%
  rename(province_state = province,
         station_code_7879 = usnm_field_number_s,
         station_code = odu_field_number_s,
         latitude = lat_su,
         longitude = lon_su) %>%
  dplyr::select(-time,
                -date,
                -water_temperature,
                -rotenone,
                -method_capture,
                -depth_from_m,
                -depth_to_m,
                -lat_si_orig,
                -lon_si_orig,
                -count,
                -duplicate_70s,
                -match_id,
                -proxy,
                -notes)

#### COMBINE SURVEY DATA AND STATION METADATA ####

data_su_all <-
  data_su %>%
  left_join(data_su_metadata,
            by = c("station_code")) %>%
  rename(date_collected = date_collected.y,
         locality = locality.y,
         station_code_7879 = station_code_7879.y,
         depth_m = depth_m.y) %>%
  dplyr::select(-contains(".x"),
                -notes_cas_verification,
                -notes)

rm(data_su,
   data_su_metadata)
