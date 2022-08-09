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
inFilePath = "../data/SU-SI_Duplicates(1).xlsx"
inFilePath = "../data/SU-SI_Duplicates_20220808.xlsx"
CAS_verified_names = "../data/All_confirmed_names.xlsx"

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
         depth_m = depth_water,
         locality = precise_locality) %>%
  mutate(adjusted_latitude = case_when(is.na(centroid_latitude) ~ as.numeric(conv_unit(dms_latitude,
                                                                            from = "deg_min_sec",
                                                                            to = "dec_deg")),
                                       TRUE ~ centroid_latitude),
         adjusted_longitude = case_when(is.na(centroid_longitude) ~ as.numeric(conv_unit(dms_longitude,
                                                                              from = "deg_min_sec",
                                                                              to = "dec_deg")),
                                        TRUE ~ centroid_longitude),
         station_code_7879 = str_replace(station_code_7879,
                                         "-",
                                         "_")) %>%
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
         -lot_id)

        
  

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
