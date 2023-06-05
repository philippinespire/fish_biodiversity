#### INITIALIZE ####

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# library(tidyverse)
# library(readxl)
# library(janitor)
# library(purrr)
# library(magrittr)
# library(lubridate)
# 
# # library(devtools)
# # install_github("decisionpatterns/tidyimpute")
# library(tidyimpute)

#### PACKAGES ####
packages_used <- 
  c("tidyverse",
    "readxl",
    "janitor",
    "purrr",
    "magrittr",
    "lubridate",
    "tidyimpute")

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
inFilePath = "../CAS/CAS fish count by site PH 2016.xlsx"
inFilePath2 = "../CAS/CAS-Fishes-VIP2016-localities.xlsx"

# inFileSheet1 = "Complete List" # this one is missing data

# this sheet seems to have had some species collapsed together.  Not suspicious, probably needed to be done
inFileSheet2 = "Phil_0416"

CAS_verified_names = "../data/All_confirmed_names.xlsx"

#### READ IN COUNT DATA ####

data_cas <-
  readxl::read_excel(inFilePath,
             range="B1:AD490",
             sheet = inFileSheet2) %>%
  janitor::clean_names() %>%
  #dplyr::rename(notes = x1) %>%
  drop_na(family:genus_species) %>%
  pivot_longer(cols = zam_05:mab_356,
               names_to = "station_code",
               values_to = "specimen_count") %>%
  drop_na(specimen_count) %>%
  left_join(readxl::read_excel(CAS_verified_names) %>%
              dplyr::select(-family),
            by = c("genus_species" = "original_id")) %>%
  dplyr::rename(identification = genus_species) %>%
  dplyr::mutate(verified_identification = case_when(is.na(verified_identification) ~ identification,
                                TRUE ~ verified_identification))
  # dplyr::rename(notes = notes.x,
  #               notes_cas_verification = notes.y)


# #### IDENTIFY TAXA TO BE VERIFIED ####

# data_cas %>% # wide version of data_cas
#   select(notes:genus_species) %>%
#   left_join(data_cas_verified_names,
#             by = c("genus_species" = "original_id")) %>%
#   filter(is.na(verified_identification) & is.na(lowest_tax_cat) & is.na(notes.y)) %>%
#   write_tsv("../data/cas2016_taxa_to_verify.tsv")



#### READ IN SITE METADATA ####
metadata_cas <-
  read_excel(inFilePath2) %>%
  clean_names() %>%
  drop_cols_all_na() %>%
  dplyr::mutate(station_code = str_replace(field_number,
                                    "\\-",
                                    "_"),
         station_code = str_to_lower(station_code),
         depth_m = str_remove(depth_of_capture,
                              " *ft\\.* *"),
         depth_m = str_remove(depth_m,
                              "^[0-9]+ *\\- *"),
         depth_m = as.numeric(depth_m) * 12 * 2.54 / 100) 

#### JOIN DATA ####

data_cas_all <-
  data_cas %>%
  left_join(metadata_cas,
            by = "station_code") %>%
  dplyr::rename(bottom = bottom_type,
                # depth_m = depth_of_capture,
                latitude = lat_deg_dec_1,
                longitude = long_deg_dec_1,
                province_state = state,
                date_collected = verbatim_coll_date) %>%
  dplyr::mutate(date_collected = dmy(date_collected),
         gear = str_to_lower(gear)) %>%
  filter(str_detect(gear,
                    "rotenone")) %>%
  dplyr::select(-coll_date_from,
                -source,
                -lat_verbatim_1,
                -long_verbatim_1,
                -country,
                -collector:-continent_ocean,
                -salinity,
                -gear,
                -coll_time:-last_edited_by,
                -notes,
                -depth_of_capture,
                -field_number,
                -island,
                -island_group)
  

rm(data_cas,
   metadata_cas)
