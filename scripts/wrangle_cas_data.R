#### INITIALIZE ####

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tidyverse)
library(readxl)
library(janitor)
library(purrr)
library(magrittr)

# library(devtools)
# install_github("decisionpatterns/tidyimpute")

library(tidyimpute)

#### USER DEFINED VARIABLES ####
inFilePath = "../CAS/CAS fish count by site PH 2016.xlsx"
inFilePath2 = "../CAS/CAS-Fishes-VIP2016-localities.xlsx"

# inFileSheet1 = "Complete List" # this one is missing data

# this sheet seems to have had some species collapsed together.  Not suspicious, probably needed to be done
inFileSheet2 = "Phil_0416"

CAS_verified_names = "../data/All_confirmed_names.xlsx"

#### READ IN COUNT DATA ####

data_cas <-
  read_excel(inFilePath,
             range="A1:AD490",
             sheet = inFileSheet2) %>%
  clean_names() %>%
  rename(notes = x1) %>%
  drop_na(family:genus_species) %>%
  pivot_longer(cols = zam_05:mab_356,
               names_to = "station_code",
               values_to = "specimen_count") %>%
  drop_na(specimen_count) %>%
  left_join(read_excel(CAS_verified_names),
            by = c("genus_species" = "original_id")) %>%
  mutate(verified_identification = case_when(is.na(verified_identification) ~ genus_species,
                                TRUE ~ verified_identification)) %>%
  rename(notes = notes.x,
         notes_cas_verification = notes.y)


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
  mutate(station_code = str_replace(field_number,
                                    "\\-",
                                    "_"),
         station_code = str_to_lower(station_code))

#### JOIN DATA ####

data_cas_all <-
  data_cas %>%
  left_join(metadata_cas,
            by = "station_code") #%>%
  # rebecca, you need to use rename to make these column names align across the data sets


