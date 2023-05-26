#### INITIALIZE ####
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tidyverse)
library(readxl)
library(janitor)

#### USER DEFINED VARIABLES ####

inFilePath1 = "../data/SU-SI_Duplicates(1).xlsx"
inFilePath2 = "../data/All_confirmed_names.xlsx"

#### Read in Data ####

SU_SI_Duplicates <- 
  read_excel(inFilePath1) %>%
  clean_names()

CAS_verified_list <-
  read_excel(inFilePath2) %>%
  clean_names() %>%
  dplyr::rename(identification = original_id)

CAS_renamed_list <-
  SU_SI_Duplicates %>%
  left_join(CAS_verified_list,
            by = "identification") #still has NA's in $verified_identification
# Need to change names to verified names



# -NO LONGER NECESSARY- 
# %>%
#   mutate(case_when(lowest_tax = original_id,
#                    TRUE ~ something))
