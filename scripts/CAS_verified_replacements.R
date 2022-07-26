#### INITIALIZE ####
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tidyverse)
library(readxl)
library(janitor)

SU_SI_Duplicates <- 
  read_excel("SU-SI_Duplicates(1).xlsx") %>%
  clean_names()

CAS_verified_list <-
  read_excel("All_confirmed_names.xlsx") %>%
  clean_names()

CAS_renamed_list <-
  SU_SI_Duplicates %>%
  left_join(identification,
            by = CAS_verified_list) %>%
  mutate(case_when(lowest_tax = original_id,
                   TRUE ~ something))
