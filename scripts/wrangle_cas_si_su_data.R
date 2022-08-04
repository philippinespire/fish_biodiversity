#### INITIALIZE ####

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# install.packages("tidyverse")
# install.packages("readxl")

library(tidyverse)
library(readxl)
library(janitor)
library(purrr)
library(magrittr)
library(lubridate)

#### USER DEFINED VARIABLES ####

wrangle_si_data_path = "./wrangleStationData_SI.R"
wrangle_su_si_data_path = "./wrangle_SU-SI_DuplicatesNewData.R"
wrangle_cas_data_path = "./wrangle_cas_data.R"

#### READ IN DATA ####
source(wrangle_si_data_path)
source(wrangle_su_si_data_path)
source(wrangle_cas_data_path)



data_cas_si_su <-
  bind_rows(data_cas_all %>%
              mutate(study = "cas_2016"), 
            data_si_station_gis %>%
              mutate(study = "si_1978"), 
            data_su %>%
              mutate(study = "su_2022")) %>%
  select(-family,
         -identification,
         -notes,
         -notes_cas_verification,
         -catalog_number,
         -order:-date_collected,
         -ecol_habitat:-barangay)
  # drop_na() %>%
  # group_by(station_code,
  #          verified_identification:study) %>%
  # summarize(specimen_count = sum(specimen_count))


  
