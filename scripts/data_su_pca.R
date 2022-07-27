####Initialize ####
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tidyverse)
library(janitor)
library(readxl)
# library(devtools)
# install_github("vqv/ggbiplot")
library(ggbiplot)

#### Read in Data ####
station_info <-
  read_excel("station_info.xlsx")

CAS_1 <-
  read_excel("CAS-SU_2016-2019_VIS.xlsx",
             skip = 2)

CAS_2 <-
  read_excel("CAS_fish_count_by_site_PH_2016.xlsx",
             sheet = "Complete List")
  

CAS_2016_data <-
  CAS_1 %>%
  full_join(CAS_2,
            by = "genus_species")


#need to combine station_info.xlsx, CAS 2016 data and all confirmed names.xlsx
