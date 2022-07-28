####Initialize ####
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tidyverse)
library(janitor)
library(readxl)
# library(devtools)
# install_github("vqv/ggbiplot")
library(ggbiplot)

#### User Defined Variables ####
inFilePath1 = "../CAS/CAS-SU_2016-2019_VIS.xlsx"
inFilePath2 = "../CAS/CAS fish count by site PH 2016.xlsx"

#### Read in Data ####
CAS_1 <-
  read_excel(inFilePath1,
             skip = 2) %>%
  clean_names()

CAS_2 <-
  read_excel(inFilePath2,
             sheet = "Phil_0416") %>%
  clean_names()
  

CAS_2016_data <-
  CAS_1 %>%
  full_join(CAS_2,
            by = "genus_species") %>%
  rename("identification" = "genus_species")

#need to combine station_info.xlsx, CAS 2016 data and all confirmed names.xlsx
