#### Initialize ####

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tidyverse)
library(janitor)
library(magrittr)
library(measurements)
library(lubridate)

#### USER DEFINED VARIABLES ####
#inFilePath = "../data/SU-SI_Duplicates(1).xlsx"
inFilePath = "../data/phl_pd_2020_1km_ASCII_XYZ.csv"


#### READ IN DATA ####

# this is all xyz coords with no pop info... 
# need to find other data source
read_csv(inFilePath)
