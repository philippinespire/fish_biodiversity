#### Initialize ####

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(rstudioapi)
library(tidyverse)
library(readxl)
library(janitor)
library(purrr)
library(magrittr)

#### Read in Data ####

read_excel("SU-SI_Duplicates(1).xlsx")

tibble$columnname