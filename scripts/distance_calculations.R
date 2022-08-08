#### INITIALIZATION ####
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#install.packages("geosphere")
library(geosphere)
library(tidyverse)
library(janitor)
library(readxl)
library(readr)

InFilePath1 = "../data/MPA_coordinates_no_deg.xlsx"
InFilePath2 = "../data/data_cas_si_su.csv"


exceldata <- read_excel(InFilePath1)

exceldata2 <- read_csv(InFilePath2)

list1 <- data.frame(exceldata) %>%
  clean_names() %>%
  select(lat,
         long) %>%
  drop_na(lat,
          long)

list2 <- data.frame(exceldata2) %>%
  clean_names() %>%
  select(adjusted_latitude,
         adjusted_longitude) %>%
  drop_na(adjusted_latitude,
          adjusted_longitude)

# create distance matrix
mat <- distm(list1[,c('long',
                      'lat')], 
             list2[,c('adjusted_longitude',
                      'adjusted_latitude')], 
             fun=distVincentyEllipsoid)

mat <- as.data.frame(mat)

# assign the name to the point in list1 based on shortest distance in the matrix
list1$longitude <- list2$longitude[max.col(-mat)]


write_excel_csv(mat, file = "distance_matrix.csv")
