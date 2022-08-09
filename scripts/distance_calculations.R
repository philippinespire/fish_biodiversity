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


data_mpa <- 
  read_excel(InFilePath1,
             na = c("na",
                    "NA")) %>%
  clean_names() %>%
  mutate(year_established_earliest = str_remove(year_established,
                                                "[ (].*$"),
         year_established_earliest = as.numeric(year_established_earliest)) %>%
  select(-x10) %>%
  drop_na(lat,
          long)

data_study_site <- 
  read_csv(InFilePath2) %>%
  clean_names() %>%
  drop_na(adjusted_latitude,
          adjusted_longitude) %>%
  distinct(study,
           station_code,
           .keep_all=TRUE) %>%
  mutate(study_station_code = str_c(study,
                                    station_code,
                                    sep = "-"))

list1 <- data.frame(data_mpa) %>%
  select(lat,
         long) 

list2 <- data.frame(data_study_site) %>%
  select(adjusted_latitude,
         adjusted_longitude) 

# create distance matrix
data_mpa_study_stationcode_distances <- 
  distm(list1[,c('long',
                      'lat')], 
         list2[,c('adjusted_longitude',
                  'adjusted_latitude')], 
         fun=distVincentyEllipsoid) %>%
  as.data.frame(mat) %>%
  rename_with(.cols = starts_with("V"),
              .fn = ~ data_study_site$study_station_code) %>%
  bind_cols(data_mpa)

data_closest_mpa <-
  data_mpa_study_stationcode_distances %>%
  pivot_longer(cols = matches("^[sc][aiu][_s]"),
               names_to = "study_station_code",
               values_to = "distance_m") %>%
  separate(study_station_code,
           into = c("study"),
           remove = FALSE) %>%
  # isolating the closest mpa to each station_code
  group_by(study_station_code) %>%
  filter(distance_m == min(distance_m)) %>%
  # if mpa didn't exist, then se




