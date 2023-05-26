#### INITIALIZATION ####
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tidyverse)
library(janitor)
library(readxl)
# install.packages("maps")
# install.packages("viridis")
require(maps)
require(viridis)
theme_set(
  theme_void()
)
# install.packages("vegan")
# install.packages("ggvegan")
library(vegan)
# install.packages("remotes")
library(remotes)
# remotes::install_github("gavinsimpson/ggvegan")
library(ggvegan)
source("wrangle_cas_si_su_data.R")

data <- data_cas_si_su

#### VEGANIZATION ####
#Vegan all data
prep_vegan <- function(data=data_cas_si_su){
  data %>%
    # filter by lowest_tax_cat (remove family)
    rename(taxon = verified_identification) %>%
    filter(specimen_count > 0) %>%
    group_by(taxon,
             station_code, #add everything but specimen_count
             study,
             field_number,
             lowest_tax_cat,
             depth_m,
             adjusted_latitude,
             adjusted_longitude) %>%
    summarize(sum_specimen_count = sum(specimen_count)) %>%
    ungroup() %>%
    # convert tibble from long to wide format
    pivot_wider(names_from = taxon,
                values_from = sum_specimen_count,
                values_fill = 0) %>%
    clean_names() %>%
    # sort by op_code
    arrange(study,
            station_code) %>%
    drop_na(station_code,
            depth_m,
            adjusted_latitude,
            adjusted_longitude)
}

data_cas_si_su_vegan <-
  prep_vegan() %>%
  dplyr::select(-station_code:-lowest_tax_cat)

#Vegan cas_2016 data
prep_vegan_cas <- function(data=data_cas_si_su){
  data %>%
    dplyr::filter(study == "cas_2016") %>%
    rename(taxon = verified_identification) %>%
    dplyr::filter(specimen_count > 0) %>%
    group_by(taxon,
             station_code, #add everything but specimen_count
             study,
             field_number,
             lowest_tax_cat) %>%
    summarize(sum_specimen_count = sum(specimen_count)) %>%
    ungroup() %>%
    # convert tibble from long to wide format
    pivot_wider(names_from = taxon,
                values_from = sum_specimen_count,
                values_fill = 0) %>%
    clean_names() %>%
    # sort by station_code
    arrange(study,
            station_code) %>%
    drop_na(station_code)
}    

data_casvegan <-
  prep_vegan_cas() %>%
  dplyr::select(-station_code:-lowest_tax_cat)

#vegan SI data
prep_vegan_si <- function(data=data_cas_si_su){
  data %>%
    dplyr::filter(study == "si_1978") %>%
    rename(taxon = verified_identification) %>%
    dplyr::filter(specimen_count > 0) %>%
    group_by(taxon,
             station_code, #add everything but specimen_count
             study,
             field_number,
             lowest_tax_cat) %>%
    summarize(sum_specimen_count = sum(specimen_count)) %>%
    ungroup() %>%
    # convert tibble from long to wide format
    pivot_wider(names_from = taxon,
                values_from = sum_specimen_count,
                values_fill = 0) %>%
    clean_names() %>%
    # sort by station_code
    arrange(study,
            station_code) %>%
    drop_na(station_code)
}    

data_sivegan <-
  prep_vegan_si() %>%
  dplyr::select(-station_code:-lowest_tax_cat)

#vegan su_2022 data
prep_vegan_su <- function(data=data_cas_si_su){
  data %>%
    dplyr::filter(study == "su_2022") %>%
    rename(taxon = verified_identification) %>%
    dplyr::filter(specimen_count > 0) %>%
    group_by(taxon,
             station_code, #add everything but specimen_count
             study,
             field_number,
             lowest_tax_cat) %>%
    summarize(sum_specimen_count = sum(specimen_count)) %>%
    ungroup() %>%
    # convert tibble from long to wide format
    pivot_wider(names_from = taxon,
                values_from = sum_specimen_count,
                values_fill = 0) %>%
    clean_names() %>%
    # sort by station_code
    arrange(study,
            station_code) %>%
    drop_na(station_code)
}    

data_suvegan <-
  prep_vegan_su() %>%
  dplyr::select(-station_code:-lowest_tax_cat)

#### PART 2 ####
#all data
data_cas_si_su_vegan.env <-
  prep_vegan() %>%
  dplyr::select(station_code:lowest_tax_cat)

#cas 2016 data
data_cas_vegan.env <-
  prep_vegan_cas() %>%
  dplyr::select(station_code:lowest_tax_cat)

#si data
data_si_vegan.env <-
  prep_vegan_si() %>%
  dplyr::select(station_code:lowest_tax_cat)

#su data
data_su_vegan.env <-
  prep_vegan_su() %>%
  dplyr::select(station_code:lowest_tax_cat)

#### ATTACH ####
#all data
attach(data_cas_si_su_vegan.env)
data_vegan <- data_cas_si_su_vegan 
data_vegan.env <- data_cas_si_su_vegan.env
attach(data_vegan.env)

#CAS 2016
attach(data_cas_vegan.env)

#si
attach(data_si_vegan.env)

#su
attach(data_su_vegan.env)


#### ADONIS ####
#all data
adonis2(data_vegan ~ depth_m*station_code, #Need to fix depth_m characters
        data = data_vegan.env,
        by = NULL)

adonis2(data_vegan ~ depth_m*station_code,
        data = data_vegan.env,
        by = "terms")

adonis2(data_vegan ~ depth_m*station_code,
        data = data_vegan.env,
        by = "margin")

adonis2(data_vegan ~ depth_m*station_code,
        data = data_vegan.env,
        method = "bray")

adonis2(data_vegan ~ depth_m*station_code,
        data = data_vegan.env,
        method = "euclidean")

adonis2(data_vegan ~ depth_m*station_code,
        data = data_vegan.env,
        na.action = na.omit)

adonis2(data_vegan ~ depth_m*station_code,
        data = data_vegan.env,
        na.action = na.exclude)

adonis2(data_vegan ~ study*bottom,
        data = data_vegan.env,
        strata = station_code)

#cas 2016 data
adonis2(data_casvegan ~ depth_m*station_code, #Need to fix depth_m characters
        data = data_vegan.env,
        by = NULL)

adonis2(data_casvegan ~ depth_m*station_code,
        data = data_vegan.env,
        by = "terms")

adonis2(data_casvegan ~ depth_m*station_code,
        data = data_vegan.env,
        by = "margin")

adonis2(data_casvegan ~ depth_m*station_code,
        data = data_vegan.env,
        method = "bray")

adonis2(data_casvegan ~ depth_m*station_code,
        data = data_vegan.env,
        method = "euclidean")

adonis2(data_casvegan ~ depth_m*station_code,
        data = data_vegan.env,
        na.action = na.omit)

adonis2(data_casvegan ~ depth_m*station_code,
        data = data_vegan.env,
        na.action = na.exclude)

adonis2(data_casvegan ~ study*bottom,
        data = data_vegan.env,
        strata = station_code)

#si data
adonis2(data_sivegan ~ depth_m*station_code, #Need to fix depth_m characters
        data = data_vegan.env,
        by = NULL)

adonis2(data_sivegan ~ depth_m*station_code,
        data = data_vegan.env,
        by = "terms")

adonis2(data_sivegan ~ depth_m*station_code,
        data = data_vegan.env,
        by = "margin")

adonis2(data_sivegan ~ depth_m*station_code,
        data = data_vegan.env,
        method = "bray")

adonis2(data_sivegan ~ depth_m*station_code,
        data = data_vegan.env,
        method = "euclidean")

adonis2(data_sivegan ~ depth_m*station_code,
        data = data_vegan.env,
        na.action = na.omit)

adonis2(data_sivegan ~ depth_m*station_code,
        data = data_vegan.env,
        na.action = na.exclude)

adonis2(data_sivegan ~ study*bottom,
        data = data_vegan.env,
        strata = station_code)

#su data
adonis2(data_suvegan ~ depth_m*station_code, #Need to fix depth_m characters
        data = data_vegan.env,
        by = NULL)

adonis2(data_suvegan ~ depth_m*station_code,
        data = data_vegan.env,
        by = "terms")

adonis2(data_suvegan ~ depth_m*station_code,
        data = data_vegan.env,
        by = "margin")

adonis2(data_suvegan ~ depth_m*station_code,
        data = data_vegan.env,
        method = "bray")

adonis2(data_suvegan ~ depth_m*station_code,
        data = data_vegan.env,
        method = "euclidean")

adonis2(data_suvegan ~ depth_m*station_code,
        data = data_vegan.env,
        na.action = na.omit)

adonis2(data_suvegan ~ depth_m*station_code,
        data = data_vegan.env,
        na.action = na.exclude)

adonis2(data_suvegan ~ study*bottom,
        data = data_vegan.env,
        strata = station_code)