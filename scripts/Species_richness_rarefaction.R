#### INTIALIZE ####
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tidyverse)
library(readxl)
library(janitor)
library(purrr)
library(magrittr)

#### READ IN FILES ####
data_vegan <-
  CAS_verified_names %>%
  # make unique taxa
#   mutate(taxon = str_c(family,
#                        genus,
#                        species,
#                        sep = "_")) %>%
#   # sum all max_n counts for a taxon and op_code
#   group_by(taxon,
#            op_code) %>%
#   summarize(sum_max_n = sum(max_n)) %>%
#   ungroup() %>%
#   # convert tibble from long to wide format
#   pivot_wider(names_from = taxon,
#               values_from = sum_max_n,
#               values_fill = 0) %>%
#   # sort by op_code
#   arrange(op_code) %>%
#   # remove the op_code column for vegan
#   dplyr::select(-op_code)
# 
# #### METADATA ####
# metadata_vegan <-
#   data_all %>%
#   # make unique taxa
#   mutate(taxon = str_c(family,
#                        genus,
#                        species,
#                        sep = "_")) %>%
#   # sum all max_n counts for a taxon and op_code
#   group_by(taxon,
#            op_code,
#            site,
#            survey_area,
#            habitat,
#            lat_n,
#            long_e,
#            depth_m,
#            bait_type) %>%
#   summarize(sum_max_n = sum(max_n)) %>%
#   ungroup() %>%
#   # convert tibble from long to wide format
#   pivot_wider(names_from = taxon,
#               values_from = sum_max_n,
#               values_fill = 0) %>%
#   # sort by op_code
#   arrange(op_code) %>%
#   # remove the op_code column for vegan
#   dplyr::select(op_code:bait_type) %>%
#   mutate(site_code = str_remove(op_code,
#                                 "_.*$"))

attach(data_vegan.env)


#### TEST ####
adonis2(data_vegan ~ depth_m*site,
        data = data_vegan.env,
        by = NULL)

adonis2(data_vegan ~ depth_m*site,
        data = data_vegan.env,
        by = "terms")

adonis2(data_vegan ~ depth_m*site,
        data = data_vegan.env,
        by = "margin")

adonis2(data_vegan ~ depth_m*site,
        data = data_vegan.env,
        method = "bray")

adonis2(data_vegan ~ depth_m*site,
        data = data_vegan.env,
        method = "euclidean")

# only non-missing site scores, remove all rows with missing data
adonis2(data_vegan ~ depth_m*site,
        data = data_vegan.env,
        na.action = na.omit)

adonis2(data_vegan ~ depth_m*site,
        data = data_vegan.env,
        na.action = na.exclude)

adonis2(data_vegan ~ bait_type*habitat,
        data = data_vegan.env,
        strata = site)
```