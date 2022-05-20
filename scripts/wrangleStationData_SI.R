#### INITIALIZE ####

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# install.packages("tidyverse")
# install.packages("readxl")

library(tidyverse)
library(readxl)
library(janitor)
library(purrr)

#### USER DEFINED VARIABLES ####
metadataDir = "../SI/Collections_Data"
metadataPattern = "*.csv"


#### READ IN METADATA ####

metadata <-
  list.files(metadataDir,
             metadataPattern,
             full.names = TRUE) %>%
  map(.,
      ~ read_csv(.x)) %>%
  bind_rows() %>%
  clean_names() %>%
  distinct(catalog_number_usnm, # if we don't do this, get 2 more records
           .keep_all = TRUE)

metadata %>% 
  select(field_number_s) %>%
  unique() %>%
  view()
  
metadata %>% 
  filter(is.na(field_number_s)) %>%
  view()

metadata %>% 
  select(field_number_s,
         date_collected) %>%
  distinct() %>%
  arrange(date_collected,
          field_number_s) %>%
  view()

metadata %>% 
  select(field_number_s,
         date_collected) %>%
  distinct() %>%
  arrange(field_number_s,
          date_collected) %>%
  view()

