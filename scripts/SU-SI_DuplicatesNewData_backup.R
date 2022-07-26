#### Initialize ####

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tidyverse)
library(readxl)
library(janitor)
library(purrr)
library(magrittr)

#### Read in Data ####
View(data_su)

data_su <- 
  read_excel("../data/SU-SI_Duplicates(1).xlsx") %>%
  clean_names() %>%
  remove_empty(which = c("cols")) %>%
  distinct(catalog_number, # if we don't do this, get 2 more records
           .keep_all = TRUE) %>%
  dplyr::rename(station_code = catalog_number) #identification in Smithsonian is updated compared to "other identification"
         # look up Changed to Pleurosicya mossambica in SU-SI - problem
         # Should run column G against E to ensure first words are contained in the other column
         # Combine catalog num and field num in smithsonian?

#Need to get running list of all unique names in 2016, 19 etc and historical

all_spec_ids <- 
  unique(c(data_su$identification, data_si$identification))
view(all_spec_ids)
capture.output(all_spec_ids, file = "all_spec_ids.tsv")

all_spec_ids <- data.frame(all_spec_ids)
write_tsv(all_spec_ids, file = "all_spec_ids.tsv")
filter(x!=y)


all_spec_ids <- tibble(all_spec_ids) %>%
  filter(all_spec_ids != "NA") %>%
  write_tsv(file = "all_spec_ids.tsv")