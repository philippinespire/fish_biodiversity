#### INITIALIZE ####

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# install.packages("tidyverse")
# install.packages("readxl")

library(tidyverse)
library(readxl)
library(janitor)
library(purrr)

#### USER DEFINED VARIABLES ####
querydataDir = "../SI/Collections_Data"
querydataPattern = "*.csv"
gisDataFile = "../SI/Coordinates/Coordinate_Conversions.xlsx"
siteMetaDataFile = "../data/station_info.xlsx"
dataDir = "../data"


dataDir = str_replace(dataDir,
                      "\\/$",
                      "")

#### READ IN COUNT DATA ####

data_si <-
  list.files(querydataDir,
             querydataPattern,
             full.names = TRUE) %>%
  map(.,
      ~ read_csv(.x)) %>%
  bind_rows() %>%
  clean_names() %>%
  remove_empty(which = c("cols")) %>%
  distinct(catalog_number_usnm, # if we don't do this, get 2 more records
           .keep_all = TRUE) %>%
  rename(prep_loc_count = preparation_details_preparation_location_count,
         field_number = field_number_s,
         collectors = collector_s) %>%
  mutate(odu_station_code = str_replace(field_number,
                                        " ",
                                        "_"),
         collection_method = str_to_lower(collection_method),
         depth_m_min = as.numeric(str_remove(depth_m,
                                             " .*$")),
         depth_m_max = as.numeric(str_remove(depth_m,
                                             "^.* ")),
         depth_cat = case_when(depth_m_max < 2 ~ "<2m",
                               depth_m_max <= 15 ~ "2-15m",
                               depth_m_max >15 ~ ">15m",
                               TRUE ~ NA_character_),
         depth_cat = factor(depth_cat,
                            levels = c("<2m",
                                       "2-15m",
                                       ">15m"))) %>%
  filter(kind_of_object != "Image",
         date_collected != "4 Dec 1967 (1967 Dec 04 - 0000 00 00; 14:15 - 15:15)",
         !is.na(field_number))

# data_si %>% filter(is.na(depth_m_min)) %>% view()
# data_si %>% filter(is.na(depth_m_max)) %>% view()
# data_si %>% filter(is.na(depth_m_min), is.na(depth_m_max)) %>% view()

# 9 out of 9 "min/max depth = NA" were due to the cells being empty
#### CREATE STATION METADATA ####
# data_si %>%
#   select(date_collected,
#          ocean,
#          sea_gulf,
#          archipelago,
#          island_grouping,
#          island_name,
#          country,
#          province_state,
#          district_county,
#          precise_locality,
#          starts_with("centroid"),
#          collectors,
#          field_number,
#          vessel,
#          cruise,
#          station,
#          expedition,
#          collection_method,
#          depth_m,
#          odu_station_code,
#          depth_m_min,
#          depth_m_max) %>%
#   distinct() %>%
  # write_csv(str_c(dataDir,
  #                 "station_info.csv",
  #                 sep = "/"))

#### JOIN FIELD DATA RECORDS WITH data_si ####
data_si_station <- 
  data_si %>%
    left_join(read_excel(siteMetaDataFile,
                         na = c("NA",
                                "",
                                "na")) %>%
                clean_names()) %>%
    mutate(dist_shore_m_min = case_when(str_detect(dist_shore,
                                                   "\\'") ~ as.numeric(str_remove(dist_shore,
                                                                                  " .*$")) * 0.3048,
                                        str_detect(dist_shore,
                                                   "m *$") ~ as.numeric(str_remove(dist_shore,
                                                                                   " .*$")) * 1,
                                        str_detect(dist_shore,
                                                   "yds") ~ as.numeric(str_remove(dist_shore,
                                                                                  " .*$")) * 0.9144,
                                        str_detect(dist_shore,
                                                   "mi") ~ as.numeric(str_remove(dist_shore,
                                                                                 " .*$")) * 1609.344),
           ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
           ## figure out how to isolate max dist
           ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
           # dist_shore_m_max = case_when(str_detect(dist_shore,
           #                                         "\\'") ~ as.numeric(str_replace(dist_shore,
           #                                                                        "^.* \\- *")) * 0.3048,
           #                              str_detect(dist_shore,
           #                                         "m *$") ~ as.numeric(str_remove(dist_shore,
           #                                                                         " .*$")) * 1,
           #                              str_detect(dist_shore,
           #                                         "yds") ~ as.numeric(str_remove(dist_shore,
           #                                                                        " .*$")) * 0.9144,
           #                              str_detect(dist_shore,
           #                                         "mi") ~ as.numeric(str_remove(dist_shore,
           #                                                                       " .*$")) * 1609.344)
           
           collection_method_manual = str_to_lower(collection_method_manual),
           chemical_euthanasia = case_when(str_detect(collection_method_type,
                                                      "rotenone") ~ "yes",
                                           str_detect(collection_method_manual,  #remove if field data record overrules the si database
                                                      "rotenone") ~ "yes",
                                           !is.na(collection_method_type) ~ "no",
                                           TRUE ~ NA_character_)) %>%
  
  filter(chemical_euthanasia == "yes")


#### READ IN GIS DATA ####
data_gis <-
  read_excel(gisDataFile) %>%
  clean_names() %>%
  select(odu_station_code:smithsonian_station_code,
         starts_with("adjusted_"),
         -starts_with("x"))

#### JOIN DATA ####
data_si_station_gis <-
  data_si_station %>%
  left_join(data_gis,
            by = "odu_station_code")

rm(data_si,
   data_si_station,
   data_gis)

#### DATA VISUALIZE ####
data_si_station_gis %>%
  select(odu_station_code,
         starts_with("depth_m_")) %>%
  pivot_longer(cols = depth_m_min:depth_m_max,
               names_to = "depth_cat",
               values_to = "meters") %>%
  distinct() %>%
  
  ggplot(aes(x=odu_station_code,
             y=meters,
             color = depth_cat)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90,
                                   hjust = 1,
                                   vjust = 0.5))

data_si_station_gis %>%
  select(odu_station_code,
         starts_with("depth_m_max")) %>%

  distinct() %>%
  
  ggplot(aes(x=depth_m_max)) +
  geom_histogram(bins = 43) +
  theme_classic()


data_si_station_gis %>%
  mutate(depth_cat = case_when(depth_m_max < 2 ~ "<2m",
                               depth_m_max <= 15 ~ "2-15m",
                               depth_m_max >15 ~ ">15m",
                               TRUE ~ NA_character_),
         depth_cat = factor(depth_cat,
                            levels = c("<2m",
                                       "2-15m",
                                       ">15m"))) %>%
  filter(!is.na(depth_cat)) %>%
  select(odu_station_code,
         starts_with("depth_cat")) %>%
  distinct() %>%
  ggplot(aes(x=depth_cat)) +
  geom_bar() +
  theme_classic()
# #### DATA CHECKING ####
# data_si %>% 
#   select(field_number_s) %>%
#   unique() %>%
#   view()
#   
# data_si %>% 
#   filter(is.na(field_number_s)) %>%
#   view()
# 
# data_si %>% 
#   select(field_number_s,
#          date_collected) %>%
#   distinct() %>%
#   arrange(date_collected,
#           field_number_s) %>%
#   view()
# 
# data_si %>% 
#   select(field_number_s,
#          date_collected) %>%
#   distinct() %>%
#   arrange(field_number_s,
#           date_collected) %>%
#   view()
# 

#### Make EstimateS Files ####

# whole data set in 1 file
data_set_name = "SI_78-79_all"
estimates_file_name = str_c(dataDir,
                            "/",
                            data_set_name,
                            ".tsv",
                            sep = "")

num_species <-
  data_si_station_gis %>%
  select(identification) %>%
  distinct() %>%
  pull() %>%
  length()

num_sites <-
  data_si_station_gis %>%
  select(odu_station_code) %>%
  distinct() %>%
  pull() %>%
  length()

data_si_station_gis_estimates <-
  data_si_station_gis %>%
    select(odu_station_code, 
           identification,
           specimen_count) %>%
    # group_by(odu_station_code,
    #          identification) %>%
    # summarize(specimen_count = sum(specimen_count)) %>%
    # ungroup() %>%
    pivot_wider(names_from = odu_station_code,
                values_from = specimen_count,
                values_fill = 0,
                values_fn = sum)


estimates_inFile <- 
  bind_rows(as_tibble(colnames(data_si_station_gis_estimates)) %>%
              mutate(index=value) %>%
              pivot_wider(names_from = index) %>%
              mutate(across(.cols=everything(),
                            .fns = ~str_replace(.,
                                                ".*",
                                                ""))) %>%
              mutate(identification = data_set_name, # better to use column num
                     `SP_78-37A` = "*SampleSet*",
                     `SP_78-22` = "1",
                     `SP_78-1` = "1",
                     `SP_78-10` = "1"),
            as_tibble(colnames(data_si_station_gis_estimates)) %>%
              mutate(index=value) %>%
              pivot_wider(names_from = index) %>%
              mutate(across(.cols=everything(),
                            .fns = ~str_replace(.,
                                               ".*",
                                               ""))) %>%
              mutate(identification = as.character(num_species), # better to use column num
                     `SP_78-37A` = as.character(num_sites)), # bettern to use column num
            as_tibble(colnames(data_si_station_gis_estimates)) %>%
              mutate(index=value) %>%
              pivot_wider(names_from = index) %>%
              mutate(identification = str_replace(identification,
                                                  "identification",
                                                  "")),
            data_si_station_gis_estimates %>%
              mutate(across(.cols = everything(),
                            .fns = ~ as.character(.)))) 

write_tsv(estimates_inFile,
          estimates_file_name,
          col_names = FALSE)  
