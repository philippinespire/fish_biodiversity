#### INITIALIZE ####

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# install.packages("tidyverse")
# install.packages("readxl")

library(tidyverse)
library(readxl)
library(janitor)
library(purrr)
library(magrittr)
library(lubridate)

#### USER DEFINED VARIABLES ####

wrangle_data_path = "./wrangleStationData_SI.R"

#### READ IN DATA ####
source(wrangle_data_path)

#### DATA VISUALIZE ####
data_si_station_gis %>%
  select(station_code,
         starts_with("depth_m_")) %>%
  pivot_longer(cols = depth_m_min:depth_m_max,
               names_to = "depth_cat",
               values_to = "meters") %>%
  distinct() %>%
  
  ggplot(aes(x=station_code,
             y=meters,
             color = depth_cat)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90,
                                   hjust = 1,
                                   vjust = 0.5))

data_si_station_gis %>%
  select(station_code,
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
  select(station_code,
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

#### Make EstimateS File Function ####

# whole data set in 1 file

mk_estimates_file <-
  function(inData,
           dataDir = "../data",
           data_set_name = "SI_78-79_all"){
    
    estimates_file_name = str_c(dataDir,
                                "/",
                                data_set_name,
                                ".tsv",
                                sep = "")
    num_species <-
      inData %>%
      select(verified_identification) %>%
      distinct() %>%
      pull() %>%
      length()
    
    num_sites <-
      inData %>%
      select(station_code) %>%
      distinct() %>%
      pull() %>%
      length()
    
    inData_estimates <-
      inData %>%
        select(station_code, 
               verified_identification,
               specimen_count) %>%
        # group_by(station_code,
        #          verified_identification) %>%
        # summarize(specimen_count = sum(specimen_count)) %>%
        # ungroup() %>%
        pivot_wider(names_from = station_code,
                    values_from = specimen_count,
                    values_fill = 0,
                    values_fn = sum)
    
    column_names <- colnames(inData_estimates)
    
    estimates_inFile <- 
      bind_rows(as_tibble(colnames(inData_estimates)) %>%
                mutate(index=value) %>%
                pivot_wider(names_from = index) %>%
                mutate(across(.cols=everything(),
                              .fns = ~str_replace(.,
                                                  ".*",
                                                  ""))) %>%
                mutate(across(column_names[1],
                              .fns = ~str_c(data_set_name)),
                       across(column_names[2],
                              .fns = ~str_c("*SampleSet*")),
                       across(column_names[3:5],
                              .fns = ~str_c("1"))),
                as_tibble(colnames(inData_estimates)) %>%
                  mutate(index=value) %>%
                  pivot_wider(names_from = index) %>%
                  mutate(across(.cols=everything(),
                                .fns = ~str_replace(.,
                                                   ".*",
                                                   ""))) %>%
                  mutate(across(column_names[1],
                                .fns = ~as.character(num_species)),
                         across(column_names[2],
                                .fns = ~as.character(num_sites))),       
                  # mutate(identification = as.character(num_species), # better to use column num
                  #        `SP_78-37A` = as.character(num_sites)), # bettern to use column num
                as_tibble(colnames(inData_estimates)) %>%
                  mutate(index=value) %>%
                  pivot_wider(names_from = index) %>%
                  mutate(across(column_names[1],
                                .fns = ~str_replace(identification,
                                                    "identification",
                                                    ""))),
                  # mutate(identification = str_replace(identification,
                  #                                     "identification",
                  #                                     "")),
                inData_estimates %>%
                  mutate(across(.cols = everything(),
                                .fns = ~ as.character(.)))) 
    
    write_tsv(estimates_inFile,
              estimates_file_name,
              col_names = FALSE) 
}

#### Make EstimateS File All 78-79 ####
data_si_station_gis %>%
  mk_estimates_file(data_set_name = "SI_78-79_all")

#### Make EstimateS File By Depth 78-79 ####
data_si_station_gis %>%
  filter(depth_cat == "<2m") %>%
  mk_estimates_file(data_set_name = "SI_78-79_0-2m")
data_si_station_gis %>%
  filter(depth_cat == "2-15m") %>%
  mk_estimates_file(data_set_name = "SI_78-79_2-15m")
data_si_station_gis %>%
  filter(depth_cat == ">15m") %>%
  mk_estimates_file(data_set_name = "SI_78-79_15-50m")
