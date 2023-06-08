#### INITIALIZE ####

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#### PACKAGES ####
packages_used <- 
  c("tidyverse",
    "readxl",
    "janitor",
    "purrr",
    "magrittr",
    "measurements",
    "lubridate",
    "readr",
    "devtools",
    "maptools",
    #"tidyimpute",
    "taxize")

packages_to_install <- 
  packages_used[!packages_used %in% installed.packages()[,1]]

if (length(packages_to_install) > 0) {
  install.packages(packages_to_install, 
                   Ncpus = Sys.getenv("NUMBER_OF_PROCESSORS") - 1)
}

lapply(packages_used, 
       require, 
       character.only = TRUE)

#### USER DEFINED VARIABLES ####

wrangle_si_data_path = "./wrangleStationData_SI.R"
wrangle_su_si_data_path = "./wrangle_SU-SI_DuplicatesNewData.R"
wrangle_cas_data_path = "./wrangle_cas_data.R"
calculate_mpa_distances = "./distance_calculations_mpa.R"
visualize_pca_path = "./visualize_pca_mpa_influence.R"
wrangle_arcgis_path = "./wrangle_arcgis.R"

#### READ IN DATA ####
source(wrangle_si_data_path)
source(wrangle_su_si_data_path)
source(wrangle_cas_data_path)

#### BIND DATA AMONG STUDIES ####
data_cas_si_su <-
  bind_rows(data_cas_all %>%
              dplyr::mutate(study = "cas_2016"), 
            data_si_station_gis %>%
              dplyr::mutate(study = "si_1978"), 
            data_su_all %>%
              dplyr::mutate(study = "su_2022")) %>%
  dplyr::select(-family,
                -identification,
                -order,
                -ecol_habitat:-catalog_number)
# drop_na() %>%
# group_by(station_code,
#          verified_identification:study) %>%
# summarize(specimen_count = sum(specimen_count))

rm(data_si_station_gis,
   data_su_all,
   data_cas_all)


#### Hackathon ######

# data_hackathon <- data_cas_si_su %>%
#   filter(study %in% c("cas_2016", "su_2022")) %>%
#   dplyr::select(station_code, date_collected, latitude, longitude, depth_m, verified_identification, specimen_count)
# 
# prep_vegan <-
#   function(data=hackathon){
#     data %>%
#       filter(specimen_count > 0) %>%
#       group_by(verified_identification,
#                station_code,
#       ) %>%
#       dplyr::summarize(sum_specimen_count = sum(specimen_count)) %>%
#       ungroup() %>%
#       pivot_wider(names_from = verified_identification,
#                   values_from = sum_specimen_count,
#                   values_fill = 0) %>%
#       clean_names() %>%
#       arrange(station_code) %>%
#       drop_na(station_code)
#   }

####
prep_vegan <- function(data = hackathon) {
  data %>%
    filter(specimen_count > 0) %>%
    group_by(verified_identification, station_code, date_collected, latitude, longitude, depth_m) %>%
    dplyr::summarize(sum_specimen_count = sum(specimen_count)) %>%
    ungroup() %>%
    pivot_wider(
      names_from = verified_identification,
      values_from = sum_specimen_count,
      values_fill = 0
    ) %>%
    clean_names() %>%
    arrange(station_code) %>%
    drop_na(station_code, depth_m)
}
####


# Call the prep_vegan function to transform data_hackathon
transformed_data <- prep_vegan(data_hackathon)

# View the transformed data
print(transformed_data)

# write file as a .csv
output_file_path <- "../data/transformed_data.csv"
write.csv(transformed_data, file = output_file_path, row.names = FALSE)
