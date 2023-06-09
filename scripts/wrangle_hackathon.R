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
estimateR_path = "./EstimateR.R"

#### READ IN DATA ####
source(wrangle_si_data_path)
source(wrangle_su_si_data_path)
source(wrangle_cas_data_path)
source(estimateR_path)

#### BIND DATA AMONG STUDIES ####
data_cas_si_su <-
  bind_rows(data_cas_all %>%
              dplyr::mutate(study = "cas_2016"), 
            data_si_station_gis %>%
              dplyr::mutate(study = "si_1978"), 
            data_su_all %>%
              dplyr::mutate(study = "su_2022")) %>%
  dplyr::select(#-family,
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

data_hackathon <- data_cas_si_su %>%
  filter(study %in% c("cas_2016", "su_2022")) %>%
  dplyr::select(station_code, date_collected, latitude, longitude, depth_m, verified_identification, specimen_count)

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
# output_file_path <- "../data/transformed_data.csv"
# write.csv(transformed_data, file = output_file_path, row.names = FALSE)


#### BIND DATA AMONG STUDIES BY FAMILY ####

data_hackathon_fam <- data_cas_si_su %>%
  filter(study %in% c("cas_2016", "su_2022")) %>%
  dplyr::select(station_code, date_collected, latitude, longitude, depth_m, family, verified_identification, specimen_count)

# data_cas_si_su_fam <-
#   bind_rows(data_cas_all %>%
#               dplyr::mutate(study = "cas_2016"), 
#             data_si_station_gis %>%
#               dplyr::mutate(study = "si_1978"), 
#             data_su_all %>%
#               dplyr::mutate(study = "su_2022")) %>%
#   dplyr::select(-identification,
#                 -order,
#                 -ecol_habitat:-catalog_number)
# drop_na() %>%
# group_by(station_code,
#          verified_identification:study) %>%
# summarize(specimen_count = sum(specimen_count))

rm(data_si_station_gis,
   data_su_all,
   data_cas_all)

prep_vegan_fam <- function(data = data_hackathon_fam) {
  data %>%
    filter(specimen_count > 0) %>%
    group_by(station_code, date_collected, latitude, longitude, depth_m, family) %>%
    summarize(sum_specimen_count = sum(specimen_count)) %>%
    ungroup() %>%
    pivot_wider(
      names_from = family,
      values_from = sum_specimen_count,
      values_fill = 0
    ) %>%
    arrange(station_code) %>%
    drop_na(station_code, depth_m)
}


# Call the prep_vegan function to transform data_hackathon
transformed_data_fam <- prep_vegan_fam(data_hackathon_fam)

# remove column of NA that hadn't received a family
transformed_data_fam <- transformed_data_fam %>%
  select(-'NA')

# add s_chao1 column from est_S to dataframe
merged_data <- left_join(transformed_data_fam, est_S[, c("station_code", "s_chao1")], by = "station_code")
merged_data <- merged_data %>%
  dplyr::select(station_code, date_collected, latitude, longitude, depth_m, s_chao1, everything())

# View the transformed data
print(transformed_data_fam)

# write file as a .csv
output_file_path <- "../data/transformed_data_fam.csv"
write.csv(transformed_data_fam, file = output_file_path, row.names = FALSE)

output_file_path <- "../data/transformed_data_fam_chao.csv"
write.csv(merged_data, file = output_file_path, row.names = FALSE)
