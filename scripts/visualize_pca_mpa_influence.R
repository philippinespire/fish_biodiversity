#### INITIALIZATION ####
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("./distance_calculations_mpa.R")
# xkm is defined in USER DEFINED VARIABLES of this script

#### PACKAGES ####
packages_used <- 
  c("geosphere",
    "ggbiplot",
    "janitor",
    "readxl",
    "readr",
    "tidyverse")

packages_to_install <- 
  packages_used[!packages_used %in% installed.packages()[,1]]

if (length(packages_to_install) > 0) {
  install.packages(packages_to_install, 
                   Ncpus = Sys.getenv("NUMBER_OF_PROCESSORS") - 1)
}

lapply(packages_used, 
       require, 
       character.only = TRUE)


#### PCA MPA INFLUENCE - 3 Factor Closest MPA ####
pca_mpa_influence <-
  data_mpa_closest %>%
  # filter(study != "si") %>%
  dplyr::select(distance_closest_mpa_km,
                area_closest_mpa_ha,
                age_closest_mpa_y) %>% 
  prcomp(center = TRUE,
         scale. = TRUE)

summary(pca_mpa_influence)

ggbiplot(pca_mpa_influence,
         ellipse=TRUE,
         ellipse.prob = .5,
         groups = data_mpa_closest %>%
           pull(study)) +
  theme_classic() +
  theme(aspect.ratio=3/4)


# pca_mpa_influence$x
# 
# data_mpa_closest <-
#   data_mpa_closest %>%
#   bind_cols(pca_mpa_influence$x) %>%
#   rename(pc1_mpa_infl = PC1)



#### PCA MPA INFLUENCE - 2 Factor Closest MPA ####

pca_mpa_influence <-
  data_mpa_closest %>%
  # filter(study != "si") %>%
  # dplyr::mutate(inv_distance_closest_mpa_km = 1/distance_closest_mpa_km) %>%
  dplyr::select(area_closest_mpa_ha,
                age_closest_mpa_y) %>%
  prcomp(center = TRUE,
         scale. = TRUE)

ggbiplot(pca_mpa_influence,
         ellipse=TRUE,
         ellipse.prob = .5,
         groups = data_mpa_closest %>%
           pull(study)) +
  theme_classic() +
  theme_myfigs +
  theme(aspect.ratio=3/4)

# as_tibble(pca_mpa_influence$x) %>%
#   ggplot() +
#   aes(x=PC1,
#       y=PC2,
#       color = data_mpa_closest %>%
#         pull(study)) +
#   geom_point()

data_mpa_closest_pc <-
  data_mpa_closest %>%
  bind_cols(pca_mpa_influence$x) %>%
  dplyr::rename(pc1_mpa_infl = PC1)


#### PCA MPA INFLUENCE - 4 Factor MPA Within Xkm####

pca_mpa_influence <-
  data_mpa_stations %>%
  # filter(study != "si") %>%
  # dplyr::mutate(inv_distance_closest_mpa_km = 1/distance_closest_mpa_km) %>%
  dplyr::select(mpa_area_within_xkm_ha,
                mpa_num_within_xkm,
                mpa_meanage_within_xkm,
                mpa_meandist_within_xkm) %>%
  prcomp(center = TRUE,
         scale. = TRUE)

summary(pca_mpa_influence)

ggbiplot(pca_mpa_influence,
         ellipse=TRUE,
         ellipse.prob = .5,
         groups = data_mpa_stations %>%
           pull(study)) +
  theme_classic() +
  theme_myfigs +
  theme(aspect.ratio=3/4)

data_mpa_stations_pc <-
  data_mpa_stations %>%
  bind_cols(pca_mpa_influence$x) %>%
  dplyr::rename(pc1_mpa_infl = PC1,
                pc2_mpa_infl = PC2,
                pc3_mpa_infl = PC3)


#### PCA MPA INFLUENCE - 6 Factor Closest MPA & MPA Within Xkm####

pca_mpa_influence <-
  data_mpa_stations %>%
  # filter(study != "si") %>%
  # dplyr::mutate(inv_distance_closest_mpa_km = 1/distance_closest_mpa_km) %>%
  dplyr::select(distance_closest_mpa_km,
                area_closest_mpa_ha,
                age_closest_mpa_y,
                mpa_area_within_xkm_ha,
                mpa_num_within_xkm,
                mpa_meanage_within_xkm) %>%
  prcomp(center = TRUE,
         scale. = TRUE)

summary(pca_mpa_influence)

ggbiplot(pca_mpa_influence,
         ellipse=TRUE,
         ellipse.prob = .5,
         groups = data_mpa_stations %>%
           pull(study)) +
  theme_classic() +
  theme_myfigs +
  theme(aspect.ratio=3/4)

# as_tibble(pca_mpa_influence$x) %>%
#   ggplot() +
#   aes(x=PC1,
#       y=PC2,
#       color = data_mpa_closest %>%
#         pull(study)) +
#   geom_point()

# data_mpa_stations_pc <-
#   data_mpa_stations %>%
#   bind_cols(pca_mpa_influence$x) %>%
#   dplyr::rename(pc1_mpa_infl = PC1,
#                 pc2_mpa_infl = PC2,
#                 pc3_mpa_infl = PC3,
#                 pc4_mpa_infl = PC4)

#### CLEAR UNNEEDED TIBBLES ####

rm(
  # data_mpa,
  data_mpa_study_stationcode_distances,
  list_mpa_latlong,
  list_station_latlong,
  pca_mpa_influence,
  theme_myfigs)
