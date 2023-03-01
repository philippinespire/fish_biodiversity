#### INITIALIZATION ####
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# source("./wrangle_cas_si_su_data.R")

#install.packages("geosphere")
# library(geosphere)
# library(ggbiplot)
# library(janitor)
# library(readxl)
# library(readr)
# library(tidyverse)

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


#### USER DEFINED VARIABLES ####
InFilePath1 = "../data/MPA_coordinates_no_deg.xlsx"


theme_myfigs <- 
  theme_classic() +
  theme(panel.background = element_rect(fill = 'white', 
                                        color = 'white'),
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(color="grey95", 
                                          size=0.25),
        panel.border = element_blank(),
        axis.text.y = element_text(size = 9, 
                                   color = 'black'),
        axis.text.x = element_text(size = 9, 
                                   color = 'black'),
        # axis.title.x = element_blank(),
        axis.title.y = element_text(size = 10, 
                                    color = 'black'),
        plot.title = element_text(size = 10, 
                                  color = 'black'),
        plot.subtitle = element_text(size = 9, 
                                     color = 'black'),
        plot.caption = element_text(size = 9, 
                                    color = 'black', 
                                    hjust = 0),
        legend.text = element_text(size = 9, 
                                   color = 'black'),
        legend.title = element_text(size = 9, 
                                    color = 'black'),
        legend.background = element_blank(),
        legend.position="right"
  )

#### WRANGLE MPA DATA ####
data_mpa <- 
  read_excel(InFilePath1,
             na = c("na",
                    "NA")) %>%
  clean_names() %>%
  filter(!is.na(lat),
         !is.na(long),
         !is.na(area_ha),
         area_ha > 0) %>%
  dplyr::mutate(year_established_earliest = str_remove(year_established,
                                                       "[ (].*$")) %>%
  dplyr::mutate(year_established_earliest = as.numeric(year_established_earliest)) %>%
  # dplyr::select(-x10) %>%
  drop_na(lat,
          long)

#### WRANGLE STUDY SITE DATA ####

data_study_site <-
  data_cas_si_su %>%
  clean_names() %>%
  drop_na(latitude,
          longitude) %>%
  distinct(study,
           station_code,
           .keep_all=TRUE) %>%
  dplyr::mutate(study_station_code = str_c(study,
                                           station_code,
                                           sep = "-"),
                year_survey = year(date_collected)) %>%
  dplyr::select(-specimen_count:-lowest_tax_cat)


#### WRANGLE DISTANCES BETWEEN ALL STATIONS AND MPAS ####

list_mpa_latlong <- 
  data.frame(data_mpa) %>%
  dplyr::select(lat,
                long) 

list_station_latlong <- 
  data.frame(data_study_site) %>%
  dplyr::select(latitude,
                longitude) 

# create distance matrix
data_mpa_study_stationcode_distances <- 
  distm(list_mpa_latlong[,c('long',
                            'lat')], 
        list_station_latlong[,c('longitude',
                                'latitude')], 
        fun=distVincentyEllipsoid) %>%
  as.data.frame() %>%
  tibble() %>%
  # convert m to km, something wrong here, FIX ME
  dplyr::mutate(across(.cols = everything(),
                       .fns = ~ .x/1000)) %>%
  rename_with(.cols = starts_with("V"),
              .fn = ~ data_study_site$study_station_code) %>%
  bind_cols(data_mpa %>%
              dplyr::rename(mpa_name = name,
                            mpa_lat = lat,
                            mpa_long = long,
                            mpa_location = location,
                            mpa_habitat_full = habitat_full,
                            mpa_year_established = year_established,
                            mpa_area_ha = area_ha,
                            mpa_province = province,
                            mpa_year = year,
                            mpa_year_established_earliest = year_established_earliest))


#### Visualize MPA vs Stations ####
data_mpa_study_stationcode_distances %>%
  pivot_longer(cols = matches("^[sc][aiu][_s]"),
               names_to = "study_station_code",
               values_to = "distance_closest_mpa_km") %>%
  left_join(data_study_site,
            by = "study_station_code") %>%
  group_by(study_station_code) %>%
  #in the next line, the 3 signifies 3 years since mpa established
  filter(year_survey >= mpa_year_established_earliest + 3) %>%
  ggplot(aes(x = distance_closest_mpa_km,
             color = study)) +
  geom_histogram() +
  theme_classic() +
  facet_wrap(. ~ study,
             scales = "free_y")

data_mpa_study_stationcode_distances %>%
  pivot_longer(cols = matches("^[sc][aiu][_s]"),
               names_to = "study_station_code",
               values_to = "distance_closest_mpa_km") %>%
  left_join(data_study_site,
            by = "study_station_code") %>%
  group_by(study_station_code) %>%
  #in the next line, the 3 signifies 3 years since mpa established
  filter(year_survey >= mpa_year_established_earliest + 3) %>%
  ggplot(aes(x = mpa_area_ha,
             color = study)) +
  geom_histogram() +
  theme_classic() +
  facet_wrap(. ~ study,
             scales = "free_y")

data_mpa_study_stationcode_distances %>%
  pivot_longer(cols = matches("^[sc][aiu][_s]"),
               names_to = "study_station_code",
               values_to = "distance_closest_mpa_km") %>%
  left_join(data_study_site,
            by = "study_station_code") %>%
  group_by(study_station_code) %>%
  #in the next line, the 3 signifies 3 years since mpa established
  filter(year_survey >= mpa_year_established_earliest + 3) %>%
  ggplot(aes(x = distance_closest_mpa_km,
             y = mpa_area_ha,
             color = study)) +
  geom_point() +
  theme_classic()

data_mpa_study_stationcode_distances %>%
  pivot_longer(cols = matches("^[sc][aiu][_s]"),
               names_to = "study_station_code",
               values_to = "distance_closest_mpa_km") %>%
  left_join(data_study_site,
            by = "study_station_code") %>%
  group_by(study_station_code) %>%
  #in the next line, the 3 signifies 3 years since mpa established
  filter(year_survey >= mpa_year_established_earliest + 3) %>%
  dplyr::mutate(mpa_area_ha_per_kmfromstation = mpa_area_ha/distance_closest_mpa_km) %>%
  ggplot(aes(x = station_code,
             y = mpa_area_ha_per_kmfromstation,
             color = study)) +
  geom_boxplot() +
  scale_y_continuous(trans='log10') +
  theme_classic() +
  facet_grid(.~study,
             scales = "free_x")

data_mpa_study_stationcode_distances %>%
  pivot_longer(cols = matches("^[sc][aiu][_s]"),
               names_to = "study_station_code",
               values_to = "distance_closest_mpa_km") %>%
  left_join(data_study_site,
            by = "study_station_code") %>%
  group_by(study_station_code) %>%
  #in the next line, the 3 signifies 3 years since mpa established
  filter(year_survey >= mpa_year_established_earliest + 3) %>%
  dplyr::mutate(mpa_haXyrs_per_kmfromstation = (year_survey - mpa_year_established_earliest) * mpa_area_ha/distance_closest_mpa_km) %>%
  ggplot(aes(x = station_code,
             y = mpa_haXyrs_per_kmfromstation,
             color = study)) +
  geom_boxplot() +
  scale_y_continuous(trans='log10') +
  theme_classic() +
  facet_grid(.~study,
             scales = "free_x")

data_mpa_study_stationcode_distances %>%
  pivot_longer(cols = matches("^[sc][aiu][_s]"),
               names_to = "study_station_code",
               values_to = "distance_closest_mpa_km") %>%
  left_join(data_study_site,
            by = "study_station_code") %>%
  group_by(station_code,
           study,
           study_station_code) %>%
  #in the next line, the 3 signifies 3 years since mpa established
  filter(year_survey >= mpa_year_established_earliest + 3) %>%
  dplyr::mutate(mpa_haXyrs_per_kmfromstation = (year_survey - mpa_year_established_earliest) * mpa_area_ha/distance_closest_mpa_km) %>%
  dplyr::summarize(mpa_haXyrs_per_kmfromstation = sum(mpa_haXyrs_per_kmfromstation,
                                                      na.rm=TRUE)) %>%
  ggplot(aes(x = station_code,
             y = mpa_haXyrs_per_kmfromstation,
             color = study)) +
  geom_col() +
  theme_classic() +
  facet_grid(.~study,
             scales = "free_x")

x_km = 80
data_mpa_study_stationcode_distances %>%
  pivot_longer(cols = matches("^[sc][aiu][_s]"),
               names_to = "study_station_code",
               values_to = "distance_closest_mpa_km") %>%
  left_join(data_study_site,
            by = "study_station_code") %>%
  group_by(station_code,
           study,
           study_station_code) %>%
  #in the next line, the 3 signifies 3 years since mpa established
  filter(year_survey >= mpa_year_established_earliest + 3) %>%
  filter(distance_closest_mpa_km < x_km)  %>%
  dplyr::mutate(mpa_haXyrs_per_kmfromstation = (year_survey - mpa_year_established_earliest) * mpa_area_ha/distance_closest_mpa_km) %>%
  dplyr::summarize(sum_mpa_area_ha_within_x_km = sum(mpa_area_ha,
                                                     na.rm=TRUE)) %>%
  ggplot(aes(x = station_code,
             y = sum_mpa_area_ha_within_x_km,
             color = study)) +
  geom_col() +
  theme_classic() +
  facet_grid(.~study,
             scales = "free_x")

#### GET DISTANCE TO CLOSEST MPA ####

data_mpa_closest <-
  data_mpa_study_stationcode_distances %>%
  pivot_longer(cols = matches("^[sc][aiu][_s]"),
               names_to = "study_station_code",
               values_to = "distance_closest_mpa_km") %>%
  left_join(data_study_site,
            by = "study_station_code") %>%
  # separate(study_station_code,
  #          into = c("study"),
  #          remove = FALSE) %>%
  # isolating the closest mpa to each station_code
  group_by(study_station_code) %>%
  #in the next line, the 3 signifies 3 years since mpa established
  filter(year_survey >= mpa_year_established_earliest + 3) %>%
  filter(distance_closest_mpa_km == min(distance_closest_mpa_km))  %>%
  ungroup() %>%
  dplyr::mutate(age_closest_mpa_y = year_survey - mpa_year_established_earliest,
                study = factor(study)) %>%
  dplyr::rename(area_closest_mpa_ha = mpa_area_ha)


#### GET MPA AREA WITHIN X KM ####

x_km = 80
data_mpa_within_xkm <-
  data_mpa_study_stationcode_distances %>%
  pivot_longer(cols = matches("^[sc][aiu][_s]"),
               names_to = "study_station_code",
               values_to = "distance_closest_mpa_km") %>%
  left_join(data_study_site,
            by = "study_station_code") %>%
  dplyr::mutate(mpa_age_during_study_yrs = year_survey - mpa_year_established_earliest) %>%
  group_by(study_station_code) %>%
  #in the next line, the 3 signifies 3 years since mpa established
  dplyr::filter(year_survey >= mpa_year_established_earliest + 3) %>%
  # retain mpa within x km of station
  dplyr::filter(distance_closest_mpa_km < x_km)  %>%
  # sum mpa area within x km
  dplyr::summarize(mpa_area_within_xkm_ha = sum(mpa_area_ha),
                   mpa_num_within_xkm = n(),
                   mpa_meanage_within_xkm = mean(mpa_age_during_study_yrs,
                                                 na.rm = TRUE),
                   mpa_meandist_within_xkm = mean(distance_closest_mpa_km)) %>%
  ungroup()

data_mpa_stations <-
  data_mpa_closest %>%
  left_join(data_mpa_within_xkm) %>%
  dplyr::mutate(across(.cols = contains("within_xkm"),
                       .fns = ~ replace_na(.x,0)))

#### VISUALIZE AREA of and DIST TO CLOSEST MPA ####
data_mpa_closest %>%
  ggplot(aes(x=distance_closest_mpa_km,
             y = area_closest_mpa_ha,
             shape = study,
             color = mpa_name)) +
  geom_point() +
  theme_classic()

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
