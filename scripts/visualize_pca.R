
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#### USER DEFINED VARIABLES ####
source("wrangle_arcgis.R")
source("distance_calculations_mpa.R")
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
  theme(aspect.ratio=3/4) +
  labs(title = "PCA w/ MPA Stats")

ggbiplot(pca_mpa_influence,
         choices=2:3,
         ellipse=TRUE,
         ellipse.prob = .5,
         groups = data_mpa_stations %>%
           pull(study)) +
  theme_classic() +
  theme_myfigs +
  theme(aspect.ratio=3/4) +
  labs(title = "PCA w/ MPA Stats")

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
  theme(aspect.ratio=3/4)  +
  labs(title = "PCA w/ MPA Stats")

ggbiplot(pca_mpa_influence,
         choices=2:3,
         ellipse=TRUE,
         ellipse.prob = .5,
         groups = data_mpa_stations %>%
           pull(study)) +
  theme_classic() +
  theme_myfigs +
  theme(aspect.ratio=3/4) +
  labs(title = "PCA w/ MPA Stats")


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

#### PCA MPA INFLUENCE - 8 Factor Closest MPA & MPA Within Xkm & Province Pop Stats####

pca_mpa_influence <-
  data_mpa_stations %>%
  left_join(data_human_pop) %>%
  # filter(study != "si") %>%
  # dplyr::mutate(inv_distance_closest_mpa_km = 1/distance_closest_mpa_km) %>%
  dplyr::select(distance_closest_mpa_km,
                area_closest_mpa_ha,
                age_closest_mpa_y,
                mpa_area_within_xkm_ha,
                mpa_num_within_xkm,
                mpa_meanage_within_xkm,
                population,
                pop_dens_province) %>%
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
  theme(aspect.ratio=3/4) +
  labs(title = "PCA w/ MPA & Province Pop Stats")

ggbiplot(pca_mpa_influence,
         choices=2:3,
         ellipse=TRUE,
         ellipse.prob = .5,
         groups = data_mpa_stations %>%
           pull(study)) +
  theme_classic() +
  theme_myfigs +
  theme(aspect.ratio=3/4) +
  labs(title = "PCA w/ MPA & Province Pop Stats")


ggbiplot(pca_mpa_influence,
         choices=3:4,
         ellipse=TRUE,
         ellipse.prob = .5,
         groups = data_mpa_stations %>%
           pull(study)) +
  theme_classic() +
  theme_myfigs +
  theme(aspect.ratio=3/4)  +
  labs(title = "PCA w/ MPA & Province Pop Stats")

