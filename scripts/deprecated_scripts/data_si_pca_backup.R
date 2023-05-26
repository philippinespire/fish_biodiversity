####Initialize ####
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tidyverse)
library(janitor)
library(readxl)
# library(devtools)
# install_github("vqv/ggbiplot")
library(ggbiplot)

data_si_station_pca <-
  data_si_station_gis %>%
    dplyr::select(identification, station_code, specimen_count) %>%
    dplyr::group_by(identification, station_code) %>%
    dplyr::summarize(specimen_count = sum(specimen_count)) %>%
    filter(specimen_count>0) %>%
    pivot_wider(names_from = identification,
                values_from = specimen_count,
                values_fill = 0)

data_si.pca <- 
  prcomp(data_si_station_pca %>%
           select(-station_code), 
         center = TRUE,
         scale. = TRUE)

summary(data_si.pca)

ggbiplot(data_si.pca,
         var.axes = FALSE) +
  theme_classic() +
  labs(title = "PC1 x PC2")

ggbiplot(lai_ratio.pca,
         labels = data_lai_ratios %>%
           pull(code)) +
  theme_classic() +
  labs(title = "PC1 x PC2",
       subtitle = "Labeled with ID #s")

ggbiplot(lai_ratio.pca,
         labels = data_lai_ratios %>%
           pull(code),
         ellipse = TRUE,
         groups = data_lai_ratios %>%
           mutate(province = str_remove(baranguay,
                                        "^.*, ")) %>%
           pull(province)) +
  theme_classic() +
  labs(title = "PC1 x PC2",
       subtitle = "Grouped by Province, With Ellipses")

ggbiplot(lai_ratio.pca,
         labels = data_lai_ratios %>%
           pull(code),
         ellipse = TRUE,
         groups = data_lai_ratios %>%
           mutate(province = str_remove(baranguay,
                                        "^.*, ")) %>%
           pull(province),
         choices = c(3,4)) +
  theme_classic() +
  labs(title = "PC3 x PC4",
       subtitle = "Grouped by Province, With Ellipses")

ggbiplot(data_si.pca,
         labels = data_si_station_pca %>%
           pull(station_code),
         ellipse = TRUE,
         groups = data_si_station_pca %>%
           mutate(station_group = str_remove(station_code,
                                        "[_\\-].*$")) %>%
           pull(station_group),
         choices = c(1,2),
         var.axes = FALSE) +
  theme_classic() +
  ylim(-2.5,2.5)+
  xlim(-2.5,2.5)+
  labs(title = "PC1 x PC2",
       subtitle = "Grouped by Province, With Ellipses, Variables Removed")

