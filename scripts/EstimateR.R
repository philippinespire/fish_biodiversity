#### INITIALIZATION ####
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tidyverse)
library(janitor)
library(readxl)
theme_set(
  theme_void()
)
library(vegan)
library(remotes)
library(ggvegan)
source("wrangle_cas_si_su_data.R")
source("distance_calculations.R")

data <- data_cas_si_su

#### VEGANIZATION ####
#Vegan all data
prep_vegan <- function(data=data_cas_si_su){
  data %>%
    rename(taxon = verified_identification) %>%
    filter(specimen_count > 0) %>%
    group_by(taxon,
             station_code,
             study,
             field_number,
             lowest_tax_cat) %>%
    summarize(sum_specimen_count = sum(specimen_count)) %>%
    ungroup() %>%
    pivot_wider(names_from = taxon,
                values_from = sum_specimen_count,
                values_fill = 0) %>%
    clean_names() %>%
    arrange(study,
            station_code) %>%
    drop_na(station_code)
}

data_cas_si_su_vegan <-
  prep_vegan() %>%
  dplyr::select(-station_code:-lowest_tax_cat)

data_cas_si_su_vegan.env <-
  prep_vegan() %>%
  dplyr::select(station_code:lowest_tax_cat)


attach(data_cas_si_su_vegan.env)
data_vegan <- data_cas_si_su_vegan 
data_vegan.env <- data_cas_si_su_vegan.env
attach(data_vegan.env)

#vegan cas data
prep_vegan_cas <- function(data=data_cas_si_su){
  data %>%
    dplyr::filter(study == "cas_2016") %>%
    rename(taxon = verified_identification) %>%
    dplyr::filter(specimen_count > 0) %>%
    group_by(taxon,
             station_code, 
             study,
             field_number,
             lowest_tax_cat) %>%
    summarize(sum_specimen_count = sum(specimen_count)) %>%
    ungroup() %>%
    pivot_wider(names_from = taxon,
                values_from = sum_specimen_count,
                values_fill = 0) %>%
    clean_names() %>%
    arrange(study,
            station_code) %>%
    drop_na(station_code)
}    

data_casvegan <-
  prep_vegan_cas() %>%
  dplyr::select(-station_code:-lowest_tax_cat)

data_cas_vegan.env <-
  prep_vegan_cas() %>%
  dplyr::select(station_code:lowest_tax_cat)

attach(data_cas_vegan.env)

#vegan si data
prep_vegan_si <- function(data=data_cas_si_su){
  data %>%
    dplyr::filter(study == "si_1978") %>%
    rename(taxon = verified_identification) %>%
    dplyr::filter(specimen_count > 0) %>%
    group_by(taxon,
             station_code, 
             study,
             field_number,
             lowest_tax_cat) %>%
    summarize(sum_specimen_count = sum(specimen_count)) %>%
    ungroup() %>%
    pivot_wider(names_from = taxon,
                values_from = sum_specimen_count,
                values_fill = 0) %>%
    clean_names() %>%
    # sort by station_code
    arrange(study,
            station_code) %>%
    drop_na(station_code)
}    

data_sivegan <-
  prep_vegan_si() %>%
  dplyr::select(-station_code:-lowest_tax_cat)

data_si_vegan.env <-
  prep_vegan_si() %>%
  dplyr::select(station_code:lowest_tax_cat)

attach(data_si_vegan.env)

#vegan su data
prep_vegan_su <- function(data=data_cas_si_su){
  data %>%
    dplyr::filter(study == "su_2022") %>%
    rename(taxon = verified_identification) %>%
    dplyr::filter(specimen_count > 0) %>%
    group_by(taxon,
             station_code, 
             study,
             field_number,
             lowest_tax_cat) %>%
    summarize(sum_specimen_count = sum(specimen_count)) %>%
    ungroup() %>%
    pivot_wider(names_from = taxon,
                values_from = sum_specimen_count,
                values_fill = 0) %>%
    clean_names() %>%
    
    arrange(study,
            station_code) %>%
    drop_na(station_code)
}    

data_suvegan <-
  prep_vegan_su() %>%
  dplyr::select(-station_code:-lowest_tax_cat)

data_su_vegan.env <-
  prep_vegan_su() %>%
  dplyr::select(station_code:lowest_tax_cat)

attach(data_su_vegan.env)


#### Estimate and Estaccum R ####

# specpool(x, pool, smallsample = TRUE)
est_S <- estimateR(data_vegan) %>%
  t() %>%
  as_tibble() %>%
  clean_names() %>%
  select(-s_ace,
         -se_ace) %>%
  bind_cols(data_vegan.env) %>%
  left_join(data_closest_mpa)

#### PLOTS ####

est_S %>%
  ggplot(aes(x = pc1_mpa_infl, 
             y = s_chao1,
             color = study)) +
  geom_point() +
  geom_errorbar(aes(ymin = s_chao1 - se_chao1,
                    ymax = s_chao1 + se_chao1)) +
  geom_smooth(se = FALSE,
              method = "lm") +
  theme_classic()

est_S %>%
  ggplot(aes(x = mpa_area_ha, 
             y = s_chao1,
             color = study)) +
  geom_point() +
  geom_errorbar(aes(ymin = s_chao1 - se_chao1,
                    ymax = s_chao1 + se_chao1)) +
  geom_smooth(se = FALSE,
              method = "lm") +
  theme_classic()
  

est_S %>%
  ggplot(aes(x = station_mpa_distance_km, 
             y = s_chao1,
             color = study)) +
  geom_point() +
  geom_errorbar(aes(ymin = s_chao1 - se_chao1,
                    ymax = s_chao1 + se_chao1)) +
  geom_smooth(se = FALSE,
              method = "lm") +
  theme_classic()

est_S %>%
  ggplot(aes(x = PC2, 
             y = s_chao1,
             color = study)) +
  geom_point() +
  geom_errorbar(aes(ymin = s_chao1 - se_chao1,
                    ymax = s_chao1 + se_chao1)) +
  geom_smooth(se = FALSE,
              method = "lm") +
  theme_classic()

est_S %>%
  ggplot(aes(x = PC3, 
             y = s_chao1,
             color = study)) +
  geom_point() +
  geom_errorbar(aes(ymin = s_chao1 - se_chao1,
                    ymax = s_chao1 + se_chao1)) +
  geom_smooth(se = FALSE,
              method = "lm") +
  theme_classic()

estimateR(data_vegan.env)

# specpool2vect(X, index = c("jack1","jack2", "chao", "boot","Species"))
# poolaccum(data_vegan, permutations = 100, minsize = 20)
estaccumR(data_vegan, permutations = 100, parallel = getOption("mc.cores"))
estaccumRcas <- 
  data_casvegan %>%
  drop_na() %>%
  estaccumR(permutations = 100, parallel = getOption("mc.cores"))
  
plot(estaccumRcas)
 
 
estaccumR(data_sivegan, permutations = 100, parallel = getOption("mc.cores"))
estaccumR(data_suvegan, permutations = 100, parallel = getOption("mc.cores"))

# ## S3 method for class 'poolaccum'
# summary(object, display, alpha = 0.05, ...)
# ## S3 method for class 'poolaccum'
# plot(x, alpha = 0.05, type = c("l","g"), ...)