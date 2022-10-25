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
source("distance_calculations_mpa.R")
source("ordination_cas_su_si.R")


#### VEGANIZATION ####
#Vegan all data
prep_vegan <- 
  function(data=data_cas_si_su){
  data %>%
    dplyr::rename(taxon = verified_identification) %>%
    filter(specimen_count > 0) %>%
    group_by(taxon,
             station_code,
             study,
             # field_number,
             # lowest_tax_cat
             ) %>%
    dplyr::summarize(sum_specimen_count = sum(specimen_count)) %>%
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
  dplyr::select(-station_code:-study)

data_cas_si_su_vegan.env <-
  prep_vegan() %>%
  dplyr::select(station_code:study)



data_vegan <- data_cas_si_su_vegan 
data_vegan.env <- data_cas_si_su_vegan.env
attach(data_vegan.env)


#### EstimateR ####

# specpool(x, pool, smallsample = TRUE)
est_S <- 
  estimateR(data_fixed_vegan) %>%
  t() %>%
  as_tibble() %>%
  clean_names() %>%
  select(-s_ace,
         -se_ace) %>%
  bind_cols(data_fixed_vegan.env) %>%
  left_join(data_closest_mpa) %>%
  filter(!is.na(adjusted_latitude))

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


# write_tsv(est_S, "estimateR_fixedvegan.tsv")

# specpool2vect(X, index = c("jack1","jack2", "chao", "boot","Species"))
# poolaccum(data_vegan, permutations = 100, minsize = 20)

#### ESTACCUM R ####

estaccumR_data_all <-
  estaccumR(data_fixed_vegan, 
            permutations = 10, 
            parallel = 8)

plot(estaccumR_data_all)
  

estaccumR_si <- 
  estaccumR(data_sivegan, 
            permutations = 999, 
            parallel = 8)

plot(estaccumR_si)

estaccumR_cas <- # error
  estaccumR(data_fixed_casvegan, 
            permutations = 999, 
            parallel = 8)

plot(estaccumR_cas)

estaccumR_su <- 
  estaccumR(data_suvegan, 
            permutations = 999, 
            parallel = 8)

plot(estaccumR_su)


#### PLOT estaccumR results by study ####

estaccumR_plot <- 
  function(data_chao,
           category_id){
    
    data_chao %>%
      as_tibble() %>%
      dplyr::mutate(N = row_number()) %>%
      pivot_longer(cols = starts_with("V"),
                   names_to = "permutation") %>%
      group_by(N) %>%
      dplyr::summarize(chao_mean = mean(value),
                       chao_ci_lower = quantile(value,
                                                probs = 0.025),
                       chao_ci_upper = quantile(value,
                                                probs = 0.975)) %>%
      ungroup() %>%
      mutate(category_id = category_id)
  }

bind_rows(estaccumR_plot(estaccumR_si$chao,
                         "si_1978-79"),
          estaccumR_plot(estaccumR_cas$chao,
                         "cas_2016"),
          estaccumR_plot(estaccumR_su$chao,
                         "su_2019-22")) %>%
  ggplot(aes(x=N,
             y=chao_mean,
             color = category_id,
             fill = category_id)) +
  geom_ribbon(aes(ymin=chao_ci_lower,
                  ymax=chao_ci_upper),
              alpha = 0.5) +
  geom_line() +
  theme_classic() +
  labs(y = "Estimated Species Richness (Chao)",
       x = "Number of Samples")

# ## S3 method for class 'poolaccum'
# summary(object, display, alpha = 0.05, ...)
# ## S3 method for class 'poolaccum'
# plot(x, alpha = 0.05, type = c("l","g"), ...)