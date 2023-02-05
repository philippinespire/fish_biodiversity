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
  estimateR(data_vegan) %>%
  t() %>%
  as_tibble() %>%
  clean_names() %>%
  dplyr::select(-s_ace,
                -se_ace) %>%
  bind_cols(data_vegan.env) %>%
  left_join(data_mpa_stations_pc) %>%
  filter(!is.na(latitude)) %>%
  mutate(depth_cat = case_when(depth_m < 3 ~ "<3m",
                               # depth_m >= 3 & depth_m <= 20 ~ "3-20m",
                               depth_m >= 3 ~ ">3m")) %>%
  mutate(depth_cat = factor(depth_cat,
                            levels = c("<3m",
                                       # "3-20m",
                                       ">3m"))) %>%
  filter(!is.na(depth_m)) %>%
  left_join(data_human_pop) %>%
  mutate(pop_dens_province_cat = case_when(pop_dens_province < 250 ~ "<250",
                                           pop_dens_province >=250 & pop_dens_province <= 500 ~ "250-500",
                                           pop_dens_province > 500 ~ ">500")) %>%
  mutate(pop_dens_province_cat = factor(pop_dens_province_cat,
                                        levels = c("<250",
                                                   "250-500",
                                                   ">500"))) %>%
  mutate(study = factor(study,
                        levels = c("si_1978",
                                   "cas_2016",
                                   "su_2022")))

est_S %>%
  ggplot(aes(x=depth_m)) +
  geom_histogram()

est_S %>%
  ggplot(aes(x=pop_dens_province)) +
  geom_histogram()


#### PLOTS pop, depth, dist from shore ####

est_S %>%
  ggplot(aes(x = pop_dens_province, 
             y = s_chao1,
             color = study)) +
  geom_point() +
  geom_errorbar(aes(ymin = s_chao1 - se_chao1,
                    ymax = s_chao1 + se_chao1)) +
  geom_smooth(method = "lm") +
  theme_classic()

est_S %>%
  ggplot(aes(x = population, 
             y = s_chao1,
             color = study)) +
  geom_point() +
  geom_errorbar(aes(ymin = s_chao1 - se_chao1,
                    ymax = s_chao1 + se_chao1)) +
  geom_smooth(method = "lm") +
  theme_classic()

est_S %>%
  ggplot(aes(x = distance_m, 
             y = s_chao1,
             color = study)) +
  geom_point() +
  geom_errorbar(aes(ymin = s_chao1 - se_chao1,
                    ymax = s_chao1 + se_chao1)) +
  geom_smooth(method = "lm") +
  theme_classic()

est_S %>%
  ggplot(aes(x = depth_m, 
             y = s_chao1,
             color = study)) +
  geom_point() +
  geom_errorbar(aes(ymin = s_chao1 - se_chao1,
                    ymax = s_chao1 + se_chao1)) +
  geom_smooth() +
  theme_classic()

#### box plots by depth and pop dens ####
est_S %>%
  ggplot(aes(y = s_chao1,
             fill = study)) +
  geom_boxplot() +
  theme_classic() +
  facet_grid(depth_cat ~ pop_dens_province_cat )

est_S %>%
  ggplot(aes(y = s_chao1,
             x = pop_dens_province_cat,
             fill = study)) +
  geom_boxplot() +
  theme_classic() +
  facet_grid(. ~ study )

est_S %>%
  ggplot(aes(y = s_chao1,
             x = depth_cat,
             fill = study)) +
  geom_boxplot() +
  theme_classic() +
  facet_grid(. ~ study )

#### PLOTS PC1 ####

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
  ggplot(aes(x = pc1_mpa_infl, 
             y = s_chao1,
             color = study)) +
  geom_point() +
  geom_errorbar(aes(ymin = s_chao1 - se_chao1,
                    ymax = s_chao1 + se_chao1)) +
  geom_smooth(se = FALSE,
              method = "lm") +
  theme_classic() +
  facet_wrap(depth_cat ~ .)

est_S %>%
  ggplot(aes(x = pc1_mpa_infl, 
             y = s_chao1,
             color = study)) +
  geom_point() +
  geom_errorbar(aes(ymin = s_chao1 - se_chao1,
                    ymax = s_chao1 + se_chao1)) +
  geom_smooth(se = FALSE,
              method = "lm") +
  theme_classic() +
  facet_wrap(pop_dens_province_cat ~ .)


#### PLOTS PC2 ####

est_S %>%
  ggplot(aes(x = pc2_mpa_infl, 
             y = s_chao1,
             color = study)) +
  geom_point() +
  geom_errorbar(aes(ymin = s_chao1 - se_chao1,
                    ymax = s_chao1 + se_chao1)) +
  geom_smooth(se = FALSE,
              method = "lm") +
  theme_classic()


est_S %>%
  ggplot(aes(x = pc2_mpa_infl, 
             y = s_chao1,
             color = study)) +
  geom_point() +
  geom_errorbar(aes(ymin = s_chao1 - se_chao1,
                    ymax = s_chao1 + se_chao1)) +
  geom_smooth(se = FALSE,
              method = "lm") +
  theme_classic() +
  facet_wrap(depth_cat ~ .)

est_S %>%
  ggplot(aes(x = pc2_mpa_infl, 
             y = s_chao1,
             color = study)) +
  geom_point() +
  geom_errorbar(aes(ymin = s_chao1 - se_chao1,
                    ymax = s_chao1 + se_chao1)) +
  geom_smooth(se = FALSE,
              method = "lm") +
  theme_classic() +
  facet_wrap(pop_dens_province_cat ~ .)


#### PLOTS Other ####
est_S %>%
  ggplot(aes(x = mpa_area_within_xkm_ha, 
             y = s_chao1,
             color = study)) +
  geom_point() +
  geom_errorbar(aes(ymin = s_chao1 - se_chao1,
                    ymax = s_chao1 + se_chao1)) +
  geom_smooth(se = FALSE,
              method = "lm") +
  theme_classic()


est_S %>%
  ggplot(aes(x = mpa_meandist_within_xkm, 
             y = s_chao1,
             color = study)) +
  geom_point() +
  geom_errorbar(aes(ymin = s_chao1 - se_chao1,
                    ymax = s_chao1 + se_chao1)) +
  geom_smooth(se = FALSE,
              method = "lm") +
  theme_classic()

est_S %>%
  ggplot(aes(x = mpa_meanage_within_xkm, 
             y = s_chao1,
             color = study)) +
  geom_point() +
  geom_errorbar(aes(ymin = s_chao1 - se_chao1,
                    ymax = s_chao1 + se_chao1)) +
  geom_smooth(se = FALSE,
              method = "lm") +
  theme_classic()

est_S %>%
  ggplot(aes(x = mpa_num_within_xkm, 
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
  estaccumR(data_vegan, 
            permutations = 10, 
            parallel = 8)

plot(estaccumR_data_all)


estaccumR_si <- 
  estaccumR(data_si_vegan, 
            permutations = 999, 
            parallel = 8)

plot(estaccumR_si)

estaccumR_cas <- # error
  estaccumR(data_cas_vegan, 
            permutations = 999, 
            parallel = 8)

plot(estaccumR_cas)

estaccumR_su <- 
  estaccumR(data_su_vegan, 
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