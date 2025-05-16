#### INITIALIZATION ####
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#### INSTALL PACKAGES ####
packages_used <- 
  c("tidyverse",
    "janitor",
    "readxl",
    "magrittr",
    "vegan",
    "remotes",
    "ggvegan",
    "ggplot2")

packages_to_install <- 
  packages_used[!packages_used %in% installed.packages()[,1]]

if (length(packages_to_install) > 0) {
  install.packages(packages_to_install, 
                   Ncpus = Sys.getenv("NUMBER_OF_PROCESSORS") - 1)
}

lapply(packages_used, 
       require, 
       character.only = TRUE)

theme_set(
  theme_void()
)

####  SOURCING DATA ####

source("wrangle_cas_si_su_data.R")
source("distance_calculations_mpa.R")
# source("ordination_cas_su_si.R")
source("veganize_data.R")

#### VEGANIZATION ####
# Vegan all data
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
  prep_vegan(data_cas_si_su) %>%
  dplyr::select(-station_code:-study)

data_cas_si_su_vegan.env <-
  prep_vegan(data_cas_si_su) %>%
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
  dplyr::mutate(depth_cat = case_when(depth_m < 3 ~ "<3m",
                                      # depth_m >= 3 & depth_m <= 20 ~ "3-20m",
                                      depth_m >= 3 ~ ">3m")) %>%
  dplyr::mutate(depth_cat = factor(depth_cat,
                                   levels = c("<3m",
                                              # "3-20m",
                                              ">3m"))) %>%
  filter(!is.na(depth_m)) %>%
  left_join(data_human_pop) %>%
  dplyr::mutate(pop_dens_province_cat = case_when(pop_dens_province < 250 ~ "<250",
                                                  pop_dens_province >=250 & pop_dens_province <= 500 ~ "250-500",
                                                  pop_dens_province > 500 ~ ">500")) %>%
  dplyr::mutate(pop_dens_province_cat = factor(pop_dens_province_cat,
                                               levels = c("<250",
                                                          "250-500",
                                                          ">500"))) %>%
  dplyr::mutate(study = factor(study,
                               levels = c("si_1978",
                                          "cas_2016",
                                          "su_2022")))

# histogram of station depths
est_S %>%
  ggplot(aes(x = depth_m)) +
  geom_histogram(binwidth = 3, color = "black", fill = "steelblue") +  # optional styling
  labs(
    x = "Depth (m)",                    # x-axis label
    y = "Number of Stations",           # y-axis label
    title = "Histogram of Station Depths"  # optional title
  ) +
  theme_minimal()

# histogram of human population density of nearest province
est_S %>%
  ggplot(aes(x = pop_dens_province)) +
  geom_histogram(binwidth = 50, color = "black", fill = "steelblue") +  # optional styling
  labs(
    x = "Population Density of Nearest Province (people/km2)",         # x-axis label
    y = "Number of Stations",           # y-axis label
    title = "Histogram of Population Density of Nearest Station"  # optional title
  ) +
  theme_minimal()


#### PLOTS pop, depth, dist from shore ####

# Estimated species richness vs human population density 
est_S %>%
  ggplot(aes(x = pop_dens_province, 
             y = s_chao1,
             color = study)) +
  geom_point() +
  geom_errorbar(aes(ymin = s_chao1 - se_chao1,
                    ymax = s_chao1 + se_chao1)) +
  labs(
    x = "Human Population Density of Nearest Province (people/km2)", # x-axis label
    y = "Estimated Species Richness"           # y-axis label
  ) +
  geom_smooth(method = "lm") +
  theme_classic()

# Total Human Population of nearest province and estimated species richness
est_S %>%
  ggplot(aes(x = population, 
             y = s_chao1,
             color = study)) +
  geom_point() +
  geom_errorbar(aes(ymin = s_chao1 - se_chao1,
                    ymax = s_chao1 + se_chao1)) +
  labs(
    x = "Total Population of Nearest Province", # x-axis label
    y = "Estimated Species Richness"           # y-axis label
  ) +
  geom_smooth(method = "lm") +
  theme_classic()

# what is distance_m?
est_S %>%
  ggplot(aes(x = distance_m, 
             y = s_chao1,
             color = study)) +
  geom_point() +
  geom_errorbar(aes(ymin = s_chao1 - se_chao1,
                    ymax = s_chao1 + se_chao1)) +
  geom_smooth(method = "lm") +
  theme_classic()

# Depth vs Estimated Species Richness
est_S %>%
  ggplot(aes(x = depth_m, 
             y = s_chao1,
             color = study)) +
  geom_point() +
  geom_errorbar(aes(ymin = s_chao1 - se_chao1,
                    ymax = s_chao1 + se_chao1)) +
  labs(
    x = "Depth (m)", # x-axis label
    y = "Estimated Species Richness"           # y-axis label
  ) +
  geom_smooth() +
  theme_classic()

#### box plots by depth and pop dens ####
# box plot of estimated species richness by study for each depth, human pop density combo
est_S %>%
  ggplot(aes(y = s_chao1,
             fill = study)) +
  geom_boxplot() +
  labs(
    y = "Estimated Species Richness"           # y-axis label
  ) +
  theme_classic() +
  facet_grid(depth_cat ~ pop_dens_province_cat ) +
  theme(
  axis.text.x = element_blank(),
  axis.ticks.x = element_blank())

# boxplot of estimated species richness by study for each human pop density combo
est_S %>%
  ggplot(aes(y = s_chao1,
             x = pop_dens_province_cat,
             fill = study)) +
  geom_boxplot() +
  theme_classic() +
  facet_grid(. ~ study ) +
  labs(
    x = "Human Population Density of Nearest Province (people/km²)", # x-axis label
    y = "Estimated Species Richness"           # y-axis label
  ) +
  theme(
    strip.text.x = element_blank()  # removes facet strip labels on top
  )

# boxplot of estimated species richness by study for each depth group
est_S %>%
  ggplot(aes(y = s_chao1,
             x = depth_cat,
             fill = study)) +
  geom_boxplot() +
  theme_classic() +
  facet_grid(. ~ study ) +
  labs(
    x = "Depth (m)", # x-axis label
    y = "Estimated Species Richness"           # y-axis label
  ) +
  theme(
    strip.text.x = element_blank()  # removes facet strip labels on top
  )

#### PLOTS PC1 ####
# estimated species richness vs pc1 mpa influence
est_S %>%
  ggplot(aes(x = pc1_mpa_infl, 
             y = s_chao1,
             color = study)) +
  geom_point() +
  geom_errorbar(aes(ymin = s_chao1 - se_chao1,
                    ymax = s_chao1 + se_chao1)) +
  geom_smooth(se = FALSE,
              method = "lm") +
  labs(
    x = "PC1 MPA Influence", # x-axis label
    y = "Estimated Species Richness"           # y-axis label
  ) +
  theme_classic()

# estimated species richness vs pc1 mpa influence by depth (>/< 3m)
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

# estimated species richness vs pc1 mpa influence by human population density
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
# estimated species richness vs pc2 mpa influence
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

# estimated species richness vs pc2 mpa influence by depth (>/< 3m)
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

# estimated species richness vs pc2 mpa influence by human population density
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
# estimated species richness by mpa area within x km (default 80)
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

# estimated species richness vs mpa mean distance within x km (default 80)
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

# estimated species richness vs mpa mean age within x km (default 80)
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

# estimated species richness vs numver of mpas within x km (default 80)
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

detach(data_vegan.env)

#### ESTACCUM R ####

#vegan CAS data
data_cas_vegan <-
  data_cas_si_su %>%
  dplyr::filter(study == "cas_2016") %>%
  prep_vegan() %>%
  dplyr::select(-station_code:-study)

data_cas_vegan.env <-
  data_cas_si_su %>%
  dplyr::filter(study == "cas_2016") %>%
  prep_vegan() %>%
  dplyr::select(station_code:study)

#vegan SI data
data_si_vegan <-
  data_cas_si_su %>%
  dplyr::filter(study == "si_1978") %>%
  prep_vegan() %>%
  dplyr::select(-station_code:-study)

data_si_vegan.env <-
  data_cas_si_su %>%
  dplyr::filter(study == "si_1978") %>%
  prep_vegan() %>%
  dplyr::select(station_code:study)

#vegan su_2022 data
data_su_vegan <- 
  data_cas_si_su %>%
  dplyr::filter(study == "su_2022") %>%
  prep_vegan() %>%
  dplyr::select(-station_code:-study)

data_su_vegan.env <-
  data_cas_si_su %>%
  dplyr::filter(study == "su_2022") %>%
  prep_vegan() %>%
  dplyr::select(station_code:study)

estaccumR_data_all <-
  estaccumR(data_vegan, 
            permutations = 999, 
            parallel = 14)

plot(estaccumR_data_all)

# error with data_si_vegan
estaccumR_si <- 
  estaccumR(data_si_vegan, 
            permutations = 999, 
            parallel = 14)

plot(estaccumR_si)

estaccumR_cas <-
  estaccumR(data_cas_vegan, 
            permutations = 999, 
            parallel = 14)

plot(estaccumR_cas)

estaccumR_su <- 
  estaccumR(data_su_vegan, 
            permutations = 999, 
            parallel = 14)

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
      dplyr::mutate(category_id = category_id)
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