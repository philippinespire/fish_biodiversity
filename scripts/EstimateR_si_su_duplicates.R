#### NOTES ####
# This script was created from EstimateR.R. 
# It was adapted to just focus on the SU-SI duplicate stations. 
# 24 SU duplicates. But this was filtered down to 21 for habitat and sampling effectiveness.
# It does include 3 proxy stations. 


#### INITIALIZATION ####
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#### INSTALL PACKAGES ####
packages_used <- 
  c("readr",
    "tibble",
    "tidyverse",
    "janitor",
    "readxl",
    "magrittr",
    "vegan",
    "remotes",
    "ggvegan",
    "ggplot2",
    "purrr",
    "tidyr",
    "stringr",
    "readr",
    "dplyr",
    "lme4"
  )

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


#### READ IN VEGANIZED DATA ####

# Community matrix (samples × species)
data_vegan <- read_csv(
  "../data/si_su_duplicates/data_vegan_si_su_duplicates_community_matrix.csv"
) %>% 
  column_to_rownames("station_code")

# vegan wants a matrix or data.frame with rownames = samples, cols = species
# you can leave it as a data.frame, or do:
# comm_matrix <- as.matrix(comm_matrix)

# Environmental metadata. Keep station_code as a column not rownames.
data_vegan.env <- read_csv(
  "../data/si_su_duplicates/data_vegan_si_su_duplicates_metadata.csv"
)

# check that the rows are aligned by station_code
identical(rownames(data_vegan), data_vegan.env$station_code)


#### SOURCE DATA ####

source("wrangle_si_su_data.R")
source("distance_calculations_mpa.R")


#### SUBSAMPLE VEGANIZED DATA ####

# BY STUDY 
data_si_vegan <- data_vegan[ data_vegan.env$study == "si_1978", ]
data_su_vegan <- data_vegan[ data_vegan.env$study == "su_2022", ]

# BY SEA
data_sulu_vegan <- data_vegan[ data_vegan.env$sea == "Sulu Sea", ]
data_bohol_vegan <- data_vegan[ data_vegan.env$sea == "Bohol Sea", ]

# BY STUDY & SEA
data_si_sulu_vegan <- data_vegan.env %>%
  filter(study=="si_1978", sea=="Sulu Sea") %>%
  pull(station_code) %>%        # get the station_codes
  { data_vegan[rownames(data_vegan) %in% ., ] }

data_su_sulu_vegan <- data_vegan.env %>%
  filter(study=="su_2022", sea=="Sulu Sea") %>%
  pull(station_code) %>%        # get the station_codes
  { data_vegan[rownames(data_vegan) %in% ., ] }

data_si_bohol_vegan <- data_vegan.env %>%
  filter(study=="si_1978", sea=="Bohol Sea") %>%
  pull(station_code) %>%        # get the station_codes
  { data_vegan[rownames(data_vegan) %in% ., ] }

data_su_bohol_vegan <- data_vegan.env %>%
  filter(study=="su_2022", sea=="Bohol Sea") %>%
  pull(station_code) %>%        # get the station_codes
  { data_vegan[rownames(data_vegan) %in% ., ] }


#### ESTIMATER #### 
# individual‐based richness estimators (S.obs, S.chao1, ACE)
estimate_ind_all <- estimateR(data_vegan)

estimate_ind_df <- as.data.frame(t(estimate_ind_all)) %>%
  rownames_to_column("station_code") %>%
  dplyr::rename(
    S_obs   = S.obs,
    S_chao1 = S.chao1,
    S_ACE   = S.ACE
  ) %>%
  left_join(data_vegan.env, by = "station_code")

# save file
# write_csv(est_ind_df, "../figures/si_su_duplicates/estimateR_individual_by_station.csv")


#### ESTIMATER STATS ####
# Model Chao1 per station
mod_chao <- lmer(S_chao1 ~ study * sea + (1 | station_pair), data = estimate_ind_df)
car::Anova(mod_chao, type = "III")

# Analysis of Deviance Table (Type III Wald chisquare tests)
# 
#   Response: S_chao1
#               Chisq     Df  Pr(>Chisq)    
#   (Intercept) 145.5250  1   < 2.2e-16 ***
#   study         0.3126  1   0.576078    
#   sea          10.4731  1   0.001211 ** 
#   study:sea     1.9749  1   0.159930    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Model ACE per station
mod_ace <- lmer(S_ACE ~ study * sea + (1 | station_pair), data = estimate_ind_df)
car::Anova(mod_ace, type = "III")

# Analysis of Deviance Table (Type III Wald chisquare tests)
# 
# Response: S_ACE
#               Chisq     Df Pr(>Chisq)    
#   (Intercept) 164.3161  1  < 2.2e-16 ***
#   study         0.1823  1  0.6693647    
#   sea          10.9997  1  0.0009113 ***
#   study:sea     1.5702  1  0.2101707    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


#### FIXED EFFECTED BY STATION ESTIMATER ####
# fixef_metric <- fixef(mod_metric)
fixef_chao  <- fixef(mod_chao)
fixef_ace   <- fixef(mod_ace)

# Helper function for a single model
tidy_fixef <- function(model, metric) {
  s <- summary(model)
  ests <- as.data.frame(s$coefficients)  # rownames = term
  # Confidence intervals: lme4 provides confint(), use Wald by default
  cis <- confint(model, method = "Wald")
  cis <- as.data.frame(cis[rownames(ests), , drop = FALSE])
  tibble(
    term     = rownames(ests),
    estimate = ests$Estimate,
    std_error = ests$`Std. Error`,
    conf_low  = cis[,1],
    conf_high = cis[,2],
    metric   = metric
  )
}

# Now bind all together
df_fixef <- bind_rows(
  tidy_fixef(mod_chao, "chao"),
  tidy_fixef(mod_ace,  "ace")
)

print(df_fixef)

# save file
outdir <- "../tables/si_su_duplicates"
outfile <- file.path(outdir, "table_estimater_station_lme_fixef.csv")
write_csv(df_fixef, outfile)


#### LME POST-HOC ####
# install.packages("emmeans")  # if not already installed
library(emmeans)

# CHAO emmeans pairwise comparisons
em_chao <- emmeans(mod_chao, ~ study * sea)
posthoc_chao <- pairs(em_chao, adjust = "tukey")  # all pairwise comparisons
summary(posthoc_chao)

# ACE emmeans pairwise comparisons
em_ace <- emmeans(mod_ace, ~ study * sea)
posthoc_ace <- pairs(em_ace, adjust = "tukey")  # all pairwise comparisons
summary(posthoc_ace)

# save file
# outdir <- "../tables/si_su_duplicates"
# outfile <- file.path(outdir, "table_diversity_alpha_lme_by_study_sea_posthoc.csv")
# write_csv(df_posthoc, outfile)



#### RARECURVE ####
## SI ##
# collapse all SI stations into one pooled vector
pool_si_vec <- colSums(data_si_vegan)

# make a one-row matrix (rarecurve expects rows = samples)
mat_si <- matrix(pool_si_vec, nrow=1)
rownames(mat_si) <- "SI-1978"

rarecurve(mat_si, step=100, col="purple", lwd=2,
          xlab="Individuals", ylab="Species")

## SU ##
# collapse all SI stations into one pooled vector
pool_su_vec <- colSums(data_su_vegan)

# make a one-row matrix (rarecurve expects rows = samples)
mat_su <- matrix(pool_su_vec, nrow=1)
rownames(mat_su) <- "SU-2022"

rarecurve(mat_su, step=100, col="purple", lwd=2,
          xlab="Individuals", ylab="Species")



#### SUMMARY STATS ####
# This has been moved to veganize_data_si_su_duplicates.R
# # count the number of stations. Should be 42 total stations. 
# # length(unique(data_si_su$station_code))
# # 42 unique stations. 21 1970's stations and 21 contemporary duplicates. 
# 
# # Table 1. Number of unique stations, families, genera, species for each survey X sea combo. 
# # 1) Build all summaries and stack them
# summary_table <- bind_rows(
#   
#   # — Overall (no filter) —
#   data_si_su %>%
#     dplyr::summarize(
#       n_stations     = dplyr::n_distinct(station_code),
#       n_family       = dplyr::n_distinct(family),
#       n_genus        = dplyr::n_distinct(genus),
#       n_species     = dplyr::n_distinct(verified_identification)
#     ) %>%
#     dplyr::mutate(study = "All", sea = "All"),
#   
#   # — By study only —
#   data_si_su %>%
#     dplyr::group_by(study) %>%
#     dplyr::summarize(
#       n_stations     = dplyr::n_distinct(station_code),
#       n_family       = dplyr::n_distinct(family),
#       n_genus        = dplyr::n_distinct(genus),
#       n_species     = dplyr::n_distinct(verified_identification)
#     ) %>%
#     dplyr::mutate(sea = "All") %>%
#     dplyr::ungroup(),
#   
#   # — By sea only —
#   data_si_su %>%
#     dplyr::group_by(sea) %>%
#     dplyr::summarize(
#       n_stations     = dplyr::n_distinct(station_code),
#       n_family       = dplyr::n_distinct(family),
#       n_genus        = dplyr::n_distinct(genus),
#       n_species     = dplyr::n_distinct(verified_identification)
#     ) %>%
#     dplyr::mutate(study = "All") %>%
#     dplyr::ungroup(),
#   
#   # — By study × sea combinations —
#   data_si_su %>%
#     dplyr::group_by(study, sea) %>%
#     dplyr::summarize(
#       n_stations     = dplyr::n_distinct(station_code),
#       n_family       = dplyr::n_distinct(family),
#       n_genus        = dplyr::n_distinct(genus),
#       n_species     = dplyr::n_distinct(verified_identification)
#     ) %>%
#     dplyr::ungroup()
# ) %>%
#   dplyr::select(study, sea, n_stations, n_family, n_genus, n_species)
# 
# # 2) Take a look
# print(summary_table)

# save file
# outdir <- "../figures/si_su_duplicates"
# outfile <- file.path(outdir, "table_sample_size_counts_by_study_sea.csv")
# write_csv(summary_table, outfile)


#### VEGANIZATION ####
# This has been moved to veganize_data_si_su_duplicates.R
# veganization function adapated fropm veganize_data_si_su.R and EstimateR_si_su_duplicates.R. 
# prep_vegan <- function(data = data_si_su) {
#   data %>%
#     filter(specimen_count > 0) %>%
#     group_by(verified_identification,
#              study, 
#              station_code, 
#              municipality, 
#              date_collected, 
#              latitude, 
#              longitude, 
#              depth_m, 
#              sea
#     ) %>%
#     dplyr::summarize(sum_specimen_count = sum(specimen_count)) %>%
#     ungroup() %>%
#     pivot_wider(
#       names_from = verified_identification,
#       values_from = sum_specimen_count,
#       values_fill = 0
#     ) %>%
#     clean_names() %>%
#     arrange(station_code) %>%
#     drop_na(station_code)
# }
# 
# data_vegan.all <-
#   prep_vegan()
# 
# data_vegan <-
#   prep_vegan() %>%
#   dplyr::select(-study, -station_code, -municipality, -date_collected, -latitude, -longitude, -depth_m, -sea)
# 
# data_vegan.env <-
#   prep_vegan() %>%
#   dplyr::select(study, station_code, municipality, date_collected, latitude, longitude, depth_m, sea)


#### EstimateR ####

# Define survey/study colors and names for the legend
study_colors <- c(
  "si_1978" = "#0072CE",  # Smithsonian Blue
  "su_2022" = "#800000" # ,  # Silliman Maroon
  # "cas_2016" = "#FF6C2F"  # CAS Orange
)

# Define legend labels
study_labels <- c(
  "si_1978" = "SI 1978/1979",
  "su_2022" = "SU 2019/2022"
)

# specpool(x, pool, smallsample = TRUE)
est_S <- 
  estimateR(data_vegan) %>%
  t() %>%
  as_tibble() %>%
  clean_names() %>%
  # dplyr::select(-s_ace,
  #               -se_ace) %>%
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
                                          "su_2022")))

# histogram of station depths
est_S %>%
  ggplot(aes(x = depth_m)) +
  geom_histogram(binwidth = 1, color = "black", fill = "steelblue") +  # optional styling
  labs(
    x = "Depth (m)",                    # x-axis label
    y = "Number of Stations",           # y-axis label
    title = "Histogram of Station Depths"  # optional title
  ) +
  theme_minimal()

# histogram of human population density of nearest province
est_S %>%
  ggplot(aes(x = pop_dens_province)) +
  geom_histogram(binwidth = 5, color = "black", fill = "steelblue") +  # optional styling
  labs(
    x = "Population Density of Nearest Province (people/km2)",         # x-axis label
    y = "Number of Stations",           # y-axis label
    title = "Histogram of Population Density of Nearest Station"  # optional title
  ) +
  xlim(0, 400) +
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
    x = "Human Population Density of Nearest Province (people/km²)", # x-axis label
    y = "Estimated Species Richness (Chao 1)",
    color = "Survey"
  ) +
  geom_smooth(method = "lm") +
  theme_classic() + 
  scale_color_manual(
    values = study_colors,
    labels = study_labels
  )

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
    y = "Estimated Species Richness (Chao 1)", 
    color = "Survey"
  ) +
  geom_smooth(method = "lm") +
  theme_classic() + 
  scale_color_manual(
    values = study_colors,
    labels = study_labels
  )

# Distance in meters (distance_m) from a station to the nearest provincial polygon. This needs to be corrected. 
est_S %>%
  ggplot(aes(x = distance_m, 
             y = s_chao1,
             color = study)) +
  geom_point() +
  geom_errorbar(aes(ymin = s_chao1 - se_chao1,
                    ymax = s_chao1 + se_chao1)) +
  geom_smooth(method = "lm") +
  labs(
    x = "Distance to Nearest Province (m)", # x-axis label
    y = "Estimated Species Richness (Chao 1)",
    color = "Survey"
  ) +
  theme_classic() + 
  scale_color_manual(
    values = study_colors,
    labels = study_labels
  )


#### DEPTH ####

# Depth vs Estimated Species Richness (Chao 1)
est_S %>%
  ggplot(aes(x = depth_m, 
             y = s_chao1,
             color = study)) +
  geom_point() +
  geom_errorbar(aes(ymin = s_chao1 - se_chao1,
                    ymax = s_chao1 + se_chao1)) +
  labs(
    x = "Depth (m)", # x-axis label
    y = "Estimated Species Richness (Chao 1)",
    color = "Survey"
  ) +
  geom_smooth() +
  theme_classic() + 
  scale_color_manual(
    values = study_colors,
    labels = study_labels) +
  theme(legend.position = "none")


# Depth vs Estimated Species Richness (ACE)
est_S %>%
  ggplot(aes(x = depth_m, 
             y = s_ace,
             color = study)) +
  geom_point() +
  geom_errorbar(aes(ymin = s_ace - se_ace,
                    ymax = s_ace + se_ace)) +
  labs(
    x = "Depth (m)", # x-axis label
    y = "Estimated Species Richness (ACE)",
    color = "Survey"
  ) +
  geom_smooth() +
  theme_classic() + 
  scale_color_manual(
    values = study_colors,
    labels = study_labels) +
  theme(legend.position = "none")


# Depth vs Observed Species Richness (S)
est_S %>%
  ggplot(aes(x = depth_m, 
             y = s_obs,
             color = study)) +
  geom_point() +
  labs(
    x = "Depth (m)", # x-axis label
    y = "Observed Species Richness (S)",
    color = "Survey"
  ) +
  geom_smooth() +
  theme_classic() + 
  scale_color_manual(
    values = study_colors,
    labels = study_labels) +
  theme(legend.position = "none")


#### PLOTS by depth and human pop density ####
# box plot of estimated species richness by study for each depth, human pop density combo
est_S %>%
  ggplot(aes(y = s_chao1,
             fill = study)) +
  geom_boxplot() +
  labs(
    y = "Estimated Species Richness (Chao 1)",           # y-axis label
    fill = "Survey"
  ) +
  scale_fill_manual(
    values = study_colors,
    labels = study_labels
  ) +
  theme_classic() +
  facet_grid(depth_cat ~ pop_dens_province_cat ) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  ) 


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
    y = "Estimated Species Richness (Chao 1)",           # y-axis label
    fill = "Survey"
  ) +
  scale_fill_manual(
    values = study_colors,
    labels = study_labels
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
  facet_grid(. ~ study ) +
  theme_classic() +
  # theme(
  #   strip.text.x = element_blank()  # removes facet strip labels on top
  # ) +
  labs(
    x = "Depth (m)", # x-axis label
    y = "Estimated Species Richness (Chao 1)",
    fill = "Survey"
  ) +
  scale_fill_manual(
    values = study_colors,
    labels = study_labels
  ) +
  theme(
    strip.text.x = element_blank()
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


