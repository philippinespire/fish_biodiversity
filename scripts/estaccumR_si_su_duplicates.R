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
    "dplyr"
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

# No species complexes for those identified to only the genus or family level
# data_vegan <- read_csv(
#   "../data/si_su_duplicates/data_vegan_si_su_duplicates_community_matrix_nospp.csv"
# ) %>%
#   column_to_rownames("station_code")

# vegan wants a matrix or data.frame with rownames = samples, cols = species
# you can leave it as a data.frame, or do:
# comm_matrix <- as.matrix(comm_matrix)

# Environmental metadata. Keep station_code as a column not rownames.
data_vegan.env <- read_csv(
  "../data/si_su_duplicates/data_vegan_si_su_duplicates_metadata.csv"
)

# check that the rows are aligned by station_code
identical(rownames(data_vegan), data_vegan.env$station_code)

#### SUBSAMPLE VEGANIZED DATA ####

# BY STUDY 
data_si_vegan <- data_vegan[ data_vegan.env$study == "si_1978", ]
data_su_vegan <- data_vegan[ data_vegan.env$study == "su_2022", ]

# BY SEA
data_sulu_vegan <- data_vegan[ data_vegan.env$sea == "sulu", ]
data_bohol_vegan <- data_vegan[ data_vegan.env$sea == "bohol", ]

# BY STUDY & SEA
data_si_sulu_vegan <- data_vegan.env %>%
  filter(study=="si_1978", sea=="sulu") %>%
  pull(station_code) %>%        # get the station_codes
  { data_vegan[rownames(data_vegan) %in% ., ] }

data_su_sulu_vegan <- data_vegan.env %>%
  filter(study=="su_2022", sea=="sulu") %>%
  pull(station_code) %>%        # get the station_codes
  { data_vegan[rownames(data_vegan) %in% ., ] }

data_si_bohol_vegan <- data_vegan.env %>%
  filter(study=="si_1978", sea=="bohol") %>%
  pull(station_code) %>%        # get the station_codes
  { data_vegan[rownames(data_vegan) %in% ., ] }

data_su_bohol_vegan <- data_vegan.env %>%
  filter(study=="su_2022", sea=="bohol") %>%
  pull(station_code) %>%        # get the station_codes
  { data_vegan[rownames(data_vegan) %in% ., ] }


#### ESTACCUMR LISTS ####
# Create estaccumR lists/dataframes for each study/sea combo. 
# Quick plots. Check below for figures.
# sample‐based SAC
## ALL ##
estaccumR_all <- estaccumR(data_vegan, permutations = 999, parallel = 14)
plot(estaccumR_all, xlab = "Samples", ylab = "Species Richness", main = "Species Accumulation Curve: All Samples")

## BY STUDY ##
estaccumR_si <- estaccumR(data_si_vegan, permutations = 999, parallel = 14)
plot(estaccumR_si, xlab = "Samples", ylab = "Species Richness", main = "Species Accumulation Curve: SI 1978/79")

estaccumR_su <- estaccumR(data_su_vegan, permutations = 999, parallel = 14)
plot(estaccumR_su, xlab = "Samples", ylab = "Species Richness", main = "Species Accumulation Curve: SU 2019/22")

## BY SEA ##
estaccumR_sulu <- estaccumR(data_sulu_vegan, permutations = 999, parallel = 14)
plot(estaccumR_sulu, xlab = "Samples", ylab = "Species Richness", main = "Species Accumulation Curve: Sulu")

estaccumR_bohol <- estaccumR(data_bohol_vegan, permutations = 999, parallel = 14)
plot(estaccumR_bohol, xlab = "Samples", ylab = "Species Richness", main = "Species Accumulation Curve: Bohol")

# BY STUDY & SEA
estaccumR_si_sulu <- estaccumR(data_si_sulu_vegan, permutations = 999, parallel = 14)
plot(estaccumR_si_sulu, xlab = "Samples", ylab = "Species Richness", main = "Species Accumulation Curve: SI 1978/79 & Sulu Sea")

estaccumR_su_sulu <- estaccumR(data_su_sulu_vegan, permutations = 999, parallel = 14)
plot(estaccumR_su_sulu, xlab = "Samples", ylab = "Species Richness", main = "Species Accumulation Curve: SU 2019/22 & Sulu Sea")

estaccumR_si_bohol <- estaccumR(data_si_bohol_vegan, permutations = 999, parallel = 14)
plot(estaccumR_si_bohol, xlab = "Samples", ylab = "Species Richness", main = "Species Accumulation Curve: SI 1978/79 & Bohol Sea")

estaccumR_su_bohol <- estaccumR(data_su_bohol_vegan, permutations = 999, parallel = 14)
plot(estaccumR_su_bohol, xlab = "Samples", ylab = "Species Richness", main = "Species Accumulation Curve: SU 2019/22 & Bohol Sea")


#### ESTACCUMR FUNCTIONS FOR PLOTS ####
## S: Simple species richness function
estaccumR_plot_S <- 
  function(data_S,
           category_id){
    
    data_S %>%
      as_tibble() %>%
      dplyr::mutate(N = row_number()) %>%
      pivot_longer(cols = starts_with("V"),
                   names_to = "permutation") %>%
      group_by(N) %>%
      dplyr::summarize(s_mean = mean(value),
                       s_ci_lower = quantile(value,
                                             probs = 0.025),
                       s_ci_upper = quantile(value,
                                             probs = 0.975)) %>%
      ungroup() %>%
      dplyr::mutate(category_id = category_id)
  }

## CHAO1 function
estaccumR_plot_chao <- 
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

## ACE function
estaccumR_plot_ace <- 
  function(data_ace,
           category_id){
    
    data_ace %>%
      as_tibble() %>%
      dplyr::mutate(N = row_number()) %>%
      pivot_longer(cols = starts_with("V"),
                   names_to = "permutation") %>%
      group_by(N) %>%
      dplyr::summarize(ace_mean = mean(value),
                       ace_ci_lower = quantile(value,
                                               probs = 0.025),
                       ace_ci_upper = quantile(value,
                                               probs = 0.975)) %>%
      ungroup() %>%
      dplyr::mutate(category_id = category_id)
  }


#### ESTACCUMR PLOTS ALL ####

# estaccumR_all object has $S, $chao, $ace as in vegan
est_S    <- estaccumR_plot_S(estaccumR_all$S,       "S")
est_chao <- estaccumR_plot_chao(estaccumR_all$chao, "chao1")
est_ace  <- estaccumR_plot_ace(estaccumR_all$ace,   "ACE")
# Reshape to long format for plotting
est_S    <- est_S    %>% dplyr::rename(mean = s_mean, lower = s_ci_lower, upper = s_ci_upper)
est_chao <- est_chao %>% dplyr::rename(mean = chao_mean, lower = chao_ci_lower, upper = chao_ci_upper)
est_ace  <- est_ace  %>% dplyr::rename(mean = ace_mean, lower = ace_ci_lower, upper = ace_ci_upper)

df_all <- bind_rows(est_S, est_chao, est_ace) %>%
  mutate(estimator = category_id)  # for easy faceting/legend

## S plot all
ggplot(est_S, aes(x = N, y = mean)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey", alpha = 0.6) +
  geom_line(color = "black", size = 1) +
  labs(
    y = "Species Richness (S)",
    x = "Samples"
  ) +
  scale_x_continuous(
    breaks = c(0, 10, 20, 30, 40),
    limits = c(1, 42)
  ) +
  scale_y_continuous(
    breaks = c(0, 200, 400, 600, 800, 1000, 1200),
    limits = c(0, 1200)
  ) +
  theme_classic(base_family = "Times New Roman", base_size = 12)

## Chao1 plot all
ggplot(est_chao, aes(x = N, y = mean)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey", alpha = 0.6) +
  geom_line(color = "black", size = 1) +
  labs(
    y = "Estimated Species Richness (Chao1)",
    x = "Samples"
  ) +
  scale_x_continuous(
    breaks = c(0, 10, 20, 30, 40),
    limits = c(1, 42)
  ) +
  scale_y_continuous(
    breaks = c(0, 200, 400, 600, 800, 1000, 1200),
    limits = c(0, 1200)
  ) +
  theme_classic(base_family = "Times New Roman", base_size = 12)

## ACE plot all
ggplot(est_ace, aes(x = N, y = mean)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey", alpha = 0.6) +
  geom_line(color = "black", size = 1) +
  labs(
    y = "Estimated Species Richness (ACE)",
    x = "Samples"
  ) +
  scale_x_continuous(
    breaks = c(0, 10, 20, 30, 40),
    limits = c(1, 42)
  ) +
  scale_y_continuous(
    breaks = c(0, 200, 400, 600, 800, 1000, 1200),
    limits = c(0, 1200)
  ) +
  theme_classic(base_family = "Times New Roman", base_size = 12)


## All 3 estimators
ggplot(df_all, aes(x = N, y = mean, color = estimator, fill = estimator)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, color = NA) +
  geom_line(size = 1) +
  facet_wrap(~ estimator, nrow = 1) +
  labs(
    y     = "Estimated Species Richness",
    x     = "Samples",
    color = "Estimator",
    fill  = "Estimator"
  ) +
  scale_x_continuous(
    breaks = c(0, 10, 20, 30, 40),
    limits = c(1, 42)
  ) +
  scale_y_continuous(
    breaks = c(0, 200, 400, 600, 800, 1000),
    limits = c(0, 1040)
  ) +
  theme_classic(base_family = "Times New Roman", base_size = 12) +
  theme(legend.position = "none")

# S plot
ggplot(estaccumR_all,
       aes(x=N,
           y=s_mean,
           color = category_id,
           fill = category_id)) +
  geom_ribbon(aes(ymin=s_ci_lower,
                  ymax=s_ci_upper),
              alpha = 0.5) +
  geom_line() +
  theme_classic() +
  labs(y = "Species Richness (S)",
       x = "Samples", 
       color = "black",  # Legend title for line
       fill = "grey"    # Legend title for ribbon
  ) +
  scale_x_continuous(
    breaks = c(0, 10, 20, 30, 40),
    limits = c(1, 42)    # adjust if you want a little padding
  ) +
  scale_y_continuous(
    breaks = c(0, 200, 400, 600, 800, 1000),
    limits = c(0, 1000)
  ) +
  theme(legend.position = "none")

#### ESTACCUMR PLOTS BY STUDY ####
# define suvey colors
survey_colors <- c(
  "si_1978-79" = "#0072CE",   # Smithsonian blue
  "su_2019-22" = "#800000"    # Silliman maroon
)

# define legend labels
survey_labels <- c(
  "si_1978-79" = "SI 1978/1979",
  "su_2019-22" = "SU 2019/2022"
)


# S plot
bind_rows(estaccumR_plot_S(estaccumR_si$S,
                           "si_1978-79"),
          estaccumR_plot_S(estaccumR_su$S,
                           "su_2019-22")) %>%
  ggplot(aes(x=N,
             y=s_mean,
             color = category_id,
             fill = category_id)) +
  geom_ribbon(aes(ymin=s_ci_lower,
                  ymax=s_ci_upper),
              alpha = 0.5) +
  geom_line() +
  theme_classic() +
  labs(y = "Species Richness (S)",
       x = "Samples", 
       color = "Survey",  # Legend title for line
       fill = "Survey"    # Legend title for ribbon
  ) +
  scale_color_manual(
    values = survey_colors,
    labels = survey_labels
  ) +
  scale_fill_manual(
    values = survey_colors,
    labels = survey_labels
  ) +
  scale_x_continuous(
    breaks = c(5, 10, 15, 20),
    limits = c(1, 21)    # adjust if you want a little padding
  ) +
  scale_y_continuous(
    breaks = c(0, 200, 400, 600, 800),
    limits = c(0, 900)
  ) +
  theme(legend.position = "none")


# Chao1 plot
bind_rows(estaccumR_plot_chao(estaccumR_si$chao,
                              "si_1978-79"),
          estaccumR_plot_chao(estaccumR_su$chao,
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
  labs(y = "Estimated Species Richness (Chao 1)",
       x = "Samples", 
       color = "Survey",  # Legend title for line
       fill = "Survey"    # Legend title for ribbon
  ) +
  scale_color_manual(
    values = survey_colors,
    labels = survey_labels
  ) +
  scale_fill_manual(
    values = survey_colors,
    labels = survey_labels
  ) +
  scale_x_continuous(
    breaks = c(5, 10, 15, 20),
    limits = c(1, 21)    # adjust if you want a little padding
  ) +
  scale_y_continuous(
    breaks = c(0, 200, 400, 600, 800),
    limits = c(0, 900)
  ) +
  theme(legend.position = "none")

# ACE plot
bind_rows(estaccumR_plot_ace(estaccumR_si$ace,
                             "si_1978-79"),
          estaccumR_plot_ace(estaccumR_su$ace,
                             "su_2019-22")) %>%
  ggplot(aes(x=N,
             y=ace_mean,
             color = category_id,
             fill = category_id)) +
  geom_ribbon(aes(ymin=ace_ci_lower,
                  ymax=ace_ci_upper),
              alpha = 0.5) +
  geom_line() +
  theme_classic() +
  labs(y = "Estimated Species Richness (ACE)",
       x = "Samples", 
       color = "Survey",  # Legend title for line
       fill = "Survey"    # Legend title for ribbon
  ) +
  scale_color_manual(
    values = survey_colors,
    labels = survey_labels
  ) +
  scale_fill_manual(
    values = survey_colors,
    labels = survey_labels
  ) +
  scale_x_continuous(
    breaks = c(5, 10, 15, 20),
    limits = c(1, 21)    # adjust if you want a little padding
  ) +
  scale_y_continuous(
    breaks = c(0, 200, 400, 600, 800),
    limits = c(0, 900)
  ) +
  theme(legend.position = "none")


#### ESTACCUMR PLOTS BY SEA ####
# define suvey colors
sea_colors <- c(
  "Sulu Sea" = "#00829A",   # FMA 5
  "Bohol Sea" = "#C6C8C5" #"#3F463E"    # FMA 9
)

## S: Simple species richness. Using function defined above.
bind_rows(estaccumR_plot_S(estaccumR_sulu$S,
                           "Sulu Sea"),
          estaccumR_plot_S(estaccumR_bohol$S,
                           "Bohol Sea")) %>%
  ggplot(aes(x=N,
             y=s_mean,
             color = category_id,
             fill = category_id)) +
  geom_ribbon(aes(ymin=s_ci_lower,
                  ymax=s_ci_upper),
              alpha = 0.5) +
  geom_line() +
  theme_classic() +
  labs(y = "Species Richness (S)",
       x = "Samples", 
       color = "Sea",  # Legend title for line
       fill = "Sea"    # Legend title for ribbon
  ) +
  scale_color_manual(
    values = sea_colors
  ) +
  scale_fill_manual(
    values = sea_colors
  ) +
  scale_x_continuous(
    breaks = c(5, 10, 15, 20, 25),
    limits = c(0, 26)    # adjust if you want a little padding
  ) +
  scale_y_continuous(
    breaks = c(0, 200, 400, 600, 800),
    limits = c(0, 800)
  ) +
  theme(legend.position = "none")

## CHAO1. Using function defined above.
bind_rows(estaccumR_plot_chao(estaccumR_sulu$chao,
                              "Sulu Sea"),
          estaccumR_plot_chao(estaccumR_bohol$chao,
                              "Bohol Sea")) %>%
  ggplot(aes(x=N,
             y=chao_mean,
             color = category_id,
             fill = category_id)) +
  geom_ribbon(aes(ymin=chao_ci_lower,
                  ymax=chao_ci_upper),
              alpha = 0.5) +
  geom_line() +
  theme_classic() +
  labs(y = "Estimated Species Richness (Chao 1)",
       x = "Samples", 
       color = "Sea",  # Legend title for line
       fill = "Sea"    # Legend title for ribbon
  ) +
  scale_color_manual(
    values = sea_colors
  ) +
  scale_fill_manual(
    values = sea_colors
  ) +
  scale_x_continuous(
    breaks = c(5, 10, 15, 20, 25),
    limits = c(0, 26)    # adjust if you want a little padding
  ) +
  scale_y_continuous(
    breaks = c(0, 200, 400, 600, 800),
    limits = c(0, 800)
  ) +
  theme(legend.position = "none")

## ACE. Using function defined above.
bind_rows(estaccumR_plot_ace(estaccumR_sulu$ace,
                             "Sulu Sea"),
          estaccumR_plot_ace(estaccumR_bohol$ace,
                             "Bohol Sea")) %>%
  ggplot(aes(x=N,
             y=ace_mean,
             color = category_id,
             fill = category_id)) +
  geom_ribbon(aes(ymin=ace_ci_lower,
                  ymax=ace_ci_upper),
              alpha = 0.5) +
  geom_line() +
  theme_classic() +
  labs(y = "Estimated Species Richness (ACE)",
       x = "Samples", 
       color = "Sea",  # Legend title for line
       fill = "Sea"    # Legend title for ribbon
  ) +
  scale_color_manual(
    values = sea_colors
  ) +
  scale_fill_manual(
    values = sea_colors
  ) +
  scale_x_continuous(
    breaks = c(5, 10, 15, 20, 25),
    limits = c(0, 26)    # adjust if you want a little padding
  ) +
  scale_y_continuous(
    breaks = c(0, 200, 400, 600, 800),
    limits = c(0, 800)
  ) +
  theme(legend.position = "none")


#### ESTACCUMR PLOTS BY STUDY & SEA ####
# define suvey colors
sea_colors <- c(
  "Sulu Sea" = "#00829A",   # FMA 5
  "Bohol Sea" = "#C6C8C5" #"#3F463E"    # FMA 9
)

# define suvey colors
survey_colors <- c(
  "SI 1978/1979" = "#0072CE",   # Smithsonian blue
  "SU 2019/2022" = "#800000"    # Silliman maroon
)

## S: Simple species richness. Using function defined above.
bind_rows(
  estaccumR_plot_S(estaccumR_si_sulu$S,  "SI 1978/1979 - Sulu Sea"),
  estaccumR_plot_S(estaccumR_si_bohol$S, "SI 1978/1979 - Bohol Sea"),
  estaccumR_plot_S(estaccumR_su_sulu$S,  "SU 2019/2022 - Sulu Sea"),
  estaccumR_plot_S(estaccumR_su_bohol$S, "SU 2019/2022 - Bohol Sea")
) %>%
  # 1) pull survey and sea back out into their own columns
  mutate(
    survey = case_when(
      str_detect(category_id, "^SI") ~ "SI 1978/1979",
      str_detect(category_id, "^SU") ~ "SU 2019/2022"
    ),
    sea = case_when(
      str_detect(category_id, "Sulu Sea$") ~ "Sulu Sea",
      str_detect(category_id, "Bohol Sea$") ~ "Bohol Sea"
    )
  ) %>%
  # 2) map color → survey, fill → sea
  ggplot(aes(x = N, y = s_mean, color = survey, fill = sea)) +
  geom_ribbon(aes(ymin = s_ci_lower, ymax = s_ci_upper), alpha = .3) +
  geom_line(size = 1) +
  scale_color_manual(name = "Survey", values = survey_colors) +
  scale_fill_manual(name = "Sea", values = sea_colors) +
  labs(
    x = "Samples",
    y = "Species Richness (S)"
  ) +
  scale_x_continuous(
    breaks = c(5, 10, 15),
    limits = c(1, 15)    # adjust if you want a little padding
  ) +
  scale_y_continuous(
    breaks = c(0, 200, 400, 600),
    limits = c(0, 600)
  ) +
  theme_classic() +
  theme(legend.position = "none")

## CHAO1. Using function defined above.
bind_rows(
  estaccumR_plot_chao(estaccumR_si_sulu$chao,  "SI 1978/1979 - Sulu Sea"),
  estaccumR_plot_chao(estaccumR_si_bohol$chao, "SI 1978/1979 - Bohol Sea"),
  estaccumR_plot_chao(estaccumR_su_sulu$chao,  "SU 2019/2022 - Sulu Sea"),
  estaccumR_plot_chao(estaccumR_su_bohol$chao, "SU 2019/2022 - Bohol Sea")
) %>%
  # 1) pull survey and sea back out into their own columns
  mutate(
    survey = case_when(
      str_detect(category_id, "^SI") ~ "SI 1978/1979",
      str_detect(category_id, "^SU") ~ "SU 2019/2022"
    ),
    sea = case_when(
      str_detect(category_id, "Sulu Sea$") ~ "Sulu Sea",
      str_detect(category_id, "Bohol Sea$") ~ "Bohol Sea"
    )
  ) %>%
  # 2) map color → survey, fill → sea
  ggplot(aes(x = N, y = chao_mean, color = survey, fill = sea)) +
  geom_ribbon(aes(ymin = chao_ci_lower, ymax = chao_ci_upper), alpha = .3) +
  geom_line(size = 1) +
  scale_color_manual(name = "Survey", values = survey_colors) +
  scale_fill_manual(name = "Sea", values = sea_colors) +
  labs(
    x = "Samples",
    y = "Estimated Species Richness (Chao1)"
  ) +
  scale_x_continuous(
    breaks = c(5, 10, 15),
    limits = c(1, 15)    # adjust if you want a little padding
  ) +
  scale_y_continuous(
    breaks = c(0, 200, 400, 600),
    limits = c(0, 600)
  ) +
  theme_classic() +
  theme(legend.position = "none")

## ACE. Using function defined above.
bind_rows(
  estaccumR_plot_ace(estaccumR_si_sulu$ace,  "SI 1978/1979 - Sulu Sea"),
  estaccumR_plot_ace(estaccumR_si_bohol$ace, "SI 1978/1979 - Bohol Sea"),
  estaccumR_plot_ace(estaccumR_su_sulu$ace,  "SU 2019/2022 - Sulu Sea"),
  estaccumR_plot_ace(estaccumR_su_bohol$ace, "SU 2019/2022 - Bohol Sea")
) %>%
  # 1) pull survey and sea back out into their own columns
  mutate(
    survey = case_when(
      str_detect(category_id, "^SI") ~ "SI 1978/1979",
      str_detect(category_id, "^SU") ~ "SU 2019/2022"
    ),
    sea = case_when(
      str_detect(category_id, "Sulu Sea$") ~ "Sulu Sea",
      str_detect(category_id, "Bohol Sea$") ~ "Bohol Sea"
    )
  ) %>%
  # 2) map color → survey, fill → sea
  ggplot(aes(x = N, y = ace_mean, color = survey, fill = sea)) +
  geom_ribbon(aes(ymin = ace_ci_lower, ymax = ace_ci_upper), alpha = .3) +
  geom_line(size = 1) +
  scale_color_manual(name = "Survey", values = survey_colors) +
  scale_fill_manual(name = "Sea", values = sea_colors) +
  labs(
    x = "Samples",
    y = "Estimated Species Richness (ACE)"
  ) +
  scale_x_continuous(
    breaks = c(5, 10, 15),
    limits = c(1, 15)    # adjust if you want a little padding
  ) +
  scale_y_continuous(
    breaks = c(0, 200, 400, 600),
    limits = c(0, 600)
  ) +
  theme_classic() +
  theme(legend.position = "none")
