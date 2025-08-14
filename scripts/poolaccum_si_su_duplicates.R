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


#### POOLACCUM LISTS ALL ####

poolaccum_all <- poolaccum(data_vegan, permutations=999, minsize = 3)
plot_poolaccum_all <- plot(poolaccum_all, ci.type="poly", col="black", lwd=2,
                           xlab="Samples", ylab="Species Richness",
                           alpha = 0.05)

summary(poolaccum_all, display, alpha = 0.05)

print(plot_poolaccum_all)

# investigate changing the minimum size for pooling sites. default seems to be 3. 
pool_all_3 <- poolaccum(data_vegan, permutations=999, minsize = 3)
# 2 increases standard deviation incredibly at the first data point at N = 2 
pool_all_2 <- poolaccum(data_vegan, permutations=999, minsize = 2)
plot(pool_all_3, ci.type="poly", col="black", lwd=2,
     xlab="N", ylab="Species Richness",
     xmin = 0, xmax = 42,
     ymin = 0, ymax = 1500)
plot(pool_all_2, ci.type="poly", col="black", lwd=2,
     xlab="N", ylab="Species Richness",
     xmin = 0, xmax = 42,
     ymin = 0, ymax = 1500)

specpool_study_sea <- specpool(data_vegan, data_vegan.env$study_sea, smallsample = TRUE)


boxplot(specnumber(data_vegan) ~ study_sea, data = data_vegan.env,
        col = "hotpink", border = "cyan3")

specpool2vect(X, index = c("jack1","jack2", "chao", "boot","Species"))

pool <- with(data_vegan.env, specpool(data_vegan, study_sea))

specpool_all <- specpool(data_vegan, data_vegan.env$study_sea, smallsample = TRUE)
pool_vec <- specpool2vect(specpool_all, index = c("jack1","jack2", "chao", "boot","Species"))

estimateR(data_vegan)

# by study
pool_all_study <- with(data_vegan.env, specpool(data_vegan, study, smallsample = TRUE))
plot(pool_all_study, ci.type="poly", col="black", lwd=2,
     xlab="Samples", ylab="Species Richness",
     alpha = 0.05)
# by sea
pool_all_sea <- with(data_vegan.env, specpool(data_vegan, sea, smallsample = TRUE))
# by study_sea
pool_all_study_sea <- with(data_vegan.env, specpool(data_vegan, study_sea, smallsample = TRUE))

#### POOLACCUM LISTS BY SURVEY ####

# "si_1978" = "#0072CE",  # Smithsonian Blue
# "su_2022" = "#800000" # ,  # Silliman Maroon

## SI ##
pool_si <- poolaccum(data_si_vegan, permutations=999, minsize = 3)
plot(pool_si, ci.type="poly", col="#0072CE", lwd=2,
     xlab="Samples", ylab="Species Richness",
     alph = 0.05)

## SU ##
pool_su <- poolaccum(data_su_vegan, permutations=999, minsize = 3)
plot(pool_su, ci.type="poly", col="#800000", lwd=2,
     xlab="Samples", ylab="Species Richness",
     alph = 0.05)


## testing specnumber & specpool2vect ##

boxplot(specnumber(data_vegan) ~ study, data = data_vegan.env,
        col = "#0072CE", border = "#007232")
boxplot(specnumber(data_vegan)/specpool2vect(pool) ~ study, 
        data = data_vegan.env,
        col = "#0072CE", border = "#007232")

summary(pool_all, display = "chao")
plot(pool_all$boot)
estimateR(data_vegan)

#### POOLACCUM LISTS BY SEA ####

# sea_colors <- c(
#   "Sulu Sea" = "#00829A",   # FMA 5
#   "Bohol Sea" = "#C6C8C5" #"#3F463E"    # FMA 9
# )

## Sulu ##
pool_sulu <- poolaccum(data_sulu_vegan, permutations=999, minsize = 3)
plot(pool_sulu, ci.type="poly", col="#00829A", lwd=2,
     xlab="Samples", ylab="Species Richness",
     alph = 0.05)

## Bohol ##
pool_bohol <- poolaccum(data_bohol_vegan, permutations=999, minsize = 3)
plot(pool_bohol, ci.type="poly", col="#C6C8C5", lwd=2,
     xlab="Samples", ylab="Species Richness",
     alph = 0.05)


#### POOLACCUM LISTS BY SURVEY & SEA ####

# survey_colors <- c(
#   "si_1978" = "#0072CE",  # Smithsonian Blue
#   "su_2022" = "#800000" # ,  # Silliman Maroon
# )
# 
# sea_colors <- c(
#   "Sulu Sea" = "#00829A",   # FMA 5
#   "Bohol Sea" = "#C6C8C5" #"#3F463E"    # FMA 9
# )

## SI Bohol ##
pool_si_bohol <- poolaccum(data_si_bohol_vegan, permutations=999, minsize = 3)
plot(pool_si_bohol, ci.type="poly", col="#0072CE", lwd=2,
     xlab="Samples", ylab="Species Richness",
     alph = 0.05)

## SU Bohol ##
pool_su_bohol <- poolaccum(data_su_bohol_vegan, permutations=999, minsize = 3)
plot(pool_su_bohol, ci.type="poly", col="#800000", lwd=2,
     xlab="Samples", ylab="Species Richness",
     alph = 0.05)

## SI Sulu ##
pool_si_sulu <- poolaccum(data_si_sulu_vegan, permutations=999, minsize = 3)
plot(pool_si_sulu, ci.type="poly", col="#0072CE", lwd=2,
     xlab="Samples", ylab="Species Richness",
     alph = 0.05)

## SU Sulu ##
pool_su_sulu <- poolaccum(data_su_sulu_vegan, permutations=999, minsize = 3)
plot(pool_su_sulu, ci.type="poly", col="#800000", lwd=2,
     xlab="Samples", ylab="Species Richness",
     alph = 0.05)


#### POOLACCUM PLOTs ALL ####
# function that takes the *replicate* matrix for one estimator
#    (e.g. pool_all$S, pool_all$chao, pool_all$jack1, etc.) and turns it
#    into a tibble with mean + 95% CI for each N:


## CHANGE THIS FUNCTION TO NOT MUTATE N TO ROW NUMBER ##

poolaccum_all <- poolaccum(data_vegan, permutations=999, minsize = 3)


## 1 
poolaccumR_tidy_all <- function(rep_mat, label) {
  as_tibble(rep_mat, rownames = "N") %>%   # Use rownames from poolaccum output as 'N'
    mutate(N = as.integer(N)) %>%          # Convert N from character to integer
    pivot_longer(
      cols = -N,
      names_to = "permutation",
      values_to = "richness"
    ) %>%
    group_by(N) %>%
    summarise(
      mean   = mean(richness),
      lower  = quantile(richness, 0.025),
      upper  = quantile(richness, 0.975),
      .groups = "drop"
    ) %>%
    mutate(estimator = label)
}



# 2
poolaccumR_tidy_all <- function(rep_mat, label) {
  as_tibble(rep_mat) %>%
    pivot_longer(
      cols = -N,                          # keep the existing N column unchanged
      names_to = "permutation",
      values_to = "richness"
    ) %>%
    group_by(N) %>%
    summarise(
      mean   = mean(richness, na.rm = TRUE),
      lower  = quantile(richness, 0.025, na.rm = TRUE),
      upper  = quantile(richness, 0.975, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(estimator = label)
}


# 3
poolaccumR_tidy_all <- function(rep_mat, label) {
  rep_mat %>%
    as.data.frame() %>%
    rownames_to_column("N") %>%
    mutate(N = as.integer(N)) %>%
    as_tibble() %>%
    pivot_longer(
      cols = -N,
      names_to = "permutation",
      values_to = "richness"
    ) %>%
    group_by(N) %>%
    summarise(
      mean   = mean(richness, na.rm = TRUE),
      lower  = quantile(richness, 0.025, na.rm = TRUE),
      upper  = quantile(richness, 0.975, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(estimator = label)
}

## This function changes N (pooled samples) to the row number, which it should not.
# poolaccumR_tidy_all <- function(rep_mat, label){
#   as_tibble(rep_mat) %>%               # columns = one permutation each
#     # mutate(N = row_number()) %>%       # N = how many samples pooled
#     pivot_longer(                      # stack all the permutations
#       cols      = N,
#       names_to  = "permutation",
#       values_to = "richness"
#     ) %>%
#     group_by(N) %>%
#     summarise(
#       mean       = mean(richness),
#       lower      = quantile(richness, 0.025),
#       upper      = quantile(richness, 0.975),
#       .groups    = "drop"
#     ) %>%
#     mutate(estimator = label)
# }

# 2) build a single data‐frame for *all* of the estimators
estimators <- c("S", "chao", "jack1", "jack2", "boot")
poolaccum_all <- map_dfr(estimators,
                         ~ poolaccumR_tidy_all(poolaccum_all[[.x]], .x)
)

# add 2 to column N to bring it back to 3:42 instead of 1:40, 
# which was caused by the function using rownames to substitute for N
poolaccum_all <- poolaccum_all %>%
  mutate(N = N + 2)

# 3) relabel the facets to human readable estimators
poolaccum_all <- poolaccum_all %>%
  mutate(
    estimator = recode(estimator,
                       S      = "Observed S",
                       chao   = "Chao",
                       jack1  = "Jackknife 1",
                       jack2  = "Jackknife 2",
                       boot   = "Bootstrap"
    ))

# Change the order
poolaccum_all <- poolaccum_all %>%
  mutate(estimator = factor(estimator,
                            levels = c("Observed S", "Chao", "Jackknife 1", "Jackknife 2", "Bootstrap")
  ))


# 4) now plot them all together
ggplot(poolaccum_all, aes(x = N, y = mean)) +
  geom_ribbon(aes(ymin = lower, ymax = upper),
              fill  = "grey70",   # your ribbon color
              alpha = .3) +
  geom_line(color = "black", size = 1) +
  facet_wrap(~ estimator,
             nrow   = 3,
             ncol   = 2,
             scales = "free_y"# ,
             # labeller = labeller(estimator = est_labels)
  ) +
  scale_x_continuous(
    breaks = c(0, 10, 20, 30, 40),
    limits = c(0, 42)
  ) +
  scale_y_continuous(
    breaks = seq(0, 1600, by = 200),
    limits = c(0, 1600)
  ) +
  labs(
    x = "Samples (pooled)",
    y = "Estimated Species Richness"
  ) +
  theme_classic(base_family = "Times New Roman", base_size = 12) +
  theme(
    legend.position      = c(0.85, 0.10),  # bottom‐right
    legend.justification = c(1, 0),
    legend.background    = element_rect(fill = "white", color = "black")
  )

# save figure
# ggsave(
#   filename = "../figures/si_su_duplicates/poolaccum_richness_estimates_species.png",
#   plot     = last_plot(),   # or give your plot object here
#   device   = "png",
#   width    = 6.5,
#   height   = 8,
#   units    = "in",
#   dpi      = 300
# )


#### POOLACCUM PLOTS BY SURVEY ####
# 1) Function that takes a poolaccum object + a survey label,
#    and returns a tibble with one row per N × estimator, with mean & CIs.
poolaccumR_tidy_survey <- function(pool_obj, survey_label) {
  # the five slot names in a poolaccum object
  est_names <- c("Observed S","Chao","Jackknife 1","Jackknife 2","Bootstrap")
  
  # for each estimator, grab its matrix and tidy it
  map_dfr(est_names, function(est) {
    mat <- pool_obj[[est]]
    as_tibble(mat) %>%
      mutate(N = row_number()) %>%
      pivot_longer(
        cols      = starts_with("V"),
        names_to  = "permutation",
        values_to = "value"
      ) %>%
      group_by(N) %>%
      summarize(
        mean  = mean(value),
        lower = quantile(value, 0.025),
        upper = quantile(value, 0.975),
        .groups = "drop"
      ) %>%
      mutate(
        survey    = survey_label,
        estimator = est
      )
  })
}


poolaccumR_tidy_survey <- function(pool_obj, survey_label) {
  # the five slot names in a poolaccum object
  est_names <- c("S","chao","jack1","jack2","boot")
  
  # for each estimator, grab its matrix and tidy it
  map_dfr(est_names, function(est) {
    mat <- pool_obj[[est]]
    as_tibble(mat) %>%
      mutate(N = row_number()) %>%
      pivot_longer(
        cols      = starts_with("V"),
        names_to  = "permutation",
        values_to = "value"
      ) %>%
      group_by(N) %>%
      summarize(
        mean  = mean(value),
        lower = quantile(value, 0.025),
        upper = quantile(value, 0.975),
        .groups = "drop"
      ) %>%
      mutate(
        survey    = survey_label,
        estimator = est
      )
  })
}

 
# 2) Tidy *both* surveys in one go:
poolaccum_survey <- bind_rows(
  poolaccumR_tidy_survey(pool_si, "Historical"),
  poolaccumR_tidy_survey(pool_su, "Modern")
)


# add 2 to column N to bring it back to 3:42 instead of 1:40, 
# which was caused by the function using rownames to substitute for N
poolaccum_survey <- poolaccum_survey %>%
  mutate(N = N + 2)

# Order for plot
poolaccum_survey <- poolaccum_survey %>%
  mutate(
    estimator = factor(estimator,
                       levels = c("S", "chao", "jack1", "jack2", "boot")
    )
  )

# 3) relabel the facets to human readable estimators
poolaccum_survey <- poolaccum_survey %>%
  mutate(
    estimator = recode(estimator,
                       S      = "Observed S",
                       chao   = "Chao",
                       jack1  = "Jackknife 1",
                       jack2  = "Jackknife 2",
                       boot   = "Bootstrap"
    ))



# 3) single ggplot with facets for each estimator
ggplot(poolaccum_survey, aes(x = N, y = mean, color = survey, fill = survey)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, color = NA) +
  geom_line(size = 1) +
  facet_wrap(~ estimator, nrow = 3, ncol = 2, scales = "free_y") +
  
  # fixed axes
  scale_x_continuous(
    breaks = c(0, 5, 10, 15, 20),
    limits = c(0, 21)
  ) +
  scale_y_continuous(
    breaks = seq(0, 1200, by = 200),
    limits = c(0, 1300)
  ) +
  
  # your survey colours
  scale_color_manual(
    name   = "Survey",
    values = c("Historical" = "#0072CE", "Modern" = "#800000")
  ) +
  scale_fill_manual(
    name   = "Survey",
    values = c("Historical" = "#0072CE", "Modern" = "#800000")
  ) +
  
  labs(
    x     = "Samples (pooled)",
    y     = "Estiamted Species Richness"
  ) +
  theme_classic() +
  theme_classic(base_family = "Times New Roman", base_size = 12) +
  # bottom‐right legend (x=1,y=0 is bottom‐right; pull it slightly inwards if you like)
  theme(
    legend.position      = c(0.85, 0.10),
    legend.justification = c(1, 0),
    legend.background    = element_rect(fill = "white", color = "black")
  )

# ggsave(
#   filename = "../figures/si_su_duplicates/poolaccum_richness_estimates_species_survey.png",
#   plot     = last_plot(),   # or give your plot object here
#   device   = "png",
#   width    = 6.5,
#   height   = 8,
#   units    = "in",
#   dpi      = 300
# )


#### POOLACCUM PLOTS BY SEA ####
# 1) function that takes a poolaccum object + a sea label,
#    and returns a tibble with one row per N × estimator, with mean & CIs.
poolaccumR_tidy_sea <- function(pool_obj, sea_label) {
  # the five slot names in a poolaccum object
  est_names <- c("S","chao","jack1","jack2","boot")
  
  # for each estimator, grab its matrix and tidy it
  map_dfr(est_names, function(est) {
    mat <- pool_obj[[est]]
    as_tibble(mat) %>%
      mutate(N = row_number()) %>%
      pivot_longer(
        cols      = starts_with("V"),
        names_to  = "permutation",
        values_to = "value"
      ) %>%
      group_by(N) %>%
      summarize(
        mean  = mean(value),
        lower = quantile(value, 0.025),
        upper = quantile(value, 0.975),
        .groups = "drop"
      ) %>%
      mutate(
        survey    = sea_label,
        estimator = est
      )
  })
}

# 2) Tidy *both* surveys in one go:
poolaccum_sea <- bind_rows(
  poolaccumR_tidy_sea(pool_bohol, "Bohol Sea"),
  poolaccumR_tidy_sea(pool_sulu, "Sulu Sea")
)

# which was caused by the function using rownames to substitute for N
poolaccum_sea <- poolaccum_sea %>%
  mutate(N = N + 2)


# Order for plot
poolaccum_sea <- poolaccum_sea %>%
  mutate(
    estimator = factor(estimator,
                       levels = c("S", "chao", "jack1", "jack2", "boot")
    )
  )

# 3) relabel the facets to human readable estimators
poolaccum_sea <- poolaccum_sea %>%
  mutate(
    estimator = recode(estimator,
                       S      = "Observed S",
                       chao   = "Chao",
                       jack1  = "Jackknife 1",
                       jack2  = "Jackknife 2",
                       boot   = "Bootstrap"
    ))

# 3) single ggplot with facets for each estimator
ggplot(poolaccum_sea, aes(x = N, y = mean, color = survey, fill = survey)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, color = NA) +
  geom_line(size = 1) +
  facet_wrap(~ estimator, nrow = 3, ncol = 2, scales = "free_y") +
  
  # fixed axes
  scale_x_continuous(
    breaks = c(0, 5, 10, 15, 20, 25),
    limits = c(0, 26)
  ) +
  scale_y_continuous(
    breaks = seq(0, 1200, by = 200),
    limits = c(0, 1300)
  ) +
  
  # your survey colours
  scale_color_manual(
    name   = "Sea",
    values = c("Bohol Sea" = "#C6C8C5", "Sulu Sea" = "#00829A")
  ) +
  scale_fill_manual(
    name   = "Sea",
    values = c("Bohol Sea" = "#C6C8C5", "Sulu Sea" = "#00829A")
  ) +
  
  labs(
    x     = "Samples (pooled)",
    y     = "Estimated Species Richness"
  ) +
  theme_classic() +
  theme_classic(base_family = "Times New Roman", base_size = 12) +
  # bottom‐right legend (x=1,y=0 is bottom‐right; pull it slightly inwards if you like)
  theme(
    legend.position      = c(0.85, 0.10),
    legend.justification = c(1, 0),
    legend.background    = element_rect(fill = "white", color = "black")
  )

# ggsave(
#   filename = "../figures/si_su_duplicates/poolaccum_richness_estimates_species_sea.png",
#   plot     = last_plot(),   # or give your plot object here
#   device   = "png",
#   width    = 6.5,
#   height   = 8,
#   units    = "in",
#   dpi      = 300
# )


#### POOLACCUM PLOTS BY SURVEY & SEA ####
# 1) A helper that takes a poolaccum object + a survey X sea label,
#    and returns a tibble with one row per N × estimator, with mean & CIs.
poolaccumR_tidy_survey_sea <- function(pool_obj, survey_sea_label) {
  # the five slot names in a poolaccum object
  est_names <- c("S","chao","jack1","jack2","boot")
  
  # for each estimator, grab its matrix and tidy it
  map_dfr(est_names, function(est) {
    mat <- pool_obj[[est]]
    as_tibble(mat) %>%
      mutate(N = row_number()) %>%
      pivot_longer(
        cols      = starts_with("V"),
        names_to  = "permutation",
        values_to = "value"
      ) %>%
      group_by(N) %>%
      summarize(
        mean  = mean(value),
        lower = quantile(value, 0.025),
        upper = quantile(value, 0.975),
        .groups = "drop"
      ) %>%
      mutate(
        survey    = survey_sea_label,
        estimator = est
      )
  })
}

# 2) Tidy surveys in one go:
poolaccum_survey_sea <- bind_rows(
  poolaccumR_tidy_survey_sea(pool_si_bohol, "Historical Bohol Sea"),
  poolaccumR_tidy_survey_sea(pool_si_sulu, "Historical Sulu Sea"),
  poolaccumR_tidy_survey_sea(pool_su_bohol, "Modern Bohol Sea"),
  poolaccumR_tidy_survey_sea(pool_su_sulu, "Modern Sulu Sea")
)


# which was caused by the function using rownames to substitute for N
poolaccum_survey_sea <- poolaccum_survey_sea %>%
  mutate(N = N + 2)

# Order for plot
poolaccum_survey_sea <- poolaccum_survey_sea %>%
  mutate(
    estimator = factor(estimator,
                       levels = c("S", "chao", "jack1", "jack2", "boot")
    )
  )

# 3) relabel the facets to human readable estimators
poolaccum_survey_sea <- poolaccum_survey_sea %>%
  mutate(
    estimator = recode(estimator,
                       S      = "Observed S",
                       chao   = "Chao",
                       jack1  = "Jackknife 1",
                       jack2  = "Jackknife 2",
                       boot   = "Bootstrap"
    ))

# 3) single ggplot with facets for each estimator
ggplot(poolaccum_survey_sea, aes(x = N, y = mean, color = survey, fill = survey)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, color = NA) +
  geom_line(size = 1) +
  facet_wrap(~ estimator, nrow = 3, ncol = 2, scales = "free_y") +
  
  # fixed axes
  scale_x_continuous(
    breaks = c(0, 2, 4, 6, 8, 10, 12, 14),
    limits = c(0, 14)
  ) +
  scale_y_continuous(
    breaks = seq(0, 1000, by = 200),
    limits = c(0, 1000)
  ) +
  scale_color_manual(
    name   = "Survey X Sea",
    values = c("Historical Bohol Sea" = "#0072CE", "Modern Bohol Sea" = "#800000", "Historical Sulu Sea" = "#0072CE", "Modern Sulu Sea" = "#800000")
  ) +
  scale_fill_manual(
    name   = "Survey X Sea",
    values = c("Historical Bohol Sea" = "#C6C8C5", "Modern Bohol Sea" = "#C6C8C5", "Historical Sulu Sea" = "#00829A", "Modern Sulu Sea" = "#00829A")
    # values = c("Bohol Sea" = "#C6C8C5", "Sulu Sea" = "#00829A", "Historical" = "#0072CE", "Modern" = "#800000")
  ) +
  
  labs(
    x     = "Samples (pooled)",
    y     = "Estimated Species Richness"
  ) +
  theme_classic() +
  theme_classic(base_family = "Times New Roman", base_size = 12) +
  # bottom‐right legend (x=1,y=0 is right, bottom)
  theme(
    legend.position      = c(0.92, 0.05),
    legend.justification = c(1, 0),
    legend.background    = element_rect(fill = "white", color = "black")
  )

ggsave(
  filename = "../figures/si_su_duplicates/poolaccum_richness_estimates_species_survey_sea.png",
  plot     = last_plot(),   # or give your plot object here
  device   = "png",
  width    = 6.5,
  height   = 8,
  units    = "in",
  dpi      = 300
)


#### ANOVA POOLACCUM ####
library(lme4)
library(lmerTest)

estimateR(data_vegan)

# 1. Add rownames as a column
df <- data_vegan %>% rownames_to_column("station_code")

# 2. Identify the species columns (assume all but first column are numeric)
species_cols <- setdiff(names(df), "station_code")

# Calculate richness per site for each estimator
site_estimates <- data_vegan %>%
  dplyr::mutate(
    
    S        = rowSums(across(all_of(species_cols), ~ . != 0)), # Observed S
    # s.obs    = apply(select(., all_of(species_cols)), 1, function(x) vegan::estimateR(as.numeric(x))["S.obs"]),
    # s.chao1  = apply(select(., all_of(species_cols)), 1, function(x) vegan::estimateR(as.numeric(x))["S.chao1"]),
    # s.ace    = apply(select(., all_of(species_cols)), 1, function(x) vegan::estimateR(as.numeric(x))["S.ACE"]),
    chao     = apply(select(., all_of(species_cols)), 1, function(x) vegan::specpool(matrix(as.numeric(x), nrow=1), data_vegan.env$station_code, smallsample = TRUE)$chao),
    jack1    = apply(select(., all_of(species_cols)), 1, function(x) vegan::specpool(matrix(as.numeric(x), nrow=1), data_vegan.env$station_code, smallsample = TRUE)$jack1),
    jack2    = apply(select(., all_of(species_cols)), 1, function(x) vegan::specpool(matrix(as.numeric(x), nrow=1), data_vegan.env$station_code, smallsample = TRUE)$jack2),
    boot     = apply(select(., all_of(species_cols)), 1, function(x) vegan::specpool(matrix(as.numeric(x), nrow=1), data_vegan.env$station_code, smallsample = TRUE)$boot)
    
    # s.obs   = apply(., 1, function(x) vegan::estimateR(x)['S.obs']),
    # s.ace   = apply(., 1, function(x) vegan::estimateR(x)['S.chao1']),
    # s.chao  = apply(., 1, function(x) vegan::estimateR(x)['S.chao1']),
    # chao    = apply(., 1, function(x) vegan::specpool(x, data_vegan.env$station_code, smallsample = TRUE)$chao),
    # jack1   = apply(., 1, function(x) vegan::specpool(x, data_vegan.env$station_code, smallsample = TRUE)$jack1),
    # jack2   = apply(., 1, function(x) vegan::specpool(x, data_vegan.env$station_code, smallsample = TRUE)$jack2),
    # boot    = apply(., 1, function(x) vegan::specpool(x, data_vegan.env$station_code, smallsample = TRUE)$boot)
  ) %>%
  dplyr::select(nice_station_code, S, s.obs, s.ace, s.chao, chao, jack1, jack2, boot)

# Join with metadata
site_estimates <- left_join(
  site_estimates,
  data_vegan.env %>% select(nice_station_code, study, sea),
  by = "nice_station_code"
)

estimateR_all <- estimateR(data_vegan)
estimateR_all <- data_frame(estimateR_all)
specpool(data_vegan, data_vegan.env$station_code, smallsample = TRUE)$chao

specpool_all <- specpool(data_vegan, data_vegan.env$station_code, smallsample = TRUE)
specpool_all <- specpool_all %>%
  rownames_to_column("station_code")
specpool_all <- left_join(
  specpool_all,
  data_vegan.env %>% select(station_code, nice_station_code, study, sea),
  by = "station_code"
)




pool_all_sea <- with(data_vegan.env, specpool(data_vegan, sea, smallsample = TRUE))

#### MIXED-EFFECTS MODEL POOLACCUM ####