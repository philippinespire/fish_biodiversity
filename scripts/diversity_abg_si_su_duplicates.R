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
    "lme4",
    "lmerTest",
    "car", 
    "broom"
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


#### WRANGLE VEGANIZED DATA ####
# change the rownames in data_vegan to the nice_station_code
# Create a lookup table of old station_code to new nice_station_code
lookup <- data_vegan.env %>%
  dplyr::select(station_code, nice_station_code)

# Reorder the labels to match your data_vegan row order
new_labels <- lookup$nice_station_code[match(rownames(data_vegan), lookup$station_code)]
rownames(data_vegan) <- new_labels

# check that the rows are aligned by station_code
identical(rownames(data_vegan), data_vegan.env$nice_station_code)


#### SUBSAMPLE VEGANIZED DATA ####

# BY STUDY 
data_si_vegan <- data_vegan[ data_vegan.env$study == "si_1978", ]
data_su_vegan <- data_vegan[ data_vegan.env$study == "su_2022", ]

# BY SEA
data_sulu_vegan <- data_vegan[ data_vegan.env$sea == "sulu", ]
data_bohol_vegan <- data_vegan[ data_vegan.env$sea == "bohol", ]

# BY STUDY & SEA

data_si_sulu_vegan  <- data_vegan[ data_vegan.env$study_sea == "si_1978_sulu_sea",  ]
data_su_sulu_vegan  <- data_vegan[ data_vegan.env$study_sea == "su_2022_sulu_sea",  ]
data_si_bohol_vegan <- data_vegan[ data_vegan.env$study_sea == "si_1978_bohol_sea", ]
data_su_bohol_vegan <- data_vegan[ data_vegan.env$study_sea == "su_2022_bohol_sea", ]

# these dplyr code wasn't working, so I made the base R code above. Should do the same thing. 
# data_si_sulu_vegan <- data_vegan.env %>%
#   filter(study=="si_1978", sea=="sulu") %>%
#   pull(station_code) %>%        # get the station_codes
#   { data_vegan[rownames(data_vegan) %in% ., ] }
# 
# data_su_sulu_vegan <- data_vegan.env %>%
#   filter(study=="su_2022", sea=="sulu") %>%
#   pull(station_code) %>%        # get the station_codes
#   { data_vegan[rownames(data_vegan) %in% ., ] }
# 
# data_si_bohol_vegan <- data_vegan.env %>%
#   filter(study=="si_1978", sea=="bohol") %>%
#   pull(station_code) %>%        # get the station_codes
#   { data_vegan[rownames(data_vegan) %in% ., ] }
# 
# data_su_bohol_vegan <- data_vegan.env %>%
#   filter(study=="su_2022", sea=="bohol") %>%
#   pull(station_code) %>%        # get the station_codes
#   { data_vegan[rownames(data_vegan) %in% ., ] }


#### FISHERFIT #### 
## fisherfit function by study_sea

fisherfit(data_si_bohol_vegan)
# Fisher log series model
# No. of species: 911 
# Fisher alpha:   271.0948
fisherfit(data_su_bohol_vegan)
# Fisher log series model
# No. of species: 796 
# Fisher alpha:   329.1691 
## In the Bohol Sea, Fisher alpha increased despite no. species decreasing

fisherfit(data_si_sulu_vegan)
# Fisher log series model
# No. of species: 806 
# Fisher alpha:   228.5484 
fisherfit(data_su_sulu_vegan)
# Fisher log series model
# No. of species: 696 
# Fisher alpha:   184.7561
# In the Sulu Sea, Fisher alpha and no. species decreased


#### ALPHA DIVERSITY BY STATION ####
## USE THIS IN MANUSCRIPT ##
## DO NOT POOL 
# diversity function in vegan package

# — 1) Compute all five indices per station — 
#    (assumes data_vegan and data_vegan.env are aligned row‐wise)
# if false, reorder data_vegan.env to match data_vegan
# data_vegan.env <- data_vegan.env[match(rownames(data_vegan), data_vegan.env$station_code), ]

div_idx <- tibble(
  nice_station_code  = data_vegan.env$nice_station_code,
  S             = specnumber(data_vegan),
  H             = diversity(data_vegan,      index = "shannon"),
  J             = H/log(S),      # Pielou's Evenness
  simpson       = diversity(data_vegan,      index = "simpson"),
  invsimpson    = diversity(data_vegan,      index = "inv"),
  unbias_simp   = simpson.unb(data_vegan,    inverse = FALSE),        # unbiased Simpson
  alpha         = fisher.alpha(data_vegan)        # Fisher α
)

# — 2) Merge back onto the metadata —
div_df <- data_vegan.env %>%
  left_join(div_idx, by = "nice_station_code")

# clean up div_idx and div_df. They are the same dataframe. Just use div_df, which has all the metadata. 
print(div_df)

# save file
# outdir <- "../tables/si_su_duplicates"
# outfile <- file.path(outdir, "table_diversity_alpha_station.csv")
# write_csv(div_df, outfile)


#### LME MODELS ON ALPHA DIVERSITY METRICS BY STATION ####
## USE THIS ##
# linear mixed effects model for each diversity metric
# Fit mixed model with random intercept for station_pair
# metric ~ study * sea + (1 | station_pair)
# Fixed effects test for overall changes by time, sea, and their interaction.
# Fits a random intercept for each station pair, correctly modeling the paired design
# Each station_pair is measured twice (once per survey period), so these are paired, repeated measures.
# By using (1 | station_pair), baseline differences between locations are accounted for.

# mod_metric <- lmer(metric ~ study * sea + (1 | nice_station_pair), data = div_df)
# mod_S       <- lmer(S ~ study * sea + (1 | nice_station_pair), data = div_df)
mod_H       <- lmer(H ~ study * sea +           (1 | nice_station_pair), data = div_df)
mod_J       <- lmer(J ~ study * sea +           (1 | nice_station_pair), data = div_df)
mod_simpson <- lmer(simpson ~ study * sea +     (1 | nice_station_pair), data = div_df)
mod_inv     <- lmer(invsimpson ~ study * sea +  (1 | nice_station_pair), data = div_df)
mod_unb     <- lmer(unbias_simp ~ study * sea + (1 | nice_station_pair), data = div_df)
mod_alpha   <- lmer(alpha ~ study * sea +       (1 | nice_station_pair), data = div_df)

# anova(mod_H) # Type I tests. DO NOT use this. It's not appropriate.
# car::Anova(mod_metric,       type = "III") # Type II/III tests
# anova_S       <- car::Anova(mod_S,       type = "III") 
anova_H       <- car::Anova(mod_H,       type = "III") # Type II/III tests
anova_J       <- car::Anova(mod_J,       type = "III") 
anova_simpson <- car::Anova(mod_simpson, type = "III")
anova_inv     <- car::Anova(mod_inv,     type = "III")
anova_unb     <- car::Anova(mod_unb,     type = "III")
anova_alpha   <- car::Anova(mod_alpha,   type = "III")

df_anova <- bind_rows(
  # tidy(car::Anova(mod_S,       type = "III"))        %>% mutate(metric = "S"),
  tidy(car::Anova(mod_H,       type = "III"))        %>% mutate(metric = "H"),
  tidy(car::Anova(mod_J,       type = "III"))        %>% mutate(metric = "J"),
  tidy(car::Anova(mod_simpson, type = "III"))        %>% mutate(metric = "simpson"),
  tidy(car::Anova(mod_inv,     type = "III"))        %>% mutate(metric = "invsimpson"),
  tidy(car::Anova(mod_unb,     type = "III"))        %>% mutate(metric = "unbias_simp"),
  tidy(car::Anova(mod_alpha,   type = "III"))        %>% mutate(metric = "alpha")
)

# fixef_metric <- fixef(mod_metric)
# fixef_S       <- fixef(mod_S)
fixef_H       <- fixef(mod_H)
fixef_J       <- fixef(mod_J)
fixef_simpson <- fixef(mod_simpson)
fixef_inv     <- fixef(mod_inv)
fixef_unb     <- fixef(mod_unb)
fixef_alpha   <- fixef(mod_alpha)

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
  # tidy_fixef(mod_S, "S"),
  tidy_fixef(mod_H, "H"),
  tidy_fixef(mod_J, "J"),
  tidy_fixef(mod_simpson, "simpson"),
  tidy_fixef(mod_inv, "invsimpson"),
  tidy_fixef(mod_unb, "unbias_simp"),
  tidy_fixef(mod_alpha, "alpha")
)

print(df_fixef)

# Make sure both dataframes have the same 'metric' and 'term' columns
# harmonizing column for term for df_fixef
df_fixef <- df_fixef %>%
  mutate(
    term_anova = case_when(
      term == "(Intercept)"              ~ "(Intercept)",
      term == "studysu_2022"             ~ "study",
      term == "seasulu"                  ~ "sea",
      term == "studysu_2022:seasulu"     ~ "study:sea",
      TRUE                               ~ NA_character_
    )
  )

summary_table <- df_fixef %>%
  left_join(
    df_anova %>% dplyr::rename(term_anova = term),
    by = c("metric", "term_anova")
  )

summary_table <- summary_table %>%
  dplyr::select(metric, term_anova, term, everything()) %>%
  dplyr::rename(
    model_term        = term_anova,
    estimate_fixef    = estimate,
    chi_square_anova  = statistic
  )

print(summary_table)

# save file
# outdir <- "../tables/si_su_duplicates"
# outfile <- file.path(outdir, "table_diversity_alpha_station_lme.csv")
# write_csv(summary_table, outfile)


#### LME POST-HOC ####
# install.packages("emmeans")  # if not already installed
library(emmeans)

# Function to run emmeans pairwise comparisons and return tidy summary
posthoc_metric <- function(mod, metric_label) {
  em <- emmeans(mod, ~ study * sea)
  posthoc <- pairs(em, adjust = "tukey")  # all pairwise comparisons
  tidy_ph <- as_tibble(summary(posthoc))  # convert to tibble
  tidy_ph$metric <- metric_label          # add metric column
  return(tidy_ph)
}

# Run function for all your models
df_posthoc <- bind_rows(
  posthoc_metric(mod_H, "H"),
  posthoc_metric(mod_J, "J"),
  posthoc_metric(mod_simpson, "simpson"),
  posthoc_metric(mod_inv, "invsimpson"),
  posthoc_metric(mod_unb, "unbias_simp"),
  posthoc_metric(mod_alpha, "alpha")
)

# Reorder columns for clarity
df_posthoc <- df_posthoc %>%
  dplyr::select(metric, contrast, estimate, SE, df, t.ratio, p.value)

print(df_posthoc)

df_posthoc <- df_posthoc %>%
  dplyr::mutate(
    contrast = dplyr::recode(contrast,
                             "si_1978 bohol - su_2022 bohol" = "Historical Bohol - Modern Bohol",
                             "si_1978 bohol - si_1978 sulu"  = "Historical Bohol - Historical Sulu",
                             "si_1978 bohol - su_2022 sulu"  = "Historical Bohol - Modern Sulu",
                             "su_2022 bohol - si_1978 sulu"  = "Modern Bohol - Historical Sulu",
                             "su_2022 bohol - su_2022 sulu"  = "Modern Bohol - Modern Sulu",
                             "si_1978 sulu - su_2022 sulu"   = "Historical Sulu - Modern Sulu"      
                             ))

# save file
# outdir <- "../tables/si_su_duplicates"
# outfile <- file.path(outdir, "table_diversity_alpha_lme_station_posthoc.csv")
# write_csv(df_posthoc, outfile)



#### ALPHA DIVERSITY GROUPED (POOLED) ####
# diversity function in vegan package

## BY STUDY SEA (4 groups) ##
div_idx_poolgroup_studysea <- tibble(
  study_sea     = unique(data_vegan.env$study_sea),
  S             = specnumber(data_vegan, data_vegan.env$study_sea),
  H             = diversity(data_vegan,   index = "shannon", data_vegan.env$study_sea, equalize.groups = FALSE, MARGIN = 1, base = exp(1)),
  H_eq          = diversity(data_vegan,   index = "shannon", data_vegan.env$study_sea, equalize.groups = TRUE, MARGIN = 1, base = exp(1)),
  J             = H/log(S),      # Pielou's Evenness
  J_eq          = H_eq/log(S),   # Pielou's Evenness using H equalized by group
  simpson       = diversity(data_vegan,   index = "simpson", data_vegan.env$study_sea, equalize.groups = FALSE, MARGIN = 1, base = exp(1)),
  simpson_eq    = diversity(data_vegan,   index = "simpson", data_vegan.env$study_sea, equalize.groups = TRUE, MARGIN = 1, base = exp(1)),
  invsimpson    = diversity(data_vegan,   index = "inv", data_vegan.env$study_sea, equalize.groups = FALSE, MARGIN = 1, base = exp(1)),
  invsimpson_eq = diversity(data_vegan,   index = "inv", data_vegan.env$study_sea, equalize.groups = TRUE, MARGIN = 1, base = exp(1))
)

# print(div_idx_poolgroup_studysea)

# study_sea             S     H  H_eq     J  J_eq simpson simpson_eq invsimpson invsimpson_eq
# <chr>             <int> <dbl> <dbl> <dbl> <dbl>   <dbl>      <dbl>      <dbl>         <dbl>
#   1 si_1978_bohol_sea   460  4.85  5.02 0.792 0.818   0.983      0.987       58.4          77.9
# 2 si_1978_sulu_sea    456  4.99  5.10 0.815 0.833   0.987      0.989       76.5          93.8
# 3 su_2022_bohol_sea   396  4.79  4.83 0.802 0.808   0.981      0.983       52.2          58.0
# 4 su_2022_sulu_sea    410  3.90  4.67 0.649 0.776   0.909      0.977       11.0          43.0

# save file
# outdir <- "../tables/si_su_duplicates"
# outfile <- file.path(outdir, "table_diversity_alpha_group_pool_equalize_studysea.csv")
# write_csv(div_idx_poolgroup_studysea, outfile)


## BY STUDY (2 groups) ##
div_idx_poolgroup_study <- tibble(
  study         = unique(data_vegan.env$study),
  S             = specnumber(data_vegan, data_vegan.env$study),
  H             = diversity(data_vegan,   index = "shannon", data_vegan.env$study, equalize.groups = FALSE, MARGIN = 1, base = exp(1)),
  H_eq          = diversity(data_vegan,   index = "shannon", data_vegan.env$study, equalize.groups = TRUE, MARGIN = 1, base = exp(1)),
  J             = H/log(S),      # Pielou's Evenness
  J_eq          = H_eq/log(S),   # Pielou's Evenness using H equalized by group
  simpson       = diversity(data_vegan,   index = "simpson", data_vegan.env$study, equalize.groups = FALSE, MARGIN = 1, base = exp(1)),
  simpson_eq    = diversity(data_vegan,   index = "simpson", data_vegan.env$study, equalize.groups = TRUE, MARGIN = 1, base = exp(1)),
  invsimpson    = diversity(data_vegan,   index = "inv", data_vegan.env$study, equalize.groups = FALSE, MARGIN = 1, base = exp(1)),
  invsimpson_eq = diversity(data_vegan,   index = "inv", data_vegan.env$study, equalize.groups = TRUE, MARGIN = 1, base = exp(1))
)

# print(div_idx_poolgroup_study)
# save file
# outfile <- file.path(outdir, "table_diversity_alpha_group_pool_equalize_study.csv")
# write_csv(div_idx_poolgroup_study, outfile)


## BY SEA (2 groups) ##
div_idx_poolgroup_sea <- tibble(
  sea           = unique(data_vegan.env$sea),
  S             = specnumber(data_vegan, data_vegan.env$sea),
  H             = diversity(data_vegan,   index = "shannon", data_vegan.env$sea, equalize.groups = FALSE, MARGIN = 1, base = exp(1)),
  H_eq          = diversity(data_vegan,   index = "shannon", data_vegan.env$sea, equalize.groups = TRUE, MARGIN = 1, base = exp(1)),
  J             = H/log(S),      # Pielou's Evenness
  J_eq          = H_eq/log(S),   # Pielou's Evenness using H equalized by group
  simpson       = diversity(data_vegan,   index = "simpson", data_vegan.env$sea, equalize.groups = FALSE, MARGIN = 1, base = exp(1)),
  simpson_eq    = diversity(data_vegan,   index = "simpson", data_vegan.env$sea, equalize.groups = TRUE, MARGIN = 1, base = exp(1)),
  invsimpson    = diversity(data_vegan,   index = "inv", data_vegan.env$sea, equalize.groups = FALSE, MARGIN = 1, base = exp(1)),
  invsimpson_eq = diversity(data_vegan,   index = "inv", data_vegan.env$sea, equalize.groups = TRUE, MARGIN = 1, base = exp(1))
)

# print(div_idx_poolgroup_sea)
# save file
# outfile <- file.path(outdir, "table_diversity_alpha_group_pool_equalize_sea.csv")
# write_csv(div_idx_poolgroup_sea, outfile)



## BY STATION PAIR (21 groups) ##
# unable to examine temporal change, so not a good option
# div_idx_poolgroup_stationpair <- tibble(
#   station_pair  = unique(data_vegan.env$nice_station_pair),
#   H             = diversity(data_vegan,   index = "shannon", data_vegan.env$nice_station_pair, MARGIN = 1, base = exp(1)),
#   simpson       = diversity(data_vegan,   index = "simpson", data_vegan.env$nice_station_pair, MARGIN = 1, base = exp(1)),
#   invsimpson    = diversity(data_vegan,   index = "inv", data_vegan.env$nice_station_pair, MARGIN = 1, base = exp(1))
# )
# 
# print(div_idx_poolgroup_stationpair)


#### ALPHA BETA GAMMA ####

## BY STUDY SEA ##
# beta is beta diversity defined as gamma/alpha - 1
# alpha is the average no. of species in a group
# gamma is the total number of species in the group

# alpha_add is the average Shannon H for each group calculated from Shannon H indices for each station
# gamma_add is the pooled Shannon H for each group calculated from pooling stations by group
# beta_add is the additive beta diversity based on Shannon Index. defined as gamma-alpha

div_abg_idx_poolgroup_studysea <- tibble(
  study_sea  = unique    (data_vegan.env$study_sea),
  alpha      = as.numeric(with(data_vegan.env, tapply(specnumber(data_vegan), study_sea, mean))), 
  gamma      = as.numeric(with(data_vegan.env, specnumber(data_vegan, study_sea))),
  beta       = as.numeric(gamma/alpha -1),
  alpha_ave  = as.numeric(with(data_vegan.env, tapply(diversity(data_vegan), study_sea, mean))), # average
  gamma_pool = as.numeric(with(data_vegan.env, diversity(data_vegan, groups=study_sea))), # pooled
  beta_add   = as.numeric(gamma_pool-alpha_ave) ## additive beta diversity based on average Shannon H
)

# print(div_abg_idx_poolgroup_studysea)

# save file
# outfile <- file.path(outdir, "table_diversity_abg_group_studysea.csv")
# write_csv(div_abg_idx_poolgroup_studysea, outfile)


## BY STUDY ##
div_abg_idx_poolgroup_study <- tibble(
  study      = unique    (data_vegan.env$study),
  alpha      = as.numeric(with(data_vegan.env, tapply(specnumber(data_vegan), study, mean))), 
  gamma      = as.numeric(with(data_vegan.env, specnumber(data_vegan, study))),
  beta       = as.numeric(gamma/alpha -1),
  alpha_ave  = as.numeric(with(data_vegan.env, tapply(diversity(data_vegan), study, mean))), # average
  gamma_pool = as.numeric(with(data_vegan.env, diversity(data_vegan, groups=study))), # pooled
  beta_add   = as.numeric(gamma_pool-alpha_ave) ## additive beta diversity based on average Shannon H
)

# print(div_abg_idx_poolgroup_study)

# save file
# outfile <- file.path(outdir, "table_diversity_abg_group_study.csv")
# write_csv(div_abg_idx_poolgroup_study, outfile)


## BY SEA ##
div_abg_idx_poolgroup_sea <- tibble(
  sea        = unique    (data_vegan.env$sea),
  alpha      = as.numeric(with(data_vegan.env, tapply(specnumber(data_vegan), sea, mean))), 
  gamma      = as.numeric(with(data_vegan.env, specnumber(data_vegan, sea))),
  beta       = as.numeric(gamma/alpha -1),
  alpha_ave  = as.numeric(with(data_vegan.env, tapply(diversity(data_vegan), sea, mean))), # average
  gamma_pool = as.numeric(with(data_vegan.env, diversity(data_vegan, groups=sea))), # pooled
  beta_add   = as.numeric(gamma_pool-alpha_ave) ## additive beta diversity based on average Shannon H
)

# print(div_abg_idx_poolgroup_sea)

# save file
# outfile <- file.path(outdir, "table_diversity_abg_group_sea.csv")
# write_csv(div_abg_idx_poolgroup_sea, outfile)


#### STATION PAIRS PLOTS ####
## BY STUDY ##
# (base R):
for(g in unique(div_df$study)){
  sub <- filter(div_df, study == g) 
  pairs(
    sub[, c("H","simpson","invsimpson","unbias_simp","alpha")],
    main = paste("diversity indices —", g),
    pch  = 21, bg = "steelblue"
  )
}

# (ggpairs from GGally, if you prefer ggplot):
# install.packages("GGally")
# library(GGally)
# div_df %>%
#   group_nest(study) %>%
#   mutate(
#     plot = map2(data, study, ~ ggpairs(.x[,c("H","simpson","invsimpson","unbias_simp","alpha")]) +
#                    ggtitle(.y))
#   ) %>%
#   pull(plot) %>%
#   walk(print)

## BY SEA ##
# (base R):
for(g in unique(div_df$sea)){
  sub <- filter(div_df, sea == g) 
  pairs(
    sub[, c("H","simpson","invsimpson","unbias_simp","alpha")],
    main = paste("diversity indices —", g),
    pch  = 21, bg = "steelblue"
  )
}


## BY STUDY X SEA ##
div_df %>%
  dplyr::group_nest(study, sea) %>%
  dplyr::mutate(
    plot = map2(data, paste(study, sea, sep=" | "),
                ~ pairs(
                  .x[,c("H","simpson","invsimpson","unbias_simp","alpha")],
                  main = .y,
                  pch  = 21,
                  bg   = "tomato"
                ))
  ) %>%
  invisible()   # all panels will pop up in your graphics device


#### ALPHA DIVERSITY BETWEEN DUPLICATE STATIONS ####
# compare with the two-way repeated measures ANOVA below instead of a paired t-test

## Already created above, can be deleted
# 1) compute metrics, keep only rows with a station_pair
div_df <- data_vegan.env %>%
  filter(!is.na(station_pair)) %>%   # station_pair column between duplicates
  mutate(
    S           = specnumber(data_vegan),
    H           = diversity(data_vegan,   index = "shannon", equalize.groups = FALSE, MARGIN = 1, base = exp(1)),
    J           = H/log(S),      # Pielou's Evenness
    simpson     = diversity(data_vegan,     index = "simpson"),
    invsimpson  = diversity(data_vegan,     index = "inv"),
    unbias_simp = simpson.unb(data_vegan),
    alpha       = fisher.alpha(data_vegan)
  ) %>%
  dplyr::select(station_code, nice_station_code, station_pair, nice_station_pair, sea, study, S, H, J, simpson, invsimpson, unbias_simp, alpha)


# 2) Pivot by station_pair
paired <- div_df %>%
  pivot_wider(
    id_cols      = c(station_pair, sea),
    names_from   = study,
    values_from  = c(S, H, J, simpson, invsimpson, unbias_simp, alpha),
    names_sep    = "_"
  ) %>%
  # only keep those that have both SI and SU values
  filter(!is.na(H_si_1978) & !is.na(H_su_2022))

# 3) compute Δ = SU − SI for each metric
paired <- paired %>%
  mutate(
    delta_S           = S_su_2022           - S_si_1978,
    delta_H           = H_su_2022           - H_si_1978,
    delta_J           = J_su_2022           - J_si_1978,
    delta_simpson     = simpson_su_2022     - simpson_si_1978,
    delta_invsimpson  = invsimpson_su_2022  - invsimpson_si_1978,
    delta_unbias_simp = unbias_simp_su_2022 - unbias_simp_si_1978,
    delta_alpha       = alpha_su_2022       - alpha_si_1978
  )

# add a nice_station_pair from the nice_station_code
df_station_pair <- data_vegan.env %>%
  dplyr::select(station_pair, nice_station_pair) %>%
  distinct()

paired <- paired %>%
  left_join(df_station_pair, by = "station_pair")

paired <- paired %>%
  dplyr::select(nice_station_pair, station_pair, sea, dplyr::everything())

# save file as table. 
# outdir <- "../tables/si_su_duplicates"
# outfile <- file.path(outdir, "table_diversity_alpha_by_paired_station.csv")
# write_csv(paired, outfile)


#### ANOVA - TWO-WAY REPEATED MEASURES ####
# DON'T USE THIS METHOD
# function with base R
run_diversity_anova <- function(metric, data = paired) {
  library(dplyr)
  library(tidyr)
  
  # 1) pivot the paired‐wide data into long form for this metric
  long_df <- data %>%
    pivot_longer(
      cols      = starts_with(paste0(metric, "_")),
      names_to  = "study",
      values_to = metric
    ) %>%
    mutate(
      # rename the two survey levels
      study = recode(
        study,
        !!paste0(metric, "_si_1978") := "si_1978",
        !!paste0(metric, "_su_2022") := "su_2022"
      ),
      station_pair = factor(station_pair)
    )
  
  # 2) build the formula:   metric ~ sea * study + Error(station_pair/study)
  fmla <- as.formula(
    paste0(metric, " ~ sea * study + Error(station_pair / study)")
  )
  
  # 3) run it and return the summary()
  aov_res <- aov(fmla, data = div_df)
  return(summary(aov_res))
}

# choose which metrics to run the function on
for (m in c("S", "H", "J", "simpson", "invsimpson", "unbias_simp", "alpha")) {
  cat("\n\n==== ANOVA for", m, "====\n")
  print(run_diversity_anova(m))
}

fmla <- as.formula(
  paste0(metric, " ~ sea * survey + Error(station_pair / survey)")
)


library(car)
library(broom)

mod_S       <-   aov(S           ~ sea * study + Error(nice_station_pair / study), data = div_df)
mod_H       <-   aov(H           ~ sea * study + Error(nice_station_pair / study), data = div_df)
mod_J       <-   aov(J           ~ sea * study + Error(nice_station_pair / study), data = div_df)
mod_simpson <-   aov(simpson     ~ sea * study + Error(nice_station_pair / study), data = div_df)
mod_inv     <-   aov(invsimpson  ~ sea * study + Error(nice_station_pair / study), data = div_df)
mod_unb     <-   aov(unbias_simp ~ sea * study + Error(nice_station_pair / study), data = div_df)
mod_alpha   <-   aov(alpha       ~ sea * study + Error(nice_station_pair / study), data = div_df)


df_anova <- bind_rows(
  tidy(car::Anova(mod_S,       type = "III"))        %>% mutate(metric = "S"),
  tidy(car::Anova(mod_H,       type = "III"))        %>% mutate(metric = "H"),
  tidy(car::Anova(mod_J,       type = "III"))        %>% mutate(metric = "J"),
  tidy(car::Anova(mod_simpson, type = "III"))        %>% mutate(metric = "simpson"),
  tidy(car::Anova(mod_inv,     type = "III"))        %>% mutate(metric = "invsimpson"),
  tidy(car::Anova(mod_unb,     type = "III"))        %>% mutate(metric = "unbias_simp"),
  tidy(car::Anova(mod_alpha,   type = "III"))        %>% mutate(metric = "alpha")
)