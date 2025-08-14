#### NOTES ####
# This script was created from EstimateR.R. 
# It was adapted to just focus on the SU-SI duplicate stations. 
# 24 SU duplicates. But this was filtered down to 21 for habitat and sampling effectiveness.
# It does include 3 proxy stations. 


#### INITIALIZATION ####
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#### INSTALL PACKAGES ####
packages_used <- 
  c("tidyverse",
    "vegan",
    "remotes",
    "ggvegan",
    "ggplot2",
    "dplyr",
    "devtools",
    "pairwise.adonis2"
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

# Install devtools if needed
if (!requireNamespace("devtools", quietly = TRUE))
  install.packages("devtools")
install.packages("devtools")

# Install from GitHub
devtools::install_github("martharowland/pairwise.adonis2S")
install_github("pmartinezarbizu/pairwiseAdonis/pairwiseAdonis")

# Load package
library(pairwiseAdonis)

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


#### BRAY-CURTIS ####

bc_dist <- vegdist(data_vegan, method = "bray")

nmds <- metaMDS(
  data_vegan, 
  distance = "bray",
  k = 2,             # start with 2D
  trymax = 100, 
  autotransform = FALSE
)
# ... Procrustes: rmse 1.908066e-05  max resid 6.234271e-05 
# ... Similar to previous best


#### NMDS PLOT ####

# use ggvegan’s autoplot or fortify + ggplot2
df_scores <- fortify(nmds, data_vegan.env)  

# filter out only the site‐scores (not the species‐scores)
site_scores <- df_scores %>%
  filter(Score == "sites") %>%       # keep only the stations
  dplyr::rename(station_code = Label)       # Label was your station_code

# join your metadata (study & sea)
site_scores <- site_scores %>%
  dplyr::left_join(
    data_vegan.env %>% dplyr::select(station_code, study, sea),
    by = "station_code"
  )

site_scores <- site_scores %>%
  mutate(
    study = recode(study,
                   "si_1978" = "Historical",
                   "su_2022" = "Modern"))

site_scores <- site_scores %>%
  mutate(
    sea = recode(sea,
                   "bohol" = "Bohol Sea",
                   "sulu"  = "Sulu Sea"))

survey_colors <- c(
  "Historical" = "#0072CE",   # Smithsonian blue
  "Modern" = "#800000"    # Silliman maroon
)

sea_colors <- c(
  "Sulu Sea" = "#00829A",   # FMA 5
  "Bohol Sea" = "#C6C8C5" #"#3F463E"    # FMA 9
)


ggplot(site_scores, aes(x = NMDS1, y = NMDS2, color = study, shape = sea)) +
  geom_point(size = 3) +
  stat_ellipse(aes(group = interaction(study, sea)), linetype = 2) +
  # labels
  labs(
    # title    = "NMDS of rotenone fish communities",
    # subtitle = paste0("stress = ", round(nmds$stress, 3)),
    x        = "NMDS1",
    y        = "NMDS2"
  ) +
  # manual scales
  scale_color_manual(
    name   = "Survey",
    values = survey_colors    # SI/SU blues & maroons
  ) +
  scale_shape_manual(
    name   = "Sea",
    values = c(16, 17) # Bohol Sea filled circle, Sulu Sea filled triangle
  ) +
  scale_x_continuous(
    breaks = seq(-0.5, 0.5, by = 0.25),
    limits = c(-0.75, 0.75)
  ) +
  scale_y_continuous(
    breaks = seq(-0.5, 0.5, by = 0.25),
    limits = c(-0.5, 0.5)
  ) +
  # theme
  theme_classic(base_family = "Times New Roman", base_size = 12) 

# ggsave(
#   filename = "../figures/si_su_duplicates/nmds_species_survey_sea_stress_0.216.png",
#   plot     = last_plot(),   # or give your plot object here
#   device   = "png",
#   width    = 6.5,
#   height   = 6.5,
#   units    = "in",
#   dpi      = 300
# )


#### MULTIVARIATE DISPERSION ####

# BY STUDY
disp_study <- betadisper(bc_dist, data_vegan.env$study)
anova(disp_study)    # are groups equally dispersed? # Type I


# Analysis of Variance Table
# 
# Response: Distances
# Df   Sum Sq   Mean Sq F value Pr(>F)
# Groups     1 0.004084 0.0040838  1.4865 0.2299
# Residuals 40 0.109887 0.0027472 
# no significant difference in Beta diversity between surveys

# BY SEA 
disp_sea <- betadisper(bc_dist, data_vegan.env$sea)
anova(disp_sea) 

# Analysis of Variance Table
# 
# Response: Distances
# Df   Sum Sq   Mean Sq F value Pr(>F)
# Groups     1 0.000005 0.0000054   0.003 0.9564
# Residuals 40 0.071291 0.0017823 
# no significant difference in Beta diversity between seas

# BY STUDY & SEA 
group4 <- interaction(data_vegan.env$study, data_vegan.env$sea, sep = "-")
disp_4 <- betadisper(bc_dist, group4)
anova(disp_4)

# Analysis of Variance Table
# 
# Response: Distances
# Df   Sum Sq   Mean Sq F value Pr(>F)
# Groups     3 0.011092 0.0036975  0.9741  0.415
# Residuals 38 0.144238 0.0037957 
# no significant difference in Beta diversity between study x sea (4 groups)

## Two‐way ANOVA. Basically the same thing as the above study x sea interaction with 4 groups
# Build a data.frame of distances + metadata
dist_df <- data.frame(
  station_code = names(disp_4$distances),
  distance     = disp_4$distances
) %>%
  # join back your env metadata by station_code
  left_join(data_vegan.env, by = "station_code")

# Two‐way ANOVA on distance ~ study * sea
aov_dist <- aov(distance ~ study * sea, data = dist_df)
summary(aov_dist)

### USE THIS IN PAPER ###
# Df  Sum Sq  Mean Sq F value Pr(>F)
# study        1 0.00560 0.005599   1.475  0.232
# sea          1 0.00032 0.000318   0.084  0.774
# study:sea    1 0.00518 0.005175   1.363  0.250
# Residuals   38 0.14424 0.003796 
# no evidence that the spread of communities 
# (i.e. multivariate dispersion, often thought of as β-diversity variance) 
# differs by time period, by sea, or by their interaction  

#### PERMANOVA ####
# bc_dist ~ study * sea + (1 | station_pair)
# did not work like the other lme formulas
perm <- adonis2(
  bc_dist ~ study * sea,
  data = data_vegan.env,
  permutations = 999
)
print(perm)

# Permutation test for adonis under reduced model
# Terms added sequentially (first to last)
# Permutation: free
# Number of permutations: 999
# 
# adonis2(formula = bc_dist ~ study * sea, data = data_vegan.env, permutations = 999)
# Df SumOfSqs      R2      F Pr(>F)    
# study      1   1.0233 0.05734 2.5307  0.001 ***
#   sea        1   0.8807 0.04935 2.1780  0.002 ** 
#   study:sea  1   0.5779 0.03238 1.4291  0.015 *  
#   Residual  38  15.3659 0.86094                  
# Total     41  17.8478 1.00000                  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# study effect (p=0.001) → fish communities differed significantly between the 1978/79 versus 2019/22 surveys.
# sea effect (p=0.002) → communities differed between Bohol and Sulu Seas.
# study:sea interaction (p=0.015) → the temporal change in community composition depended on which sea you look at (i.e. the magnitude or direction of change over time differs between the two seas). 
# PERMANOVA Summary:
# There are clear, statistically significant shifts in community composition both through time (SI vs. SU) and across space (Bohol vs. Sulu), and those shifts differ in the two regions.

# Run pairwise PERMANOVA
pairwise_results <- pairwise.adonis(
  x = bc_dist,
  factors = data_vegan.env$study_sea,
  perm = 999,
  p.adjust.m = "bonferroni"  # Options: "holm", "fdr", etc.
)

# View the results
print(pairwise_results)

# save file
outdir <- "../tables/si_su_duplicates"
outfile <- file.path(outdir, "table_diversity_beta_station_studysea_pairwise.csv")
write_csv(pairwise_results, outfile)




