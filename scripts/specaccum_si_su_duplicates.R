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


#### CREATE SPECACCUM LISTS ####
## ALL ## 
# sample-based. subsampling without replacement (Gotelli & Colwell 2001)
specaccum_rand <- specaccum(data_vegan, method="random", permutations=999)
# sample-based. (Coleman et al. 1982)
specaccum_cole  <- specaccum(data_vegan, method="coleman")
# sample-based. expected species richness aka Mao Tau estimate (Chiarucci et al. 2008; Colwell et al. 2012; 
specaccum_exac <- specaccum(data_vegan, method="exact")
# Individual‐based. 
specaccum_rare <- specaccum(data_vegan, method="rarefaction")

## BY STUDY ##
# SI #
specaccum_rand_si <- specaccum(data_si_vegan, method="random", permutations=999)
specaccum_cole_si  <- specaccum(data_si_vegan, method="coleman")
specaccum_exac_si <- specaccum(data_si_vegan, method="exact")
specaccum_rare_si <- specaccum(data_si_vegan, method="rarefaction")
# SU #
specaccum_rand_su <- specaccum(data_su_vegan, method="random", permutations=999)
specaccum_cole_su  <- specaccum(data_su_vegan, method="coleman")
specaccum_exac_su <- specaccum(data_su_vegan, method="exact")
specaccum_rare_su <- specaccum(data_su_vegan, method="rarefaction")

## BY SEA ##
# SULU #
specaccum_rand_sulu <- specaccum(data_sulu_vegan, method="random", permutations=999)
specaccum_cole_sulu  <- specaccum(data_sulu_vegan, method="coleman")
specaccum_exac_sulu <- specaccum(data_sulu_vegan, method="exact")
specaccum_rare_sulu <- specaccum(data_sulu_vegan, method="rarefaction")
# BOHOL #
specaccum_rand_bohol <- specaccum(data_bohol_vegan, method="random", permutations=999)
specaccum_cole_bohol  <- specaccum(data_bohol_vegan, method="coleman")
specaccum_exac_bohol  <- specaccum(data_bohol_vegan, method="exact")
specaccum_rare_bohol  <- specaccum(data_bohol_vegan, method="rarefaction")

## BY STUDY & SEA #
# SI SULU #
specaccum_rand_si_sulu <- specaccum(data_si_sulu_vegan, method="random", permutations=999)
specaccum_cole_si_sulu  <- specaccum(data_si_sulu_vegan, method="coleman")
specaccum_exac_si_sulu  <- specaccum(data_si_sulu_vegan, method="exact")
specaccum_rare_si_sulu  <- specaccum(data_si_sulu_vegan, method="rarefaction")
# SU SULU #
specaccum_rand_su_sulu <- specaccum(data_su_sulu_vegan, method="random", permutations=999)
specaccum_cole_su_sulu  <- specaccum(data_su_sulu_vegan, method="coleman")
specaccum_exac_su_sulu  <- specaccum(data_su_sulu_vegan, method="exact")
specaccum_rare_su_sulu  <- specaccum(data_su_sulu_vegan, method="rarefaction")
# SI BOHOL #
specaccum_rand_si_bohol <- specaccum(data_si_bohol_vegan, method="random", permutations=999)
specaccum_cole_si_bohol  <- specaccum(data_si_bohol_vegan, method="coleman")
specaccum_exac_si_bohol  <- specaccum(data_si_bohol_vegan, method="exact")
specaccum_rare_si_bohol  <- specaccum(data_si_bohol_vegan, method="rarefaction")
# SU BOHOL #
specaccum_rand_su_bohol <- specaccum(data_su_bohol_vegan, method="random", permutations=999)
specaccum_cole_su_bohol  <- specaccum(data_su_bohol_vegan, method="coleman")
specaccum_exac_su_bohol  <- specaccum(data_su_bohol_vegan, method="exact")
specaccum_rare_su_bohol  <- specaccum(data_su_bohol_vegan, method="rarefaction")


#### FUNCTIONS ####

# speccaccum method rarefaction list to tibble
specaccum_rare_to_tibble <- function(sa) {
  # sa: the list returned by specaccum(...)
  tibble(
    sites       = sa$sites,
    richness    = sa$richness,                 # the mean accumulated richness
    sd          = sa$sd,
    individuals = sa$individuals
  )
}


#### SPECACCUM PLOTS ALL ####
## Random & Coleman
plot(specaccum_rand, ci.type="poly", col="black", lwd=2,
     xlab="Samples",    ylab="Species Richness",
     xlim= c(0, 45), ylim= c(0, 1000))
plot(specaccum_cole,  add=TRUE, col="grey", lwd=2, lty=2)
legend("bottomright",
       legend=c("Gotelli & Colwell, 2001","Coleman et al., 1982"),
       col=c("black","grey"), lty=1:2, bty="n")

## Rarefaction (individual-based)
specaccum_rare_tibble <- specaccum_rare_to_tibble(specaccum_rare)

# plot richness vs. individuals, with sd as a ribbon
ggplot(specaccum_rare_tibble, aes(x = individuals, y = richness)) +
  geom_ribbon(aes(ymin = richness - sd, ymax = richness + sd),
              fill = "grey80", alpha = 0.5) +
  geom_line(color = "black", size = 1) +
  labs(
    x = "Individuals (pooled)",
    y = "Species Richness"
  ) +
  scale_x_continuous(
    breaks = c(0, 5000, 10000, 15000, 20000, 25000, 30000),
    limits = c(0, 30000)    # adjust if you want a little padding
  ) +
  scale_y_continuous(
    breaks = c(0, 200, 400, 600, 800, 1000),
    limits = c(0, 1000)
  ) +
  theme_classic(base_family = "Times New Roman", base_size = 12) +
  theme_classic()

# ggsave(
#   filename = "../figures/si_su_duplicates/speccaccum_rare_individuals_species.png",
#   plot     = last_plot(),   # or give your plot object here
#   device   = "png",
#   width    = 6.5,
#   height   = 8,
#   units    = "in",
#   dpi      = 300
# )

#### SPECACCUM PLOTS BY SURVEY ####

# "si_1978" = "#0072CE",  # Smithsonian Blue
# "su_2022" = "#800000" # ,  # Silliman Maroon

## Random & Coleman
# SI #
plot(specaccum_rand_si, ci.type="poly", col="#0072CE", lwd=2,
     xlab="Samples",    ylab="Species Richness")
plot(specaccum_cole_si,  add=TRUE, col="darkblue", lwd=2, lty=2)
legend("bottomright",
       legend=c("Gotelli & Colwell, 2001","Coleman et al., 1982"),
       col=c("#0072CE","darkblue"), lty=1:2, bty="n")

# SU #
plot(specaccum_rand_su, ci.type="poly", col="#800000", lwd=2,
     xlab="Samples",    ylab="Species Richness")
plot(specaccum_cole_su,  add=TRUE, col="#4B0101", lwd=2, lty=2)
legend("bottomright",
       legend=c("Gotelli & Colwell, 2001","Coleman et al., 1982"),
       col=c("#800000","#4B0101"), lty=1:2, bty="n")

## Overlapped SAC by Survey##
# Compute overall x‐ and y‐limits #
# all the “number of samples” (‘sites’) across both studies
all_sites    <- c(specaccum_rand_si$sites,    specaccum_rand_su$sites)
# all the mean richnesses and their CIs
all_richness <- c(
  specaccum_rand_si$richness + specaccum_rand_si$sd,  # upper CI
  specaccum_rand_si$richness - specaccum_rand_si$sd,  # lower CI
  specaccum_rand_si$richness,                          # mean
  specaccum_cole_si$richness,                          # Coleman mean
  specaccum_rand_su$richness + specaccum_rand_su$sd,
  specaccum_rand_su$richness - specaccum_rand_su$sd,
  specaccum_rand_su$richness,
  specaccum_cole_su$richness
)
xlim <- range(all_sites)
ylim <- c(0, max(all_richness))

# Plot the SI “random” curve (with its CI polygon) to set up the canvas ##
plot(specaccum_rand_si,
     ci.type="poly",
     col="#0072CE", lwd=2,
     xlim=xlim, ylim=ylim,
     xlab="Samples", ylab="Estimated Species Richness")

# Add the other three curves #
# SI Coleman‐analytic
plot(specaccum_cole_si, add=TRUE, col="darkblue", lwd=2, lty=2)

# SU “random”
plot(specaccum_rand_su, add=TRUE, ci.type="poly", col="#800000", lwd=2)

# SU Coleman
plot(specaccum_cole_su, add=TRUE, col="#4B0101", lwd=2, lty=2)

# legend describing all four lines #
legend("bottomright",
       legend = c("SI Random",
                  "SI Coleman",
                  "SU Random",
                  "SU Coleman"),
       col    = c("#0072CE", "darkblue", "#800000","#4B0101"),
       lty    = c(1, 2, 1, 2),
       lwd    = c(2, 2, 2, 2),
       bty    = "n")

## Rarefaction (individual-based)
specaccum_rare_si_tibble <- specaccum_rare_to_tibble(specaccum_rare_si) %>% 
  mutate(survey = "Historical")
specaccum_rare_su_tibble <- specaccum_rare_to_tibble(specaccum_rare_su) %>% 
  mutate(survey = "Modern")

specaccum_rare_survey <- bind_rows(specaccum_rare_si_tibble, specaccum_rare_su_tibble)


# plot richness vs. individuals, with sd as a ribbon
ggplot(specaccum_rare_survey, aes(x = individuals, y = richness, colour = survey, fill = survey)) +
  geom_ribbon(aes(ymin = richness - sd, ymax = richness + sd),
              alpha = 0.5) +
  geom_line(size = 1) +
  scale_color_manual(values = c("Historical" = "#0072CE",
                                "Modern" = "#800000")) +
  scale_fill_manual (values = c("Historical" = "#0072CE",
                                "Modern" = "#800000")) +
  labs(
    x = "Individuals (pooled)",
    y = "Species Richness"
  ) +
  scale_x_continuous(
    breaks = c(0, 2500, 5000, 7500, 10000, 12500, 15000),
    limits = c(0, 15100)    # adjust if you want a little padding
  ) +
  scale_y_continuous(
    breaks = c(0, 200, 400, 600, 800),
    limits = c(0, 800)
  ) +
  theme_classic(base_family = "Times New Roman", base_size = 12) +
  theme_classic() + 
  theme(legend.position = "none")

# ggsave(
#   filename = "../figures/si_su_duplicates/speccaccum_rare_individuals_species_survey.png",
#   plot     = last_plot(),   # or give your plot object here
#   device   = "png",
#   width    = 6.5,
#   height   = 8,
#   units    = "in",
#   dpi      = 300
# )


#### SPECACCUM PLOTS BY SEA ####
## Random & Colwell

# sea_colors <- c(
#   "Sulu Sea" = "#00829A",   # FMA 5
#   "Bohol Sea" = "#C6C8C5" #"#3F463E"    # FMA 9
# )

# BOHOL
plot(specaccum_rand_bohol, ci.type="poly", col="#C6C8C5", lwd=2,
     xlab="Samples",    ylab="Species Richness")
plot(specaccum_cole_bohol,  add=TRUE, col="black", lwd=2, lty=2)
legend("bottomright",
       legend=c("Gotelli & Colwell, 2001","Coleman et al., 1982"),
       col=c("#C6C8C5","black"), lty=1:2, bty="n")

# SULU #
plot(specaccum_rand_sulu, ci.type="poly", col="#00829A", lwd=2,
     xlab="Samples",    ylab="Species Richness")
plot(specaccum_cole_sulu,  add=TRUE, col="black", lwd=2, lty=2)
legend("bottomright",
       legend=c("Gotelli & Colwell, 2001","Coleman et al., 1982"),
       col=c("#00829A","black"), lty=1:2, bty="n")

## Overlapped SAC by Sea ##
# Compute overall x‐ and y‐limits #
# all the “number of samples” (‘sites’) across both seas
all_sites    <- c(specaccum_rand_bohol$sites,    specaccum_rand_sulu$sites)
# all the mean richnesses and their CIs
all_richness <- c(
  specaccum_rand_bohol$richness + specaccum_rand_si$sd,  # upper CI
  specaccum_rand_bohol$richness - specaccum_rand_si$sd,  # lower CI
  specaccum_rand_bohol$richness,                          # mean
  specaccum_cole_bohol$richness,                          # Coleman mean
  specaccum_rand_sulu$richness + specaccum_rand_su$sd,
  specaccum_rand_sulu$richness - specaccum_rand_su$sd,
  specaccum_rand_sulu$richness,
  specaccum_cole_sulu$richness
)
xlim <- range(all_sites)
ylim <- c(0, max(all_richness))

# Plot the Bohol “random” curve (with its CI polygon) to set up the canvas ##
plot(specaccum_rand_bohol,
     ci.type="poly",
     col="#C6C8C5", lwd=2,
     xlim=xlim, ylim=ylim,
     xlab="Samples", ylab="Estimated Species Richness")

# Add the other three curves #
# Bohol Coleman‐analytic
plot(specaccum_cole_bohol, add=TRUE, col="black", lwd=2, lty=2)

# Sulu “random”
plot(specaccum_rand_sulu, add=TRUE, ci.type="poly", col="#00829A", lwd=2)

# Sulu Coleman
plot(specaccum_cole_sulu, add=TRUE, col="black", lwd=2, lty=2)

## legend describing all four lines ##
legend("bottomright",
       legend = c("Bohol Random",
                  "Bohol Coleman",
                  "Sulu Random",
                  "Sulu Coleman"),
       col    = c("#C6C8C5", "black", "#00829A","black"),
       lty    = c(1, 2, 1, 2),
       lwd    = c(2, 2, 2, 2),
       bty    = "n")


## Rarefaction (individual-based)
specaccum_rare_bohol_tibble <- specaccum_rare_to_tibble(specaccum_rare_bohol) %>% 
  mutate(sea = "Bohol Sea")
specaccum_rare_sulu_tibble <- specaccum_rare_to_tibble(specaccum_rare_sulu) %>% 
  mutate(sea = "Sulu Sea")

specaccum_rare_sea <- bind_rows(specaccum_rare_bohol_tibble, specaccum_rare_sulu_tibble)


# plot richness vs. individuals, with sd as a ribbon

# sea_colors <- c(
#   "Sulu Sea" = "#00829A",   # FMA 5
#   "Bohol Sea" = "#C6C8C5" #"#3F463E"    # FMA 9
# )


ggplot(specaccum_rare_sea, aes(x = individuals, y = richness, colour = sea, fill = sea)) +
  geom_ribbon(aes(ymin = richness - sd, ymax = richness + sd),
              alpha = 0.5) +
  geom_line(size = 1) +
  scale_color_manual(values = c("Bohol Sea" = "#C6C8C5",
                                "Sulu Sea" = "#00829A")) +
  scale_fill_manual (values = c("Bohol Sea" = "#C6C8C5",
                                "Sulu Sea" = "#00829A")) +
  labs(
    x = "Individuals (pooled)",
    y = "Species Richness"
  ) +
  scale_x_continuous(
    breaks = c(0, 4000, 8000, 12000, 16000),
    limits = c(0, 16000)    # adjust if you want a little padding
  ) +
  scale_y_continuous(
    breaks = c(0, 200, 400, 600, 800),
    limits = c(0, 800)
  ) +
  theme_classic(base_family = "Times New Roman", base_size = 12) +
  theme_classic() + 
  theme(legend.position = "none")

# ggsave(
#   filename = "../figures/si_su_duplicates/speccaccum_rare_individuals_species_sea.png",
#   plot     = last_plot(),   # or give your plot object here
#   device   = "png",
#   width    = 6.5,
#   height   = 8,
#   units    = "in",
#   dpi      = 300
# )


#### SPECACCUM PLOTS BY STUDY & SEA ####

# "si_1978" = "#0072CE",  # Smithsonian Blue
# "su_2022" = "#800000" # ,  # Silliman Maroon

# sea_colors <- c(
#   "Sulu Sea" = "#00829A",   # FMA 5
#   "Bohol Sea" = "#C6C8C5" #"#3F463E"    # FMA 9
# )

## Overlapped SAC by Study & Sea ##
# Compute overall x‐ and y‐limits #
# all the “number of samples” (‘sites’) across both studies and both seas
all_sites    <- c(specaccum_rand_si_bohol$sites, specaccum_rand_su_bohol$sites, specaccum_rand_si_sulu$sites, specaccum_rand_su_sulu$sites)
# all the mean richnesses and their CIs
all_richness <- c(
  specaccum_rand_si_bohol$richness + specaccum_rand_si$sd,  # upper CI
  specaccum_rand_si_bohol$richness - specaccum_rand_si$sd,  # lower CI
  specaccum_rand_si_bohol$richness,                          # mean
  specaccum_cole_si_bohol$richness,                          # Coleman mean
  specaccum_rand_su_bohol$richness + specaccum_rand_si$sd,  # upper CI
  specaccum_rand_su_bohol$richness - specaccum_rand_si$sd,  # lower CI
  specaccum_rand_su_bohol$richness,                          # mean
  specaccum_cole_su_bohol$richness,                          # Coleman mean
  specaccum_rand_si_sulu$richness + specaccum_rand_su$sd,
  specaccum_rand_si_sulu$richness - specaccum_rand_su$sd,
  specaccum_rand_si_sulu$richness,
  specaccum_cole_si_sulu$richness,
  specaccum_rand_su_sulu$richness + specaccum_rand_su$sd,
  specaccum_rand_su_sulu$richness - specaccum_rand_su$sd,
  specaccum_rand_su_sulu$richness,
  specaccum_cole_su_sulu$richness
)
xlim <- range(all_sites)
ylim <- c(0, max(all_richness))

# Plot the SI Bohol “random” curve (with its CI polygon) to set up the canvas ##
plot(specaccum_rand_si_bohol,
     ci.type="poly",
     col="#C6C8C5", lwd=2,
     xlim=xlim, ylim=ylim,
     xlab="Samples", ylab="Estimated Species Richness")

# SI Bohol Coleman
plot(specaccum_cole_si_bohol, add=TRUE, col="#0072CE", lwd=2, lty=2)

# SU Bohol “random”
plot(specaccum_rand_su_bohol, add=TRUE, ci.type="poly", col="#C6C8C5", lwd=2)

# SU Bohol Coleman
plot(specaccum_cole_su_bohol, add=TRUE, col="#800000", lwd=2, lty=2)

# SI Sulu “random”
plot(specaccum_rand_si_sulu, add=TRUE, ci.type="poly", col="#00829A", lwd=2)

# SI Sulu Coleman
plot(specaccum_cole_si_sulu, add=TRUE, col="#0072CE", lwd=2, lty=2)

# SU Sulu “random”
plot(specaccum_rand_su_sulu, add=TRUE, ci.type="poly", col="#00829A", lwd=2)

# SU Sulu Coleman
plot(specaccum_cole_su_sulu, add=TRUE, col="#800000", lwd=2, lty=2)

## legend describing all four lines ##
legend("bottomright",
       legend = c("Bohol",
                  "Sulu",
                  "SI",
                  "SU"),
       col    = c("#C6C8C5", "#00829A","#0072CE", "#800000"),
       lty    = c(1, 1, 2, 2),
       lwd    = c(2, 2, 2, 2),
       bty    = "n")


## Rarefaction (individual-based)
specaccum_rare_si_bohol_tibble <- specaccum_rare_to_tibble(specaccum_rare_si_bohol) %>% 
  mutate(survey_sea = "Historical Bohol Sea")
specaccum_rare_si_sulu_tibble <- specaccum_rare_to_tibble(specaccum_rare_si_sulu) %>% 
  mutate(survey_sea = "Historical Sulu Sea")
specaccum_rare_su_bohol_tibble <- specaccum_rare_to_tibble(specaccum_rare_su_bohol) %>% 
  mutate(survey_sea = "Modern Bohol Sea")
specaccum_rare_su_sulu_tibble <- specaccum_rare_to_tibble(specaccum_rare_su_sulu) %>% 
  mutate(survey_sea = "Modern Sulu Sea")

specaccum_rare_survey_sea <- bind_rows(specaccum_rare_si_bohol_tibble, specaccum_rare_si_sulu_tibble, specaccum_rare_su_bohol_tibble, specaccum_rare_su_sulu_tibble)

# plot richness vs. individuals, with sd as a ribbon

# sea_colors <- c(
#   "Sulu Sea" = "#00829A",   # FMA 5
#   "Bohol Sea" = "#C6C8C5" #"#3F463E"    # FMA 9
# )


ggplot(specaccum_rare_survey_sea, aes(x = individuals, y = richness, colour = survey_sea, fill = survey_sea)) +
  geom_ribbon(aes(ymin = richness - sd, ymax = richness + sd),
              alpha = 0.5) +
  geom_line(size = 1) +
  scale_color_manual(    
    name   = "Survey X Sea",
    values = c("Historical Bohol Sea" = "#0072CE",
               "Modern Bohol Sea" = "#800000",
               "Historical Sulu Sea" = "#0072CE",
               "Modern Sulu Sea" = "#800000")) +
  scale_fill_manual (
    name   = "Survey X Sea",
    values = c("Historical Bohol Sea" = "#C6C8C5",
               "Modern Bohol Sea" = "#C6C8C5",
               "Historical Sulu Sea" = "#00829A",
               "Modern Sulu Sea" = "#00829A")) +
  labs(
    x = "Individuals (pooled)",
    y = "Species Richness"
  ) +
  scale_x_continuous(
    breaks = c(0, 2000, 4000, 6000, 8000),
    limits = c(0, 8000)    # adjust if you want a little padding
  ) +
  scale_y_continuous(
    breaks = c(0, 100, 200, 300, 400, 500),
    limits = c(0, 500)
  ) +
  theme_classic(base_family = "Times New Roman", base_size = 12) +
  theme_classic() + 
  theme(
    legend.position      = c(0.95, 0.05),
    legend.justification = c(1, 0),
    legend.background    = element_rect(fill = "white", color = "black")
  )

# ggsave(
#   filename = "../figures/si_su_duplicates/speccaccum_rare_individuals_species_survey_sea.png",
#   plot     = last_plot(),   # or give your plot object here
#   device   = "png",
#   width    = 6.5,
#   height   = 8,
#   units    = "in",
#   dpi      = 300
# )


#### SPECSLOPE ####

specslope(specaccum_rare_su_sulu, 7)

#### fitspecaccum ####
fitspeccacum_rare_all <- fitspecaccum(specaccum_rare, "lomolino")
coef(fitspeccacum_rare_all)
fitted(fitspeccacum_rare_all)
plot(fitspeccacum_rare_all, add = TRUE, col=2, lwd=2)

fitspeccaccum_rand_all <- fitspecaccum(specaccum_rare, "arrh")
plot(fitspeccaccum_rand_all)

#### https://rdrr.io/rforge/vegan/man/specaccum.html ####
sp1 <- specaccum(data_vegan)
sp2 <- specaccum(data_vegan, "random")

summary(sp2)
plot(sp1, ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue")
boxplot(sp2, col="yellow", add=TRUE, pch="+")
## Fit Lomolino model to the exact accumulation
mod1 <- fitspecaccum(sp1, "lomolino")
coef(mod1)
fitted(mod1)
plot(sp1)
## Add Lomolino model using argument 'add'
plot(mod1, add = TRUE, col=2, lwd=2)
## Fit Arrhenius models to all random accumulations
mods <- fitspecaccum(sp2, "arrh")
plot(mods, col="hotpink")
boxplot(sp2, col = "yellow", border = "blue", lty=1, cex=0.3, add= TRUE)
## Use nls() methods to the list of models
sapply(mods$models, AIC)


