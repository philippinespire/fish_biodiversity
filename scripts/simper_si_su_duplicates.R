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
    "tibble",
    "dplyr",
    "readr",
    "vegan",
    "remotes",
    "ggvegan",
    "ggplot2"
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

data_vegan.env$study_sea <- interaction(data_vegan.env$study, data_vegan.env$sea, sep = "_")


#### SIMPER BY STUDY ####
sim_study <- simper(data_vegan, data_vegan.env$study, permutations = 999)

# check the comparison name with names(sim_study). it should be the concatenated names of the two studies separated by an underscore
cmp_name_study <- names(sim_study)[1]  
# si_1978_su_2022
# si_1978: Group A (ava) 
# su_2022: Group B (avb)

# pull out the raw summary table
sim_tibble_study <- summary(sim_study, ordered = TRUE)[[ cmp_name_study ]]

# add species column
sim_tibble_study <- summary(sim_study, ordered = TRUE)[[ cmp_name_study ]] %>%
  rownames_to_column("species")

# reorder for publication
sim_tibble_study <- sim_tibble_study %>% 
  select(species, average, sd, cumsum, ava, avb, ratio, p)

# print the species that are the top 10 contributors
sim_tibble_study %>% slice_max(order_by = average, n = 10) 

# print the species whose cumulative contribution exceeds 20%
sim_tibble_study %>%
  filter(cumsum < 0.21)

# save file
sim_tibble_study %>% write_csv("../figures/si_su_duplicates/simper_all_species_study.csv")


#### SIMPER BY SEA ####
sim_sea  <- simper(data_vegan, data_vegan.env$sea, permutations = 999)

# check the comparison name with names(sim_sea). it should be the concatenated names of the two seas separated by an underscore
cmp_name_sea <- names(sim_sea)[1]  
# Bohol Sea_Sulu Sea
# Bohol Sea: Group A (ava) 
# Sulu Sea: Group B (avb)

# pull out the raw summary table
sim_tibble_sea <- summary(sim_sea, ordered = TRUE)[[ cmp_name_sea ]]

# add species column
sim_tibble_sea <- summary(sim_sea, ordered = TRUE)[[ cmp_name_sea ]] %>%
  rownames_to_column("species")

# reorder for publication
sim_tibble_sea <- sim_tibble_sea %>% 
  select(species, average, sd, cumsum, ava, avb, ratio, p)

# print the species that are the top 10 contributors
sim_tibble_sea %>% slice_max(order_by = average, n = 10) 

# print the species whose cumulative contribution exceeds 20%
sim_tibble_sea %>%
  filter(cumsum < 0.21)

# save file
sim_tibble_sea %>% write_csv("../figures/si_su_duplicates/simper_all_species_sea.csv")


#### SIMPER BY STUDY & SEA ####
sim_study_sea  <- simper(data_vegan, data_vegan.env$study_sea, permutations = 999)

# check the comparison names with names(sim_study_sea). it should contain 6 comparisons between the 2 studies and seas.
names(sim_study_sea) 
# si_1978_Bohol Sea_si_1978_Sulu Sea
# si_1978_Bohol Sea_su_2022_Bohol Sea
# si_1978_Bohol Sea_su_2022_Sulu Sea
# si_1978_Sulu Sea_su_2022_Bohol Sea
# si_1978_Sulu Sea_su_2022_Sulu Sea
# su_2022_Bohol Sea_su_2022_Sulu Sea


## Bohol Sea temporal change ##
# si_1978_Bohol Sea_su_2022_Bohol Sea
# si_1978_Bohol Sea: Group A (ava) 
# su_2022_Bohol Sea: Group B (avb)

# pull out the raw summary table
sim_tibble_study_bohol_sea <- summary(sim_study_sea, ordered = TRUE)[[ "si_1978_Bohol Sea_su_2022_Bohol Sea" ]]

# add species column
sim_tibble_study_bohol_sea <- summary(sim_study_sea, ordered = TRUE)[[ "si_1978_Bohol Sea_su_2022_Bohol Sea" ]] %>%
  rownames_to_column("species")

# reorder for publication
sim_tibble_study_bohol_sea <- sim_tibble_study_bohol_sea %>% 
  select(species, average, sd, cumsum, ava, avb, ratio, p)

# print the species that are the top 10 contributors
sim_tibble_study_bohol_sea %>% slice_max(order_by = average, n = 10) 

# print the species whose cumulative contribution exceeds 20%
sim_tibble_study_bohol_sea %>%
  filter(cumsum < 0.21)

# save file
sim_tibble_study_bohol_sea %>% write_csv("../figures/si_su_duplicates/simper_all_species_study_bohol_sea.csv")

 
## Sulu Sea temporal change ##
# si_1978_Sulu Sea_su_2022_Sulu Sea
# si_1978_Sulu Sea: Group A (ava) 
# su_2022_Sulu Sea: Group B (avb)

# pull out the raw summary table
sim_tibble_study_sulu_sea <- summary(sim_study_sea, ordered = TRUE)[[ "si_1978_Sulu Sea_su_2022_Sulu Sea" ]]

# add species column
sim_tibble_study_sulu_sea <- summary(sim_study_sea, ordered = TRUE)[[ "si_1978_Sulu Sea_su_2022_Sulu Sea" ]] %>%
  rownames_to_column("species")

# reorder for publication
sim_tibble_study_sulu_sea <- sim_tibble_study_sulu_sea %>% 
  select(species, average, sd, cumsum, ava, avb, ratio, p)

# print the species that are the top 10 contributors
sim_tibble_study_sulu_sea %>% slice_max(order_by = average, n = 10) 

# print the species whose cumulative contribution exceeds 20%
sim_tibble_study_sulu_sea %>%
  filter(cumsum < 0.21)

# save file
sim_tibble_study_sulu_sea %>% write_csv("../figures/si_su_duplicates/simper_all_species_study_sulu_sea.csv")
write_csv(sim_tibble_study_sulu_sea, "../figures/si_su_duplicates/simper_all_species_study_sulu_sea.csv")
