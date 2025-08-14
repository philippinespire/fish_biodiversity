#### NOTES ####
# This script was created from veganize_data_si_su_duplicates.R EstimateR_si_su_duplicates.R. 
# It was adapted to just focus on the SU-SI duplicate stations. 
# 24 SU duplicates. But this was filtered down to 21 for habitat and sampling effectiveness.
# It does include 3 proxy stations. 
# if interested in MPA or human effects, then add this to the metadata dataframe data_vegan.env
# this is sourced from the other scripts. 

#### INITIALIZATION ####
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#### INSTALL PACKAGES ####
packages_used <- 
  c("tidyverse",
    "janitor",
    "readxl",
    "stringr",
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

#### SOURCING DATA ####

source("wrangle_si_su_data.R")
source("distance_calculations_mpa.R")
# source("ordination_cas_su_si.R")
# source("veganize_data_si_su.R") # use veganization function in the VEGANIZATION sectino of this script


#### SUMMARY STATS ####
# count the number of stations. Should be 42 total stations. 
# length(unique(data_si_su$station_code))
# 42 unique stations. 21 1970's stations and 21 contemporary duplicates. 

# Table 1. Number of unique stations, families, genera, species, individuals for each survey X sea combo. 
# 1) Build all summaries and stack them
summary_table <- bind_rows(

  # — Overall (no filter) —
  data_si_su %>%
    dplyr::summarize(
      n_stations     = dplyr::n_distinct(station_code),
      n_family       = dplyr::n_distinct(family),
      n_genus        = dplyr::n_distinct(genus),
      n_species      = dplyr::n_distinct(verified_identification),
      n_ind          = sum(specimen_count, na.rm = TRUE)
    ) %>%
    dplyr::mutate(study = "All", sea = "All"),

  # — By study only —
  data_si_su %>%
    dplyr::group_by(study) %>%
    dplyr::summarize(
      n_stations     = dplyr::n_distinct(station_code),
      n_family       = dplyr::n_distinct(family),
      n_genus        = dplyr::n_distinct(genus),
      n_species      = dplyr::n_distinct(verified_identification),
      n_ind          = sum(specimen_count, na.rm = TRUE)
    ) %>%
    dplyr::mutate(sea = "All") %>%
    dplyr::ungroup(),

  # — By sea only —
  data_si_su %>%
    dplyr::group_by(sea) %>%
    dplyr::summarize(
      n_stations     = dplyr::n_distinct(station_code),
      n_family       = dplyr::n_distinct(family),
      n_genus        = dplyr::n_distinct(genus),
      n_species      = dplyr::n_distinct(verified_identification),
      n_ind          = sum(specimen_count, na.rm = TRUE)
    ) %>%
    dplyr::mutate(study = "All") %>%
    dplyr::ungroup(),

  # — By study × sea combinations —
  data_si_su %>%
    dplyr::group_by(study, sea) %>%
    dplyr::summarize(
      n_stations     = dplyr::n_distinct(station_code),
      n_family       = dplyr::n_distinct(family),
      n_genus        = dplyr::n_distinct(genus),
      n_species      = dplyr::n_distinct(verified_identification),
      n_ind          = sum(specimen_count, na.rm = TRUE)
    ) %>%
    dplyr::ungroup()
) %>%
  dplyr::select(study, sea, n_stations, n_family, n_genus, n_species, n_ind)

# 2) Take a look
print(summary_table)

# save file
# outdir <- "../tables/si_su_duplicates"
# outfile <- file.path(outdir, "table_sample_size_counts_by_study_sea.csv")
# write_csv(summary_table, outfile)


#### VEGANIZATION ####
# veganization function adapated fropm veganize_data_si_su.R and EstimateR_si_su_duplicates.R. 
prep_vegan <- function(data = data_si_su) {
  data %>%
    filter(specimen_count > 0) %>%
    group_by(verified_identification,
             study, 
             station_code, 
             province,
             municipality, 
             date_collected, 
             latitude, 
             longitude, 
             depth_m, 
             sea
    ) %>%
    dplyr::summarize(sum_specimen_count = sum(specimen_count)) %>%
    ungroup() %>%
    pivot_wider(
      names_from = verified_identification,
      values_from = sum_specimen_count,
      values_fill = 0
    ) %>%
    clean_names() %>%
    arrange(station_code) %>%
    drop_na(station_code)
}

# run prep_vegan to get all sites × species plus metadata columns
data_vegan_si_su_duplicates.all <-
  prep_vegan() %>%
  arrange(station_code)

# 957 "species" columns when including family and genus complexes
# 835 species columns when excluding family and genus complexes

## NO SPECIES COMPLEXES ##
# remove rows with genus and family in the column lowest_tax_cat
data_si_su_nospp <- data_si_su %>%
  filter(data_si_su$lowest_tax_cat == 'species')
# 3040 rows (lots) w/ species
# 346 rows (lots) w/ genus
# 107 rows (lots) w/ family

# veganization function adapated fropm veganize_data_si_su.R and EstimateR_si_su_duplicates.R. 
prep_vegan_nospp <- function(data = data_si_su_nospp) {
  data %>%
    filter(specimen_count > 0) %>%
    group_by(verified_identification,
             study, 
             station_code, 
             province,
             municipality, 
             date_collected, 
             latitude, 
             longitude, 
             depth_m, 
             sea
    ) %>%
    dplyr::summarize(sum_specimen_count = sum(specimen_count)) %>%
    ungroup() %>%
    pivot_wider(
      names_from = verified_identification,
      values_from = sum_specimen_count,
      values_fill = 0
    ) %>%
    clean_names() %>%
    arrange(station_code) %>%
    drop_na(station_code)
}

# run prep_vegan to get all sites × species plus metadata columns
data_vegan_si_su_duplicates.nospp <-
  prep_vegan_nospp() %>%
  arrange(station_code)

# rename for easier downstream wrangling.
# change name when saving. 
# data_vegan_si_su_duplicates.all <- data_vegan_si_su_duplicates.nospp

#### WRANGLE ALL ####

# change all dashes in station_code to underscores
data_vegan_si_su_duplicates.all$station_code <- 
  data_vegan_si_su_duplicates.all$station_code %>%
  str_replace_all("-", "_")

data_su_metadata$station_code_7879 <- 
  data_su_metadata$station_code_7879 %>%
  str_replace_all("-", "_")

data_su_metadata$station_code <- 
  data_su_metadata$station_code %>%
  str_replace_all("-", "_")

# create a station_pair column
data_su_metadata <- data_su_metadata %>%
  mutate(
    station_pair = paste0(station_code_7879, "_", station_code) %>%
      str_replace_all("-", "_")
  )

# build a two‐row per pair lookup:
pair_lookup <- data_su_metadata %>%
  dplyr::select(station_pair, station_code_7879, station_code) %>%
  pivot_longer(
    cols      = c(station_code_7879, station_code),
    names_to  = "which_survey",
    values_to = "station_code"
  ) %>%
  dplyr::select(-which_survey)

# join station_pair onto all:
data_vegan_si_su_duplicates.all <- data_vegan_si_su_duplicates.all %>%
  left_join(pair_lookup, by = "station_code")

# reorder
data_vegan_si_su_duplicates.all <- 
  data_vegan_si_su_duplicates.all %>%
  dplyr::select(
    station_code, 
    station_pair, 
    study, 
    province,
    municipality, 
    date_collected, 
    latitude, 
    longitude, 
    depth_m, 
    sea,
    everything()
  )


#### EXTRACT COMMUNITY & METADATA ####

# community matrix with station_code as first column
data_vegan_si_su_duplicates_community_matrix <- data_vegan_si_su_duplicates.all %>%
  # drop all non‐species columns
  dplyr::select(-station_pair, -study, -province, -municipality, -date_collected, -latitude, -longitude, -depth_m, -sea)

# environmetnal metadata matrix (explanatory variables) with station_code as column name
data_vegan_si_su_duplicates_metadata <- data_vegan_si_su_duplicates.all %>%
  # drop all non‐species columns
  dplyr::select(station_code, station_pair, study, province, municipality, date_collected, latitude, longitude, depth_m, sea)


#### WRANGLE COMMUNITY & METADATA ####
# add a study_sea column
data_vegan_si_su_duplicates_metadata$study_sea <- 
  interaction(
    data_vegan_si_su_duplicates_metadata$study, 
    data_vegan_si_su_duplicates_metadata$sea, sep = "_")

# change study_sea to all lower and underscores
data_vegan_si_su_duplicates_metadata$study_sea <-
  data_vegan_si_su_duplicates_metadata$study_sea %>%
  str_to_lower() %>%
  str_replace_all(" ", "_")

# change the different seas to one word
data_vegan_si_su_duplicates_metadata <- data_vegan_si_su_duplicates_metadata %>%
  mutate(
    sea = recode(sea,
                 "Bohol Sea" = "bohol",
                 "Sulu Sea"  = "sulu"
    )
  )

# manually create the nice_station_pair
station_pair_match <- tribble(
  ~nice_station_pair, ~station_pair,
  "01_bohol", "LK_79_03_SU_22_06",
  "02_bohol", "SP_78_44_SU_22_05",
  "03_bohol", "LK_79_02_SU_22_04",
  "04_bohol", "LK_79_04_SU_22_08",
  "05_bohol", "SP_78_09_SU_22_07",
  "06_bohol", "SP_78_11_SU_22_20",
  "07_bohol", "SP_78_10_SU_22_19",
  "08_bohol", "LK_79_16_SU_22_23",
  "09_bohol", "SP_78_03_SU_22_24",
  "10_bohol", "SP_78_04_SU_19_03",
  "11_bohol", "SP_78_07_SU_19_02",
  "12_bohol", "LK_79_15_SU_22_22",
  "13_bohol", "LK_79_13_SU_19_01",
  "14_sulu",  "SP_78_17_SU_22_18",
  "15_sulu",  "SP_78_20_SU_22_14",
  "16_sulu",  "SP_78_21_SU_22_13",
  "17_sulu",  "SP_78_25_SU_22_16",
  "18_sulu",  "SP_78_26_SU_22_15",
  "19_sulu",  "SP_78_18_SU_22_09",
  "20_sulu",  "SP_78_24_SU_22_11",
  "21_sulu",  "SP_78_27_SU_22_12"
)

# add nice_station_pair column to metadata by station_pair
data_vegan_si_su_duplicates_metadata <- data_vegan_si_su_duplicates_metadata %>%
  left_join(station_pair_match, by = "station_pair")

# create a nice_station_code from study from study and nice_station_pair
data_vegan_si_su_duplicates_metadata <- data_vegan_si_su_duplicates_metadata %>%
  mutate(
    nice_station_code = paste0(str_sub(study, 1, 2), "_", nice_station_pair)
  )

# reorder metadata columns
data_vegan_si_su_duplicates_metadata <- data_vegan_si_su_duplicates_metadata %>%
  dplyr::select(station_code, nice_station_code, station_pair, nice_station_pair, study, sea, study_sea, province, municipality, date_collected, latitude, longitude, depth_m)

# check that the rows are aligned by station_code
identical(data_vegan_si_su_duplicates_community_matrix$station_code, data_vegan_si_su_duplicates_metadata$station_code)

# if false, reorder metadata to match the community matrix
data_vegan_si_su_duplicates_metadata <- 
  data_vegan_si_su_duplicates_metadata[match(data_vegan_si_su_duplicates_community_matrix$station_code, 
                                             data_vegan_si_su_duplicates_metadata$station_code), ]

identical(data_vegan_si_su_duplicates_community_matrix$station_code, data_vegan_si_su_duplicates_metadata$station_code)

# save community matrix file to data/si_su_duplicates
# comm_matrix <- data_vegan_si_su_duplicates_community_matrix
# readr::write_csv(comm_matrix, "../data/si_su_duplicates/data_vegan_si_su_duplicates_community_matrix.csv")
# save metadata file to data/si_su_duplicates 
# metadata <- data_vegan_si_su_duplicates_metadata
# readr::write_csv(metadata, "../data/si_su_duplicates/data_vegan_si_su_duplicates_metadata.csv")
