#### NOTES ####
# This script was created from EstimateR.R. 
# It was adapted to just focus on the SU-SI duplicate stations. 
# 24 SU duplicates. But this was filtered down to 21 for habitat and sampling effectiveness.
# It does include 3 proxy stations. 


#### INITIALIZATION ####
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#### INSTALL PACKAGES ####
packages_used <- 
  c("rfishbase",
    "devtools",
    "tidyverse",
    "vegan",
    "ggvegan",
    "tidyr",
    "dplyr",
    "stringr",
    "lme4",
    "car",
    "ggplot2",
    "readxl"
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

CAS_verified <- read_excel("../data/All_confirmed_names.xlsx")

taxon_table <- CAS_verified %>%
  dplyr::select(
    species = verified_identification,
    family
  ) %>%
  distinct() %>%
  # Split species into genus and specific epithet
  tidyr::separate(
    col = species,
    into = c("genus", "species_epithet"),
    sep = "_",
    remove = FALSE
  ) %>%
  dplyr::select(species, family, genus, species_epithet)

species_missing <- setdiff(colnames(data_vegan), taxon_table$species)
if(length(species_missing) > 0) {
  print("Some species missing from taxon_table!")
  print(species_missing)
} else {
  print("All species matched.")
}

### ERROR ###
taxon_table <- taxon_table[match(colnames(data_vegan), taxon_table$species), ]
dup_species <- taxon_table$species[duplicated(taxon_table$species)]
print(dup_species)




rownames(taxon_table) <- taxon_table$species


#### TAXONDIVE ####


taxon_table <- tibble(species = colnames(data_vegan)) %>%
  separate(species, into = c("genus", "species_epithet"), sep = "_", remove = FALSE) %>%
  # join family from some source, e.g. CAS_verified
  left_join(CAS_verified %>% select(genus, family) %>% distinct(), by = "genus")





taxon_table %>%
  species <- colnames(data_vegan)


# taxon distance matrix 

# 1. Make sure the order matches the community matrix
taxon_table <- taxon_table[match(colnames(data_vegan), taxon_table$species), ]

# 2. Set species as rownames
rownames(taxon_table) <- taxon_table$species

# 3. Create distance matrix
taxdis <- taxa2dist(
  taxon_table[, c("Family", "Genus", "Species")],
  varstep = TRUE
)
