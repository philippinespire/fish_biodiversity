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
    "tidyverse",
    "janitor",
    "readxl",
    "vegan",
    "ggvegan"
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


#### DISPERSION-WEIGHTING BY STUDY_SEA ####
# Usage
# dispweight(comm, groups, nsimul = 999, nullmodel = "c0_ind",
#            plimit = 0.05)
# gdispweight(formula, data, plimit = 0.05)


## dispweight and its summary
data_vegan_study_sea.dw <- with(data_vegan.env,                  # metadata file
                                dispweight(data_vegan,           # community matrix
                                           study_sea,            # groups: factor describing the group structure. if missing all sites are regarded as belonging to one group
                                           nsimul    = 999,      # Number of simulations
                                           # nullmodel = "c0_ind"  # The nullmodel used in commsim within groups. The default 'c0_ind' follows Clarke et al 2006
                                           plimit    = 0.05      # downweight species if their p-value is at or below this limit
                                )
)

## IGNORE_RDIFF_BEGIN
summary(data_vegan_study_sea.dw)

# save dispersion-based weighted community matrix file to data/si_su_duplicates
readr::write_csv(data_vegan_study_sea.dw, "../data/si_su_duplicates/data_vegan_si_su_duplicates_community_matrix_study_sea.dw.csv")



#### GENERALIZED DISPERSION-WEIGHTING BY STUDY_SEA & ####
data_vegan_study_sea_depth.gdw <- gdispweight(data_vegan ~ study_sea + depth_m, 
                                        data = data_vegan.env)

data_vegan_study_sea_depth.gdw.rda <- rda(data_vegan_study_sea_depth.gdw ~ study_sea + depth_m, 
                                          data = data_vegan.env)





