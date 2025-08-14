#### NOTES ####
# This script was created from EstimateR.R. 
# It was adapted to just focus on the SU-SI duplicate stations. 
# 24 SU duplicates. But this was filtered down to 21 for habitat and sampling effectiveness.
# It does include 3 proxy stations. 


#### INITIALIZATION ####
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#### INSTALL PACKAGES ####
packages_used <- 
  c("rioja",
    "devtools",
    "tidyverse",
    "vegan",
    "ggvegan",
    "dplyr",
    "stringr",
    "clustsig",
    "dendextend",
    "tibble"
    # "remotes",
    # "ggplot2"
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

# install.packages("dendextend")
# install_github("cran/clustsig")
# library(clustsig)
# library(dendextend)

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

# if false, reorder metadata to match the community matrix
# data_vegan.env <- data_vegan.env[match(rownames(data_vegan), data_vegan.env$station_code), ]


#### WRANGLE VEGANIZED DATA ####
# change the rownames in data_vegan to the nice_station_code
# Create a lookup table of old station_code to new nice_station_code
lookup <- data_vegan.env %>%
  dplyr::select(station_code, nice_station_code)

# Reorder the labels to match your data_vegan row order
new_labels <- lookup$nice_station_code[match(rownames(data_vegan), lookup$station_code)]
rownames(data_vegan) <- new_labels


#### Bray-Curtis Distance & Heirarchical clustering ####
bc <- vegdist(data_vegan, method="bray")
hc <- hclust(bc, method="average")


#### SIMPROF CLUSTSIG ####
simprof_res <- simprof(
  data               = data_vegan,
  num.expected       = 1000,           # number of expected clusters
  num.simulated      = 999,            # number of simulated clusters
  method.cluster     = "average",      # "average" = UPGMA
  method.distance    = "braycurtis",   # Bray–Curtis similarity 
  sample.orientation = "row",          # samples are in rows (usual vegan output)
  method.transform   = "identity",     # no transformation
  const              = 0,              # do not apply a constant to be used in adjusting the BC dissimilarity coefficient
  silent             = TRUE,           # A logical value indicating whether anything should be printed during the code execution
  increment          = 100,            # print output every 100 increments
  undef.zero         = TRUE,           # undefined (NA) due to a 0 in denominator will be replaced by 0 
  warn.braycurtis    = TRUE,           # logical value indicating whether a warning should be printed when using the "braycurtis" option
  alpha              = 0.05            # threshold for significance
)


# plot the dendrogram
dend <- simprof.plot(simprof_res, leafcolors=NA, plot=TRUE, fill=TRUE, 
                     leaflab="perpendicular", siglinetype=5)
# save as a dendrogram
dend <- as.dendrogram(dend)

# Make sure your group names (used in label_colors) match your tip_groups
# e.g. "si_1978_bohol_sea", etc.
label_colors <- c(
  "si_1978_sulu_sea"  = "#0072CE",  # SI Sulu Sea: blue
  "su_2022_sulu_sea"  = "#E90303",  # SU Sulu Sea: red
  "si_1978_bohol_sea" = "#024072",  # SI Bohol Sea: navy blue
  "su_2022_bohol_sea" = "#800000"   # SU Bohol Sea: maroon
)

# Build a named vector: names are the tip labels, values are group codes
# nice_station_code must match the labels in dend
tip_groups <- setNames(data_vegan.env$study_sea, data_vegan.env$nice_station_code)

# Assign label colors based on group
dend <- dend %>%
  set("labels_col", value = label_colors[tip_groups[labels(dend)]])

# Set font (base R will try "Times" if available)
par(family = "Times", cex = 1)
plot(dend)

# ggsave(
#   filename = "../figures/si_su_duplicates/simprof_clustsig_species_survey_sea_17.png",
#   plot     = last_plot(),   # or give your plot object here
#   device   = "png",
#   width    = 6.5,
#   height   = 8,
#   units    = "in",
#   dpi      = 300
# )


# Examine a table of the 17 groups
print(datafsimprof_res$significantclusters)
  
# Extract the significantclusters list
sig_clusters <- simprof_res$significantclusters

# For each cluster, create a data.frame of sample indices and their cluster number
df_clusters <- purrr::imap_dfr(
  sig_clusters,
  function(indices, cluster) {
    tibble(
      row_index = indices,
      cluster_id = as.integer(cluster)
    )
  }
)


# re-name row_index to match data_vegan.env$nice_station_code
df_clusters <- df_clusters %>% rename(
  nice_station_code = row_index)

# join significant clusters with environmental data
df_clusters <- df_clusters %>%
  dplyr::left_join(data_vegan.env, by = "nice_station_code") 

print(df_clusters)

# save file
df_clusters %>% write_csv("../tables/si_su_duplicates/table_simprof_species_significant_clusters_17.csv")

#### SIMPROF RIOJA ####
# not completed
# rioja’s version has a different signature:
args(rioja::simprof)
# function (data, sample.order = NULL, perm = 999, method = "B",
#           dendrogram = TRUE, dist.fun = NULL, clust.fun = NULL, ...) 

simprof_res_rioja <- rioja::simprof(
  data         = data_vegan,
  sample.order = rownames(data_vegan),
  perm         = 999,
  method       = "B"        # “B” stands for Bray–Curtis
)

plot(simprof_res_rioja)