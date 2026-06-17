############################################################
#### BETA DIVERSITY: SI–SU ROTENONE / ICHTHYOCIDE DATA  ####
#### beta_si_su_duplicates.R                            ####
############################################################


###############
#### SETWD ####
###############

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


##################
#### PACKAGES ####
##################

packages_used <- c(
  "tidyverse",
  "vegan",
  "ggplot2",
  "cowplot",
  "patchwork",
  "janitor"
)

packages_to_install <- packages_used[!packages_used %in% installed.packages()[, 1]]

if (length(packages_to_install) > 0) {
  install.packages(packages_to_install)
}

lapply(packages_used, require, character.only = TRUE)


####################
#### OUTPUT DIRS ####
####################

out_dir_tab <- "../tables/si_su_duplicates/beta_results"
if (!dir.exists(out_dir_tab)) dir.create(out_dir_tab, recursive = TRUE)

out_dir_fig <- "../figures/si_su_duplicates/beta_results"
if (!dir.exists(out_dir_fig)) dir.create(out_dir_fig, recursive = TRUE)


#####################
#### COLOR SCHEMES ####
#####################

era_cols <- c(
  "historical"    = "#F8766D",
  "contemporary" = "#00BFC4"
)

era_name_map <- c(
  "historical"    = "Historical",
  "contemporary" = "Contemporary"
)

sea_cols <- c(
  "bohol" = "#66A61E",  # olive green
  "sulu"  = "#7B3294"   # purple
)

sea_name_map <- c(
  "bohol" = "Bohol",
  "sulu"  = "Sulu"
)

era_sea_cols <- c(
  "historical_bohol"    = "#F8766D",
  "contemporary_bohol" = "#00BFC4",
  "historical_sulu"     = "#C44E52",
  "contemporary_sulu"  = "#4C72B0"
)


# IN PLOTS
# scale_color_manual(
#   values = sea_cols,
#   labels = sea_name_map,
#   name = "Sea"
# ) +
#   scale_fill_manual(
#     values = sea_cols,
#     labels = sea_name_map,
#     name = "Sea"
#   )


#####################
#### IMPORT DATA ####
#####################

# Community matrix: stations × species
# One row per rotenone / ichthyocide station.
# Species columns should be raw abundance counts, not densities.
data_vegan <- readr::read_csv(
  "../data/si_su_duplicates/data_vegan_si_su_duplicates_community_matrix.csv",
  show_col_types = FALSE
) %>%
  janitor::clean_names()

# Metadata: one row per station.
data_vegan.env <- readr::read_csv(
  "../data/si_su_duplicates/data_vegan_si_su_duplicates_metadata.csv",
  show_col_types = FALSE
) %>%
  janitor::clean_names()

# Move station_code to rownames for vegan community matrix.
data_vegan <- data_vegan %>%
  tibble::column_to_rownames("station_code") %>%
  as.data.frame()

# Quick checks.
dim(data_vegan)
dim(data_vegan.env)
head(data_vegan.env)

# Confirm station codes are present.
stopifnot("station_code" %in% names(data_vegan.env))


################
#### DESIGN ####
################
data_vegan.env <- data_vegan.env %>%
  dplyr::mutate(
    era  = recode(study, 
                  "si_1978" = "historical", 
                  "su_2022" = "contemporary"),
    era  = factor(era, levels = c("historical", "contemporary")),
    sea  = factor(sea, levels = c("sulu","bohol"))
  ) 


design <- data_vegan.env %>%
  dplyr::mutate(
    
    # Standardize era.
    # If era already exists, this keeps it.
    # If era is missing or inconsistent, derive it from study.
    era = dplyr::case_when(
      "era" %in% names(.) ~ tolower(as.character(era)),
      study == "si_1978" ~ "historical",
      study == "su_2022" ~ "contemporary",
      TRUE ~ NA_character_
    ),
    
    era = dplyr::recode(
      era,
      "1978" = "historical",
      "1979" = "historical",
      "historical" = "historical",
      "2022" = "contemporary",
      "2019" = "contemporary",
      "modern" = "contemporary",
      "contemporary" = "contemporary",
      .default = era
    ),
    
    era = factor(
      era,
      levels = c("historical", "contemporary")
    ),
    
    # Standardize sea.
    sea = tolower(as.character(sea)),
    sea = factor(
      sea,
      levels = c("bohol", "sulu")
    ),
    
    # Combined group for beta analyses and plotting.
    era_sea = paste(era, sea, sep = "_"),
    era_sea = factor(
      era_sea,
      levels = c(
        "historical_bohol",
        "contemporary_bohol",
        "historical_sulu",
        "contemporary_sulu"
      )
    ),
    
    # Pretty plotting labels.
    era_label = dplyr::recode(as.character(era), !!!era_name_map),
    sea_label = dplyr::recode(as.character(sea), !!!sea_name_map),
    
    era_label = factor(
      era_label,
      levels = c("Historical", "Contemporary")
    ),
    
    sea_label = factor(
      sea_label,
      levels = c("Bohol", "Sulu")
    ),
    
    era_sea_label = paste(era_label, sea_label, sep = " × "),
    
    # Depth covariate.
    depth_m = as.numeric(depth_m),
    depth_scaled = as.numeric(scale(depth_m))
  )

# Check design.
dplyr::count(design, era)
dplyr::count(design, sea)
dplyr::count(design, era, sea)
summary(design$depth_m)


############################################
#### ALIGN COMMUNITY MATRIX AND METADATA ####
############################################

# If rownames are missing or not station codes, assume row order matches metadata.
if (!all(design$station_code %in% rownames(data_vegan))) {
  
  if (nrow(data_vegan) == nrow(design)) {
    rownames(data_vegan) <- design$station_code
  } else {
    stop("Cannot match data_vegan rows to data_vegan.env$station_code.")
  }
}

# Reorder community matrix to match metadata.
X_counts <- data_vegan[design$station_code, , drop = FALSE]

# Confirm alignment.
stopifnot(identical(rownames(X_counts), design$station_code))

# Confirm numeric community matrix.
non_numeric_cols <- names(X_counts)[!vapply(X_counts, is.numeric, logical(1))]

if (length(non_numeric_cols) > 0) {
  stop(
    "Non-numeric species columns found in data_vegan: ",
    paste(non_numeric_cols, collapse = ", ")
  )
}

# Replace NA counts with 0.
X_counts[is.na(X_counts)] <- 0

# Remove species absent from all stations.
X_counts <- X_counts[, colSums(X_counts, na.rm = TRUE) > 0, drop = FALSE]

# Remove stations with zero total abundance, if any.
zero_station <- rowSums(X_counts, na.rm = TRUE) == 0

if (any(zero_station)) {
  warning("Removing stations with zero total abundance: ",
          paste(rownames(X_counts)[zero_station], collapse = ", "))
  
  X_counts <- X_counts[!zero_station, , drop = FALSE]
  design <- design[!zero_station, , drop = FALSE]
}

# Final checks.
dim(X_counts)
dim(design)
stopifnot(identical(rownames(X_counts), design$station_code))


###########################################
#### COMMUNITY TRANSFORMS + DISTANCES  ####
###########################################

# Raw count matrix.
X_raw <- X_counts

# Square-root transformed abundance matrix.
X_sqrt <- sqrt(X_counts)

# Presence-absence matrix, useful as sensitivity analysis.
X_pa <- vegan::decostand(X_counts, method = "pa")

# Hellinger matrix, useful for Euclidean methods such as RDA/dbRDA-style workflows.
X_hell <- vegan::decostand(X_counts, method = "hellinger")

# Bray-Curtis dissimilarity on square-root transformed abundance.
bray_sqrt <- vegan::vegdist(X_sqrt, method = "bray")

# Bray-Curtis dissimilarity on raw abundance, optional sensitivity.
bray_raw <- vegan::vegdist(X_raw, method = "bray")

# Jaccard dissimilarity on presence-absence, optional sensitivity.
jaccard_pa <- vegan::vegdist(X_pa, method = "jaccard", binary = TRUE)


#####################################
#### PERMANOVA: ERA, SEA, DEPTH  ####
#####################################

set.seed(123)

# Main model: temporal change, sea differences, and depth effects.
adonis_era_sea_depth <- vegan::adonis2(
  bray_sqrt ~ era * sea + depth_scaled,
  data = design,
  permutations = 9999,
  by = "terms"
)

adonis_era_sea_depth

# Save Table
# readr::write_csv(
#   as.data.frame(adonis_era_sea_depth) %>%
#     tibble::rownames_to_column("term"),
#   file.path(out_dir_tab, "table_adonis_bray_sqrt_era_sea_depth.csv")
# )


# Marginal tests: evaluates each term after accounting for all others.
adonis_era_sea_depth_margin <- vegan::adonis2(
  bray_sqrt ~ era * sea + depth_scaled,
  data = design,
  permutations = 9999,
  by = "margin"
)

adonis_era_sea_depth_margin

# Save Table
# readr::write_csv(
#   as.data.frame(adonis_era_sea_depth_margin) %>%
#     tibble::rownames_to_column("term"),
#   file.path(out_dir_tab, "table_adonis_bray_sqrt_era_sea_depth_margin.csv")
# )


##################################################
#### TEMPORAL CHANGE WITHIN BOHOL AND SULU    ####
##################################################

adonis_by_sea <- purrr::map_dfr(levels(design$sea), function(sea_i) {
  
  idx <- design$sea == sea_i
  
  dist_i <- vegan::vegdist(X_sqrt[idx, , drop = FALSE], method = "bray")
  design_i <- droplevels(design[idx, , drop = FALSE])
  
  mod_i <- vegan::adonis2(
    dist_i ~ era + depth_scaled,
    data = design_i,
    permutations = 9999,
    by = "terms"
  )
  
  as.data.frame(mod_i) %>%
    tibble::rownames_to_column("term") %>%
    dplyr::mutate(sea = sea_i)
})

adonis_by_sea

# Save Table
# readr::write_csv(
#   adonis_by_sea,
#   file.path(out_dir_tab, "table_adonis_bray_sqrt_era_depth_by_sea.csv")
# )


#########################################
#### BETADISPER: DISPERSION TESTS    ####
#########################################

# Dispersion by era.
bd_era <- vegan::betadisper(bray_sqrt, group = design$era)
bd_era_perm <- vegan::permutest(bd_era, permutations = 9999)

bd_era_perm

# Dispersion by sea.
bd_sea <- vegan::betadisper(bray_sqrt, group = design$sea)
bd_sea_perm <- vegan::permutest(bd_sea, permutations = 9999)

bd_sea_perm

# Dispersion by era × sea.
bd_era_sea <- vegan::betadisper(bray_sqrt, group = design$era_sea)
bd_era_sea_perm <- vegan::permutest(bd_era_sea, permutations = 9999)

bd_era_sea_perm

# Extract distances to centroid for plotting/stat summaries.
disp_df <- tibble::tibble(
  station_code = design$station_code,
  era = design$era,
  sea = design$sea,
  era_sea = design$era_sea,
  era_label = design$era_label,
  sea_label = design$sea_label,
  distance_to_centroid_era = bd_era$distances,
  distance_to_centroid_sea = bd_sea$distances,
  distance_to_centroid_era_sea = bd_era_sea$distances
)

# Save Table
# readr::write_csv(
#   disp_df,
#   file.path(out_dir_tab, "table_betadisper_distances_to_centroid.csv")
# )


#########################
#### NMDS ORDINATION ####
#########################

set.seed(123)

nmds_bray_sqrt <- vegan::metaMDS(
  X_sqrt,
  distance = "bray",
  k = 2,
  trymax = 500,
  autotransform = FALSE,
  trace = FALSE
)

nmds_bray_sqrt

g_nmds_stressplot <- stressplot(nmds_bray_sqrt)

print(g_nmds_stressplot)

ggsave(
  file.path(out_dir_fig, "figure_nmds_bray_sqrt_stressplot.png"),
  g_nmds_stressplot,
  width = 6.5,
  height = 7.5,
  dpi = 300
)

nmds_scores <- vegan::scores(nmds_bray_sqrt, display = "sites") %>%
  as.data.frame() %>%
  tibble::rownames_to_column("station_code") %>%
  dplyr::left_join(
    design,
    by = "station_code"
  )

head(nmds_scores)

# readr::write_csv(
#   nmds_scores,
#   file.path(out_dir_tab, "table_nmds_scores_bray_sqrt.csv")
# )


###################
#### NMDS PLOT ####
###################

g_nmds_era_sea <- ggplot(
  nmds_scores,
  aes(x = NMDS1, y = NMDS2)
) +
  geom_point(
    aes(color = era, shape = sea),
    size = 4,
    alpha = 0.85
  ) +
  # stat_ellipse(
  #   aes(color = era, group = interaction(era, sea)),
  #   linewidth = 0.8,
  #   linetype = "dashed",
  #   level = 0.68,
  #   show.legend = FALSE
  # ) +
  facet_wrap(~ sea_label) +
  scale_color_manual(
    values = era_cols,
    labels = era_name_map,
    name = "Era"
  ) +
  scale_shape_manual(
    values = c(
      "bohol" = 16,
      "sulu" = 17
    ),
    labels = sea_name_map,
    name = "Sea"
  ) +
  labs(
    x = "NMDS1",
    y = "NMDS2"
  ) +
  scale_y_continuous(
    limits = c(-1.75, 1.75),
    breaks = c(-1, 0, 1)
  ) +
  scale_x_continuous(
    limits = c(-1.75, 1.75),
    breaks = c(-1, 0, 1)
  ) +
  theme_classic(base_size = 12) +
  theme(
    text = element_text(family = "Times New Roman"),
    legend.position = "bottom",
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12),
    strip.background = element_blank(),
    strip.text = element_text(size = 12)
  )

print(g_nmds_era_sea)

# ggsave(
#   file.path(out_dir_fig, "figure_nmds_bray_sqrt_era_by_sea_facet_sea_leg_bot_man.png"),
#   g_nmds_era_sea,
#   width = 6.5,
#   height = 7.5,
#   dpi = 300
# )


############################################################
#### dbRDA: DISTANCE-BASED REDUNDANCY ANALYSIS          ####
#### SI–SU rotenone / ichthyocide duplicates            ####
############################################################

# Primary community matrix for dbRDA
# Keep this consistent with PERMANOVA:
# square-root transformed abundance + Bray-Curtis distance.
X_dbrda <- X_sqrt

# Metadata for dbRDA
design_dbrda <- design

# Remove rows with missing model variables.
keep_dbrda <- complete.cases(
  design_dbrda[, c("era", "sea", "depth_scaled")]
)

X_dbrda <- X_dbrda[keep_dbrda, , drop = FALSE]
design_dbrda <- design_dbrda[keep_dbrda, , drop = FALSE] %>%
  droplevels()

# Remove species absent after filtering.
X_dbrda <- X_dbrda[, colSums(X_dbrda, na.rm = TRUE) > 0, drop = FALSE]

# Confirm alignment.
stopifnot(identical(rownames(X_dbrda), design_dbrda$station_code))

# Check sample sizes.
dplyr::count(design_dbrda, era, sea)
summary(design_dbrda$depth_m)

##################################
#### MAIN dbRDA MODEL         ####
##################################

set.seed(123)

mod_dbrda_full <- vegan::dbrda(
  X_dbrda ~ era * sea + depth_scaled,
  data = design_dbrda,
  distance = "bray",
  sqrt.dist = TRUE
)

summary(mod_dbrda_full)


#######################################
#### PERMUTATION TESTS FOR dbRDA   ####
#######################################

set.seed(123)

# Overall constrained model test
anova_dbrda_overall <- anova(
  mod_dbrda_full,
  permutations = 9999
)

anova_dbrda_overall


# Sequential tests: terms tested in model order
anova_dbrda_terms <- anova(
  mod_dbrda_full,
  by = "terms",
  permutations = 9999
)

anova_dbrda_terms


# Marginal tests: each term after accounting for the others
anova_dbrda_margin <- anova(
  mod_dbrda_full,
  by = "margin",
  permutations = 9999
)

anova_dbrda_margin


# Axis tests: significance of constrained dbRDA axes
anova_dbrda_axis <- anova(
  mod_dbrda_full,
  by = "axis",
  permutations = 9999
)

anova_dbrda_axis


#################################
#### SAVE dbRDA TEST TABLES  ####
#################################

write_anova_table <- function(x, file) {
  x %>%
    as.data.frame() %>%
    tibble::rownames_to_column("term") %>%
    readr::write_csv(file)
}

# write_anova_table(
#   anova_dbrda_overall,
#   file.path(out_dir_tab, "table_dbrda_bray_sqrt_overall.csv")
# )
# 
# write_anova_table(
#   anova_dbrda_terms,
#   file.path(out_dir_tab, "table_dbrda_bray_sqrt_terms.csv")
# )
# 
# write_anova_table(
#   anova_dbrda_margin,
#   file.path(out_dir_tab, "table_dbrda_bray_sqrt_margin.csv")
# )
# 
# write_anova_table(
#   anova_dbrda_axis,
#   file.path(out_dir_tab, "table_dbrda_bray_sqrt_axis.csv")
# )


################################
#### dbRDA DIAGNOSTICS      ####
################################

# Adjusted R2 for constrained model
r2_dbrda <- vegan::RsquareAdj(mod_dbrda_full)

r2_dbrda

r2_dbrda_tbl <- tibble::tibble(
  r_squared = r2_dbrda$r.squared,
  adjusted_r_squared = r2_dbrda$adj.r.squared
)

# readr::write_csv(
#   r2_dbrda_tbl,
#   file.path(out_dir_tab, "table_dbrda_bray_sqrt_r2.csv")
# )

# Variance inflation factors for constraints
# Useful for checking collinearity among predictors.
vif_dbrda <- vegan::vif.cca(mod_dbrda_full)

vif_dbrda_tbl <- tibble::tibble(
  term = names(vif_dbrda),
  vif = as.numeric(vif_dbrda)
)

vif_dbrda_tbl

# readr::write_csv(
#   vif_dbrda_tbl,
#   file.path(out_dir_tab, "table_dbrda_bray_sqrt_vif.csv")
# )


################################
#### EXTRACT dbRDA SCORES   ####
################################


site_scores_tmp <- vegan::scores(
  mod_dbrda_full,
  display = "sites",
  choices = 1:2,
  scaling = 2
) %>%
  as.data.frame()

names(site_scores_tmp)
head(site_scores_tmp)

# Site scores
site_scores_dbrda <- vegan::scores(
  mod_dbrda_full,
  display = "sites",
  choices = 1:2,
  scaling = 2
) %>%
  as.data.frame() %>%
  tibble::rownames_to_column("station_code") %>%
  dplyr::rename(
    dbRDA1 = dplyr::any_of(c("dbRDA1", "CAP1")),
    dbRDA2 = dplyr::any_of(c("dbRDA2", "CAP2", "MDS1"))
  ) %>%
  dplyr::left_join(
    design_dbrda,
    by = "station_code"
  )

head(site_scores_dbrda)


#####################################
#### EXTRACT dbRDA BIPLOT SCORES ####
#####################################

bp_scores_raw <- vegan::scores(
  mod_dbrda_full,
  display = "bp",
  choices = 1:2,
  scaling = 2
)

bp_scores_dbrda <- NULL

if (!is.null(bp_scores_raw)) {
  
  bp_scores_raw <- bp_scores_raw %>%
    as.data.frame()
  
  bp_axis_cols <- names(bp_scores_raw)[1:2]
  
  bp_scores_dbrda <- bp_scores_raw %>%
    dplyr::rename(
      dbRDA1 = dplyr::all_of(bp_axis_cols[1]),
      dbRDA2 = dplyr::all_of(bp_axis_cols[2])
    ) %>%
    tibble::rownames_to_column("variable")
  
  print(bp_scores_dbrda)
}

#########################################
#### CLEAN dbRDA VECTOR LABELS       ####
#########################################

bp_scores_dbrda <- bp_scores_dbrda %>%
  dplyr::mutate(
    variable_label = dplyr::recode(
      variable,
      "eracontemporary" = "Contemporary",
      "seasulu" = "Sulu Sea",
      "depth_scaled" = "Depth",
      "eracontemporary:seasulu" = "Contemporary × Sulu",
      .default = variable
    )
  )

bp_scores_dbrda

bp_scores_depth <- bp_scores_dbrda %>%
  dplyr::filter(variable == "depth_scaled") %>%
  dplyr::mutate(variable_label = "Depth")

# Constrained-axis variance labels
eig_con <- vegan::eigenvals(
  mod_dbrda_full,
  model = "constrained"
)

axis_pct <- 100 * eig_con / sum(eig_con)

x_lab <- paste0("dbRDA1 (", round(axis_pct[1], 1), "% constrained variation)")

if (length(axis_pct) >= 2) {
  y_lab <- paste0("dbRDA2 (", round(axis_pct[2], 1), "% constrained variation)")
} else {
  y_lab <- "Axis 2"
}


########################
#### dbRDA PLOT     ####
########################

g_dbrda_era_sea <- ggplot(
  site_scores_dbrda,
  aes(x = dbRDA1, y = dbRDA2)
) +
  #geom_hline(yintercept = 0, linewidth = 0.3, linetype = "dashed", color = "grey60") +
  #geom_vline(xintercept = 0, linewidth = 0.3, linetype = "dashed", color = "grey60") +
  geom_point(
    aes(color = era, shape = sea),
    size = 4,
    alpha = 0.85
  ) +
  # stat_ellipse(
  #   aes(color = era, group = interaction(era, sea)),
  #   linewidth = 0.8,
  #   linetype = "dashed",
  #   level = 0.68,
  #   show.legend = FALSE
  # ) +
  scale_color_manual(
    values = era_cols,
    labels = era_name_map,
    name = "Era"
  ) +
  scale_shape_manual(
    values = c(
      "bohol" = 16,
      "sulu" = 17
    ),
    labels = sea_name_map,
    name = "Sea"
  ) +
  labs(
    x = x_lab,
    y = y_lab
  ) +
  scale_y_continuous(
    limits = c(-2, 2),
    breaks = seq(-2, 2, 1)
  ) +
  scale_x_continuous(
    limits = c(-2, 2),
    breaks = seq(-2, 2, 1)
  ) +
  theme_classic(base_size = 12) +
  theme(
    text = element_text(family = "Times New Roman"),
    legend.position = "bottom",
    strip.background = element_blank(),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12)
  )

print(g_dbrda_era_sea)

# ggsave(
#   file.path(out_dir_fig, "figure_dbrda_bray_sqrt_era_sea_leg_bot_man.png"),
#   g_dbrda_era_sea,
#   width = 6.5,
#   height = 7.5,
#   dpi = 300
# )


##################################
#### dbRDA PLOT WITH DEPTH    ####
##################################

# Scale arrows so they are visible on the site-score plot.
arrow_mult <- 1.2 * min(
  diff(range(site_scores_dbrda$dbRDA1, na.rm = TRUE)) / diff(range(bp_scores_dbrda$dbRDA1, na.rm = TRUE)),
  diff(range(site_scores_dbrda$dbRDA2, na.rm = TRUE)) / diff(range(bp_scores_dbrda$dbRDA2, na.rm = TRUE))
)

# bp_scores_plot <- bp_scores_dbrda %>%
#   dplyr::mutate(
#     dbRDA1_arrow = dbRDA1 * arrow_mult,
#     dbRDA2_arrow = dbRDA2 * arrow_mult
#   )

bp_scores_depth_plot <- bp_scores_dbrda %>%
  dplyr::filter(variable == "depth_scaled") %>%
  dplyr::mutate(
    variable_label = "Depth",
    # dbRDA1_arrow = dbRDA1 * arrow_mult,
    # dbRDA2_arrow = dbRDA2 * arrow_mult
  )

g_dbrda_era_sea_vector_all <- g_dbrda_era_sea +
  geom_segment(
    data = bp_scores_dbrda,
    aes(
      x = 0,
      y = 0,
      xend = dbRDA1,
      yend = dbRDA2
    ),
    inherit.aes = FALSE,
    arrow = arrow(length = unit(0.25, "cm")),
    linewidth = 0.8,
    color = "black"
  ) +
  geom_text(
    data = bp_scores_dbrda,
    aes(
      x = dbRDA1,
      y = dbRDA2,
      label = variable_label
    ),
    inherit.aes = FALSE,
    family = "Times New Roman",
    size = 4.5,
    hjust = 0.5,
    vjust = -0.6
  )

print(g_dbrda_era_sea_vector_all)

# ggsave(
#   file.path(out_dir_fig, "figure_dbrda_bray_sqrt_era_sea_leg_bot_all_vector_man.png"),
#   g_dbrda_era_sea_vector_all,
#   width = 6.5,
#   height = 7.5,
#   dpi = 300
# )

#### DEPTH ONLY 
g_dbrda_era_sea_vector_depth <- g_dbrda_era_sea +
  geom_segment(
    data = bp_scores_depth_plot,
    aes(
      x = 0,
      y = 0,
      xend = dbRDA1,
      yend = dbRDA2
    ),
    inherit.aes = FALSE,
    arrow = arrow(length = unit(0.25, "cm")),
    linewidth = 0.8,
    color = "black"
  ) +
  geom_text(
    data = bp_scores_ddepth_plot,
    aes(
      x = dbRDA1,
      y = dbRDA2,
      label = variable_label
    ),
    inherit.aes = FALSE,
    family = "Times New Roman",
    size = 4.5,
    hjust = 0.5,
    vjust = -0.6
  )

print(g_dbrda_era_sea_vector_depth)

# ggsave(
#   file.path(out_dir_fig, "figure_dbrda_bray_sqrt_era_sea_leg_bot_depth_vector_man.png"),
#   g_dbrda_era_sea_vector_depth,
#   width = 6.5,
#   height = 7.5,
#   dpi = 300
# )


################################################
#### dbRDA TEMPORAL CHANGE WITHIN EACH SEA  ####
################################################

dbrda_by_sea <- purrr::map(levels(design_dbrda$sea), function(sea_i) {
  
  idx <- design_dbrda$sea == sea_i
  
  X_i <- X_dbrda[idx, , drop = FALSE]
  design_i <- design_dbrda[idx, , drop = FALSE] %>%
    droplevels()
  
  # Remove species absent within this sea.
  X_i <- X_i[, colSums(X_i, na.rm = TRUE) > 0, drop = FALSE]
  
  mod_i <- vegan::dbrda(
    X_i ~ era + depth_scaled,
    data = design_i,
    distance = "bray",
    sqrt.dist = TRUE
  )
  
  list(
    sea = sea_i,
    model = mod_i,
    overall = anova(mod_i, permutations = 9999),
    terms = anova(mod_i, by = "terms", permutations = 9999),
    margin = anova(mod_i, by = "margin", permutations = 9999),
    r2 = vegan::RsquareAdj(mod_i)
  )
})

# Save tests by sea
dbrda_by_sea_terms <- purrr::map_dfr(dbrda_by_sea, function(x) {
  as.data.frame(x$terms) %>%
    tibble::rownames_to_column("term") %>%
    dplyr::mutate(sea = x$sea)
})

dbrda_by_sea_margin <- purrr::map_dfr(dbrda_by_sea, function(x) {
  as.data.frame(x$margin) %>%
    tibble::rownames_to_column("term") %>%
    dplyr::mutate(sea = x$sea)
})

dbrda_by_sea_r2 <- purrr::map_dfr(dbrda_by_sea, function(x) {
  tibble::tibble(
    sea = x$sea,
    r_squared = x$r2$r.squared,
    adjusted_r_squared = x$r2$adj.r.squared
  )
})

# readr::write_csv(
#   dbrda_by_sea_terms,
#   file.path(out_dir_tab, "table_dbrda_bray_sqrt_terms_by_sea.csv")
# )
# 
# readr::write_csv(
#   dbrda_by_sea_margin,
#   file.path(out_dir_tab, "table_dbrda_bray_sqrt_margin_by_sea.csv")
# )
# 
# readr::write_csv(
#   dbrda_by_sea_r2,
#   file.path(out_dir_tab, "table_dbrda_bray_sqrt_r2_by_sea.csv")
# )


##############################################
#### NMDS ENVFIT: DEPTH ENVIRONMENTAL FIT ####
##############################################

# Make sure metadata are aligned with NMDS scores
design_env <- nmds_scores %>%
  dplyr::mutate(
    era = factor(era, levels = c("historical", "contemporary")),
    sea = factor(sea, levels = c("bohol", "sulu")),
    era_sea = interaction(era, sea, sep = "_", drop = TRUE),
    depth_m = as.numeric(depth_m),
    depth_scaled = as.numeric(scale(depth_m))
  )

# Check grouping
dplyr::count(design_env, era, sea, era_sea)


set.seed(123)

envfit_depth_era_sea <- vegan::envfit(
  nmds_bray_sqrt ~ depth_scaled,
  data = design_env,
  permutations = 9999,
  strata = design_env$era_sea,
  na.rm = TRUE
)

envfit_depth_era_sea

#############################################
#### EXTRACT ENVFIT DEPTH RESULTS TABLE  ####
#############################################

# Get variable names from the vector scores, not from names(pvals)
envfit_vec_names <- rownames(envfit_depth_era_sea$vectors$arrows)

envfit_depth_tbl <- tibble::tibble(
  variable = envfit_vec_names,
  R2 = as.numeric(envfit_depth_era_sea$vectors$r),
  p_value = as.numeric(envfit_depth_era_sea$vectors$pvals)
) %>%
  dplyr::mutate(
    variable_label = dplyr::recode(
      .data$variable,
      "depth_scaled" = "Depth",
      .default = .data$variable
    ),
    significant = p_value <= 0.05
  )

envfit_depth_tbl

# Save Table
# write_csv(
#   envfit_depth_tbl,
#   file.path(out_dir_tab, "nmds_envfit_depth_era_sea_restricted.csv")
# )

################################################
#### EXTRACT ENVFIT VECTOR COORDS W/ FACET  ####
################################################

envfit_depth_scores <- vegan::scores(
  envfit_depth_era_sea,
  display = "vectors"
) %>%
  as.data.frame() %>%
  tibble::rownames_to_column("variable") %>%
  dplyr::left_join(envfit_depth_tbl, by = "variable") %>%
  dplyr::filter(significant)

envfit_depth_scores


# Scale arrow to fit within your current NMDS limits
nmds_xlim <- c(-1.75, 1.75)
nmds_ylim <- c(-1.75, 1.75)

if (nrow(envfit_depth_scores) > 0) {
  
  arrow_mult_nmds <- 0.75 * min(
    max(abs(nmds_xlim)) / max(abs(envfit_depth_scores$NMDS1), na.rm = TRUE),
    max(abs(nmds_ylim)) / max(abs(envfit_depth_scores$NMDS2), na.rm = TRUE)
  )
  
  envfit_depth_plot <- envfit_depth_scores %>%
    dplyr::mutate(
      NMDS1_arrow = NMDS1 * arrow_mult_nmds,
      NMDS2_arrow = NMDS2 * arrow_mult_nmds
    )
  
  # Repeat the same overall envfit vector in each sea facet
  envfit_depth_plot_facet <- tidyr::crossing(
    sea_label = unique(nmds_scores$sea_label),
    envfit_depth_plot
  )
  
  g_nmds_era_sea_envfit_facet <- g_nmds_era_sea +
    geom_segment(
      data = envfit_depth_plot_facet,
      aes(
        x = 0,
        y = 0,
        xend = NMDS1_arrow,
        yend = NMDS2_arrow
      ),
      inherit.aes = FALSE,
      arrow = grid::arrow(length = grid::unit(0.25, "cm")),
      linewidth = 0.9,
      color = "black"
    ) +
    geom_text(
      data = envfit_depth_plot_facet,
      aes(
        x = NMDS1_arrow,
        y = NMDS2_arrow,
        label = variable_label
      ),
      inherit.aes = FALSE,
      family = "Times New Roman",
      size = 4.5,
      hjust = 0.5,
      vjust = -0.9,
      color = "black"
    )
  
  print(g_nmds_era_sea_envfit_facet)
}

# ggsave(
#   file.path(out_dir_fig, "figure_nmds_bray_sqrt_era_by_sea_facet_sea_vector_depth_leg_bot_man.png"),
#   g_nmds_era_sea_envfit_facet,
#   width = 6.5,
#   height = 7.5,
#   dpi = 300
# )


##############################
#### NMDS PLOT, NO FACET  ####
##############################

g_nmds_era_sea_envfit <- ggplot(
  nmds_scores,
  aes(x = NMDS1, y = NMDS2)
) +
  geom_point(
    aes(color = era, shape = sea),
    size = 4,
    alpha = 0.85
  ) +
  geom_segment(
    data = envfit_depth_plot,
    aes(
      x = 0,
      y = 0,
      xend = NMDS1_arrow,
      yend = NMDS2_arrow
    ),
    inherit.aes = FALSE,
    arrow = grid::arrow(length = grid::unit(0.25, "cm")),
    linewidth = 0.9,
    color = "black"
  ) +
  geom_text(
    data = envfit_depth_plot,
    aes(
      x = NMDS1_arrow,
      y = NMDS2_arrow,
      label = variable_label
    ),
    inherit.aes = FALSE,
    family = "Times New Roman",
    size = 5,
    hjust = 0.5,
    vjust = -0.9,
    color = "black"
  ) +
  scale_color_manual(
    values = era_cols,
    labels = era_name_map,
    name = "Era"
  ) +
  scale_shape_manual(
    values = c(
      "bohol" = 16,
      "sulu" = 17
    ),
    labels = sea_name_map,
    name = "Sea"
  ) +
  coord_cartesian(
    xlim = nmds_xlim,
    ylim = nmds_ylim,
    clip = "off"
  ) +
  scale_x_continuous(
    breaks = c(-1, 0, 1)
  ) +
  scale_y_continuous(
    breaks = c(-1, 0, 1)
  ) +
  labs(
    x = "NMDS1",
    y = "NMDS2"
  ) +
  theme_classic(base_size = 12) +
  theme(
    text = element_text(family = "Times New Roman"),
    legend.position = "bottom",
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12),
    plot.margin = margin(5.5, 20, 5.5, 5.5)
  )

print(g_nmds_era_sea_envfit)

# ggsave(
#   file.path(out_dir_fig, "figure_nmds_bray_sqrt_era_by_sea_vector_depth_leg_bot_man.png"),
#   g_nmds_era_sea_envfit,
#   width = 6.5,
#   height = 7.5,
#   dpi = 300
# )



#########################################
#### SENSITIVITY ENVFIT TESTS        ####
#########################################

set.seed(123)

# 1. Unrestricted: descriptive, but can be confounded by sea/era
envfit_depth_unrestricted <- vegan::envfit(
  nmds_bray_sqrt ~ depth_scaled,
  data = design_env,
  permutations = 9999,
  na.rm = TRUE
)

# 2. Sea-restricted: controls broad sea differences
envfit_depth_sea_restricted <- vegan::envfit(
  nmds_bray_sqrt ~ depth_scaled,
  data = design_env,
  permutations = 9999,
  strata = design_env$sea,
  na.rm = TRUE
)

# 3. Era × sea restricted: most conservative for this design
envfit_depth_era_sea <- vegan::envfit(
  nmds_bray_sqrt ~ depth_scaled,
  data = design_env,
  permutations = 9999,
  strata = design_env$era_sea,
  na.rm = TRUE
)

extract_envfit_vector <- function(fit, model_name) {
 tibble::tibble(
    model = model_name,
    variable = rownames(fit$vectors$arrows),
    R2 = as.numeric(fit$vectors$r),
    p_value = as.numeric(fit$vectors$pvals)
  )
}

envfit_depth_compare <- dplyr::bind_rows(
  extract_envfit_vector(envfit_depth_unrestricted, "Unrestricted"),
  extract_envfit_vector(envfit_depth_sea_restricted, "Restricted within sea"),
  extract_envfit_vector(envfit_depth_era_sea, "Restricted within era × sea")
) %>%
  dplyr::mutate(
    variable_label = dplyr::recode(
      variable,
      "depth_scaled" = "Depth",
      .default = variable
    )
  )

envfit_depth_compare

# Save Table
# write_csv(
#   envfit_depth_compare,
#   file.path(out_dir_tab, "nmds_envfit_depth_sensitivity.csv")
# )


#######################################
#### PREPARE ENVIRONMENTAL FITTING ####
#######################################

design_env <- design %>%
  dplyr::mutate(
    depth_m = as.numeric(depth_m),
    depth_scaled = as.numeric(scale(depth_m)),
    municipality = factor(municipality),
    sea = factor(sea, levels = c("bohol", "sulu")),
    era = factor(era, levels = c("historical", "contemporary"))
  ) %>%
  droplevels()

# Check sample sizes by municipality
dplyr::count(design_env, sea, municipality, era)

# Optional: check if municipality is nested within sea
dplyr::count(design_env, municipality, sea)


###############################################
#### ENVFIT: DEPTH AND SPATIAL FACTORS     ####
###############################################

set.seed(123)

envfit_all <- vegan::envfit(
  nmds_bray_sqrt ~ depth_scaled + era + sea,
  data = design_env,
  permutations = 10000,
  na.rm = TRUE
)

envfit_depth <- vegan::envfit(
  nmds_bray_sqrt ~ depth_scaled,
  data = design_env,
  permutations = 9999,
  na.rm = TRUE
)

envfit_municipality <- vegan::envfit(
  nmds_bray_sqrt ~ municipality,
  data = design_env,
  permutations = 9999,
  na.rm = TRUE
)

envfit_sea <- vegan::envfit(
  nmds_bray_sqrt ~ sea,
  data = design_env,
  permutations = 9999,
  na.rm = TRUE
)

envfit_era <- vegan::envfit(
  nmds_bray_sqrt ~ era,
  data = design_env,
  permutations = 9999,
  na.rm = TRUE
)

envfit_depth
envfit_municipality
envfit_sea
envfit_era


###################################
#### SAVE ENVFIT RESULT TABLES ####
###################################

# Continuous vector results, such as depth
envfit_vectors_tbl <- tibble::tibble(
  variable = names(envfit_all$vectors$r),
  r2 = as.numeric(envfit_all$vectors$r),
  p_value = as.numeric(envfit_all$vectors$pvals)
)

envfit_vectors_tbl

readr::write_csv(
  envfit_vectors_tbl,
  file.path(out_dir_tab, "table_envfit_vectors_nmds_bray_sqrt.csv")
)


# Factor results, such as era, sea, municipality
envfit_factors_tbl <- tibble::tibble(
  variable = names(envfit_all$factors$r),
  r2 = as.numeric(envfit_all$factors$r),
  p_value = as.numeric(envfit_all$factors$pvals)
)

envfit_factors_tbl

readr::write_csv(
  envfit_factors_tbl,
  file.path(out_dir_tab, "table_envfit_factors_nmds_bray_sqrt.csv")
)


#################################
#### EXTRACT ENVFIT SCORES   ####
#################################

# Depth vector
depth_vec <- vegan::scores(
  envfit_all,
  display = "vectors"
) %>%
  as.data.frame() %>%
  tibble::rownames_to_column("variable") %>%
  dplyr::filter(variable == "depth_scaled")

names(depth_vec)[names(depth_vec) == "NMDS1"] <- "x"
names(depth_vec)[names(depth_vec) == "NMDS2"] <- "y"

# Scale arrow to fit the NMDS plot
site_x_range <- diff(range(nmds_scores$NMDS1, na.rm = TRUE))
site_y_range <- diff(range(nmds_scores$NMDS2, na.rm = TRUE))
site_range <- min(site_x_range, site_y_range)

vec_len <- sqrt(depth_vec$x^2 + depth_vec$y^2)

arrow_mult <- 0.35 * site_range / vec_len

depth_vec_plot <- depth_vec %>%
  dplyr::mutate(
    xend = x * arrow_mult,
    yend = y * arrow_mult,
    label = "Depth"
  )

depth_vec_plot


#########################################
#### EXTRACT MUNICIPALITY CENTROIDS  ####
#########################################

municipality_centroids <- vegan::scores(
  envfit_all,
  display = "factors"
) %>%
  as.data.frame() %>%
  tibble::rownames_to_column("factor_level")

names(municipality_centroids)[names(municipality_centroids) == "NMDS1"] <- "NMDS1"
names(municipality_centroids)[names(municipality_centroids) == "NMDS2"] <- "NMDS2"

municipality_centroids <- municipality_centroids %>%
  dplyr::filter(grepl("^municipality", factor_level)) %>%
  dplyr::mutate(
    municipality = stringr::str_remove(factor_level, "^municipality")
  )

municipality_centroids


#####################################
#### NMDS + ENVFIT DEPTH VECTOR  ####
#####################################

g_nmds_envfit_depth <- ggplot(
  nmds_scores,
  aes(x = NMDS1, y = NMDS2)
) +
  geom_point(
    aes(color = era, shape = sea),
    size = 4,
    alpha = 0.85
  ) +
  stat_ellipse(
    aes(color = era, group = interaction(era, sea)),
    linewidth = 0.8,
    linetype = "dashed",
    level = 0.68,
    show.legend = FALSE
  ) +
  geom_segment(
    data = depth_vec_plot,
    aes(x = 0, y = 0, xend = xend, yend = yend),
    inherit.aes = FALSE,
    arrow = arrow(length = unit(0.25, "cm")),
    linewidth = 0.9,
    color = "black"
  ) +
  geom_text(
    data = depth_vec_plot,
    aes(x = xend, y = yend, label = label),
    inherit.aes = FALSE,
    family = "Times New Roman",
    size = 5,
    hjust = -0.1,
    vjust = -0.4
  ) +
  scale_color_manual(
    values = era_cols,
    labels = era_name_map,
    name = "Era"
  ) +
  scale_shape_manual(
    values = c(
      "bohol" = 16,
      "sulu" = 17
    ),
    labels = sea_name_map,
    name = "Sea"
  ) +
  labs(
    x = "NMDS1",
    y = "NMDS2"
  ) +
  theme_classic(base_size = 18) +
  theme(
    text = element_text(family = "Times New Roman"),
    legend.position = "top",
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16)
  )

print(g_nmds_envfit_depth)

ggsave(
  file.path(out_dir_fig, "figure_nmds_bray_sqrt_envfit_depth.png"),
  g_nmds_envfit_depth,
  width = 8,
  height = 6.5,
  dpi = 300
)


##############################################
#### NMDS + MUNICIPALITY ENVFIT CENTROIDS  ####
##############################################

g_nmds_envfit_municipality <- ggplot(
  nmds_scores,
  aes(x = NMDS1, y = NMDS2)
) +
  geom_point(
    aes(color = era, shape = sea),
    size = 4,
    alpha = 0.85
  ) +
  geom_text(
    data = municipality_centroids,
    aes(x = NMDS1, y = NMDS2, label = municipality),
    inherit.aes = FALSE,
    family = "Times New Roman",
    size = 4.5,
    fontface = "bold"
  ) +
  scale_color_manual(
    values = era_cols,
    labels = era_name_map,
    name = "Era"
  ) +
  scale_shape_manual(
    values = c(
      "bohol" = 16,
      "sulu" = 17
    ),
    labels = sea_name_map,
    name = "Sea"
  ) +
  labs(
    x = "NMDS1",
    y = "NMDS2"
  ) +
  theme_classic(base_size = 18) +
  theme(
    text = element_text(family = "Times New Roman"),
    legend.position = "top",
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16)
  )

print(g_nmds_envfit_municipality)

ggsave(
  file.path(out_dir_fig, "figure_nmds_bray_sqrt_envfit_municipality.png"),
  g_nmds_envfit_municipality,
  width = 8,
  height = 6.5,
  dpi = 300
)


bray_sqrt ~ era * sea + depth_scaled
bray_sqrt ~ era + municipality + depth_scaled
bray_sqrt ~ era + depth_scaled + Condition(municipality)

####################################################
#### dbRDA WITH MUNICIPALITY AS SPATIAL FACTOR   ####
####################################################

mod_dbrda_muni <- vegan::dbrda(
  X_dbrda ~ era + municipality + depth_scaled,
  data = design_dbrda,
  distance = "bray",
  sqrt.dist = TRUE
)

anova_dbrda_muni_terms <- anova(
  mod_dbrda_muni,
  by = "terms",
  permutations = 9999
)

anova_dbrda_muni_margin <- anova(
  mod_dbrda_muni,
  by = "margin",
  permutations = 9999
)

anova_dbrda_muni_terms
anova_dbrda_muni_margin

vegan::RsquareAdj(mod_dbrda_muni)
vegan::vif.cca(mod_dbrda_muni)


###########################################################
#### PARTIAL dbRDA: ERA AND DEPTH AFTER MUNICIPALITY   ####
###########################################################

mod_dbrda_muni_partial <- vegan::dbrda(
  X_dbrda ~ era + depth_scaled + Condition(municipality),
  data = design_dbrda,
  distance = "bray",
  sqrt.dist = TRUE
)

anova_dbrda_muni_partial_terms <- anova(
  mod_dbrda_muni_partial,
  by = "terms",
  permutations = 9999
)

anova_dbrda_muni_partial_margin <- anova(
  mod_dbrda_muni_partial,
  by = "margin",
  permutations = 9999
)

anova_dbrda_muni_partial_terms
anova_dbrda_muni_partial_margin

vegan::RsquareAdj(mod_dbrda_muni_partial)
















################################################################
################################################################
#### README ####
################################################################
################################################################

# Created by John Whalen
# Date: 5/1/26
# ICHTHYOCIDE


#### SETWD ####

# set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#### PACKAGES ####
packages_used <- 
  c("tidyverse",
    "permute",
    "vegan",
    "pairwiseAdonis",
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


####################
#### OUTPUT DIR ####
####################
out_dir_tab <- "../tables/si_su_duplicates/inext_results"
# if (!dir.exists(out_dir_tab)) dir.create(out_dir_tab, recursive = TRUE)
out_dir_fig <- "../figures/si_su_duplicates/inext_results"
# if (!dir.exists(out_dir_fig)) dir.create(out_dir_fig, recursive = TRUE)


########################
#### COLORS / LABELS ####
########################

era_cols <- c(
  "historical"    = "#F8766D",
  "contemporary" = "#00BFC4"
)

era_name_map <- c(
  "historical"    = "Historical",
  "contemporary" = "Contemporary"
)


#####################
#### IMPORT DATA ####
#####################
# READ IN VEGANIZED DATA #

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


################
#### DESIGN ####
################
data_vegan.env <- data_vegan.env %>%
  dplyr::mutate(
    era  = recode(study, 
                  "si_1978" = "historical", 
                  "su_2022" = "contemporary"),
    era  = factor(era, levels = c("historical", "contemporary")),
    sea  = factor(sea, levels = c("sulu","bohol"))
  ) 

# Pretty names (project-wide)
sea_name_map <- c(bohol = "Bohol", sulu = "Sulu")
era_name_map <- c(historical = "Historical", contemporary = "Contemporary")

# If station_code is still a column in data_vegan, move it to rownames.
if ("station_code" %in% names(data_vegan)) {
  X_counts <- data_vegan %>%
    tibble::column_to_rownames("station_code") %>%
    as.data.frame()
} else {
  X_counts <- as.data.frame(data_vegan)
}

# Standardize metadata.
design <- data_vegan.env %>%
  dplyr::mutate(
    era = tolower(as.character(era)),
    era = factor(era, levels = c("historical", "contemporary")),
    sea = factor(as.character(sea), levels = c("bohol", "sulu"))
  )

# If rownames are missing or not station codes, assume current row order matches metadata.
if (!all(design$station_code %in% rownames(X_counts))) {
  if (nrow(X_counts) == nrow(design)) {
    rownames(X_counts) <- design$station_code
  } else {
    stop("data_vegan rows cannot be matched to data_vegan.env$station_code.")
  }
}

# Reorder community matrix to match metadata.
X_counts <- X_counts[design$station_code, , drop = FALSE]

# Confirm alignment.
stopifnot(identical(rownames(X_counts), design$station_code))

# Confirm that all community columns are numeric.
non_numeric_cols <- names(X_counts)[!vapply(X_counts, is.numeric, logical(1))]

if (length(non_numeric_cols) > 0) {
  stop(
    "These columns in data_vegan are not numeric species-count columns: ",
    paste(non_numeric_cols, collapse = ", ")
  )
}

# Check for missing or invalid era values.
if (any(is.na(design$era))) {
  stop("Some rows in data_vegan.env have missing or non-standard era values.")
}

# Check that abundance data are non-negative.
if (any(as.matrix(X_counts) < 0, na.rm = TRUE)) {
  stop("Negative abundance values detected in data_vegan.")
}

# Optional warning if counts are not integers.
if (any(abs(as.matrix(X_counts) - round(as.matrix(X_counts))) > 1e-8, na.rm = TRUE)) {
  warning(
    "Some values in data_vegan are not integers. ",
    "For iNEXT datatype = 'abundance', use raw individual counts, not densities."
  )
}

# Remove species columns with zero individuals across the entire dataset.
X_counts <- X_counts[, colSums(X_counts, na.rm = TRUE) > 0, drop = FALSE]


################################################################
################################################################
#### FVC-SPECIFIC ####
################################################################
################################################################
# Bray on √(density per 100 m²) — primary
X_bray <- sqrt(as.matrix(data_vegan_density_per100))
bray_sub <- function(idx) vegdist(X_bray[idx, , drop = FALSE], method = "bray")

# OPTIONAL: Euclidean on Hellinger matrix (set TRUE to run)
USE_HELL <- FALSE
X_hell   <- as.matrix(data_vegan_hell)
hell_sub <- function(idx) dist(X_hell[idx, , drop = FALSE], method = "euclidean")


#### HELPERS ####
tidy_adonis <- function(ad, label) {
  tibble::as_tibble(ad, rownames = "Term") %>%
    dplyr::mutate(model = label, .before = 1)
}

tidy_permutest <- function(pt, label, nperm = NULL) {
  tab <- as_tibble(pt$tab, rownames = "Term")
  if (!"N.Perm" %in% names(tab)) {
    tab$`N.Perm` <- if (!is.null(nperm)) nperm else tryCatch(pt$permutations, error = function(e) NA)
  }
  tab %>% mutate(test = label, .before = 1)
}

### PAIRED !?
pairwise_site_tests <- function(sub, d) {
  lv <- levels(sub$site)
  if (length(lv) < 2) return(tibble())
  combn(lv, 2, simplify = FALSE) %>%
    purrr::map_dfr(function(p) {
      take <- sub$site %in% p
      sub2 <- droplevels(sub[take, ])
      d2   <- as.dist(as.matrix(d)[take, take])
      ad2  <- adonis2(d2 ~ site, data = sub2, permutations = 9999)
      tibble(
        contrast = paste(p, collapse = " vs "),
        F        = ad2$F[1],
        R2       = ad2$R2[1],
        p        = ad2$`Pr(>F)`[1],
        n1       = sum(sub2$site == p[1]),
        n2       = sum(sub2$site == p[2])
      )
    }) %>%
    mutate(p_adj_bh = p.adjust(p, method = "BH"))
}


#### A) GLOBAL TWO-FACTOR PERMANOVA (unpaired)
# Model: distance ~ year * site (Bray on √density)
idx_all <- seq_len(nrow(design))
D_all   <- bray_sub(idx_all)
ad_global <- adonis2(D_all ~ year * site, data = design, permutations = 9999, by = "margin")
# readr::write_csv(tidy_adonis(ad_global, "Bray √density: year * site"),
#                  "../data/vegan_results/vegan_step2_beta/table_braycurtis_permanova_global_year_by_site.csv")


#### DISPERSION CHECKS ####
# Dispersion checks that correspond to the factors:
# (i) Dispersion by site (all years pooled)
bd_site <- betadisper(D_all, group = design$site)
pt_site <- permutest(bd_site, permutations = 9999)
# readr::write_csv(tidy_permutest(pt_site, "PERMDISP (by site, pooled years)", 9999),
#                  "../data/vegan_results/vegan_step2_beta/table_braycurtis_permdisp_by_site_pooled.csv")

# (ii) Dispersion by year (all sites pooled)
bd_year <- betadisper(D_all, group = design$year)
pt_year <- permutest(bd_year, permutations = 9999)
# readr::write_csv(tidy_permutest(pt_year, "PERMDISP (by year, pooled sites)", 9999),
#                  "../data/vegan_results/vegan_step2_beta/table_braycurtis_permdisp_by_year_pooled.csv")


#### B) WITHIN-YEAR AMONG-SITE PERMANOVA (unpaired) # USES PAIRED ANALYSIS WITH pair_code
do_year_block <- function(yr) {
  sub <- droplevels(design %>% filter(year == yr))
  idx <- which(design$year == yr)
  D   <- bray_sub(idx)
  ad  <- adonis2(D ~ site, data = sub, permutations = 9999)
  pw  <- pairwise_site_tests(sub, D)
  bd  <- betadisper(D, group = sub$site)
  pt  <- permutest(bd, permutations = 9999)
  
  list(sub=sub, ad=ad, pw=pw, bd=bd, pt=pt, yr=yr)
}

res78 <- do_year_block("1978") # PAIRED
res11 <- do_year_block("2011") # PAIRED

# readr::write_csv(tidy_adonis(res78$ad, "1978: site"),
#                  "../data/vegan_results/vegan_step2_beta/table_braycurtis_permanova_1978_site.csv")
# readr::write_csv(tidy_adonis(res11$ad, "2011: site"),
#                  "../data/vegan_results/vegan_step2_beta/table_braycurtis_permanova_2011_site.csv")

# readr::write_csv(res78$pw, "../data/vegan_results/vegan_step2_beta/table_pairwise_permanova_1978_sites.csv")
# readr::write_csv(res11$pw, "../data/vegan_results/vegan_step2_beta/table_pairwise_permanova_2011_sites.csv")

# readr::write_csv(tidy_permutest(res78$pt, "PERMDISP (1978 among sites)", 9999),
#                  "../data/vegan_results/vegan_step2_beta/table_dispersion_permdisp_1978_among_sites.csv")
# readr::write_csv(tidy_permutest(res11$pt, "PERMDISP (2011 among sites)", 9999),
#                  "../data/vegan_results/vegan_step2_beta/table_dispersion_permdisp_2011_among_sites.csv")

# Tukey HSD for dispersion pairwise differences (1978 & 2011)
tukey_tbl <- function(bd_obj, map = site_name_map) {
  TK <- TukeyHSD(bd_obj)$group %>%
    as.data.frame() %>%
    tibble::rownames_to_column("Comparison") %>%
    tidyr::separate(Comparison, into = c("Group1","Group2"), sep = "-") %>%
    dplyr::rename(diff = diff, lwr = lwr, upr = upr, p_adj = `p adj`) %>%
    mutate(
      Group1_pretty = dplyr::recode(Group1, !!!map),
      Group2_pretty = dplyr::recode(Group2, !!!map)
    )
  as_tibble(TK)
}

tukey_res78_bd <- tukey_tbl(res78$bd)
tukey_res11_bd <- tukey_tbl(res11$bd)

# readr::write_csv(tukey_tbl(res78$bd), "../data/vegan_results/vegan_step2_beta/table_dispersion_tukey_1978_among_sites.csv")
# readr::write_csv(tukey_tbl(res11$bd), "../data/vegan_results/vegan_step2_beta/table_dispersion_tukey_2011_among_sites.csv")


# Dispersion pairwise differences (1978 & 2011) Adonis


#### C) WITHIN-SITE TEMPORAL TESTS (unpaired) ####
# For each site, test 1978 vs 2011 (no pairing, no strata)
within_site_unpaired <- purrr::map_dfr(levels(design$site), function(s) {
  sub <- droplevels(design %>% filter(site == s, year %in% c("1978","2011")))
  if (nrow(sub) < 3 || nlevels(sub$year) < 2) return(NULL)
  idx <- which(design$site == s & design$year %in% c("1978","2011"))
  D   <- bray_sub(idx)
  ad  <- adonis2(D ~ year, data = sub, permutations = 9999)
  bd  <- betadisper(D, group = sub$year)
  pt  <- permutest(bd, permutations = 9999)
  tibble(
    site     = s,
    term     = "year",
    df       = ad$Df[1],
    sumsq    = ad$SumOfSqs[1],
    R2       = ad$R2[1],
    F        = ad$F[1],
    p        = ad$`Pr(>F)`[1],
    disp_p   = pt$tab[1, "Pr(>F)"],
    n_1978   = sum(sub$year == "1978"),
    n_2011   = sum(sub$year == "2011")
  )
})

# BH adjust the within-site year tests (4 tests)
# mutate only if the table has rows
if (!is.null(within_site_unpaired) && nrow(within_site_unpaired) > 0) {
  within_site_unpaired <- within_site_unpaired %>%
    dplyr::mutate(
      p_adj_BH   = p.adjust(p, method = "BH"),
      # make sure we're recoding a character (or factor) using dplyr::recode
      site_char  = as.character(site),
      site_pretty = dplyr::recode(site_char, !!!site_name_map, .default = site_char)
    ) %>%
    dplyr::relocate(site_pretty, .after = site) %>%
    dplyr::select(-site_char)
}

# write only if we have something to write
if (!is.null(within_site_unpaired) && nrow(within_site_unpaired) > 0) {
  readr::write_csv(
    within_site_unpaired,
    "../data/vegan_results/vegan_step2_beta/table_braycurtis_permanova_year_within_site.csv"
  )
}

# if (nrow(within_site_unpaired)) {
#   within_site_unpaired <- within_site_unpaired %>%
#     mutate(p_adj_BH = p.adjust(p, method = "BH"),
#            site_pretty = dplyr::recode(site, !!!site_name_map)) %>%
#     relocate(site_pretty, .after = site)
#   readr::write_csv(within_site_unpaired,
#                    "../data/vegan_results/vegan_step2_beta/table_braycurtis_permanova_year_within_site.csv")
# }

#### D) NMDS (no arrows; color = year, shape = site)
ord <- metaMDS(X_bray, distance = "bray", k = 2, trymax = 100, autotransform = FALSE)
scr <- as.data.frame(scores(ord, display = "sites"))
scr$transect_code <- rownames(scr)

plot_df <- scr %>%
  left_join(design, by = "transect_code") %>%
  dplyr::mutate(
    site_pretty = dplyr::recode(as.character(site), !!!site_name_map),
    site_pretty = factor(site_pretty, levels = c("Mactan","Olango","Sumilon West","Sumilon East"))
  )

# Year colors
year_cols <- c("1978" = "#F8766D", "2011" = "#00BFC4")
site_pch  <- c("Mactan"=16, "Olango"=17, "Sumilon East"=15, "Sumilon West"=18)
# site-specific linetypes
site_lty <- c(
  "Mactan"        = "solid",
  "Olango"        = "dashed",
  "Sumilon East"  = "dotdash",
  "Sumilon West"  = "dotted"
)


p_nmds <- ggplot(plot_df, aes(NMDS1, NMDS2, color = year, shape = site_pretty)) +
  geom_point(size = 2.2, alpha = 0.9) +
  scale_color_manual(values = year_cols, name = "Year") +
  scale_shape_manual(values = site_pch, name = "Site") +
  labs(title = paste0("NMDS (Bray–Curtis on √density); stress = ", round(ord$stress, 3)),
       x = "NMDS1", y = "NMDS2") +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank())

print(p_nmds)

# ggsave("../figures/vegan_step2_beta/NMDS_unpaired_by_year_site.png", p_nmds, width = 6.5, height = 8, dpi = 300)
# ggsave("../figures/vegan_step2_beta/NMDS_unpaired_by_year_site.pdf", p_nmds, width = 8, height = 6)


## nMDS plot with 95% confidence ellipses
p_nmds_ell <- ggplot(plot_df, aes(x = NMDS1, y = NMDS2)) +
  # points: color = year, shape = site
  geom_point(aes(color = year, shape = site_pretty),
             size = 2.2, alpha = 0.9) +
  # 95% confidence ellipses per (year × site) group
  stat_ellipse(
    aes(color = year,
        linetype = site_pretty,
        group = interaction(year, site_pretty)),
    type = "norm", level = 0.95, # Guassian 95% ellipses
    linewidth = 0.7, fill = NA
  ) +
  scale_color_manual(values = year_cols, name = "Year") +
  scale_shape_manual(values = site_pch,  name = "Site") +
  scale_linetype_manual(values = site_lty, name = "Site") +
  coord_equal() +
  labs(
    title = paste0("NMDS (Bray–Curtis on √density); stress = ",
                   round(ord$stress, 3)),
    x = "NMDS1", y = "NMDS2"
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank()) +
  # keep Year above Site in the legend stack
  guides(
    color   = guide_legend(order = 1),
    shape   = guide_legend(order = 2),
    linetype= guide_legend(order = 2)
  )

print(p_nmds_ell)

# ggsave("../figures/vegan_step2_beta/NMDS_unpaired_by_year_site.png", p_nmds_ell, width = 6.5, height = 8, dpi = 300)
# ggsave("../figures/vegan_step2_beta/NMDS_unpaired_by_year_site.pdf", p_nmds_ell, width = 8, height = 6)


## ---- Faceted NMDS by site (2×2), color = year, one shape, same ellipse linetype
# set facet order of plots by site
site_order <- c("Mactan", "Olango", "Sumilon West", "Sumilon East")

# (optional) sanity check: warn if any requested names aren't present
missing_sites <- setdiff(site_order, unique(as.character(plot_df$site_pretty)))
if (length(missing_sites)) warning("Missing in data: ", paste(missing_sites, collapse = ", "))

# apply the order
plot_df <- plot_df %>%
  dplyr::mutate(site_pretty = factor(as.character(site_pretty), levels = site_order))

## ---- Faceted NMDS by site (2×2), color = year, one shape, same ellipse linetype
pt_shape  <- 16

p_nmds_faceted <- ggplot(plot_df, aes(x = NMDS1, y = NMDS2)) +
  geom_point(aes(color = year), shape = pt_shape, size = 2.2, alpha = 0.9) +
  stat_ellipse(aes(color = year, group = year),
               type = "norm", level = 0.95,
               linewidth = 0.7, linetype = "solid", fill = NA) +
  scale_color_manual(values = year_cols, name = "Year") +
  coord_equal() +
  facet_wrap(~ site_pretty, ncol = 2) +   # facet order follows factor levels
  labs(
    title = paste0("NMDS (Bray–Curtis on √density) by site; stress = ", round(ord$stress, 3)),
    x = "NMDS1", y = "NMDS2"
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "grey95", color = NA),
        strip.text = element_text(face = "bold"))

print(p_nmds_faceted)
ggsave("../figures/vegan_step2_beta/figure_braycurtis_NMDS_unpaired_faceted_by_site.png", p_nmds_faceted, width = 6.5, height = 8, dpi = 300)
# ggsave("../figures/vegan_step2_beta/figure_braycurtis_NMDS_unpaired_faceted_by_site.pdf",  p_nmds_faceted, width = 9, height = 7)



#### REEF AREA TEST - CONTINUOUS ####
# distances to centroids of site×year groups
bd_sy <- vegan::betadisper(D_all, group = design$site_code)
dist_to_centroid <- bd_sy$distances

# fit a simple model (normal errors are OK for distances; we’ll get a perm p)
lm_area <- lm(dist_to_centroid ~ log_area_m2_hab_reef + site + year, data = design)
summary(lm_area)

# ANOVA (type I here; for a single continuous focal term it’s fine)
aov_tbl <- as.data.frame(anova(lm_area)) %>%
  rownames_to_column("term") %>%
  as_tibble()

# Coefficients table
coef_tbl <- broom::tidy(lm_area) %>%    # term, estimate, std.error, statistic, p.value
  mutate(sig = case_when(
    p.value < 0.001 ~ "***",
    p.value < 0.01  ~ "**",
    p.value < 0.05  ~ "*",
    p.value < 0.1   ~ ".",
    TRUE ~ ""
  ))

# Blocked permutations (site×year) for the log-area term (Freedman–Lane style)
set.seed(123)
ctrl <- permute::how(blocks = design$site_code)
perm_set <- permute::shuffleSet(n = nrow(design), control = ctrl, nset = 9999)

# Observed F for log-area
F_obs <- anova(lm_area)["log_area_m2_hab_reef", "F value"]

# Reduced model once on original order
lm_red <- lm(dist_to_centroid ~ site + year, data = design)
fit_red <- fitted(lm_red)
res_red <- resid(lm_red)

perm_F <- vapply(seq_len(nrow(perm_set)), function(i) {
  # shuffle residuals within blocks by permuting row indices
  idx <- perm_set[i, ]
  y_star <- fit_red + res_red[idx]
  anova(lm(y_star ~ log_area_m2_hab_reef + site + year, data = design))["log_area_m2_hab_reef","F value"]
}, numeric(1))

p_perm <- (sum(perm_F >= F_obs) + 1) / (length(perm_F) + 1)

perm_tbl <- tibble(
  term   = "log_area_m2_hab_reef",
  F_obs  = as.numeric(F_obs),
  p_perm = p_perm,
  n_perm = nrow(perm_set),
  blocks = "site×year (site_code)"
)

# Context: mean distance-to-centroid by site×year
dist_df <- tibble(
  transect_code       = names(dist_to_centroid),
  dist_to_centroid    = unname(dist_to_centroid)
) %>%
  left_join(design %>% 
              dplyr::select(transect_code, site, year, site_code), by = "transect_code")

mean_dist_tbl <- dist_df %>%
  group_by(site, year, site_code) %>%
  summarise(n = n(),
            mean_dist = mean(dist_to_centroid),
            sd_dist   = sd(dist_to_centroid),
            .groups = "drop")

## PLOT 

# issues when renaming buyo to Mactan instead of Buyong
site_lookup <- tibble::tibble(
  site        = c("buyo","olan","east","west"),
  site_pretty = c("Mactan","Olango","Sumilon East","Sumilon West")
)

plot_df <- tibble(
  transect_code    = names(dist_to_centroid),
  dist_to_centroid = unname(dist_to_centroid)
) %>%
  dplyr::left_join(
    design %>% dplyr::select(transect_code, site, year, log_area_m2_hab_reef),
    by = "transect_code"
  ) %>%
  dplyr::left_join(site_lookup, by = "site") %>%
  dplyr::mutate(
    site_pretty = factor(site_pretty, levels = site_order),
    year        = factor(year)
  ) %>%
  dplyr::filter(is.finite(log_area_m2_hab_reef), is.finite(dist_to_centroid))

# plot_df <- tibble(
#   transect_code    = names(dist_to_centroid),
#   dist_to_centroid = unname(dist_to_centroid)
# ) %>%
#   left_join(design %>% 
#               dplyr::select(transect_code, site, year, log_area_m2_hab_reef), by = "transect_code") %>%
#   dplyr::mutate(
#     site_pretty = recode(as.character(site), !!!site_name_map),
#     site_pretty = factor(site_pretty, levels = site_order),
#     year        = factor(year)
#   ) %>%
#   filter(is.finite(log_area_m2_hab_reef), is.finite(dist_to_centroid))

# 5) Plot: points colored by year; single lm line per facet (black) with 95% CI
p_area_disp <- ggplot(plot_df, aes(x = log_area_m2_hab_reef, y = dist_to_centroid)) +
  geom_point(aes(color = year), size = 2, alpha = 0.85) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +   # common slope per facet
  scale_color_manual(values = year_cols, name = "Year") +
  facet_wrap(~ site_pretty, ncol = 2) +
  labs(
    title = "Dispersion vs. reef area by site",
    subtitle = "Points = transects (Bray on √density); line = OLS fit within site",
    x = "log(Reef area in transect, m²)",
    y = "Distance to site×year centroid"
  ) +
  theme_classic(base_size = 12) +
  theme(panel.grid = element_blank())

print(p_area_disp)

# # Optional save
# ggsave("../figures/vegan_step2_beta/dispersion_vs_logarea_by_site.png",
#        p_area_disp, width = 8, height = 6, dpi = 300)

## ---- WRITE CSVs
# outdir <- "../data/vegan_results/vegan_step2_beta/area"
# dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
# 
# readr::write_csv(coef_tbl,      file.path(outdir, "lm_bc_dist_to_centroid_coefficients.csv"))
# readr::write_csv(aov_tbl,       file.path(outdir, "lm_bc_dist_to_centroid_anova.csv"))
# readr::write_csv(perm_tbl,      file.path(outdir, "lm_bc_dist_to_centroid_permtest_log_area.csv"))
# readr::write_csv(mean_dist_tbl, file.path(outdir, "bc_dist_to_centroid_by_site_year.csv"))



#### REEF AREA TEST - BINS ####
## DESIGN ##
# Probably don't use a bin design because variance in log_reef_area is explained by the site_code (year_site)
# Balanced bins that respect ties
# picks breakpoints near 25/50/75% but moves them to include whole ties, so counts are close to equal (not exact)
bin_by_count_ties <- function(x, k = 4, labels = NULL) {
  stopifnot(k >= 2L)
  ok <- is.finite(x)
  xr <- x[ok]
  
  uvals <- sort(unique(xr))
  freq  <- vapply(uvals, function(v) sum(xr == v), integer(1))
  cum   <- cumsum(freq)
  n     <- sum(ok)
  # targets near equal counts
  targets <- floor((1:(k-1)) * n / k)
  
  # for each target, choose the largest unique value index not exceeding it
  cut_idx <- sapply(targets, function(t) {
    i <- max(which(cum <= t))
    if (length(i) == 0) NA_integer_ else i
  })
  cut_idx <- cut_idx[is.finite(cut_idx)]
  cut_idx <- unique(pmin(pmax(cut_idx, 1L), length(uvals) - 1L))
  breaks  <- c(-Inf, uvals[cut_idx], Inf)
  
  out <- cut(x, breaks = breaks, include.lowest = TRUE, right = TRUE)
  if (is.null(labels)) labels <- paste0("Q", seq_along(levels(out)))
  levels(out) <- labels
  out
}

design <- design %>%
  mutate(
    log_area_m2_hab_reef = log(area_m2_hab_reef),
    area_bin4 = bin_by_count_ties(
      log_area_m2_hab_reef, k = 4,
      labels = c("Q1 (smallest)","Q2","Q3","Q4 (largest)")
    )
  )

table(design$area_bin4)   # near 16 each, with ties kept together


## PERMANOVA ##
# Global two-factor permanova w/ log reef area
# Model: distance ~ year * site  + log(area_m2_hab_reef) (Bray on √density)
ad_global_log_reef_area <- adonis2(D_all ~ year * site + log(area_m2_hab_reef), data = design, permutations = 9999, by = "margin")
# readr::write_csv(tidy_adonis(ad_global_log_reef_area, "Bray √density: year * site + log(area_m2_hab_reef"),
#                  "../data/vegan_results/vegan_step2_beta/table_braycurtis_permanova_global_year_by_site_log_area_reef.csv")
print(ad_global_log_reef_area)


## DISPERSION CHECKS ##
# (iii) Dispersion across log reef area bins, permutations blocked within site x year (site_code)
bd_area <- vegan::betadisper(D_all, group = design$area_bin4)
ctrl    <- permute::how(blocks = design$site_code)
pt_area <- vegan::permutest(bd_area, permutations = ctrl)

print(pt_area)       # F and (blocked) permutation p
plot(bd_area)        # optional: distances-to-centroid by area bin
# readr::write_csv(tidy_permutest(pt_site, "PERMDISP (by log reef area)", 9999),
# "../data/vegan_results/vegan_step2_beta/table_braycurtis_permdisp_by_logreefarea_pooled.csv")






#### HELLINGER (EUCLIDEAN) ####

# if (USE_HELL) {
#   D_all_e <- hell_sub(idx_all)
#   ad_global_e  <- adonis2(D_all_e ~ year * site, data = design, permutations = 9999, by = "margin")
#   readr::write_csv(tidy_adonis(ad_g_e, "Hellinger/Euclidean: year * site"),
#                    "../data/vegan_results/vegan_step2_beta/table_hell_permanova_global_year_by_site.csv")
# }

#### A) GLOBAL TWO-FACTOR PERMANOVA (unpaired)
# Model: distance ~ year * site (Bray on √density)
D_all_e   <- hell_sub(idx_all)
ad_global_e <- adonis2(D_all_e ~ year * site, data = design, permutations = 9999, by = "margin")
# readr::write_csv(tidy_adonis(ad_global_e, "Hellinger (Euclidean) √density: year * site"),
# "../data/vegan_results/vegan_step2_beta/table_hell_permanova_global_year_by_site.csv")

# Model: distance ~ year * site  + log(area_m2_hab_reef) (Bray on √density)
ad_global_e_log_reef_area <- adonis2(D_all_e ~ year * site + log(area_m2_hab_reef), data = design, permutations = 9999, by = "margin")
# readr::write_csv(tidy_adonis(ad_global_e_log_reef_area, "Hellinger (Euclidean) √density: year * site + log(area_m2_hab_reef"),
# "../data/vegan_results/vegan_step2_beta/table_hell_permanova_global_year_by_site_log_area_reef.csv")

# Dispersion checks that correspond to the factors:
# (i) Dispersion by site (all years pooled)
bd_site_e <- betadisper(D_all_e, group = design$site)
pt_site_e <- permutest(bd_site_e, permutations = 9999)
# readr::write_csv(tidy_permutest(pt_site_e, "PERMDISP Hellinger (by site, pooled years)", 9999),
# "../data/vegan_results/vegan_step2_beta/table_hell_permdisp_by_site_pooled.csv")

# (ii) Dispersion by year (all sites pooled)
bd_year_e <- betadisper(D_all_e, group = design$year)
pt_year_e <- permutest(bd_year_e, permutations = 9999)
# readr::write_csv(tidy_permutest(pt_year_e, "PERMDISP Hellinger (by year, pooled sites)", 9999),
#                  "../data/vegan_results/vegan_step2_beta/table_hell_permdisp_by_year_pooled.csv")

# (iii) Dispersion by log reef area (all sites pooled)
bd_reef_e <- betadisper(D_all_e, group = design$site)
pt_reef_e <- permutest(bd_site_e, permutations = 9999)
# readr::write_csv(tidy_permutest(pt_site_e, "PERMDISP Hellinger (by site, pooled years)", 9999),
# "../data/vegan_results/vegan_step2_beta/table_hell_permdisp_by_site_pooled.csv")


#### B) WITHIN-YEAR AMONG-SITE PERMANOVA (unpaired)
do_year_block <- function(yr) {
  sub <- droplevels(design %>% filter(year == yr))
  idx <- which(design$year == yr)
  D   <- hell_sub(idx)
  ad  <- adonis2(D ~ site, data = sub, permutations = 9999)
  pw  <- pairwise_site_tests(sub, D)
  bd  <- betadisper(D, group = sub$site)
  pt  <- permutest(bd, permutations = 9999)
  
  list(sub=sub, ad=ad, pw=pw, bd=bd, pt=pt, yr=yr)
}

res78_e <- do_year_block("1978")
res11_e <- do_year_block("2011")

# readr::write_csv(tidy_adonis(res78_e$ad, "1978: site"),
#                  "../data/vegan_results/vegan_step2_beta/table_hell_permanova_1978_site.csv")
# readr::write_csv(tidy_adonis(res11_e$ad, "2011: site"),
#                  "../data/vegan_results/vegan_step2_beta/table_hell_permanova_2011_site.csv")

# readr::write_csv(res78_e$pw, "../data/vegan_results/vegan_step2_beta/table_hell_pairwise_permanova_1978_sites.csv")
# readr::write_csv(res11_e$pw, "../data/vegan_results/vegan_step2_beta/table_hell_pairwise_permanova_2011_sites.csv")

# readr::write_csv(tidy_permutest(res78_e$pt, "PERMDISP (1978 among sites)", 9999),
#                  "../data/vegan_results/vegan_step2_beta/table_hell_dispersion_permdisp_1978_among_sites.csv")
# readr::write_csv(tidy_permutest(res11_e$pt, "PERMDISP (2011 among sites)", 9999),
#                  "../data/vegan_results/vegan_step2_beta/table_hell_dispersion_permdisp_2011_among_sites.csv")

# Tukey HSD for dispersion pairwise differences (1978 & 2011)
tukey_tbl_e <- function(bd_obj, map = site_name_map) {
  TK <- TukeyHSD(bd_obj)$group %>%
    as.data.frame() %>%
    tibble::rownames_to_column("Comparison") %>%
    tidyr::separate(Comparison, into = c("Group1","Group2"), sep = "-") %>%
    dplyr::rename(diff = diff, lwr = lwr, upr = upr, p_adj = `p adj`) %>%
    mutate(
      Group1_pretty = dplyr::recode(Group1, !!!map),
      Group2_pretty = dplyr::recode(Group2, !!!map)
    )
  as_tibble(TK)
}
# readr::write_csv(tukey_tbl_e(res78_e$bd), "../data/vegan_results/vegan_step2_beta/table_hell_dispersion_tukey_1978_among_sites.csv")
# readr::write_csv(tukey_tbl_e(res11_e$bd), "../data/vegan_results/vegan_step2_beta/table_hell_dispersion_tukey_2011_among_sites.csv")

#### C) WITHIN-SITE TEMPORAL TESTS (unpaired)
# For each site, test 1978 vs 2011 (no pairing, no strata)
within_site_unpaired <- purrr::map_dfr(levels(design$site), function(s) {
  sub <- droplevels(design %>% filter(site == s, year %in% c("1978","2011")))
  if (nrow(sub) < 3 || nlevels(sub$year) < 2) return(NULL)
  idx <- which(design$site == s & design$year %in% c("1978","2011"))
  D   <- hell_sub(idx)
  ad  <- adonis2(D ~ year, data = sub, permutations = 9999)
  bd  <- betadisper(D, group = sub$year)
  pt  <- permutest(bd, permutations = 9999)
  tibble(
    site     = s,
    term     = "year",
    df       = ad$Df[1],
    sumsq    = ad$SumOfSqs[1],
    R2       = ad$R2[1],
    F        = ad$F[1],
    p        = ad$`Pr(>F)`[1],
    disp_p   = pt$tab[1, "Pr(>F)"],
    n_1978   = sum(sub$year == "1978"),
    n_2011   = sum(sub$year == "2011")
  )
})
# BH adjust the within-site year tests (4 tests)
if (nrow(within_site_unpaired)) {
  within_site_unpaired <- within_site_unpaired %>%
    dplyr::mutate(p_adj_BH = p.adjust(p, method = "BH"),
                  site_pretty = recode(site, !!!site_name_map)) %>%
    relocate(site_pretty, .after = site)
  readr::write_csv(within_site_unpaired,
                   "../data/vegan_results/vegan_step2_beta/table_hell_permanova_year_within_site.csv")
}

#### D) NMDS (no arrows; color = year, shape = site)
ord_e <- metaMDS(X_hell, distance = "hellinger", k = 2, trymax = 100, autotransform = FALSE)
scr_e <- as.data.frame(scores(ord_e, display = "sites"))
scr_e$transect_code <- rownames(scr_e)

plot_df <- scr_e %>%
  dplyr::left_join(design, by = "transect_code") %>%
  dplyr::mutate(site = as.character(site)) %>%
  dplyr::left_join(site_lookup, by = "site") %>%
  dplyr::mutate(
    site_pretty = factor(site_pretty, levels = c("Mactan","Olango","Sumilon West","Sumilon East"))
  )

# plot_df <- scr_e %>%
#   left_join(design, by = "transect_code") %>%
#   dplyr::mutate(
#     site_pretty = recode(as.character(site), !!!site_name_map),
#     site_pretty = factor(site_pretty, levels = c("Mactan","Olango","Sumilon West","Sumilon East"))
#   )

site_pch  <- c("Mactan"=16, "Olango"=17, "Sumilon East"=15, "Sumilon West"=18)
# site-specific linetypes
site_lty <- c(
  "Mactan"        = "solid",
  "Olango"        = "dashed",
  "Sumilon East"  = "dotdash",
  "Sumilon West"  = "dotted"
)

p_nmds <- ggplot(plot_df, aes(NMDS1, NMDS2, color = year, shape = site_pretty)) +
  geom_point(size = 2.2, alpha = 0.9) +
  scale_color_manual(values = year_cols, name = "Year") +
  scale_shape_manual(values = site_pch, name = "Site") +
  labs(title = paste0("NMDS (Hellinger on √density); stress = ", round(ord$stress, 3)),
       x = "NMDS1", y = "NMDS2") +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank())

print(p_nmds)

# ggsave("../figures/vegan_step2_beta/figure_hell_NMDS_unpaired_by_year_site.png", p_nmds, width = 8, height = 6, dpi = 300)
# ggsave("../figures/vegan_step2_beta/figure_hell_NMDS_unpaired_by_year_site.pdf", p_nmds, width = 8, height = 6)


p_nmds_ell <- ggplot(plot_df, aes(x = NMDS1, y = NMDS2)) +
  # points: color = year, shape = site
  geom_point(aes(color = year, shape = site_pretty),
             size = 2.2, alpha = 0.9) +
  # 95% confidence ellipses per (year × site) group
  stat_ellipse(
    aes(color = year,
        linetype = site_pretty,
        group = interaction(year, site_pretty)),
    type = "norm", level = 0.95, # Guassian 95% ellipses
    linewidth = 0.7, fill = NA
  ) +
  scale_color_manual(values = year_cols, name = "Year") +
  scale_shape_manual(values = site_pch,  name = "Site") +
  scale_linetype_manual(values = site_lty, name = "Site") +
  coord_equal() +
  labs(
    title = paste0("NMDS (Hellinger on √density); stress = ",
                   round(ord$stress, 3)),
    x = "NMDS1", y = "NMDS2"
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank()) +
  # keep Year above Site in the legend stack
  guides(
    color   = guide_legend(order = 1),
    shape   = guide_legend(order = 2),
    linetype= guide_legend(order = 2)
  )

print(p_nmds_ell)

# ggsave("../figures/vegan_step2_beta/figure_hell_NMDS_unpaired_by_year_site.png", p_nmds_ell, width = 8, height = 6, dpi = 300)
# ggsave("../figures/vegan_step2_beta/figure_hell_NMDS_unpaired_by_year_site.pdf", p_nmds_ell, width = 8, height = 6)


## ---- Hellinger faceted NMDS by site (2×2), color = year, one shape, same ellipse linetype
# set facet order of plots by site
site_order <- c("Mactan", "Olango", "Sumilon West", "Sumilon East")

# (optional) sanity check: warn if any requested names aren't present
missing_sites <- setdiff(site_order, unique(as.character(plot_df$site_pretty)))
if (length(missing_sites)) warning("Missing in data: ", paste(missing_sites, collapse = ", "))

# apply the order
plot_df <- plot_df %>%
  dplyr::mutate(site_pretty = factor(as.character(site_pretty), levels = site_order))

## ---- Faceted NMDS by site (2×2), color = year, one shape, same ellipse linetype
pt_shape  <- 16

p_nmds_faceted <- ggplot(plot_df, aes(x = NMDS1, y = NMDS2)) +
  geom_point(aes(color = year), shape = pt_shape, size = 2.2, alpha = 0.9) +
  stat_ellipse(aes(color = year, group = year),
               type = "norm", level = 0.95,
               linewidth = 0.7, linetype = "solid", fill = NA) +
  scale_color_manual(values = year_cols, name = "Year") +
  coord_equal() +
  facet_wrap(~ site_pretty, ncol = 2) +   # facet order follows factor levels
  labs(
    title = paste0("NMDS (Bray–Curtis on √density) by site; stress = ", round(ord$stress, 3)),
    x = "NMDS1", y = "NMDS2"
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "grey95", color = NA),
        strip.text = element_text(face = "bold"))

print(p_nmds_faceted)
# ggsave("../figures/vegan_step2_beta/figure_braycurtis_NMDS_unpaired_faceted_by_site.png", p_nmds_faceted, width = 9, height = 7, dpi = 300)
# ggsave("../figures/vegan_step2_beta/figure_braycurtis_NMDS_unpaired_faceted_by_site.pdf",  p_nmds_faceted, width = 9, height = 7)







