#### README ####

# Created by John Whalen
# Date: 3/10/26
# FVC data from the Philippines from 1978 and duplicated in 2011

# Order of wrangling scripts:
# 1. C:/projects/fvc_1978-2011/scripts/wrangle_fvc_1978-2011_data.R
# 2. C:/projects/fvc_1978-2011/scripts/verifyid_fvc_1978-2011_data.R
# 3. C:/projects/fvc_1978-2011/scripts/veganize_fvc_1978-2011_data.R
# 4. C:/projects/fvc_1978-2011/scripts/standardized_fvc_1978-2011_data.R
############################################################
#### iNEXT COVERAGE-BASED SPECIES ACCUMULATION CURVES   ####
#### Ichthyocide survey: whole dataset + by era         ####
#### Abundance data, gamma diversity, q = 0, 1, 2       ####
############################################################

###############
#### SETWD ####
###############
# set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


##################
#### PACKAGES ####
##################
packages_used <- 
  c("tidyverse",
    "iNEXT",
    "ggplot2",
    "patchwork",
    "dplyr",
    "ggh4x",
    "patchwork"
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


################################
#### HELPER FUNCTIONS       ####
################################

# Build one pooled abundance vector from selected rows.
make_abund_vector <- function(X, rows = rownames(X)) {
  v <- colSums(X[rows, , drop = FALSE], na.rm = TRUE)
  v <- v[v > 0]
  v
}

# Summarize species and individual counts for each iNEXT assemblage.
summarize_abund_list <- function(abund_list) {
  purrr::imap_dfr(abund_list, function(v, nm) {
    tibble::tibble(
      assemblage = nm,
      n_stations = NA_integer_,
      n_species_observed = length(v),
      n_individuals = sum(v),
      n_singletons = sum(v == 1),
      n_doubletons = sum(v == 2)
    )
  })
}

filter_iNEXT_q <- function(out, q_keep = 0) {
  
  out2 <- out
  
  # Filter sample-size-based estimates
  if (!is.null(out2$iNextEst$size_based)) {
    out2$iNextEst$size_based <- out2$iNextEst$size_based %>%
      dplyr::filter(.data[["Order.q"]] %in% q_keep)
  }
  
  # Filter coverage-based estimates
  if (!is.null(out2$iNextEst$coverage_based)) {
    out2$iNextEst$coverage_based <- out2$iNextEst$coverage_based %>%
      dplyr::filter(.data[["Order.q"]] %in% q_keep)
  }
  
  # Filter asymptotic estimates only if possible.
  # AsyEst often does NOT have Order.q.
  if (!is.null(out2$AsyEst)) {
    
    if ("Order.q" %in% names(out2$AsyEst)) {
      
      out2$AsyEst <- out2$AsyEst %>%
        dplyr::filter(.data[["Order.q"]] %in% q_keep)
      
    } else if ("Diversity" %in% names(out2$AsyEst)) {
      
      q_to_diversity <- c(
        "0" = "Species richness",
        "1" = "Shannon diversity",
        "2" = "Simpson diversity"
      )
      
      out2$AsyEst <- out2$AsyEst %>%
        dplyr::filter(.data[["Diversity"]] %in% q_to_diversity[as.character(q_keep)])
    }
  }
  
  out2
}

#####################################################
#### 1. WHOLE DATASET: POOLED ABUNDANCE GAMMA    ####
#####################################################

# This treats the full ichthyocide survey as one pooled assemblage.
abund_all <- make_abund_vector(X_counts)

inext_abu_all <- list(
  "all" = abund_all
)

# Basic observed data summary: species and individuals.
summ_abu_all <- summarize_abund_list(inext_abu_all) %>%
  dplyr::mutate(
    n_stations = nrow(X_counts)
  )

print(summ_abu_all)

# iNEXT data information: sample size, observed richness, coverage, frequency counts.
info_abu_all <- iNEXT::DataInfo(
  inext_abu_all,
  datatype = "abundance"
)

print(info_abu_all)

# Use observed coverage capped at 0.95 for the single pooled dataset.
targetC_abu_all <- min(0.95, info_abu_all$SC, na.rm = TRUE)

targetC_abu_all

# Estimate q0, q1, q2 at this target coverage.
est_abu_all <- iNEXT::estimateD(
  inext_abu_all,
  q = c(0, 1, 2),
  datatype = "abundance",
  base = "coverage",
  level = targetC_abu_all,
  conf = 0.95,
  nboot = 1000
)

print(est_abu_all)

# Save tables.
# readr::write_csv(
#   summ_abu_all,
#   file.path(out_dir_tab, "table_inext_abundance_summary_whole_dataset.csv")
# )
# 
# readr::write_csv(
#   info_abu_all,
#   file.path(out_dir_tab, "table_inext_abundance_DataInfo_whole_dataset.csv")
# )
# 
# readr::write_csv(
#   est_abu_all,
#   file.path(out_dir_tab, "table_inext_abundance_estimateD_q012_coverage_whole_dataset_level095_conf095_nboot1000.csv")
# )


# Build coverage-based curves.
out_abu_all <- iNEXT::iNEXT(
  inext_abu_all,
  q = c(0, 1, 2),
  datatype = "abundance",
  se = TRUE,
  conf = 0.95,
  knots = 1000,
  nboot = 1000
)

############################################################
#### 1. WHOLE DATASET: POOLED ABUNDANCE GAMMA DIVERSITY ####
#### 1.1 Species richness curve, q = 0                  ####
#### 1.2 Faceted Hill diversity curves, q = 0, 1, 2     ####
############################################################

###############################
#### PLOT SETTINGS         ####
###############################

# Check that out_abu_all contains all three q values
table(out_abu_all$iNextEst$coverage_based$Order.q)

# Assemblage name from the iNEXT object, probably "all"
all_assemblage <- unique(out_abu_all$iNextEst$coverage_based$Assemblage)

# Whole-dataset color
all_cols <- setNames("#333333", all_assemblage)

# Whole-dataset label
all_name_map <- setNames("All stations", all_assemblage)


###############################################
#### FILTER WHOLE DATASET iNEXT OBJECT: q0 ####
###############################################

out_abu_all_q0 <- out_abu_all

out_abu_all_q0$iNextEst$size_based <- out_abu_all_q0$iNextEst$size_based %>%
  dplyr::filter(.data[["Order.q"]] == 0)

out_abu_all_q0$iNextEst$coverage_based <- out_abu_all_q0$iNextEst$coverage_based %>%
  dplyr::filter(.data[["Order.q"]] == 0)


###############################################
#### PLOT: WHOLE DATASET SPECIES RICHNESS  ####
###############################################

# Check y limits from qD.UCL
# q0 =     nospp = 1001.647
nth(out_abu_all[["iNextEst"]][["coverage_based"]][["qD.UCL"]], 1000) 
# q1 =     nospp = 201.081
nth(out_abu_all[["iNextEst"]][["coverage_based"]][["qD.UCL"]], 2000)
# q2 =     nospp = 69.788
nth(out_abu_all[["iNextEst"]][["coverage_based"]][["qD.UCL"]], 3000)

g_abu_all_q0 <- iNEXT::ggiNEXT(
  out_abu_all_q0,
  type = 3,
  se = TRUE,
  color.var = "Assemblage"
) +
  scale_colour_manual(
    values = all_cols,
    labels = all_name_map,
    name = NULL,
    guide = "none"
  ) +
  scale_fill_manual(
    values = all_cols,
    labels = all_name_map,
    name = NULL,
    guide = "none"
  ) +
  coord_cartesian(xlim = c(0, 1.01)) +
  scale_x_continuous(
    # labels = scales::percent_format(accuracy = 1)
  ) +
  scale_y_continuous(limits = c(0, 1020), breaks = seq(0, 1000, by = 250)) +
  labs(
    x = "Sample Coverage",
    y = "Species Diversity"
  ) +
  guides(
    colour = "none",
    fill   = "none",
    shape  = "none",
    linetype = guide_legend(title = NULL)
  ) +
  theme_classic(base_size = 12) +
  theme(
    text = element_text(family = "Times New Roman"),
    legend.position = "bottom",
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12)
  )

print(g_abu_all_q0)

# Optional: thicken curve and reference point
g_abu_all_q0$layers <- lapply(g_abu_all_q0$layers, function(lyr) {
  if (inherits(lyr$geom, "GeomLine")) {
    lyr$aes_params$linewidth <- 2.2
    lyr$aes_params$alpha <- 0.9
  }
  if (inherits(lyr$geom, "GeomPoint")) {
    lyr$aes_params$size <- 4
    lyr$aes_params$shape <- 16
  }
  lyr
})

print(g_abu_all_q0)

# ggsave(
#   file.path(out_dir_fig, "figure_inext_abundance_all_q0_coverage_level095_conf095_nboot1000_knots1000_leg_bot_man.png"),
#   g_abu_all_q0,
#   width = 6.5,
#   height = 7.5,
#   dpi = 300
# )


####################################################################
#### PLOT: WHOLE DATASET q0, q1, q2 FACETED                     ####
#### Manuscript Figure S1. Coverage-based SAC Pooled Assemblage ####
#### Manual y-axis scale for each Hill number                   ####
####################################################################
panel_labs <- tibble::tibble(
  Order.q = c(0, 1, 2),
  panel_label = c("A", "B", "C")
)

g_abu_all_q012 <- iNEXT::ggiNEXT(
  out_abu_all,
  type = 3,
  se = TRUE,
  facet.var = "Order.q",
  color.var = "Assemblage"
) +
  facet_wrap(
    ~ Order.q,
    ncol = 1,
    scales = "free_y",
    labeller = as_labeller(c(
      "0" = "q = 0: Species Richness",
      "1" = "q = 1: Common Species",
      "2" = "q = 2: Dominant Species"
    ))
  ) +
  ggh4x::facetted_pos_scales(
    y = list(
      Order.q == 0 ~ scale_y_continuous(
        limits = c(0, 1050),
        breaks = seq(0, 1000, 250)
      ),
      Order.q == 1 ~ scale_y_continuous(
        limits = c(0, 210),
        breaks = seq(0, 200, 50)
      ),
      Order.q == 2 ~ scale_y_continuous(
        limits = c(0, 80),
        breaks = seq(0, 80, 20)
      )
    )
  ) +
  geom_text(
    data = panel_labs,
    aes(x = -Inf, y = Inf, label = panel_label),
    inherit.aes = FALSE,
    hjust = -0.6,
    vjust = 1.4,
    family = "Times New Roman",
    fontface = "bold",
    size = 4.5
  ) +
  scale_colour_manual(
    values = all_cols,
    labels = all_name_map,
    name = NULL,
    guide = "none"
  ) +
  scale_fill_manual(
    values = all_cols,
    labels = all_name_map,
    name = NULL,
    guide = "none"
  ) +
  guides(
    colour = "none",
    color = "none",
    fill = "none",
    shape = "none",
    linetype = guide_legend(title = NULL)
  ) +
  coord_cartesian(xlim = c(0, 1.01)) +
  labs(
    x = "Sample Coverage",
    y = "Species Diversity"
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

print(g_abu_all_q012)

# Thicken curves and reference points
g_abu_all_q012$layers <- lapply(g_abu_all_q012$layers, function(lyr) {
  if (inherits(lyr$geom, "GeomLine")) {
    lyr$aes_params$linewidth <- 2.2
    lyr$aes_params$alpha <- 0.9
  }
  if (inherits(lyr$geom, "GeomPoint")) {
    lyr$aes_params$size <- 4
    lyr$aes_params$shape <- 16
  }
  lyr
})

print(g_abu_all_q012)

# ggsave(
#   file.path(out_dir_fig, "figure_inext_abundance_all_q012_coverage_level095_conf095_nboot1000_knots1000_facet_leg_bot_man.png"),
#   g_abu_all_q012,
#   width = 6.5,
#   height = 7.5,
#   dpi = 300
# )


#################################################
#### PLOT: WHOLE DATASET q0, q1, q2 TOGETHER ####
#################################################
# Prepare plotting dataframe directly from iNEXT coverage-based output
plot_abu_all_q012 <- out_abu_all$iNextEst$coverage_based %>%
  dplyr::mutate(
    Order.q = as.numeric(Order.q),
    q_label = dplyr::case_when(
      Order.q == 0 ~ "q = 0: Species Richness",
      Order.q == 1 ~ "q = 1: Common Species",
      Order.q == 2 ~ "q = 2: Dominant Species",
      TRUE ~ as.character(Order.q)
    ),
    q_label = factor(
      q_label,
      levels = c(
        "q = 0: Species Richness",
        "q = 1: Common Species",
        "q = 2: Dominant Species"
      )
    )
  )

# Reference / observed points
plot_abu_all_ref <- plot_abu_all_q012 %>%
  dplyr::filter(Method == "Observed")

# Shades for Hill numbers
hill_cols <- c(
  "q = 0: Species Richness" = "black",
  "q = 1: Common Species"   = "grey40",
  "q = 2: Dominant Species" = "grey70"
)

# Shapes for Hill numbers
hill_shapes <- c(
  "q = 0: Species Richness" = 16,  # circle
  "q = 1: Common Species"   = 17,  # triangle
  "q = 2: Dominant Species" = 15   # square
)

g_abu_all_q012_together <- ggplot(
  plot_abu_all_q012,
  aes(x = SC, y = qD, color = q_label)
) +
  geom_ribbon(
    aes(
      ymin = qD.LCL,
      ymax = qD.UCL,
      fill = q_label,
      group = q_label
    ),
    alpha = 0.15,
    color = NA
  ) +
  geom_line(
    aes(group = q_label),
    linewidth = 2.2,
    alpha = 0.9
  ) +
  geom_point(
    data = plot_abu_all_ref,
    aes(shape = q_label),
    size = 4
  ) +
  scale_color_manual(
    values = hill_cols,
    name = "Hill Diversity"
  ) +
  scale_fill_manual(
    values = hill_cols,
    name = "Hill Diversity"
  ) +
  scale_shape_manual(
    values = hill_shapes,
    name = "Hill Diversity"
  ) +
  coord_cartesian(
    xlim = c(0, 1.01),
    ylim = c(0, 1050)
  ) +
  scale_y_continuous(
    breaks = seq(0, 1000, 250)
    # trans = 'log10'
  ) +
  labs(
    x = "Sample Coverage",
    y = "Species Diversity"
  ) +
  theme_classic(base_size = 12) +
  theme(
    text = element_text(family = "Times New Roman"),
    legend.position = "inside",
    legend.position.inside = c(0.20, 0.85),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12)
  )

print(g_abu_all_q012_together)

# Thicken curves and reference points
g_abu_all_q012_together$layers <- lapply(g_abu_all_q012_together$layers, function(lyr) {
  if (inherits(lyr$geom, "GeomLine")) {
    lyr$aes_params$linewidth <- 2.2
    lyr$aes_params$alpha <- 0.9
  }
  if (inherits(lyr$geom, "GeomPoint")) {
    lyr$aes_params$size <- 4
    lyr$aes_params$shape <- 16
  }
  lyr
})

print(g_abu_all_q012_together)

# ggsave(
#   file.path(out_dir_fig, "figure_inext_abundance_all_q012_coverage_level095_conf095_nboot1000_knots1000_man.png"),
#   g_abu_all_q012_together,
#   width = 6.5,
#   height = 7.5,
#   dpi = 300
# )


#################################################
#### 2. BY ERA: HISTORICAL VS CONTEMPORARY   ####
#################################################

# Build pooled abundance vectors by era.
inext_abu_era <- lapply(levels(design$era), function(e) {
  rows_e <- design$station_code[design$era == e]
  make_abund_vector(X_counts, rows = rows_e)
})

names(inext_abu_era) <- levels(design$era)

# Basic observed data summary: species and individuals by era.
summ_abu_era <- summarize_abund_list(inext_abu_era) %>%
  dplyr::mutate(
    n_stations = as.integer(table(design$era)[assemblage])
  ) %>%
  dplyr::select(
    assemblage,
    n_stations,
    n_species_observed,
    n_individuals,
    n_singletons,
    n_doubletons
  )

print(summ_abu_era)

# iNEXT data information by era.
info_abu_era <- iNEXT::DataInfo(
  inext_abu_era,
  datatype = "abundance"
)

print(info_abu_era)

# Common target coverage for fair temporal comparison.
# This uses the lower observed sample coverage between eras, capped at 0.95.
targetC_abu_era <- min(0.95, min(info_abu_era$SC, na.rm = TRUE))

targetC_abu_era

# Coverage-standardized Hill numbers by era.
est_abu_era <- iNEXT::estimateD(
  inext_abu_era,
  q = c(0, 1, 2),
  datatype = "abundance",
  base = "coverage",
  level = targetC_abu_era,
  conf = 0.95,
  nboot = 1000
)

print(est_abu_era)

# Save tables.
# readr::write_csv(
#   summ_abu_era,
#   file.path(out_dir_tab, "table_inext_abundance_summary_by_era.csv")
# )
# readr::write_csv(
#   info_abu_era,
#   file.path(out_dir_tab, "table_inext_abundance_DataInfo_by_era.csv")
# )
# readr::write_csv(
#   est_abu_era,
#   file.path(out_dir_tab, "table_inext_abundance_estimateD_q012_coverage_by_era_level095_conf095_nboot1000.csv")
# )


#######################################
#### 3. COVERAGE-BASED ERA CURVES  ####
#######################################

out_abu_era <- iNEXT::iNEXT(
  inext_abu_era,
  q = c(0, 1, 2),
  datatype = "abundance",
  se = TRUE,
  conf = 0.95,
  knots = 1000,
  nboot = 1000
)

# q = 0 only: species richness / species accumulation curve.
out_abu_era_q0 <- filter_iNEXT_q(out_abu_era, q_keep = 0)

# reorder
out_abu_era_q0_plot <- out_abu_era_q0

era_plot_order <- c("historical", "contemporary")

out_abu_era_q0_plot$iNextEst$size_based <- out_abu_era_q0_plot$iNextEst$size_based %>%
  dplyr::mutate(
    Assemblage = factor(Assemblage, levels = era_plot_order)
  ) %>%
  dplyr::arrange(Assemblage)

out_abu_era_q0_plot$iNextEst$coverage_based <- out_abu_era_q0_plot$iNextEst$coverage_based %>%
  dplyr::mutate(
    Assemblage = factor(Assemblage, levels = era_plot_order)
  ) %>%
  dplyr::arrange(Assemblage)


g_abu_era_q0 <- iNEXT::ggiNEXT(
  out_abu_era_q0,
  type = 3,
  se = TRUE,
  color.var = "Assemblage"
) +
  scale_colour_manual(
    values = era_cols,
    labels = era_name_map,
    name = "Era"
  ) +
  scale_fill_manual(
    values = era_cols,
    labels = era_name_map,
    name = "Era"
  ) +
  scale_shape_manual(
    values = c(
      "historical" = 16,     # filled circle
      "contemporary" = 17    # filled triangle
    ),
    labels = era_name_map,
    name = "Era"
  ) +
  # coord_cartesian(xlim = c(0, 1.01)) +
  # scale_x_continuous(
  #   labels = scales::percent_format(accuracy = 1)
  # ) +
  labs(
    x = "Sample Coverage",
    y = "Species Diversity"
  ) +
  theme_classic(base_size = 12) +
  theme(
    text = element_text(family = "Times New Roman"),
    legend.position = "none",
    #legend.title = element_text(size = 12),
    #legend.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12)
  )

print(g_abu_era_q0)

line_size  <- 3
line_alpha <- 0.6

g_abu_era_q0$layers <- lapply(g_abu_era_q0$layers, function(lyr) {
  if (inherits(lyr$geom, "GeomLine")) {
    lyr$aes_params$linewidth <- line_size   # use linewidth (ggplot2 >= 3.4)
    lyr$aes_params$alpha     <- line_alpha
  }
  lyr
})

print(g_abu_era_q0)

# ggsave(
#   file.path(out_dir_fig, "figure_inext_abundance_q0_coverage_by_era_level095_conf095_nboot1000_knots1000_man.png"),
#   g_abu_era_q0,
#   width = 6.5,
#   height = 7.5,
#   dpi = 300
# )


#####################################
#### q = 0, 1, 2 faceted figure. ####
#####################################
g_abu_era_q012 <- iNEXT::ggiNEXT(
  out_abu_era,
  type = 3,
  se = TRUE,
  facet.var = "Order.q",
  color.var = "Assemblage"
) +
  facet_wrap(
    ~ Order.q,
    ncol = 1,
    scales = "free_y",
    labeller = as_labeller(c(
      "0" = "q = 0: Species Richness",
      "1" = "q = 1: Common Species",
      "2" = "q = 2: Dominant Species"
    ))
  ) +
  scale_colour_manual(
    values = era_cols,
    labels = era_name_map,
    name = "Era"
  ) +
  scale_fill_manual(
    values = era_cols,
    labels = era_name_map,
    name = "Era"
  ) +
  scale_shape_manual(
    values = c(
      "historical" = 16,     # filled circle
      "contemporary" = 17    # filled triangle
    ),
    labels = era_name_map,
    name = "Era"
  ) +
  # coord_cartesian(xlim = c(0, 1.01)) +
  # scale_x_continuous(
  #   labels = scales::percent_format(accuracy = 1)
  # ) +
  labs(
    x = "Sample Coverage",
    y = "Species Diversity"
  ) +
  theme_classic(base_size = 12) +
  theme(
    text = element_text(family = "Times New Roman"),
    legend.position = "none",
    # legend.title = element_text(size = 12),
    # legend.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12),
    strip.background = element_blank(),
    strip.text = element_text(size = 12)
  )

print(g_abu_era_q012)

line_size  <- 3
line_alpha <- 0.6

g_abu_era_q012$layers <- lapply(g_abu_era_q012$layers, function(lyr) {
  if (inherits(lyr$geom, "GeomLine")) {
    lyr$aes_params$linewidth <- line_size   # use linewidth (ggplot2 >= 3.4)
    lyr$aes_params$alpha     <- line_alpha
  }
  lyr
})

print(g_abu_era_q012)

# ggsave(
#   file.path(out_dir_fig, "figure_inext_abundance_q012_coverage_by_era_level095_conf095_nboot1000_knots1000_facet_man.png"),
#   g_abu_era_q012,
#   width = 6.5,
#   height = 7.5,
#   dpi = 300
# )



##############################################################
#### 4. BOOTSTRAP DELTA: CONTEMPORARY - HISTORICAL        ####
####    at common target coverage                         ####
##############################################################

set.seed(123)

v_hist <- inext_abu_era[["historical"]]
v_cont <- inext_abu_era[["contemporary"]]

targetC <- targetC_abu_era
B <- 1000
q_vec <- c(0, 1, 2)

# Bootstrap abundance vector by resampling individuals.
bootstrap_abund <- function(v) {
  sp <- rep(names(v), times = as.integer(v))
  sp_boot <- sample(sp, size = length(sp), replace = TRUE)
  tab <- table(sp_boot)
  as.numeric(tab) |> setNames(names(tab))
}

# Estimate Hill diversity at fixed target coverage.
qD_at_targetC <- function(v_named, targetC, q_vec) {
  est <- tryCatch(
    {
      iNEXT::estimateD(
        list(x = v_named),
        q = q_vec,
        datatype = "abundance",
        base = "coverage",
        level = targetC,
        conf = 0.95,
        nboot = 1000
      )
    },
    error = function(e) NULL
  )
  
  out <- rep(NA_real_, length(q_vec))
  names(out) <- as.character(q_vec)
  
  if (!is.null(est)) {
    out[as.character(est$Order.q)] <- est$qD
  }
  
  out
}

# Run bootstrap.
boot_delta <- matrix(
  NA_real_,
  nrow = B,
  ncol = length(q_vec)
)

colnames(boot_delta) <- paste0("q", q_vec)

for (b in seq_len(B)) {
  v_hist_b <- bootstrap_abund(v_hist)
  v_cont_b <- bootstrap_abund(v_cont)
  
  q_hist <- qD_at_targetC(v_hist_b, targetC, q_vec)
  q_cont <- qD_at_targetC(v_cont_b, targetC, q_vec)
  
  # Delta = contemporary - historical
  boot_delta[b, ] <- q_cont[as.character(q_vec)] - q_hist[as.character(q_vec)]
}
exit
boot_delta <- as.data.frame(boot_delta)

summ_delta_era <- purrr::map_dfr(names(boot_delta), function(qname) {
  d <- boot_delta[[qname]]
  d <- d[is.finite(d)]
  
  p_two <- 2 * min(mean(d <= 0), mean(d >= 0))
  p_two <- min(1, p_two)
  
  tibble::tibble(
    Order.q = qname,
    target_coverage = targetC,
    delta_mean = mean(d),
    delta_median = stats::median(d),
    CI_low = stats::quantile(d, 0.025),
    CI_high = stats::quantile(d, 0.975),
    p_boot_two_sided = p_two
  )
})

print(summ_delta_era)

# Save Tables
# readr::write_csv(
#   summ_delta_era,
#   file.path(out_dir_tab, "table_inext_abundance_bootstrap_delta_contemporary_minus_historical_q012_at_target_coverage.csv")
# )
# 
# readr::write_csv(
#   boot_delta,
#   file.path(out_dir_tab, "table_inext_abundance_bootstrap_delta_raw_contemporary_minus_historical_q012.csv")
# )



############################################################
#### 3. BY ERA X SEA iNEXT:                             ####
#### Coverage-based SACs + bootstrap Δ Hill diversity   ####
############################################################

#### COLORS AND LABELS 

era_cols <- c(
  "historical"    = "#F8766D",
  "contemporary" = "#00BFC4"
)

era_name_map <- c(
  "historical"    = "Historical",
  "contemporary" = "Contemporary"
)

sea_name_map <- c(
  "bohol" = "Bohol Sea",
  "sulu"  = "Sulu Sea"
)

q_label_map <- c(
  "0" = "q = 0: Species Richness",
  "1" = "q = 1: Common Species",
  "2" = "q = 2: Dominant Species"
)


##################################
#### ALIGN COMMUNITY + DESIGN ####
##################################

# If station_code is still a column in data_vegan, move it to rownames.
if ("station_code" %in% names(data_vegan)) {
  X_counts <- data_vegan %>%
    tibble::column_to_rownames("station_code") %>%
    as.data.frame()
} else {
  X_counts <- as.data.frame(data_vegan)
}

design <- data_vegan.env %>%
  dplyr::mutate(
    era = tolower(as.character(era)),
    sea = tolower(as.character(sea)),
    era = factor(era, levels = c("historical", "contemporary")),
    sea = factor(sea, levels = c("bohol", "sulu"))
  )

# If rownames are missing or do not match station codes, assume row order matches metadata.
if (!all(design$station_code %in% rownames(X_counts))) {
  if (nrow(X_counts) == nrow(design)) {
    rownames(X_counts) <- design$station_code
  } else {
    stop("Rows in data_vegan cannot be matched to data_vegan.env$station_code.")
  }
}

# Reorder community matrix to match metadata.
X_counts <- X_counts[design$station_code, , drop = FALSE]

stopifnot(identical(rownames(X_counts), design$station_code))

# Keep only species columns with at least one individual.
X_counts <- X_counts[, colSums(X_counts, na.rm = TRUE) > 0, drop = FALSE]

# Create era × sea grouping variable.
design <- design %>%
  dplyr::mutate(
    era_sea = paste(era, sea, sep = "_"),
    era_sea = factor(
      era_sea,
      levels = c(
        "historical_bohol",
        "contemporary_bohol",
        "historical_sulu",
        "contemporary_sulu"
      )
    )
  )


###########################################
#### BUILD ABUNDANCE LIST BY ERA × SEA ####
###########################################

make_abund_vector <- function(X, rows) {
  v <- colSums(X[rows, , drop = FALSE], na.rm = TRUE)
  v <- v[v > 0]
  v
}

make_abund_by_group <- function(X, metadata, group_col) {
  rows_by_group <- split(metadata$station_code, metadata[[group_col]], drop = TRUE)
  
  out <- lapply(rows_by_group, function(rows) {
    make_abund_vector(X, rows)
  })
  
  out
}

inext_abu_era_sea <- make_abund_by_group(
  X = X_counts,
  metadata = design,
  group_col = "era_sea"
)

# Check assemblages.
names(inext_abu_era_sea)

# Quick observed summary.
summ_abu_era_sea <- purrr::imap_dfr(inext_abu_era_sea, function(v, nm) {
  tibble::tibble(
    assemblage = nm,
    n_stations = sum(design$era_sea == nm),
    n_species_observed = length(v),
    n_individuals = sum(v),
    n_singletons = sum(v == 1),
    n_doubletons = sum(v == 2)
  )
}) %>%
  tidyr::separate(
    assemblage,
    into = c("era", "sea"),
    sep = "_",
    remove = FALSE
  )

print(summ_abu_era_sea)

#### DATAINFO + TARGET COVERAGE

info_abu_era_sea <- iNEXT::DataInfo(
  inext_abu_era_sea,
  datatype = "abundance"
) %>%
  tidyr::separate(
    Assemblage,
    into = c("era", "sea"),
    sep = "_",
    remove = FALSE
  )

print(info_abu_era_sea)

# Common target coverage within each sea.
targetC_by_sea <- info_abu_era_sea %>%
  dplyr::group_by(sea) %>%
  dplyr::summarise(
    min_observed_SC = min(SC, na.rm = TRUE),
    target_coverage = min(0.95, min_observed_SC),
    .groups = "drop"
  )

print(targetC_by_sea)

# Optional global target if you want all four assemblages compared at one shared coverage.
targetC_global_era_sea <- min(0.95, min(info_abu_era_sea$SC, na.rm = TRUE))

targetC_global_era_sea

#### ESTIMATE q0, q1, q2 AT COMMON COVERAGE BY SEA
q_vec <- c(0, 1, 2)

est_abu_era_sea <- purrr::map_dfr(c("bohol", "sulu"), function(sea_i) {
  
  target_i <- targetC_by_sea %>%
    dplyr::filter(sea == sea_i) %>%
    dplyr::pull(target_coverage)
  
  assemblages_i <- paste(c("historical", "contemporary"), sea_i, sep = "_")
  
  iNEXT::estimateD(
    inext_abu_era_sea[assemblages_i],
    q = q_vec,
    datatype = "abundance",
    base = "coverage",
    level = target_i,
    conf = 0.95,
    nboot = 1000
  ) %>%
    dplyr::mutate(
      sea = sea_i,
      target_coverage = target_i
    ) %>%
    tidyr::separate(
      Assemblage,
      into = c("era", "sea_from_assemblage"),
      sep = "_",
      remove = FALSE
    )
})

print(est_abu_era_sea)

# Save Tables
# readr::write_csv(
#   summ_abu_era_sea,
#   file.path(out_dir_tab, "table_inext_abundance_summary_by_era_sea.csv")
# )
# 
# readr::write_csv(
#   info_abu_era_sea,
#   file.path(out_dir_tab, "table_inext_abundance_DataInfo_by_era_sea.csv")
# )
# 
# readr::write_csv(
#   est_abu_era_sea,
#   file.path(out_dir_tab, "table_inext_abundance_estimateD_q012_coverage_by_era_sea.csv")
# )


########################################
#### iNEXT CURVES BY ERA × SEA      ####
########################################

set.seed(123)

out_abu_era_sea <- iNEXT::iNEXT(
  inext_abu_era_sea,
  q = c(0, 1, 2),
  datatype = "abundance",
  se = TRUE,
  conf = 0.95,
  knots = 1000,
  nboot = 1000
)


#################################
#### PREPARE COVERAGE PLOT DF ####
#################################

plot_abu_era_sea <- out_abu_era_sea$iNextEst$coverage_based %>%
  tidyr::separate(
    Assemblage,
    into = c("era", "sea"),
    sep = "_",
    remove = FALSE
  ) %>%
  dplyr::mutate(
    era = factor(era, levels = c("historical", "contemporary")),
    sea = factor(sea, levels = c("bohol", "sulu")),
    sea_label = dplyr::recode(as.character(sea), !!!sea_name_map),
    sea_label = factor(sea_label, levels = sea_name_map),
    Order.q = as.numeric(Order.q),
    q_label = dplyr::recode(as.character(Order.q), !!!q_label_map),
    q_label = factor(q_label, levels = q_label_map)
  )

# Reference sample points.
ref_abu_era_sea <- plot_abu_era_sea %>%
  dplyr::filter(Method == "Observed")


#####################################################
#### q0 COVERAGE-BASED SPECIES ACCUMULATION CURVES ####
#####################################################

plot_q0 <- plot_abu_era_sea %>%
  dplyr::filter(Order.q == 0)

ref_q0 <- ref_abu_era_sea %>%
  dplyr::filter(Order.q == 0)

# Split layers so contemporary is plotted on top of historical.
plot_q0_hist <- plot_q0 %>% dplyr::filter(era == "historical")
plot_q0_cont <- plot_q0 %>% dplyr::filter(era == "contemporary")

ref_q0_hist <- ref_q0 %>% dplyr::filter(era == "historical")
ref_q0_cont <- ref_q0 %>% dplyr::filter(era == "contemporary")

g_abu_era_sea_q0 <- ggplot() +
  geom_ribbon(
    data = plot_q0_hist,
    aes(x = SC, ymin = qD.LCL, ymax = qD.UCL, fill = era, group = Assemblage),
    alpha = 0.18,
    color = NA
  ) +
  geom_line(
    data = plot_q0_hist,
    aes(x = SC, y = qD, color = era, group = Assemblage),
    linewidth = 2.2,
    alpha = 0.65
  ) +
  geom_point(
    data = ref_q0_hist,
    aes(x = SC, y = qD, color = era, shape = era),
    size = 4
  ) +
  geom_ribbon(
    data = plot_q0_cont,
    aes(x = SC, ymin = qD.LCL, ymax = qD.UCL, fill = era, group = Assemblage),
    alpha = 0.18,
    color = NA
  ) +
  geom_line(
    data = plot_q0_cont,
    aes(x = SC, y = qD, color = era, group = Assemblage),
    linewidth = 2.2,
    alpha = 0.65
  ) +
  geom_point(
    data = ref_q0_cont,
    aes(x = SC, y = qD, color = era, shape = era),
    size = 4
  ) +
  facet_wrap(
    ~ sea_label,
    nrow = 1
  ) +
  scale_colour_manual(
    values = era_cols,
    labels = era_name_map,
    name = "Era"
  ) +
  scale_fill_manual(
    values = era_cols,
    labels = era_name_map,
    name = "Era"
  ) +
  scale_shape_manual(
    values = c(
      "historical" = 16,
      "contemporary" = 17
    ),
    labels = era_name_map,
    name = "Era"
  ) +
  # coord_cartesian(xlim = c(0, 1.01)) +
  # scale_x_continuous(
  #   labels = scales::percent_format(accuracy = 1)
  # ) +
  labs(
    x = "Sample Coverage",
    y = "Species Diversity"
  ) +
  theme_classic(base_size = 18) +
  theme(
    text = element_text(family = "Times New Roman"),
    legend.position = "none",
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16),
    strip.background = element_blank(),
    strip.text = element_text(size = 16)
  )

print(g_abu_era_sea_q0)

# ---- tweak the line layer(s) that are already there ----
line_size  <- 3
line_alpha <- 0.6

g_abu_era_sea_q0$layers <- lapply(g_abu_era_sea_q0$layers, function(lyr) {
  if (inherits(lyr$geom, "GeomLine")) {
    lyr$aes_params$linewidth <- line_size   # use linewidth (ggplot2 >= 3.4)
    lyr$aes_params$alpha     <- line_alpha
  }
  lyr
})

print(g_abu_era_sea_q0)

# ggsave(
#   file.path(out_dir_fig, "figure_inext_abundance_q0_coverage_by_era_sea_man.png"),
#   g_abu_era_sea_q0,
#   width = 6.5,
#   height = 7.5,
#   dpi = 300
# )


#########################################################
#### q0, q1, q2 COVERAGE-BASED CURVES BY ERA × SEA   ####
#########################################################

plot_hist <- plot_abu_era_sea %>% dplyr::filter(era == "historical")
plot_cont <- plot_abu_era_sea %>% dplyr::filter(era == "contemporary")

ref_hist <- ref_abu_era_sea %>% dplyr::filter(era == "historical")
ref_cont <- ref_abu_era_sea %>% dplyr::filter(era == "contemporary")

g_abu_era_sea_q012 <- ggplot() +
  geom_ribbon(
    data = plot_hist,
    aes(x = SC, ymin = qD.LCL, ymax = qD.UCL, fill = era, group = Assemblage),
    alpha = 0.18,
    color = NA
  ) +
  geom_line(
    data = plot_hist,
    aes(x = SC, y = qD, color = era, group = Assemblage),
    linewidth = 2.2,
    alpha = 0.65
  ) +
  geom_point(
    data = ref_hist,
    aes(x = SC, y = qD, color = era, shape = era),
    size = 3.5
  ) +
  geom_ribbon(
    data = plot_cont,
    aes(x = SC, ymin = qD.LCL, ymax = qD.UCL, fill = era, group = Assemblage),
    alpha = 0.18,
    color = NA
  ) +
  geom_line(
    data = plot_cont,
    aes(x = SC, y = qD, color = era, group = Assemblage),
    linewidth = 2.2,
    alpha = 0.65
  ) +
  geom_point(
    data = ref_cont,
    aes(x = SC, y = qD, color = era, shape = era),
    size = 3.5
  ) +
  facet_grid(
    q_label ~ sea_label,
    scales = "free_y"
  ) +
  scale_colour_manual(
    values = era_cols,
    labels = era_name_map,
    name = "Era"
  ) +
  scale_fill_manual(
    values = era_cols,
    labels = era_name_map,
    name = "Era"
  ) +
  scale_shape_manual(
    values = c(
      "historical" = 16,
      "contemporary" = 17
    ),
    labels = era_name_map,
    name = "Era"
  ) +
  # coord_cartesian(xlim = c(0, 1.01)) +
  # scale_x_continuous(
  #   labels = scales::percent_format(accuracy = 1)
  # ) +
  labs(
    x = "Sample Coverage",
    y = "Species Diversity"
  ) +
  theme_classic(base_size = 12) +
  theme(
    text = element_text(family = "Times New Roman"),
    legend.position = "none",
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12),
    strip.background = element_blank(),
    strip.text = element_text(size = 12)
  )

print(g_abu_era_sea_q012)

# ---- tweak the line layer(s) that are already there ----
line_size  <- 3
line_alpha <- 0.6

g_abu_era_sea_q012$layers <- lapply(g_abu_era_sea_q012$layers, function(lyr) {
  if (inherits(lyr$geom, "GeomLine")) {
    lyr$aes_params$linewidth <- line_size   # use linewidth (ggplot2 >= 3.4)
    lyr$aes_params$alpha     <- line_alpha
  }
  lyr
})

print(g_abu_era_sea_q012)

# ggsave(
#   file.path(out_dir_fig, "figure_inext_abundance_q012_coverage_by_era_sea_faceted_man.png"),
#   g_abu_era_sea_q012,
#   width = 6.5,
#   height = 7.5,
#   dpi = 300
# )


###########################################################
#### BOOTSTRAP Δ HILL DIVERSITY BY SEA                 ####
#### Δ = contemporary - historical at common coverage   ####
###########################################################

# Δ = contemporary - historical

set.seed(123)

B <- 1000
q_vec <- c(0, 1, 2)

# Efficient abundance bootstrap using multinomial resampling.
bootstrap_abund <- function(v) {
  n <- sum(v)
  p <- v / n
  
  boot_counts <- as.vector(rmultinom(
    n = 1,
    size = n,
    prob = p
  ))
  
  names(boot_counts) <- names(v)
  boot_counts <- boot_counts[boot_counts > 0]
  boot_counts
}

# Estimate qD at fixed sample coverage.
qD_at_targetC <- function(v_named, targetC, q_vec) {
  
  est <- tryCatch(
    {
      iNEXT::estimateD(
        list(x = v_named),
        q = q_vec,
        datatype = "abundance",
        base = "coverage",
        level = targetC,
        conf = 0.95,
        nboot = 0
      )
    },
    error = function(e) NULL
  )
  
  out <- rep(NA_real_, length(q_vec))
  names(out) <- as.character(q_vec)
  
  if (!is.null(est)) {
    out[as.character(est$Order.q)] <- est$qD
  }
  
  out
}

bootstrap_delta_one_sea <- function(sea_i, inext_list, targetC, B = 1000, q_vec = c(0, 1, 2)) {
  
  hist_name <- paste("historical", sea_i, sep = "_")
  cont_name <- paste("contemporary", sea_i, sep = "_")
  
  if (!all(c(hist_name, cont_name) %in% names(inext_list))) {
    stop("Missing historical or contemporary assemblage for sea = ", sea_i)
  }
  
  v_hist <- inext_list[[hist_name]]
  v_cont <- inext_list[[cont_name]]
  
  boot_delta <- matrix(
    NA_real_,
    nrow = B,
    ncol = length(q_vec)
  )
  
  colnames(boot_delta) <- paste0("q", q_vec)
  
  for (b in seq_len(B)) {
    
    v_hist_b <- bootstrap_abund(v_hist)
    v_cont_b <- bootstrap_abund(v_cont)
    
    q_hist <- qD_at_targetC(v_hist_b, targetC, q_vec)
    q_cont <- qD_at_targetC(v_cont_b, targetC, q_vec)
    
    boot_delta[b, ] <- q_cont[as.character(q_vec)] - q_hist[as.character(q_vec)]
  }
  
  boot_delta_df <- as.data.frame(boot_delta) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(
      sea = sea_i,
      target_coverage = targetC,
      bootstrap = dplyr::row_number()
    ) %>%
    dplyr::select(sea, target_coverage, bootstrap, dplyr::everything())
  
  summ_delta <- purrr::map_dfr(colnames(boot_delta), function(qname) {
    
    d <- boot_delta[, qname]
    d <- d[is.finite(d)]
    n_eff <- length(d)
    
    # Plus-one correction prevents reporting p = 0.
    p_two <- 2 * min(
      (sum(d <= 0) + 1) / (n_eff + 1),
      (sum(d >= 0) + 1) / (n_eff + 1)
    )
    
    p_two <- min(1, p_two)
    
    tibble::tibble(
      sea = sea_i,
      Order.q = qname,
      target_coverage = targetC,
      delta_mean = mean(d),
      delta_median = stats::median(d),
      CI_low = unname(stats::quantile(d, 0.025)),
      CI_high = unname(stats::quantile(d, 0.975)),
      p_boot_two_sided = p_two,
      n_boot_successful = n_eff
    )
  })
  
  list(
    raw = boot_delta_df,
    summary = summ_delta
  )
}

# Use sea-specific target coverage.
boot_out_era_sea <- purrr::map(c("bohol", "sulu"), function(sea_i) {
  
  target_i <- targetC_by_sea %>%
    dplyr::filter(sea == sea_i) %>%
    dplyr::pull(target_coverage)
  
  bootstrap_delta_one_sea(
    sea_i = sea_i,
    inext_list = inext_abu_era_sea,
    targetC = target_i,
    B = B,
    q_vec = q_vec
  )
})

boot_delta_era_sea_raw <- purrr::map_dfr(boot_out_era_sea, "raw")

summ_delta_era_sea <- purrr::map_dfr(boot_out_era_sea, "summary") %>%
  dplyr::mutate(
    sea_label = dplyr::recode(sea, !!!sea_name_map)
  )

print(summ_delta_era_sea)

# Save Tables
# readr::write_csv(
#   summ_delta_era_sea,
#   file.path(out_dir_tab, "table_inext_abundance_bootstrap_delta_contemporary_minus_historical_q012_by_sea.csv")
# )
# 
# readr::write_csv(
#   boot_delta_era_sea_raw,
#   file.path(out_dir_tab, "table_inext_abundance_bootstrap_delta_raw_contemporary_minus_historical_q012_by_sea.csv")
# )