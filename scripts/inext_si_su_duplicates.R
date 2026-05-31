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
  nboot = 200
)

print(est_abu_all)

# Build coverage-based curves.
out_abu_all <- iNEXT::iNEXT(
  inext_abu_all,
  q = c(0, 1, 2),
  datatype = "abundance",
  se = TRUE,
  conf = 0.95,
  knots = 500,
  nboot = 50
)

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
#   file.path(out_dir_tab, "table_inext_abundance_estimateD_q012_coverage_whole_dataset.csv")
# )

############################################################
#### WHOLE DATASET: POOLED ABUNDANCE GAMMA DIVERSITY     ####
#### 1. Species richness curve, q = 0                    ####
#### 2. Faceted Hill diversity curves, q = 0, 1, 2        ####
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

g_abu_all_q0 <- iNEXT::ggiNEXT(
  out_abu_all_q0,
  type = 3,
  se = TRUE,
  color.var = "Assemblage"
) +
  scale_colour_manual(
    values = all_cols,
    labels = all_name_map,
    name = NULL
  ) +
  scale_fill_manual(
    values = all_cols,
    labels = all_name_map,
    name = NULL
  ) +
  coord_cartesian(xlim = c(0, 1.01)) +
  scale_x_continuous(
    labels = scales::percent_format(accuracy = 1)
  ) +
  labs(
    x = "% Coverage",
    y = "Species Richness"
  ) +
  theme_classic(base_size = 18) +
  theme(
    text = element_text(family = "Times New Roman"),
    legend.position = "none",
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16)
  )

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
#   file.path(out_dir_fig, "figure_inext_abundance_all_q0_coverage.png"),
#   g_abu_all_q0,
#   width = 5.5,
#   height = 7.5,
#   dpi = 300
# )


#################################################
#### PLOT: WHOLE DATASET q0, q1, q2 FACETED  ####
#################################################

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
  scale_colour_manual(
    values = all_cols,
    labels = all_name_map,
    name = NULL
  ) +
  scale_fill_manual(
    values = all_cols,
    labels = all_name_map,
    name = NULL
  ) +
  coord_cartesian(xlim = c(0, 1.01)) +
  scale_x_continuous(
    labels = scales::percent_format(accuracy = 1)
  ) +
  labs(
    x = "% Coverage",
    y = "Hill Diversity"
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

# Optional: thicken curves and reference points
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


ggsave(
  file.path(out_dir_fig, "figure_inext_abundance_all_q012_coverage_faceted.png"),
  g_abu_all_q012,
  width =5.5,
  height = 7.5,
  dpi = 300
)


#################################################
#### 2. BY ERA: HISTORICAL VS CONTEMPORARY    ####
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
  nboot = 200
)

print(est_abu_era)

# Save tables.
readr::write_csv(
  summ_abu_era,
  file.path(out_dir_tab, "table_inext_abundance_summary_by_era.csv")
)

readr::write_csv(
  info_abu_era,
  file.path(out_dir_tab, "table_inext_abundance_DataInfo_by_era.csv")
)

readr::write_csv(
  est_abu_era,
  file.path(out_dir_tab, "table_inext_abundance_estimateD_q012_coverage_by_era.csv")
)


#######################################
#### 3. COVERAGE-BASED ERA CURVES  ####
#######################################

out_abu_era <- iNEXT::iNEXT(
  inext_abu_era,
  q = c(0, 1, 2),
  datatype = "abundance",
  se = TRUE,
  conf = 0.95,
  knots = 500,
  nboot = 50
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
  coord_cartesian(xlim = c(0, 1.01)) +
  scale_x_continuous(
    labels = scales::percent_format(accuracy = 1)
  ) +
  labs(
    x = "% Coverage",
    y = "Species Richness"
  ) +
  theme_classic(base_size = 18) +
  theme(
    text = element_text(family = "Times New Roman"),
    legend.position = NULL,
    #legend.title = element_text(size = 14),
    #legend.text = element_text(size = 14),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16)
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

ggsave(
  file.path(out_dir_fig, "figure_inext_abundance_q0_coverage_by_era_leg_r.png"),
  g_abu_era_q0,
  width = 8,
  height = 7.5,
  dpi = 300
)


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
  coord_cartesian(xlim = c(0, 1.01)) +
  scale_x_continuous(
    labels = scales::percent_format(accuracy = 1)
  ) +
  labs(
    x = "% Coverage",
    y = "Hill Diversity"
  ) +
  theme_classic(base_size = 18) +
  theme(
    text = element_text(family = "Times New Roman"),
    legend.position = "right",
    # legend.title = element_text(size = 14),
    # legend.text = element_text(size = 14),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16),
    strip.background = element_blank(),
    strip.text = element_text(size = 16)
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


ggsave(
  file.path(out_dir_fig, "figure_inext_abundance_q012_coverage_by_era_leg_r.png"),
  g_abu_era_q012,
  width = 5,
  height = 7.5,
  dpi = 300
)



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

readr::write_csv(
  summ_delta_era,
  file.path(out_dir_tab, "table_inext_abundance_bootstrap_delta_contemporary_minus_historical_q012_at_target_coverage.csv")
)

readr::write_csv(
  boot_delta,
  file.path(out_dir_tab, "table_inext_abundance_bootstrap_delta_raw_contemporary_minus_historical_q012.csv")
)



############################################################
#### iNEXT: POOLED ABUNDANCE BY ERA × SEA                ####
#### Coverage-based SACs + bootstrap Δ Hill diversity    ####
############################################################

############################
#### COLORS AND LABELS  ####
############################

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


#################################
#### ALIGN COMMUNITY + DESIGN ####
#################################

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


##########################################
#### BUILD ABUNDANCE LIST BY ERA × SEA ####
##########################################

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

# readr::write_csv(
#   summ_abu_era_sea,
#   file.path(out_dir_tab, "table_inext_abundance_summary_by_era_sea.csv")
# )





##################################
#### DATAINFO + TARGET COVERAGE ####
##################################

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

# readr::write_csv(
#   info_abu_era_sea,
#   file.path(out_dir_tab, "table_inext_abundance_DataInfo_by_era_sea.csv")
# )

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





#########################################################
#### ESTIMATE q0, q1, q2 AT COMMON COVERAGE BY SEA    ####
#########################################################

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
    nboot = 200
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

# readr::write_csv(
#   est_abu_era_sea,
#   file.path(out_dir_tab, "table_inext_abundance_estimateD_q012_coverage_by_era_sea.csv")
# )


########################################
#### iNEXT CURVES BY ERA × SEA       ####
########################################

set.seed(123)

out_abu_era_sea <- iNEXT::iNEXT(
  inext_abu_era_sea,
  q = c(0, 1, 2),
  datatype = "abundance",
  se = TRUE,
  conf = 0.95,
  knots = 500,
  nboot = 50
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
  coord_cartesian(xlim = c(0, 1.01)) +
  scale_x_continuous(
    labels = scales::percent_format(accuracy = 1)
  ) +
  labs(
    x = "% Coverage",
    y = "Species Richness"
  ) +
  theme_classic(base_size = 18) +
  theme(
    text = element_text(family = "Times New Roman"),
    legend.position = NULL,
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16),
    strip.background = element_blank(),
    strip.text = element_text(size = 16)
  )

print(g_abu_era_sea_q0)

ggsave(
  file.path(out_dir_fig, "figure_inext_abundance_q0_coverage_by_era_sea.png"),
  g_abu_era_sea_q0,
  width = 5.5,
  height = 7.5,
  dpi = 300
)


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
  coord_cartesian(xlim = c(0, 1.01)) +
  scale_x_continuous(
    labels = scales::percent_format(accuracy = 1)
  ) +
  labs(
    x = "% Coverage",
    y = "Hill Diversity"
  ) +
  theme_classic(base_size = 14) +
  theme(
    text = element_text(family = "Times New Roman"),
    legend.position = "top",
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 12),
    strip.background = element_blank(),
    strip.text = element_text(size = 16)
  )

print(g_abu_era_sea_q012)

ggsave(
  file.path(out_dir_fig, "figure_inext_abundance_q012_coverage_by_era_sea_faceted_leg_top.png"),
  g_abu_era_sea_q012,
  width = 7,
  height = 7.5,
  dpi = 300
)


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

# readr::write_csv(
#   summ_delta_era_sea,
#   file.path(out_dir_tab, "table_inext_abundance_bootstrap_delta_contemporary_minus_historical_q012_by_sea.csv")
# )
# 
# readr::write_csv(
#   boot_delta_era_sea_raw,
#   file.path(out_dir_tab, "table_inext_abundance_bootstrap_delta_raw_contemporary_minus_historical_q012_by_sea.csv")
# )










#########################################################################################
########## FVC #################
#########################################################################################
#### SUBSAMPLE VEGANIZED DATA ####
#########################################################################################
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


# Optional save
# readr::write_csv(
#   X_counts,
#   file.path("../data/vegan_data/data_vegan_all_transect_code_habitat_reef_x_counts.csv")
# )
# readr::write_csv(
#   X_counts,
#   file.path("../data/vegan_data/data_vegan_all_transect_code_habitat_reef_density_per100.csv")
# )




###################
#### FUNCTIONS ####
###################
## Build iNEXT input lists
# (1) Abundance list for each assemblage (site × year)
make_abundance_by_assemblage <- function(design, X) {
  split_rows <- split(design$transect_code, droplevels(design$site_code))
  abu <- lapply(split_rows, function(rows) {
    v <- colSums(X[rows, , drop = FALSE], na.rm = TRUE)
    v[v > 0]
  })
  names(abu) <- names(split_rows)
  abu
}

# (2) Incidence_raw list for each assemblage (species x transect presence/absence matrix)
make_incidence_raw_by_assemblage <- function(design, X) {
  split_rows <- split(design$transect_code, droplevels(design$site_code))
  inc_raw <- lapply(split_rows, function(rows) {
    as.data.frame((X[rows, , drop = FALSE] > 0) * 1L)  # rows = transects, cols = species
  })
  names(inc_raw) <- names(split_rows)
  inc_raw
}

# (4) Site-specific two-group extractor, returns list("1978 Site"=..., "2011 Site"=...)
two_groups_for_site <- function(named_list, site_pretty) {
  wanted <- c(paste0("1978 ", site_pretty), paste0("2011 ", site_pretty))
  # Fallback to regex if exact names missing
  if (!all(wanted %in% names(named_list))) {
    idx <- grep(paste0("^(1978|2011).*", site_pretty), names(named_list), ignore.case=TRUE, value=TRUE)
    stopifnot(length(idx) == 2L)
    idx <- idx[order(grepl("2011", idx))]
    res <- named_list[idx]; names(res) <- wanted; return(res)
  }
  named_list[wanted]
}

# (5) Common coverage target helper: min observed across a set, capped at 0.95
common_targetC <- function(info_df, cap = 0.95) min(cap, min(info_df$SC, na.rm = TRUE))


#####################################################
#### COVERAGE-BASED, ERA-POOLED, ABUNDANCE (q=0,1,2) #
#####################################################

# 1) Build pooled abundance vectors by era
abund_1978 <- colSums(X_counts[design$year == "1978", , drop = FALSE], na.rm = TRUE)
abund_2011 <- colSums(X_counts[design$year == "2011", , drop = FALSE], na.rm = TRUE)

abund_1978 <- abund_1978[abund_1978 > 0]
abund_2011 <- abund_2011[abund_2011 > 0]

inext_abu_era <- list(
  "1978" = abund_1978,
  "2011" = abund_2011
)

# 2) DataInfo + common target coverage (cap at 0.95)
info_abu_era <- iNEXT::DataInfo(inext_abu_era, datatype = "abundance")
print(info_abu_era)

targetC_abu_era <- min(0.95, min(info_abu_era$SC, na.rm = TRUE))
targetC_abu_era

# 3) Coverage-standardized estimates at the common target coverage
est_abu_era <- iNEXT::estimateD(
  inext_abu_era,
  q = c(0, 1, 2),
  datatype = "abundance",
  base = "coverage",
  level = targetC_abu_era,
  conf = 0.95,
  nboot = 200
)
print(est_abu_era)

# Optional: save
# readr::write_csv(info_abu_era, file.path(out_dir_tab, "table_inext_abundance_DataInfo_by_era.csv"))
# readr::write_csv(est_abu_era,  file.path(out_dir_tab, "table_inext_abundance_estimateD_q012_coverage_by_era.csv"))

# 4) Curves (coverage-based) for plotting
out_abu_era <- iNEXT::iNEXT(
  inext_abu_era,
  q = c(0, 1, 2),
  datatype = "abundance",
  se = TRUE,
  conf = 0.95,
  knots = 500,
  nboot = 50
)

# 5) Example plot (q = 0 only) in your established style (optional)
out_abu_era_q0 <- out_abu_era
out_abu_era_q0$iNextEst$coverage_based <- dplyr::filter(out_abu_era_q0$iNextEst$coverage_based, Order.q == 0)
out_abu_era_q0$iNextEst$size_based     <- dplyr::filter(out_abu_era_q0$iNextEst$size_based,     Order.q == 0)

year_cols <- c("1978" = "#F8766D", "2011" = "#00BFC4")

g_abu_era_q0 <- iNEXT::ggiNEXT(out_abu_era_q0, type = 3, se = TRUE, color.var = "Assemblage") +
  scale_colour_manual(values = year_cols, name = "Year") +
  scale_fill_manual(values = year_cols, name = "Year") +
  geom_line(size = 3, alpha = 1.0) +
  coord_cartesian(xlim = c(0, 1.01)) +
  scale_y_continuous(limits = c(0, 270), breaks = seq(0, 250, 50), expand = expansion(add = c(10,10))) +
  labs(x = "% Coverage", y = "Species Richness") +
  theme_classic(base_size = 18) + 
  theme(
    legend.position = "none",
    text = element_text(family = "Times New Roman"))

print(g_abu_era_q0)

g_abu_era_q0 <- iNEXT::ggiNEXT(out_abu_era_q0, type = 3, se = TRUE, color.var = "Assemblage") +
  scale_colour_manual(values = year_cols, name = "Year") +
  scale_fill_manual(values = year_cols, name = "Year") +
  coord_cartesian(xlim = c(0, 1.01)) +
  scale_y_continuous(limits = c(0, 270),
                     breaks = seq(0, 250, 50),
                     expand = expansion(add = c(10,10))) +
  labs(x = "% Coverage", y = "Species Richness") +
  theme_classic(base_size = 18) + 
  theme(
    legend.position = "top", "left",
    text = element_text(family = "Times New Roman")
  )

# ---- tweak the line layer(s) that are already there ----
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

# Optional save
ggsave(file.path(out_dir_fig, "figure_inext_abundance_q0_vs_coverage_by_era.png"),
       g_abu_era_q0, width = 7, height = 7.5, dpi = 300)

############################################
#### Test-like bootstrap Δ(2011-1978)   ####
#### at common target coverage (abund) ####
############################################

set.seed(123)

# Inputs: abundance vectors you already built
v78 <- inext_abu_era[["1978"]]
v11 <- inext_abu_era[["2011"]]

targetC <- targetC_abu_era
B <- 1000        # increase if you want smoother tails (e.g., 2000)
q_vec <- c(0, 1, 2)

# Helper: bootstrap an abundance vector by resampling individuals
# Returns a named vector of species counts
bootstrap_abund <- function(v) {
  # v is a named vector: names = species, values = counts
  sp <- rep(names(v), times = as.integer(v))
  sp_boot <- sample(sp, size = length(sp), replace = TRUE)
  tab <- table(sp_boot)
  # ensure numeric named vector
  as.numeric(tab) |> setNames(names(tab))
}

# Helper: compute qD at fixed target coverage for a single abundance vector
qD_at_targetC <- function(v_named, targetC, q_vec) {
  lst <- list(All = v_named)
  est <- iNEXT::estimateD(
    lst,
    q = q_vec,
    datatype = "abundance",
    base = "coverage",
    level = targetC,
    conf = 0.95,
    nboot = 0
  )
  # return named numeric vector qD by Order.q
  out <- est$qD
  names(out) <- as.character(est$Order.q)
  out
}

# Run bootstrap
boot_delta <- matrix(NA_real_, nrow = B, ncol = length(q_vec))
colnames(boot_delta) <- paste0("q", q_vec)

for (b in seq_len(B)) {
  v78_b <- bootstrap_abund(v78)
  v11_b <- bootstrap_abund(v11)
  
  q78 <- qD_at_targetC(v78_b, targetC, q_vec)
  q11 <- qD_at_targetC(v11_b, targetC, q_vec)
  
  # Δ = 2011 - 1978
  boot_delta[b, ] <- q11[as.character(q_vec)] - q78[as.character(q_vec)]
}

boot_delta <- as.data.frame(boot_delta)

# Summarize Δ with CI and a “p-value-like” metric
summ_delta <- purrr::map_dfr(names(boot_delta), function(qname) {
  d <- boot_delta[[qname]]
  d <- d[is.finite(d)]
  
  p_two <- 2 * min(mean(d <= 0), mean(d >= 0))  # bootstrap tail area (two-sided)
  tibble::tibble(
    Order.q = qname,
    delta_mean = mean(d),
    delta_median = stats::median(d),
    CI_low = stats::quantile(d, 0.025),
    CI_high = stats::quantile(d, 0.975),
    p_boot_two_sided = p_two
  )
})

print(summ_delta)

# Optional: save
readr::write_csv(summ_delta, file.path(out_dir_tab, "table_inext_abundance_bootstrap_delta_2011_minus_1978_q012_at_target_coverage.csv"))




###########################
#### BUILD INPUT LISTS ####
###########################
abu_list  <- rename_to_pretty(make_abundance_by_assemblage(design, X_counts))
incR_list <- rename_to_pretty(make_incidence_raw_by_assemblage(design, X_counts))

#####################################
#### PLOTS: POOLED (Fig. 2A, 2B) ####
#####################################
## Plot styling (consistent)
year_cols <- c("1978"="#F8766D","2011"="#00BFC4")
base_theme <- theme_classic(base_size = 12) + theme(text = element_text(family = "Times New Roman"))

# Abundance pooled
abund_all <- colSums(X_counts, na.rm = TRUE); abund_all <- abund_all[abund_all > 0]
inext_abund_pooled <- list("All" = abund_all)
info_abund_pooled  <- iNEXT::DataInfo(inext_abund_pooled, datatype = "abundance")
targetC_abund_pool <- common_targetC(info_abund_pooled, cap = 0.95) #cap = 1.0)

min(info_abund_pooled$SC, na.rm = TRUE)

out_abund_pooled   <- iNEXT::iNEXT(inext_abund_pooled, q = 0, datatype = "abundance", se = TRUE, conf = 0.95, nboot = 200)
out_abund_pooled_10   <- iNEXT::iNEXT(inext_abund_pooled, q = 0, datatype = "abundance", se = TRUE, conf = 0.95, nboot = 10)

out_abund_pooled$
  
  targetC_abund_pool
common_targetC

#### FIGURE 2.A. SAC Richness vs Coverage ####
g_pooled_abu <- iNEXT::ggiNEXT(out_abund, type = 3, se = TRUE, color.var = "Assemblage") +
  scale_color_manual(values = c(All="black"), guide = "none") +
  scale_fill_manual(values  = c(All="black"), guide = "none") +
  scale_shape_manual(values = 16, guide = "none") +
  coord_cartesian(xlim = c(0, 1.01)) +
  scale_y_continuous(limits = c(0, 350), breaks = seq(0, 350, 50), expand = expansion(add = c(10,10))) +
  labs(x = "Sample coverage (Ĉ)", y = "Estimated species richness") +
  theme_classic(base_size = 12) + 
  theme(
    legend.position = "none",
    text = element_text(family = "Times New Roman"))
print(g_pooled_abu)
# ggsave(file.path(out_dir_fig, "figure_inext_abundance_q0_vs_coverage_all_assemblage.png"), g_pooled_abu, width = 6.5, height = 8, dpi = 300)

#### FIGURE S2. SAC Richness vs Individuals ####
g_pooled_abu_type1 <- iNEXT::ggiNEXT(out_abund, type = 1, se = TRUE, color.var = "Assemblage") +
  scale_color_manual(values = c(Assemblage1="black"), guide = "none") +
  scale_fill_manual(values  = c(Assemblage1="black"), guide = "none") +
  scale_shape_manual(values = 16, guide = "none") +
  # coord_cartesian(xlim = c(0, 1.01)) +
  scale_x_continuous(limits = c(0, 107000), breaks = seq(0, 100000, 20000), labels = scales::comma_format()) + #, expand = expansion(add = c(10,10))) +
  scale_y_continuous(limits = c(0, 350), breaks = seq(0, 350, 50), expand = expansion(add = c(10,10))) +
  labs(x = "Individuals", y = "Estimated species richness") +
  theme_classic(base_size = 12) + 
  theme(
    legend.position = "none",
    text = element_text(family = "Times New Roman"))
print(g_pooled_abu_type1)
# ggsave(file.path(out_dir_fig, "figure_inext_abundance_q0_vs_individuals_all_assemblage.png"), g_pooled_abu_type1, width = 6.5, height = 8, dpi = 300)

# Incidence pooled (sensitivity; Fig. 2B)
# Use incidence_raw when units exist. For pooled, we can bind all transects.
inc_pooled_raw <- as.data.frame((X_counts > 0) * 1L)
inext_inc_pooled <- list("All" = inc_pooled_raw)  # incidence_raw expects a data.frame of units x species
info_inc_pooled  <- iNEXT::DataInfo(inext_inc_pooled, datatype = "incidence_raw")
targetC_inc_pool <- common_targetC(info_inc_pooled, cap = 0.95)
out_inc_pooled   <- iNEXT::iNEXT(inext_inc_pooled, q = 0, datatype = "incidence_raw", se = TRUE, conf = 0.95, nboot = 200)
out_inc_pooled_freq   <- iNEXT::iNEXT(inext_inc_pooled, q = 0, datatype = "incidence_freq", se = TRUE, conf = 0.95, nboot = 200)

est_inc_cov <- iNEXT::estimateD(
  inext_inc_list, q = 0, base = "coverage", level = targetC_inc,
  conf = 0.95, datatype = "incidence_freq"
)

g_pooled_inc <- iNEXT::ggiNEXT(out_inc, type = 3, se = TRUE, color.var = "Assemblage") +
  scale_color_manual(values = c(All="black"), guide = "none") +
  scale_fill_manual(values  = c(All="black"), guide = "none") +
  scale_shape_manual(values = 16, guide = "none") +
  coord_cartesian(xlim = c(0, 1.01)) +
  scale_y_continuous(limits = c(0, 350), breaks = seq(0, 350, 50), expand = expansion(add = c(10,10))) +
  labs(x = "Sample coverage (Ĉ)", y = "Species richness (q = 0)") +
  theme_classic(base_size = 12) + 
  theme(
    legend.position = "bottom",
    text = element_text(family = "Times New Roman"))
print(g_pooled_inc)
ggsave(file.path(out_dir_fig, "figure_inext_incidence_q0_vs_coverage_all.png"), g_pooled_inc, width = 6, height = 8, dpi = 300)

# Combine pooled A/B (Figure 2)
panel_pooled <- cowplot::plot_grid(g_pooled_abu, g_pooled_inc, labels = c("A","B"), label_size = 12, ncol = 1, align = "v")
ggsave(file.path(out_dir_fig, "figure_inext_abu_inc_q0_vs_coverage_pooled_1x2.png"), panel_pooled, width = 6, height = 8, dpi = 300)


########################################################
#### SINGLETONS / DOUBLETONS: ALL POOLED ASSEMBLAGE ####
########################################################

# Abundance pooled
abund_all <- colSums(X_counts, na.rm = TRUE)
abund_all <- abund_all[abund_all > 0]
inext_abund_pooled <- list(All = abund_all)

info_abund_pooled <- iNEXT::DataInfo(
  inext_abund_pooled,
  datatype = "abundance"
)

print(info_abund_pooled)

# Extract abundance singleton / doubleton counts
abund_rare_tbl <- info_abund_pooled %>%
  dplyr::transmute(
    Assemblage,
    datatype   = "abundance",
    n          = n,
    S_obs      = S.obs,
    SC         = SC,
    singletons = f1,
    doubletons = f2
  )

print(abund_rare_tbl)


# Incidence pooled (incidence_freq)
T_units <- nrow(X_counts)  # number of transects
inc_all <- colSums(X_counts > 0, na.rm = TRUE)
inc_all <- inc_all[inc_all > 0]
inext_inc_pooled <- list(All = c(T_units, inc_all))

info_inc_pooled <- iNEXT::DataInfo(
  inext_inc_pooled,
  datatype = "incidence_freq"
)

print(info_inc_pooled)

# Extract incidence uniques / duplicates
# In incidence_freq output:
# Q1 = species occurring in exactly 1 sampling unit
# Q2 = species occurring in exactly 2 sampling units
inc_rare_tbl <- info_inc_pooled %>%
  dplyr::transmute(
    Assemblage,
    datatype    = "incidence_freq",
    T           = T,
    U           = U,
    S_obs       = S.obs,
    SC          = SC,
    uniques_Q1  = Q1,
    duplicates_Q2 = Q2
  )

print(inc_rare_tbl)


# Combine into one summary table
rare_species_summary_pooled <- dplyr::bind_rows(
  abund_rare_tbl %>%
    dplyr::rename(rare_1 = singletons, rare_2 = doubletons),
  inc_rare_tbl %>%
    dplyr::rename(rare_1 = uniques_Q1, rare_2 = duplicates_Q2)
)

print(rare_species_summary_pooled)

# Optional save
# readr::write_csv(
#   rare_species_summary_pooled,
#   file.path(out_dir_tab, "table_inext_rare_species_counts_pooled.csv")
# )





############################################
#### BY-SITE ABUNDANCE (MAIN; Figure 3) ####
############################################
# iNEXT object for all 8 assemblages (q=0, abundance)
out_abu_all <- iNEXT::iNEXT(
  x        = abu_list, 
  q        = 0, 
  datatype = "abundance", 
  knots    = 1000,
  se       = TRUE, 
  conf     = 0.95, 
  nboot    = 10
)

# Faceted 2×2 (panels = sites; color = year)
panel_levels <- c("Mactan","Olango","Sumilon West","Sumilon East")
panel_labels <- c("Mactan"="Mactan","Olango"="Olango","Sumilon West"="Sumilon West","Sumilon East"="Sumilon East")

df_cov <- out_abu_all$iNextEst$coverage_based |>
  tidyr::separate(Assemblage, into = c("year","site"), sep = " ", remove = FALSE, extra = "merge") |>
  mutate(year = factor(year, levels = c("1978","2011")),
         site = factor(site, levels = panel_levels))

p_cov_facets <- ggplot(df_cov, aes(x = SC, y = qD, color = year, group = interaction(year, Method))) +
  geom_ribbon(aes(ymin = qD.LCL, ymax = qD.UCL, fill = year), alpha = 0.15, color = NA, show.legend = FALSE) +
  geom_line(linewidth = 0.9) +
  geom_point(data = subset(df_cov, Method == "observed"), size = 1.9, shape = 16) +
  scale_color_manual(values = year_cols, name = "Year") +
  scale_fill_manual(values = year_cols, guide = "none") +
  coord_cartesian(xlim = c(0, 1.01)) +
  facet_wrap(~ site, ncol = 2, labeller = as_labeller(panel_labels)) +
  labs(x = "Sample coverage (Ĉ)", y = "Estimated richness (q = 0)") +
  base_theme + theme(strip.text = element_text(face = "bold"), legend.position = "bottom", legend.title = element_text(face = "bold"))
print(p_cov_facets)
ggsave(file.path(out_dir_fig, "figure_inext_abundance_q0_vs_coverage_2x2.png"), p_cov_facets, width = 6, height = 8, dpi = 300)

#### BY-SITE INCIDENCE (SENSITIVITY; Figure 4) ####
# Build incidence_raw iNEXT for all 8 assemblages
out_inc_all <- iNEXT::iNEXT(incR_list, q = 0, datatype = "incidence_raw", se = TRUE, conf = 0.95, nboot = 200)

df_cov_inc <- out_inc_all$iNextEst$coverage_based |>
  tidyr::separate(Assemblage, into = c("year","site"), sep = " ", remove = FALSE, extra = "merge") |>
  mutate(year = factor(year, levels = c("1978","2011")),
         site = factor(site, levels = panel_levels))

p_cov_inc_facets <- ggplot(df_cov_inc, aes(x = SC, y = qD, color = year, group = interaction(year, Method))) +
  geom_ribbon(aes(ymin = qD.LCL, ymax = qD.UCL, fill = year), alpha = 0.15, color = NA, show.legend = FALSE) +
  geom_line(linewidth = 0.9) +
  geom_point(data = subset(df_cov_inc, Method == "observed"), size = 1.9, shape = 16) +
  scale_color_manual(values = year_cols, name = "Year") +
  scale_fill_manual(values = year_cols, guide = "none") +
  coord_cartesian(xlim = c(0, 1.01)) +
  facet_wrap(~ site, ncol = 2, labeller = as_labeller(panel_labels)) +
  labs(x = "Sample coverage (Ĉ)", y = "Estimated richness (q = 0)") +
  base_theme + theme(strip.text = element_text(face = "bold"), legend.position = "bottom", legend.title = element_text(face = "bold"))
print(p_cov_inc_facets)
# ggsave(file.path(out_dir_fig, "figure_inext_incidence_q0_vs_coverage_2x2.png"), p_cov_inc_facets, width = 6, height = 8, dpi = 300)

#### (Optional) TABLES: Coverage-standardized point estimates at a common target C per assemblage set ####
# Abundance: choose targetC as min observed across the eight assemblages, cap 0.95
info_abu_grp <- iNEXT::DataInfo(abu_list, datatype = "abundance")
targetC_groups_abu <- common_targetC(info_abu_grp, cap = 0.95)
est_abu_grp <- iNEXT::estimateD(abu_list, q = c(0, 1, 2), base = "coverage", level = targetC_groups_abu, conf = 0.95, datatype = "abundance")
# readr::write_csv(est_abu_grp, file.path(out_dir_tab, "table_inext_abundance_estimateD_q0_coverage_by_group.csv"))

# Incidence_raw sensitivity at a common coverage (optional table)
info_inc_grp <- iNEXT::DataInfo(incR_list, datatype = "incidence_raw")
targetC_groups_inc <- common_targetC(info_inc_grp, cap = 0.95)
est_inc_grp <- iNEXT::estimateD(incR_list, q = 0, base = "coverage", level = targetC_groups_inc, conf = 0.95, datatype = "incidence_raw")
# readr::write_csv(est_inc_grp, file.path(out_dir_tab, "table_inext_incidence_raw_estimateD_q0_coverage_by_group.csv"))







#################################


## Build lists
lists_by_grp <- make_inext_lists_by_group(design, X_counts, group_col = "site_code")

## rename the list elements used by iNEXT
names(lists_by_grp$abu) <- pretty_site_year(names(lists_by_grp$abu))
names(lists_by_grp$inc) <- pretty_site_year(names(lists_by_grp$inc))

stopifnot(identical(rownames(X_counts), design$transect_code))


#### FUNCTIONS ####
# make abundance and incidence lists for iNEXT inputs by group (default = site_code)
make_inext_lists_by_group <- function(design, X_counts, group_col = "site_code") {
  grp <- droplevels(design[[group_col]])
  split_rows <- split(design$transect_code, grp)   # list of row names per group
  
  # Abundance lists: positive totals only
  abu_list <- lapply(split_rows, function(rows) {
    v <- colSums(X_counts[rows, , drop = FALSE], na.rm = TRUE)
    v[v > 0]
  })
  
  # Incidence-frequency lists: c(T, f1, f2, ...)
  inc_list <- lapply(split_rows, function(rows) {
    Xg <- X_counts[rows, , drop = FALSE]
    T_g <- nrow(Xg)                                # number of transects in the group
    f  <- colSums(Xg > 0, na.rm = TRUE)
    f  <- f[f > 0]
    c(T_g, f)
  })
  
  # Name the lists with the group labels
  names(abu_list) <- names(split_rows)
  names(inc_list) <- names(split_rows)
  
  # quick sanity check
  lens <- vapply(abu_list, length, integer(1))
  if (any(lens == 0)) {
    warning("Some groups have no species with positive abundance: ",
            paste(names(lens)[lens == 0], collapse = ", "))
  }
  
  list(abu = abu_list, inc = inc_list)
}


###################################################
#### COVERAGE-BASED, ALL SITES, ABUNDANCE DATA ####
###################################################
## 1) Build the abundance vector for the pooled assemblage
abund_all <- colSums(X_counts, na.rm = TRUE)          # integer counts per species
abund_all <- abund_all[abund_all > 0]                 # drop zeros if any
inext_abund_list <- list(All = abund_all)

## 2) Coverage of the observed pooled sample
info_abund <- iNEXT::DataInfo(inext_abund_list, datatype = "abundance")

# save table
# readr::write_csv(info_abund, file.path(out_dir_tab, "table_inext_abundance_DataInfo_all.csv"))

## Choose a common target coverage (for one group this just sets the target).
## Use min(Observed SC, 0.95) as a sensible target.
# SC = 0.9993, so using 0.95
targetC_abund <- min(0.95, as.numeric(info_abund$SC))
targetC_abund

## 3) Point estimate at target coverage with 95% CIs
est_abund_cov <- iNEXT::estimateD(
  inext_abund_list, q = 0, base = "coverage", level = targetC_abund,
  conf = 0.95, datatype = "abundance"
)
print(est_abund_cov)
# Assemblage        m      Method Order.q   SC      qD   qD.LCL   qD.UCL
# All 799.1616 Rarefaction       0 0.95 98.1806 95.98641 100.3748

# save table
# readr::write_csv(est_abund_cov, file.path(out_dir_tab, "table_inext_abundance_estimateD_q0_coverage_all.csv"))

## 4) Full rarefaction/extrapolation object + quick plots
out_abund <- iNEXT::iNEXT(inext_abund_list, q = 0, datatype = "abundance",
                          se = TRUE, conf = 0.95, nboot = 200) # 10) #

# also compute the point estimate at that coverage (with CI)
pt <- iNEXT::estimateD(inext_abund_list, q = 0, datatype = "abundance",
                       base = "coverage", level = targetC_abund, nboot = 200)

print(pt)
# Assemblage        m      Method Order.q   SC      qD   qD.LCL   qD.UCL
# All 799.1616 Rarefaction       0 0.95 98.1806 96.14226 100.2189

## PLOT: Coverage-based R/E curve of richness (q = 0)
g <- iNEXT::ggiNEXT(out_abund, type = 3, se = TRUE, color.var = "Order.q") + ggplot2::labs(title = "q=0 vs Coverage (pooled)")

print(g)

# add a vertical reference line and tighter x-limits near completeness
g +
  geom_vline(xintercept = targetC_abund, linetype = 3) +
  coord_cartesian(xlim = c(0.8, 1.01)) +
  labs(subtitle = paste("Comparison at coverage =", round(targetC_abund, 3)))

## save plot
# ggplot2::ggsave(file.path(out_dir_fig, "figure_inext_abundance_q0_vs_coverage_all.png"), g, width = 6, height = 4, dpi = 300)

## Style pooled coverage-based rarefaction/extrapolation (q = 0)
g_pooled_abu <- iNEXT::ggiNEXT(out_abund, type = 3, se = TRUE, color.var = "Assemblage") +
  scale_color_manual(values = c(All = "black"), guide = "none") +
  scale_fill_manual(values  = c(All = "black"), guide = "none") +   # CI ribbon in black
  scale_shape_manual(values = 16, guide = "none") +                 # circles
  guides(linetype = "none", size = "none") +
  coord_cartesian(xlim = c(0, 1.01)) +
  scale_y_continuous(limits = c(0, 350),
                     breaks = seq(0, 350, 50),
                     expand = expansion(add = c(10, 10))) +           # small buffer below 0
  labs(title = NULL,
       x = "Sample coverage (Ĉ)",
       y = "Species richness (q = 0)") +
  theme_classic(base_size = 12) +
  theme(
    text = element_text(family = "Times New Roman"),
    legend.position = "none",
    plot.title = element_blank()
  )

print(g_pooled_abu)

# Save
ggsave(file.path(out_dir_fig, "figure_inext_abundance_q0_vs_coverage_all.png"),
       g_pooled_abu, width = 6, height = 8, dpi = 300)


#################################################
#### COVERAGE-BASED, BY SITE, ABUNDANCE DATA ####
#################################################

### ATTEMPT ###
# vvvv

## ----- Prep: parse Assemblage into year/site and build panel labels -----
# library(dplyr)
# library(tidyr)
# library(ggplot2)

# year colors
year_cols <- c("1978" = "#F8766D", "2011" = "#00BFC4")

# map site codes -> panel names you want
site_map <- c(
  "buyo" = "Mactan",        # Buyong on Mactan
  "olan" = "Olango",
  "west" = "Sumilon West",
  "east" = "Sumilon East"
)

# panel order + strip labels with (a)-(d)
panel_levels <- c("Mactan", "Olango", "Sumilon West", "Sumilon East")
panel_labels <- c(
  "Mactan"        = "(a) Mactan",
  "Olango"        = "(b) Olango",
  "Sumilon West"  = "(c) Sumilon West",
  "Sumilon East"  = "(d) Sumilon East"
)

## iNEXT output -> tidy coverage-based dataframe
df_cov <- out_abu_grp$iNextEst$coverage_based %>%
  # Assemblage looks like "1978_buyo"
  tidyr::separate(Assemblage, into = c("year","site_code"), sep = "_", remove = FALSE) %>%
  dplyr::mutate(
    year = factor(year, levels = c("1978","2011")),
    site_panel = factor(site_map[site_code], levels = panel_levels)
  )

## ----- Plot: 2×2 panels (site), color by year, all shapes circles -----
p_cov_facets <- ggplot(df_cov, aes(x = SC, y = qD, color = year, group = interaction(year, Method))) +
  # ribbons (optional; comment out next 3 lines if you don't want CIs)
  geom_ribbon(aes(ymin = qD.LCL, ymax = qD.UCL, fill = year),
              alpha = 0.15, color = NA, show.legend = FALSE) +
  # curves (observed/interp/extrap)
  geom_line(linewidth = 0.9) +
  # observed points only (optional: Method == "observed")
  geom_point(data = subset(df_cov, Method == "observed"), size = 1.9, shape = 16) +
  scale_color_manual(values = year_cols, name = "Year") +
  scale_fill_manual(values = year_cols, guide = "none") +
  coord_cartesian(xlim = c(0, 1.01)) +
  facet_wrap(~ site_panel, ncol = 2, labeller = as_labeller(panel_labels)) +
  labs(
    title = "Coverage-based rarefaction/extrapolation of species richness (q = 0)",
    subtitle = "Abundance data, by site (panels) and era (colors)",
    x = "Sample coverage (Ĉ)",
    y = "Estimated richness (q = 0)"
  ) +
  theme_classic(base_size = 12) +
  theme(
    strip.text = element_text(face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  )

print(p_cov_facets)
# ggsave("figure_inext_abundance_q0_vs_coverage_by_site_panels.png", p_cov_facets, width = 9, height = 8, dpi = 300)




## --- 1) Pull only Buyong (Mactan) groups, preserving your names if present ---
# Try the exact “pretty” names first:
want_names <- c("1978 Buyong", "2011 Buyong")
has_pretty <- all(want_names %in% names(lists_by_grp$abu))

if (has_pretty) {
  abu_mactan <- lists_by_grp$abu[want_names]
} else {
  # Fallback: try underscore codes such as "1978_buyo", "2011_buyo"
  # (adjust pattern here if your internal naming differs)
  cand <- grep("^(1978|2011).*(buyo|Buyong)", names(lists_by_grp$abu), ignore.case = TRUE, value = TRUE)
  stopifnot(length(cand) == 2)  # fail early if we didn't find exactly two
  # Reorder as 1978, 2011
  cand <- cand[order(grepl("2011", cand))]
  abu_mactan <- lists_by_grp$abu[cand]
}

# Peek to confirm
print(names(abu_mactan))

## --- 2) Run iNEXT for coverage-based richness (q=0) on these two groups ---
out_abu_mactan <- iNEXT(abu_mactan, q = 0, datatype = "abundance",
                        se = TRUE, conf = 0.95, nboot = 200)

## --- 3) Plot coverage-based curves (type = 3) with year colors, all circles ---
year_cols <- c("1978 Buyong" = "#F8766D", "2011 Buyong" = "#00BFC4")

g_mactan <- ggiNEXT(out_abu_mactan, type = 3, se = TRUE, color.var = "Assemblage") +
  scale_color_manual(values = year_cols, name = "Year × Site") +
  scale_shape_manual(values = rep(16, 2)) +   # all circles
  guides(shape = "none") +
  coord_cartesian(xlim = c(0, 1.01)) +
  labs(
    title = "Coverage-based rarefaction/extrapolation of richness (q = 0)",
    subtitle = "Mactan (Buyong): 1978 vs 2011",
    x = "Sample coverage (Ĉ)", y = "Estimated richness (q = 0)"
  ) +
  theme_classic(base_size = 12)

print(g_mactan)

# Optionally save
# ggsave(file.path(out_dir_fig, "figure_inext_abundance_q0_vs_coverage_mactan.png", g_mactan, width = 8, height = 6, dpi = 300)

#^^^^
# SUCCESSFUL WITH MACTAN 


library(iNEXT)
library(stringr)

# Helper: pull the two abundance vectors for a given site ("Buyong", "Olango",
# "Sumilon East", "Sumilon West") and run iNEXT(q = 0) for 1978 vs 2011.
make_out_abu_for_site <- function(site_pretty, abu_list, nboot = 200) {
  # Prefer exact pretty names, e.g., "1978 Buyong", "2011 Buyong"
  want <- c(paste0("1978 ", site_pretty), paste0("2011 ", site_pretty))
  have_pretty <- all(want %in% names(abu_list))
  
  if (have_pretty) {
    use <- abu_list[want]
  } else {
    # Fallback: regex match (handles underscores/codes like 1978_buyo, etc.)
    # Build a tolerant pattern for the site name
    pat_site <- site_pretty |>
      # escape spaces for regex, allow either space or underscore between words
      str_replace_all("\\s+", "[ _]+") |>
      # make case-insensitive match easier later
      paste0("(", ., ")")
    
    idx <- grep(paste0("^(1978|2011).*", pat_site), names(abu_list),
                ignore.case = TRUE, value = TRUE)
    if (length(idx) != 2L)
      stop("Could not uniquely find 1978/2011 groups for site: ", site_pretty,
           " in names(abu_list). Found: ", paste(idx, collapse = ", "))
    
    # Order as 1978, 2011
    idx <- idx[order(grepl("2011", idx))]
    use <- abu_list[idx]
    # Give friendly names if needed
    names(use) <- c(paste0("1978 ", site_pretty), paste0("2011 ", site_pretty))
  }
  
  iNEXT(use, q = 0, datatype = "abundance", se = TRUE, conf = 0.95, nboot = nboot)
}

# ---- STEP 1: build the four outputs ----
out_abu_buyong       <- make_out_abu_for_site("Buyong",       lists_by_grp$abu, nboot = 200)
out_abu_olango       <- make_out_abu_for_site("Olango",       lists_by_grp$abu, nboot = 200)
out_abu_sumilon_west <- make_out_abu_for_site("Sumilon West", lists_by_grp$abu, nboot = 200)
out_abu_sumilon_east <- make_out_abu_for_site("Sumilon East", lists_by_grp$abu, nboot = 200)

# Quick sanity check: names present in each out object
lapply(
  list(buyong = out_abu_buyong,
       olango = out_abu_olango,
       sumilon_west = out_abu_sumilon_west,
       sumilon_east = out_abu_sumilon_east),
  function(o) unique(o$iNextEst$size_based$Assemblage)
)


#### PLOT ABUNDANCE BY SITE ####

## ── Year colors (fixed across sites) ─────────────────────────────────────────
yr_cols <- c("1978 Buyong"       = "#F8766D",  "2011 Buyong"       = "#00BFC4",
             "1978 Olango"       = "#F8766D",  "2011 Olango"       = "#00BFC4",
             "1978 Sumilon West" = "#F8766D",  "2011 Sumilon West" = "#00BFC4",
             "1978 Sumilon East" = "#F8766D",  "2011 Sumilon East" = "#00BFC4")

## Helper to style a single site’s coverage-based curve (q = 0)
style_inext_covplot <- function(out_obj, title_txt) {
  ggiNEXT(out_obj, type = 3, se = TRUE, color.var = "Assemblage") +
    scale_color_manual(values = yr_cols, name = "Year × Site") +
    scale_shape_manual(values = rep(16, 8)) +  # all circles
    guides(shape = "none") +
    coord_cartesian(xlim = c(0, 1.01)) +
    labs(title = title_txt,
         x = "Sample coverage (Ĉ)",
         y = "Species richness (q = 0)") +
    theme_classic(base_size = 12) +
    theme(legend.position = "bottom",
          legend.key.width = unit(18, "pt"))
}

## ── Individual plots ─────────────────────────────────────────────────────────
g_mactan  <- style_inext_covplot(out_abu_buyong,       "Mactan: q = 0 vs coverage")
g_olango  <- style_inext_covplot(out_abu_olango,       "Olango: q = 0 vs coverage")
g_sum_w   <- style_inext_covplot(out_abu_sumilon_west, "Sumilon West: q = 0 vs coverage")
g_sum_e   <- style_inext_covplot(out_abu_sumilon_east, "Sumilon East: q = 0 vs coverage")

## ── Print Individual plots ─────────────────────────────────────────────────────────
print(g_mactan)
print(g_olango)
print(g_sum_w)
print(g_sum_e)

## ── Save each plot ───────────────────────────────────────────────────────────
# ggsave(file.path(out_dir_fig, "figure_inext_abundance_q0_vs_coverage_mactan_leg.png"),
#        g_mactan, width = 8, height = 6, dpi = 300)
# ggsave(file.path(out_dir_fig, "figure_inext_abundance_q0_vs_coverage_olango_leg.png"),
#        g_olango, width = 8, height = 6, dpi = 300)
# ggsave(file.path(out_dir_fig, "figure_inext_abundance_q0_vs_coverage_sumilon_west_leg.png"),
#        g_sum_w, width = 8, height = 6, dpi = 300)
# ggsave(file.path(out_dir_fig, "figure_inext_abundance_q0_vs_coverage_sumilon_east_leg.png"),
#        g_sum_e, width = 8, height = 6, dpi = 300)


## NO LEGEND, STD Y-AXIS: Helper to style a single site’s coverage-based curve (q = 0)
style_inext_covplot_noleg <- function(out_obj) {
  iNEXT::ggiNEXT(out_obj, type = 3, se = TRUE, color.var = "Assemblage") +
    scale_color_manual(values = yr_cols, breaks = names(yr_cols), guide = "none") +
    scale_shape_manual(values = rep(16, 8), guide = "none") +  # all circles, no legend
    guides(color = "none", shape = "none", linetype = "none", fill = "none") +
    coord_cartesian(xlim = c(0, 1.01)) +
    coord_cartesian(xlim = c(0, 1.01)) +
    scale_y_continuous(limits = c(0, 200),
                       breaks = seq(0, 200, 50),
                       expand = expansion(add = c(10, 0))) +
    labs(title = NULL,
         x = "Sample coverage (Ĉ)",
         y = "Species richness (q = 0)") +
    theme_classic(base_size = 12) +
    theme(
      text = element_text(family = "Times New Roman"),
      legend.position = "none",
      plot.title = element_blank()
    )
}

## ── Individual plots ─────────────────────────────────────────────────────────
g_mactan_noleg  <- style_inext_covplot_noleg(out_abu_buyong)
g_olango_noleg  <- style_inext_covplot_noleg(out_abu_olango)
g_sum_w_noleg   <- style_inext_covplot_noleg(out_abu_sumilon_west)
g_sum_e_noleg   <- style_inext_covplot_noleg(out_abu_sumilon_east)

## ── Print Individual plots ─────────────────────────────────────────────────────────
print(g_mactan_noleg)
print(g_olango_noleg)
print(g_sum_w_noleg)
print(g_sum_e_noleg)

## ── Save each plot ───────────────────────────────────────────────────────────
ggsave(file.path(out_dir_fig, "figure_inext_abundance_q0_vs_coverage_mactan.png"),
       g_mactan_noleg, width = 8, height = 6, dpi = 300)
ggsave(file.path(out_dir_fig, "figure_inext_abundance_q0_vs_coverage_olango.png"),
       g_olango_noleg, width = 8, height = 6, dpi = 300)
ggsave(file.path(out_dir_fig, "figure_inext_abundance_q0_vs_coverage_sumilon_west.png"),
       g_sum_w_noleg, width = 8, height = 6, dpi = 300)
ggsave(file.path(out_dir_fig, "figure_inext_abundance_q0_vs_coverage_sumilon_east.png"),
       g_sum_e_noleg, width = 8, height = 6, dpi = 300)

## ── 2×2 panel (a–d) ─────────────────────────────────────────────────────────
panel_2x2 <- plot_grid(
  g_mactan_noleg, g_olango_noleg,
  g_sum_w_noleg,  g_sum_e_noleg,
  labels = c("A", "B", "C", "D"),
  label_size = 12, ncol = 2, align = "hv"
)

print(panel_2x2)

ggsave(file.path(out_dir_fig, "figure_inext_abundance_q0_vs_coverage_2x2.png"),
       panel_2x2, width = 6, height = 8, dpi = 300)

# Add a shared legend under the grid
legend_g <- cowplot::get_legend(g_mactan + theme(legend.position = "bottom"))
panel_2x2_with_legend <- cowplot::plot_grid(panel_2x2, legend_g,
                                            ncol = 1, rel_heights = c(1, 0.12))

print(panel_2x2_with_legend)
# didn't add legend

# ggsave(file.path(out_dir_fig, "figure_inext_abundance_q0_vs_coverage_2x2.png"),
# panel_2x2_with_legend, width = 12, height = 10, dpi = 300)

# ^^^^
### ATTEMPT ###

info_abu_grp <- iNEXT::DataInfo(lists_by_grp$abu, datatype = "abundance")
print(info_abu_grp)

## save table
# readr::write_csv(info_abu_grp, file.path(out_dir_tab, "table_inext_abundance_DataInfo_by_group.csv"))

## Choose a common target coverage across groups (cap at 0.95)
targetC_groups <- min(0.95, min(info_abu_grp$SC, na.rm = TRUE), min(info_inc_grp$SC, na.rm = TRUE))

## Coverage-standardized richness (q=0) with 95% CIs
est_abu_grp <- iNEXT::estimateD(lists_by_grp$abu, q = 0, base = "coverage",
                                level = targetC_groups, conf = 0.95, datatype = "abundance")
print(est_abu_grp)

# save table
# readr::write_csv(est_abu_grp, file.path(out_dir_tab, "table_inext_abundance_estimateD_q0_coverage_by_group.csv"))


## PLOT: coverage-based curves per group 
# figure panels
out_abu_grp <- iNEXT::iNEXT(lists_by_grp$abu, q = 0, datatype = "abundance", se = TRUE, 
                            conf = 0.95, nboot = 200) # 10) #

ng <- length(unique(out_abu_grp$iNextEst$size_based$Assemblage))

gA <- iNEXT::ggiNEXT(out_abu_grp, type = 3, se = TRUE, color.var = "Assemblage") + 
  ggplot2::scale_shape_manual(values = rep(16, max(ng, 8))) +  # all circles
  ggplot2::guides(shape = "none") +                            # hide shape legend
  ggplot2::labs(title = "q=0 vs Coverage (abundance, by site×year)") + 
  ggplot2::coord_cartesian(xlim = c(0, 1.01))  # avoid “removed rows” when SC > 1 by rounding

print(gA)

# save plot
# ggplot2::ggsave(file.path(out_dir_fig, "figure_inext_abundance_q0_vs_coverage_by_group.png"), gA, width = 8, height = 6, dpi = 300)


###################################################
#### COVERAGE-BASED, ALL SITES, INCIDENCE DATA ####
###################################################
## 1) Build the incidence-frequency vector for the pooled assemblage
T_units <- nrow(X_counts)                               # number of transects (sampling units)
inc_counts <- colSums(X_counts > 0, na.rm = TRUE)       # species incidence across transects
inc_counts <- inc_counts[inc_counts > 0]                 # drop zero-incidence species (recommended)
inext_inc_list <- list(All = c(T_units, inc_counts))

## 2) Coverage of the observed pooled incidence sample
info_inc <- iNEXT::DataInfo(inext_inc_list, datatype = "incidence_freq")

print(info_inc)

# save table
# readr::write_csv(info_inc, file.path(out_dir_tab, "table_inext_incidence_DataInfo_all.csv"))

## 3) Point estimate at target coverage (match abundance target for comparability)
targetC_inc <- min(0.95, as.numeric(info_inc$SC))
est_inc_cov <- iNEXT::estimateD(
  inext_inc_list, q = 0, base = "coverage", level = targetC_inc,
  conf = 0.95, datatype = "incidence_freq"
)

print(est_inc_cov)
# Assemblage        t      Method Order.q   SC       qD   qD.LCL   qD.UCL
# All 20.24768 Rarefaction       0 0.95 228.7524 221.6032 235.9015

# save table
# readr::write_csv(est_inc_cov, file.path(out_dir_tab, "table_inext_incidence_estimateD_q0_coverage_all.csv"))

## 4) Full iNEXT object + quick plot
out_inc <- iNEXT::iNEXT(inext_inc_list, q = 0, datatype = "incidence_freq",
                        se = TRUE, conf = 0.95, nboot = 200) # 10) #


## PLOT
g2 <- iNEXT::ggiNEXT(out_inc, type = 3) + ggplot2::labs(title = "q=0 vs Coverage (incidence pooled)")

print(g2)


## Style pooled coverage-based rarefaction/extrapolation (q = 0) — INCIDENCE
g_pooled_inc <- iNEXT::ggiNEXT(out_inc, type = 3, se = TRUE, color.var = "Assemblage") +
  scale_color_manual(values = c(All = "black"), guide = "none") +
  scale_fill_manual(values  = c(All = "black"), guide = "none") +   # CI ribbon black
  scale_shape_manual(values = 16, guide = "none") +                 # circles
  guides(linetype = "none", size = "none") +
  coord_cartesian(xlim = c(0, 1.01)) +
  scale_y_continuous(limits = c(0, 350),
                     breaks = seq(0, 350, 50),
                     expand = expansion(add = c(10, 10))) +           # small buffer below 0
  labs(title = NULL,
       x = "Sample coverage (Ĉ)",
       y = "Species richness (q = 0)") +
  theme_classic(base_size = 12) +
  theme(
    text = element_text(family = "Times New Roman"),
    legend.position = "none",
    plot.title = element_blank()
  )

print(g_pooled_inc)

# Save
ggsave(file.path(out_dir_fig, "figure_inext_incidence_q0_vs_coverage_all.png"),
       g_pooled_inc, width = 6, height = 8, dpi = 300)


### COMBINE POOLED ABUNDANCE AND INCIDENCE PLOTS INTO ONE FIGURE
# 1 col, 2 rows; labels A/B; vertically aligned
panel_pooled <- cowplot::plot_grid(
  g_pooled_abu,          # (A) pooled abundance
  g_pooled_inc,          # (B) pooled incidence
  labels = c("A", "B"),
  label_size = 12,
  ncol = 1, nrow = 2,
  align = "v", axis = "lr",
  rel_heights = c(1, 1)
)

# View
print(panel_pooled)

# Save (PNG + optional PDF)
ggsave(file.path(out_dir_fig, "figure_inext_abu_inc_q0_vs_coverage_pooled_1x2.png"),
       panel_pooled, width = 6, height = 8, dpi = 300)

# ggsave(file.path(out_dir_fig, "figure_inext_q0_vs_coverage_pooled_1x2.pdf"),
#        panel_pooled, width = 6.5, height = 9)



#################################################
#### COVERAGE-BASED, BY SITE, INCIDENCE DATA ####
#################################################
info_inc_grp <- iNEXT::DataInfo(lists_by_grp$inc, datatype = "incidence_freq")

print(info_inc_grp)

# save table
# readr::write_csv(info_inc_grp, file.path(out_dir_tab, "table_inext_incidence_DataInfo_by_group.csv"))

## Choose a common target coverage across groups (cap at 0.95)
targetC_groups <- min(0.95, min(info_abu_grp$SC, na.rm = TRUE), min(info_inc_grp$SC, na.rm = TRUE))

## Coverage-standardized richness (q=0) with 95% CIs
est_inc_grp <- iNEXT::estimateD(lists_by_grp$inc, q = 0, base = "coverage",
                                level = targetC_groups, conf = 0.95, datatype = "incidence_freq")

print(est_inc_grp)

# save table
# readr::write_csv(est_inc_grp, file.path(out_dir_tab, "table_inext_incidence_estimateD_q0_coverage_by_group.csv"))

## PLOT: coverage-based curves per group
# figure panels
out_inc_grp <- iNEXT::iNEXT(lists_by_grp$inc, q = 0, datatype = "incidence_freq", se = TRUE, conf = 0.95, nboot = 200)
gB <- iNEXT::ggiNEXT(out_inc_grp, type = 3) + ggplot2::labs(title = "q=0 vs Coverage (incidence, by site×year)")

print(gB)

# save plot
ggplot2::ggsave(file.path(out_dir_fig, "figure_inext_incidence_q0_vs_coverage_by_group.png"), gB, width = 8, height = 6, dpi = 300)




### ATTEMPT ###
#### PLOT INCIDENCE BY SITE ####

# VVVV #
## ---- Helper: build a two-assemblage incidence list for one site -------------
make_inc_site_list <- function(inc_all, site_label) {
  # expected names inside inc_all:
  #   paste0("1978 ", site_label)  and  paste0("2011 ", site_label)
  want <- paste(rep(c("1978","2011"), each = 1), site_label)
  miss <- setdiff(want, names(inc_all))
  if (length(miss)) {
    stop("Missing incidence vectors for: ", paste(miss, collapse = ", "),
         ". Check names in lists_by_grp$inc.")
  }
  inc_all[want]
}

## ---- Build four iNEXT objects (incidence_freq) -------------------------------
# Site labels must match those used when you created `lists_by_grp`
site_labels <- c("Buyong", "Olango", "Sumilon West", "Sumilon East")

inc_buyong       <- make_inc_site_list(lists_by_grp$inc, "Buyong")
inc_olango       <- make_inc_site_list(lists_by_grp$inc, "Olango")
inc_sumilon_west <- make_inc_site_list(lists_by_grp$inc, "Sumilon West")
inc_sumilon_east <- make_inc_site_list(lists_by_grp$inc, "Sumilon East")

# iNEXT for q = 0 (richness), with CIs
out_inc_buyong       <- iNEXT(inc_buyong,       q = 0, datatype = "incidence_freq", se = TRUE, conf = 0.95, nboot = 200)
out_inc_olango       <- iNEXT(inc_olango,       q = 0, datatype = "incidence_freq", se = TRUE, conf = 0.95, nboot = 200)
out_inc_sumilon_west <- iNEXT(inc_sumilon_west, q = 0, datatype = "incidence_freq", se = TRUE, conf = 0.95, nboot = 200)
out_inc_sumilon_east <- iNEXT(inc_sumilon_east, q = 0, datatype = "incidence_freq", se = TRUE, conf = 0.95, nboot = 200)

## ---- (Optional) quick sanity checks -----------------------------------------
# DataInfo per site (should show correct T and SC per assemblage)
print(iNEXT::DataInfo(inc_buyong,       "incidence_freq"))
print(iNEXT::DataInfo(inc_olango,       "incidence_freq"))
print(iNEXT::DataInfo(inc_sumilon_west, "incidence_freq"))
print(iNEXT::DataInfo(inc_sumilon_east, "incidence_freq"))

## Helper: run iNEXT for a single site (incidence_freq)
run_inext_inc_site <- function(site_label_pretty, inc_list) {
  keys <- c(paste0("1978 ", site_label_pretty), paste0("2011 ", site_label_pretty))
  stopifnot(all(keys %in% names(inc_list)))
  iNEXT::iNEXT(inc_list[keys], q = 0, datatype = "incidence_freq",
               se = TRUE, conf = 0.95, nboot = 200)
}

## Per-site incidence iNEXT objects
out_inc_buyong       <- run_inext_inc_site("Buyong",       lists_by_grp$inc)
out_inc_olango       <- run_inext_inc_site("Olango",       lists_by_grp$inc)
out_inc_sumilon_west <- run_inext_inc_site("Sumilon West", lists_by_grp$inc)
out_inc_sumilon_east <- run_inext_inc_site("Sumilon East", lists_by_grp$inc)

## Helper to style a single site’s coverage-based curve (q = 0)
style_inext_inc_noleg <- function(out_obj, title_txt) {
  iNEXT::ggiNEXT(out_obj, type = 3, se = TRUE, color.var = "Assemblage") +
    scale_color_manual(values = yr_cols, breaks = names(yr_cols), guide = "none") +
    scale_shape_manual(values = rep(16, 8), guide = "none") +  # all circles
    guides(color = "none", shape = "none", linetype = "none", fill = "none") +
    coord_cartesian(xlim = c(0, 1.01)) +
    labs(title = title_txt, x = "Sample coverage (Ĉ)", y = "Species richness (q = 0)") +
    theme_classic(base_size = 12) +
    theme(
      text = element_text(family = "Times New Roman"),
      legend.position = "none",
      plot.title = element_blank()
    )
}

## ── Individual plots ─────────────────────────────────────────────────────────
## Individual plots
g_inc_mactan  <- style_inext_inc_noleg(out_inc_buyong,       "Mactan: q = 0 vs coverage")
g_inc_olango  <- style_inext_inc_noleg(out_inc_olango,       "Olango: q = 0 vs coverage")
g_inc_sum_w   <- style_inext_inc_noleg(out_inc_sumilon_west, "Sumilon West: q = 0 vs coverage")
g_inc_sum_e   <- style_inext_inc_noleg(out_inc_sumilon_east, "Sumilon East: q = 0 vs coverage")

## ── Print Individual plots ─────────────────────────────────────────────────────────
print(g_inc_mactan)
print(g_inc_olango)
print(g_inc_sum_w)
print(g_inc_sum_e)

## ── Save each plot ───────────────────────────────────────────────────────────
ggsave(file.path(out_dir_fig, "figure_inext_incidence_q0_vs_coverage_mactan_leg.png"),
       g_inc_mactan, width = 6, height = 8, dpi = 300)
ggsave(file.path(out_dir_fig, "figure_inext_incidence_q0_vs_coverage_olango_leg.png"),
       g_inc_olango, width = 6, height = 8, dpi = 300)
ggsave(file.path(out_dir_fig, "figure_inext_incidence_q0_vs_coverage_sumilon_west_leg.png"),
       g_inc_sum_w, width = 6, height = 8, dpi = 300)
ggsave(file.path(out_dir_fig, "figure_inext_incidence_q0_vs_coverage_sumilon_east_leg.png"),
       g_inc_sum_e, width = 6, height = 8, dpi = 300)


## NO LEGEND, STD Y-AXIS: Helper to style a single site’s coverage-based curve (q = 0)
style_inext_inc_noleg <- function(out_obj) {
  iNEXT::ggiNEXT(out_obj, type = 3, se = TRUE, color.var = "Assemblage") +
    scale_color_manual(values = yr_cols, breaks = names(yr_cols), guide = "none") +
    scale_shape_manual(values = rep(16, 8), guide = "none") +  # all circles
    guides(color = "none", shape = "none", linetype = "none", fill = "none") +
    coord_cartesian(xlim = c(0, 1.01)) +
    scale_y_continuous(
      limits = c(0, 250),
      breaks = seq(0, 250, 50),
      expand = expansion(add = c(10,0))   # small buffer below 0
    ) +
    labs(title = NULL, x = "Sample coverage (Ĉ)", y = "Species richness (q = 0)") +
    theme_classic(base_size = 12) +
    theme(
      text = element_text(family = "Times New Roman"),
      legend.position = "none",
      plot.title = element_blank()
    )
}

## Individual plots
g_inc_mactan  <- style_inext_inc_noleg(out_inc_buyong)
g_inc_olango  <- style_inext_inc_noleg(out_inc_olango)
g_inc_sum_w   <- style_inext_inc_noleg(out_inc_sumilon_west)
g_inc_sum_e   <- style_inext_inc_noleg(out_inc_sumilon_east)

## Print (optional)
print(g_inc_mactan); print(g_inc_olango); print(g_inc_sum_w); print(g_inc_sum_e)

## Save each figure
ggsave(file.path(out_dir_fig, "figure_inext_incidence_q0_vs_coverage_mactan.png"),
       g_inc_mactan, width = 6, height = 8, dpi = 300)
ggsave(file.path(out_dir_fig, "figure_inext_incidence_q0_vs_coverage_olango.png"),
       g_inc_olango, width = 6, height = 8, dpi = 300)
ggsave(file.path(out_dir_fig, "figure_inext_incidence_q0_vs_coverage_sumilon_west.png"),
       g_inc_sum_w, width = 6, height = 8, dpi = 300)
ggsave(file.path(out_dir_fig, "figure_inext_incidence_q0_vs_coverage_sumilon_east.png"),
       g_inc_sum_e, width = 6, height = 8, dpi = 300)


## Individual plots
g_inc_mactan  <- style_inext_inc_noleg(out_inc_buyong)
g_inc_olango  <- style_inext_inc_noleg(out_inc_olango)
g_inc_sum_w   <- style_inext_inc_noleg(out_inc_sumilon_west)
g_inc_sum_e   <- style_inext_inc_noleg(out_inc_sumilon_east)

## Print (optional)
print(g_inc_mactan); print(g_inc_olango); print(g_inc_sum_w); print(g_inc_sum_e)

## Save each figure
ggsave(file.path(out_dir_fig, "figure_inext_incidence_q0_vs_coverage_mactan.png"),
       g_inc_mactan, width = 8, height = 6, dpi = 300)
ggsave(file.path(out_dir_fig, "figure_inext_incidence_q0_vs_coverage_olango.png"),
       g_inc_olango, width = 8, height = 6, dpi = 300)
ggsave(file.path(out_dir_fig, "figure_inext_incidence_q0_vs_coverage_sumilon_west.png"),
       g_inc_sum_w, width = 8, height = 6, dpi = 300)
ggsave(file.path(out_dir_fig, "figure_inext_incidence_q0_vs_coverage_sumilon_east.png"),
       g_inc_sum_e, width = 8, height = 6, dpi = 300)


##CREATE 2x2 PANEL FIGURE

panel_inc_2x2 <- plot_grid(
  g_inc_mactan, g_inc_olango,
  g_inc_sum_w,  g_inc_sum_e,
  labels = c("A", "B", "C", "D"),
  label_size = 12, ncol = 2, align = "hv"
)

print(panel_inc_2x2)

ggsave(file.path(out_dir_fig, "figure_inext_incidence_q0_vs_coverage_2x2.png"),
       panel_inc_2x2, width = 6, height = 8, dpi = 300)

# ^^^^ # 
### ATTEMPT ### 
