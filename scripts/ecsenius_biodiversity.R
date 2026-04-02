# ============================================================
# Ecsenius spatiotemporal analysis using metadata
# Non-paired methods only
# ============================================================

library(tidyverse)
library(vegan)
library(tibble)
library(stringr)

# ----------------------------
# 1. Read data
# ----------------------------

output_dir <- "C:/projects/fish_biodiversity/results/si_su_duplicates/ecsenius"
data_dir  <- "C:/projects/fish_biodiversity/data/si_su_duplicates"
file_name <- "data_vegan_si_su_duplicates_community_matrix_ecsenius.csv"
metadata_file_name <- "data_vegan_si_su_duplicates_metadata.csv"

metadata <- read.csv(file.path(data_dir, metadata_file_name), check.names = FALSE)
dat <- read.csv(file.path(data_dir, file_name), check.names = FALSE)

# Move first column to row names, then remove it
rownames(dat) <- dat[[1]]
dat <- dat[-1]

# Convert row names back to a column for joining
comm <- dat %>%
  rownames_to_column("station_code")

# Species columns are all columns in the community matrix after station_code
species_cols <- setdiff(names(comm), "station_code")

# Make sure species columns are numeric
comm[species_cols] <- lapply(comm[species_cols], as.numeric)

# ----------------------------
# 2. Prepare metadata
# ----------------------------

metadata <- metadata %>%
  mutate(
    date_collected_chr = as.character(date_collected),
    year = suppressWarnings(as.integer(str_extract(date_collected_chr, "(19|20)\\d{2}"))),
    era = case_when(
      year %in% c(1978, 1979) ~ "historical",
      year %in% c(2019, 2022) ~ "modern",
      TRUE ~ NA_character_
    ),
    era = factor(era, levels = c("historical", "modern")),
    sea = factor(sea),
    province = factor(province),
    municipality = factor(municipality),
    study = factor(study),
    study_sea = factor(study_sea)
  )

# ----------------------------
# 3. Join metadata + community matrix
# ----------------------------

ecs <- metadata %>%
  left_join(comm, by = "station_code")

# Check for unmatched stations
missing_in_metadata <- setdiff(comm$station_code, metadata$station_code)
if (length(missing_in_metadata) > 0) {
  warning("These station_code values were in the community matrix but not in metadata:\n",
          paste(missing_in_metadata, collapse = ", "))
}

# ----------------------------
# 4. Sample-level metrics
# ----------------------------

ecs <- ecs %>%
  mutate(
    ecsenius_abundance = rowSums(across(all_of(species_cols)), na.rm = TRUE),
    ecsenius_richness  = rowSums(across(all_of(species_cols), ~ .x > 0), na.rm = TRUE),
    ecsenius_present   = ecsenius_abundance > 0
  )

# ----------------------------
# 5. Summary tables
# ----------------------------

summarize_ecsenius <- function(df, group_vars) {
  df %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(
      total_stations = n(),
      stations_with_ecsenius = sum(ecsenius_present, na.rm = TRUE),
      total_individuals = sum(ecsenius_abundance, na.rm = TRUE),
      mean_abundance = mean(ecsenius_abundance, na.rm = TRUE),
      sd_abundance = sd(ecsenius_abundance, na.rm = TRUE),
      mean_richness = mean(ecsenius_richness, na.rm = TRUE),
      sd_richness = sd(ecsenius_richness, na.rm = TRUE),
      total_species = sum(colSums(pick(all_of(species_cols)), na.rm = TRUE) > 0),
      .groups = "drop"
    )
}

# Overall by era
table_era <- summarize_ecsenius(ecs, "era")

# By era and sea
table_era_sea <- summarize_ecsenius(ecs, c("era", "sea"))

# By era and province
table_era_province <- summarize_ecsenius(ecs, c("era", "province"))

print(table_era)
print(table_era_sea)
print(table_era_province)



# ----------------------------
# 6. Species count tables
# ----------------------------

species_counts_tidy <- ecs %>%
  select(station_code, era, sea, province, all_of(species_cols)) %>%
  pivot_longer(
    cols = all_of(species_cols),
    names_to = "species",
    values_to = "abundance"
  ) %>%
  group_by(era, sea, species) %>%
  summarise(
    total_individuals = sum(abundance, na.rm = TRUE),
    stations_present = sum(abundance > 0, na.rm = TRUE),
    .groups = "drop"
  )

# Wide table like your draft by era/sea
species_counts_era_sea_wide <- species_counts_tidy %>%
  mutate(
    sea_short = case_when(
      str_detect(str_to_lower(as.character(sea)), "sulu")  ~ "sulu",
      str_detect(str_to_lower(as.character(sea)), "bohol") ~ "bohol",
      TRUE ~ str_replace_all(str_to_lower(as.character(sea)), "[^a-z0-9]+", "_")
    ),
    group = paste0(ifelse(era == "historical", "his", "mod"), "_", sea_short)
  ) %>%
  select(group, species, total_individuals) %>%
  pivot_wider(
    names_from = species,
    values_from = total_individuals,
    values_fill = 0
  ) %>%
  arrange(group)

print(species_counts_era_sea_wide)

# ----------------------------
# 7. Non-paired tests for richness and abundance
# ----------------------------

# Overall historical vs modern
richness_test <- wilcox.test(ecsenius_richness ~ era, data = ecs, exact = FALSE)
abundance_test <- wilcox.test(ecsenius_abundance ~ era, data = ecs, exact = FALSE)

cat("\nOverall Wilcoxon test: richness ~ era\n")
print(richness_test)
# Significant difference in Richness by Era
# W = 310, p-value = 0.01142
# alternative hypothesis: true location shift is not equal to 0

cat("\nOverall Wilcoxon test: abundance ~ era\n")
print(abundance_test)
# Significant difference in Abundance by Era
# W = 315.5, p-value = 0.008351
# alternative hypothesis: true location shift is not equal to 0

# Presence / absence of any Ecsenius by era
presence_tab <- table(ecs$era, ecs$ecsenius_present)
presence_test <- fisher.test(presence_tab)

cat("\nFisher's exact test: Ecsenius present vs absent by era\n")
print(presence_test)
# Significant difference in presence/absence by Era
# Fisher's Exact Test for Count Data
# data:  presence_tab
# p-value = 0.02778
# alternative hypothesis: true odds ratio is not equal to 1
# 95 percent confidence interval:
#  0.0401665 0.8632002
# sample estimates:
# odds ratio 
#  0.2008495 


# Optional: run the same unpaired tests within each sea
tests_by_sea <- ecs %>%
  filter(!is.na(era), !is.na(sea)) %>%
  group_split(sea) %>%
  lapply(function(x) {
    if (n_distinct(x$era) < 2) return(NULL)
    
    data.frame(
      sea = as.character(unique(x$sea)),
      n_historical = sum(x$era == "historical"),
      n_modern = sum(x$era == "modern"),
      richness_p = wilcox.test(ecsenius_richness ~ era, data = x, exact = FALSE)$p.value,
      abundance_p = wilcox.test(ecsenius_abundance ~ era, data = x, exact = FALSE)$p.value,
      presence_p = fisher.test(table(x$era, x$ecsenius_present))$p.value
    )
  }) %>%
  bind_rows()

cat("\nUnpaired tests within sea\n")
print(tests_by_sea)
# sea n_historical n_modern richness_p abundance_p  presence_p
# 1 bohol           13       13 0.00104966 0.001232414 0.001647597
# 2  sulu            8        8 0.50445840 0.449999876 1.000000000

# ----------------------------
# 8. Community composition
# ----------------------------
# IMPORTANT:
# Bray-Curtis / PERMANOVA / NMDS cannot use all-zero samples.
# Keep all stations for univariate summaries above,
# but remove zero-abundance rows here.

ecs_comp <- ecs %>%
  filter(ecsenius_abundance > 0, !is.na(era), !is.na(sea))

if (nrow(ecs_comp) >= 3) {
  
  comm_comp <- as.matrix(ecs_comp[, species_cols, drop = FALSE])
  rownames(comm_comp) <- ecs_comp$station_code
  
  comm_comp_sqrt <- sqrt(comm_comp)
  bray <- vegdist(comm_comp_sqrt, method = "bray")
  
  # Non-paired PERMANOVA by era
  perm_era <- adonis2(bray ~ era, data = ecs_comp, permutations = 999)
  
  # Non-paired PERMANOVA by sea
  perm_sea <- adonis2(bray ~ sea, data = ecs_comp, permutations = 999)
  
  cat("\nPERMANOVA: community composition ~ era\n")
  print(perm_era)
  
  cat("\nPERMANOVA: community composition ~ sea\n")
  print(perm_sea)
  
  # Dispersion tests
  disp_era <- betadisper(bray, ecs_comp$era)
  disp_era_test <- permutest(disp_era, permutations = 999)
  
  cat("\nDispersion test by era\n")
  print(disp_era_test)
  
  # NMDS
  set.seed(123)
  nmds <- metaMDS(comm_comp_sqrt, distance = "bray", k = 2, trymax = 200, autotransform = FALSE)
  
  nmds_scores <- as.data.frame(scores(nmds, display = "sites")) %>%
    bind_cols(ecs_comp %>% select(station_code, era, sea, province))
  
  p_nmds <- ggplot(nmds_scores, aes(NMDS1, NMDS2, color = era, shape = sea)) +
    geom_point(size = 3) +
    theme_classic() +
    labs(
      title = "Ecsenius composition across stations",
      subtitle = "Only stations with Ecsenius > 0 included in ordination",
      color = "Era",
      shape = "Sea"
    )
  
  print(p_nmds)
  
  ggsave(
    filename = file.path(data_dir, "ecsenius_nmds_nonpaired.png"),
    plot = p_nmds,
    width = 7,
    height = 5,
    dpi = 300
  )
  
} else {
  cat("\nNot enough non-zero stations for Bray-Curtis / PERMANOVA / NMDS.\n")
}

# Run 196 stress 9.631622e-05 
# ... Procrustes: rmse 0.104007  max resid 0.1658668 
# Run 197 stress 0.0009007759 
# Run 198 stress 0.004530575 
# Run 199 stress 0.001467428 
# Run 200 stress 0.0007798103 
# *** Best solution was not repeated -- monoMDS stopping criteria:
#   183: no. of iterations >= maxit
# 17: stress < smin
# Warning message:
#   In metaMDS(comm_comp_sqrt, distance = "bray", k = 2, trymax = 200,  :
#                stress is (nearly) zero: you may have insufficient data

# ----------------------------
# 9. Save outputs
# ----------------------------

write.csv(ecs,
          file.path(output_dir, "ecsenius_station_level_with_metadata.csv"),
          row.names = FALSE)

write.csv(table_era,
          file.path(output_dir, "ecsenius_summary_by_era.csv"),
          row.names = FALSE)

write.csv(table_era_sea,
          file.path(output_dir, "ecsenius_summary_by_era_sea.csv"),
          row.names = FALSE)

write.csv(table_era_province,
          file.path(output_dir, "ecsenius_summary_by_era_province.csv"),
          row.names = FALSE)

write.csv(species_counts_tidy,
          file.path(output_dir, "ecsenius_species_counts_tidy.csv"),
          row.names = FALSE)

write.csv(species_counts_era_sea_wide,
          file.path(output_dir, "ecsenius_species_counts_by_era_sea_wide.csv"),
          row.names = FALSE)

write.csv(tests_by_sea,
          file.path(output_dir, "ecsenius_unpaired_tests_by_sea.csv"),
          row.names = FALSE)

# Save overall test results to a text file
sink(file.path(output_dir, "ecsenius_nonpaired_test_results.txt"))

cat("Overall Wilcoxon test: richness ~ era\n")
print(richness_test)

cat("\nOverall Wilcoxon test: abundance ~ era\n")
print(abundance_test)

cat("\nFisher's exact test: Ecsenius presence by era\n")
print(presence_test)

cat("\nUnpaired tests within sea\n")
print(tests_by_sea)

if (exists("perm_era")) {
  cat("\nPERMANOVA: community composition ~ era\n")
  print(perm_era)
}

if (exists("perm_sea")) {
  cat("\nPERMANOVA: community composition ~ sea\n")
  print(perm_sea)
}

if (exists("disp_era_test")) {
  cat("\nDispersion test by era\n")
  print(disp_era_test)
}

sink()

cat("\nAnalysis complete.\n")
