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

# check that the rows are aligned by station_code
identical(rownames(data_vegan), data_vegan.env$station_code)


#### TROPHIC LEVEL ####
# get all the unique species names in your community matrix
specs <- colnames(data_vegan) %>%
  gsub("_", " ", .)

# change first character to uppercase (Genus species) so that rfishbase can recognize it. 
specs <- str_to_upper(str_sub(specs, 1, 1)) %>% str_c(str_sub(specs, 2))

# Keep only "Genus species" (two words)
is_binomial <- str_count(specs, "\\S+") == 2
specs_binomials <- specs[is_binomial]

# fetch trophic trait data
traits <- ecology(specs_binomials,
                  fields = c("Species", "DietTroph", "DietSeTroph", "FoodTroph", "FoodSeTroph"))

print(traits)
cat(sum(specs_binomials %in% traits$Species), "of", length(specs_binomials), "species matched in FishBase.\n")
# 638 of 828 species matched in FishBase of 957 total species (including species complexes)

# build a consensus trophic level
traits <- traits %>%
  transmute(
    Species,
    TrophicLevel = coalesce(DietTroph, FoodTroph),
    TL_SE        = coalesce(DietSeTroph, FoodSeTroph),
    Source       = if_else(!is.na(DietTroph), "DietTroph", "FoodTroph")
  ) %>%
  arrange(desc(TrophicLevel))


# Prioritizing FoodTroph over DietTroph did not make the outcome significant.
# traits <- traits %>%
#   select("Species", "FoodTroph", "FoodSeTroph", "DietTroph", "DietSeTroph")
# 
# traits <- traits %>%
#   transmute(
#     Species,
#     TrophicLevel = coalesce(FoodTroph, DietTroph),
#     TL_SE        = coalesce(FoodSeTroph, DietSeTroph),
#     Source       = if_else(!is.na(FoodTroph), "FoodTroph", "DietTroph")
#   ) %>%
#   arrange(desc(TrophicLevel))


# Add a column to match original names
traits <- traits %>%
  mutate(spec_col = gsub(" ", "_", Species)) 

# mutate to lowercase  
traits <- traits %>%
  mutate(spec_col = str_to_lower(spec_col))

# Only keep species that are in your vegan data
traits <- traits %>%
  filter(spec_col %in% colnames(data_vegan))

# Only use columns present in traits$spec_col
data_vegan_trait <- data_vegan[, colnames(data_vegan) %in% traits$spec_col]

# Reorder traits to match vegan columns
traits_matched <- traits[match(colnames(data_vegan_trait), traits$spec_col), ]
# Now traits_matched$TrophicLevel matches columns in data_vegan_trait

# Compute CWM per site: sum(p_i * trait_i)
site_totals <- rowSums(data_vegan_trait)
# Community matrix divided by row sums = proportional abundance matrix
prop_matrix <- sweep(data_vegan_trait, 1, site_totals, FUN = "/")

# Multiply each species' relative abundance by its trophic level
cwm_troph <- rowSums(prop_matrix * traits_matched$TrophicLevel, na.rm = TRUE)

# add cwm to metadata
data_vegan.env$CWM_TrophicLevel <- cwm_troph


#### CWM PLOTS ####
## BOXPLOT Basic 
ggplot(data_vegan.env, aes(x = sea, y = CWM_TrophicLevel, fill = study)) +
  geom_boxplot() +
  theme_classic() +
  labs(y = "Trophic Level (Community Weighted Mean)")

## BOXPLOT Formatted
# 1. Clean up sea categories and study color variable
data_vegan.env <- data_vegan.env %>%
  mutate(
    sea = dplyr::recode(sea, "bohol" = "Bohol Sea", "sulu" = "Sulu Sea"),
    study = factor(study, levels = c("si_1978", "su_2022"),
                   labels = c("SI 1978/79", "SU 2019/22"))
  )

# 2. Set study colors
study_colors <- c("SI 1978/79" = "#0072CE", "SU 2019/22" = "#800000")

# 3. Boxplot
p <- ggplot(data_vegan.env, aes(x = sea, y = CWM_TrophicLevel, fill = study)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.8) +
  scale_fill_manual(values = study_colors, name = "Survey") +
  scale_y_continuous(
    limits = c(0, 4),
    breaks = seq(0, 4, by = 0.5)
  ) +
  labs(x = NULL, y = "Trophic Level (Community Weighted Mean )") +
  theme_classic(base_family = "Times New Roman", base_size = 12) +
  theme(legend.position = "none")

print(p)

## VIOLIN PLOT
p <- ggplot(data_vegan.env, aes(x = sea, y = CWM_TrophicLevel, fill = study)) +
  geom_violin(alpha = 0.8, trim = FALSE) +  # violin instead of boxplot
  scale_fill_manual(values = study_colors, name = "Survey") +
  scale_y_continuous(
    limits = c(0, 4),
    breaks = seq(0, 4, by = 0.5)
  ) +
  labs(x = NULL, y = "Trophic Level (Community Weighted Mean)") +
  theme_classic(base_family = "Times New Roman", base_size = 12) +
  theme(legend.position = "none")  +
# overlay median points
# stat_summary(fun = "median", geom = "point", shape = 95, size = 3, color = "black", position = position_dodge(width = 0.9))
# overlay mean points
stat_summary(fun = "mean", geom = "point", shape = 95, size = 3, color = "black", position = position_dodge(width = 0.9))
# overlay boxplot
# geom_boxplot(width = 0.1, outlier.shape = NA, alpha = 0.4, position = position_dodge(0.9))

print(p)

# 4. Save
# ggsave("../figures/si_su_duplicates/trophic_species_cwm_violinplot.png",
#        plot = p,
#        width = 6.5,
#        height = 8,
#        units = "in",
#        dpi = 300)


#### ANOVA & FIXED EFFECTS #### 
# rename for readability of downstream output
data_vegan.env <- data_vegan.env %>%
  dplyr::mutate(
    study = dplyr::recode(
      study,
      "SI 1978/79" = "historical",
      "SU 2019/22" = "modern"
    ))
data_vegan.env <- data_vegan.env %>%
  dplyr::mutate(
    sea = dplyr::recode(
      sea,
      "Bohol Sea" = "bohol",
      "Sulu Sea"  = "sulu"
    ))

# lmer model as with diversity metrics (station_pair = random effect)
mod_CWM <- lmer(CWM_TrophicLevel ~ study * sea + (1 | station_pair), data = data_vegan.env)
summary(mod_CWM) # Type I. Might not be as appropriate as type III. ANOVA Type III was used for the same lmer formula for the alpha diversity metrics
anova_CWM <- car::Anova(mod_CWM, type = "III")
anova_CWM

# ANOVA: Model term tests. Extract wald statistics (chi-sq, df, p-value) from anova_CWM
anova_CWM_df <- as.data.frame(anova_CWM)
anova_CWM_df$term <- rownames(anova_CWM_df)
anova_CWM_df <- dplyr::select(anova_CWM_df, term, Chisq, Df, `Pr(>Chisq)`)
colnames(anova_CWM_df) <- c("term", "chi_square", "df", "p_value")

# Print table
print(anova_CWM_df)

# save file as table. 
# outdir <- "../tables/si_su_duplicates"
# outfile <- file.path(outdir, "table_trophic_species_cwm_anova.csv")
# write_csv(anova_CWM_df, outfile)


# Fixed Effects: Extract the estimates and standard errors
fixef_CWM <- tibble::tibble(
  term     = names(fixef(mod_CWM)),
  estimate = fixef(mod_CWM),
  std_error = sqrt(diag(vcov(mod_CWM)))
)

# Fixed Effects: Calculate 95% confidence intervals
conf <- as.data.frame(confint(mod_CWM, parm = "beta_", level = 0.95))
conf$term <- rownames(conf)
colnames(conf) <- c("conf_low", "conf_high", "term")

# Fixed Effects: Join together
CWM_params <- dplyr::left_join(fixef_CWM, conf, by = "term") %>%
  dplyr::select(term, estimate, std_error, conf_low, conf_high)

# Print table
print(CWM_params)

# save file as table. 
outdir <- "../tables/si_su_duplicates"
outfile <- file.path(outdir, "table_trophic_species_cwm_fixef.csv")
write_csv(CWM_params, outfile)
