#### NOTES ####
# No good statistical test
# Adapted to just focus on the SU-SI duplicate stations. 
# 24 SU duplicates. But this was filtered down to 21 for habitat and sampling effectiveness.
# It does include 3 proxy stations. 


#### INITIALIZATION ####
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#### INSTALL PACKAGES ####
packages_used <- 
  c("tidyverse",
    "readxl",
    "vegan",
    "ggvegan",
    "ggplot2",
    "dplyr",
    "nlme"
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

# change the rownames in data_vegan to the nice_station_code
# Create a lookup table of old station_code to new nice_station_code
lookup <- data_vegan.env %>%
  dplyr::select(station_code, nice_station_code)

# Reorder the labels to match your data_vegan row order
new_labels <- lookup$nice_station_code[match(rownames(data_vegan), lookup$station_code)]
rownames(data_vegan) <- new_labels

# check that the rows are aligned by nice_station_code
identical(rownames(data_vegan), data_vegan.env$nice_station_code)


#### SPECPOOL GROUP BY STUDY SEA ####
specpool_study_sea <- specpool(data_vegan, data_vegan.env$study_sea, smallsample = TRUE)
# don't subset the dataset by region. don't reduce species pool.

print(specpool_study_sea)

# If needed, turn rownames into a column
specpool_study_sea <- specpool_study_sea %>%
  rownames_to_column("study_sea")

# Gather for plotting
specpool_long <- specpool_study_sea %>%
  pivot_longer(
    cols = c(Species, chao, jack1, jack2, boot),
    names_to = "index",
    values_to = "estimate"
  ) %>%
  # Attach SE for each estimator
  mutate(
    se = case_when(
      index == "chao"  ~ chao.se,
      index == "jack1" ~ jack1.se,
      index == "boot"  ~ boot.se,
      TRUE             ~ NA_real_
    )
  )

# remove redundant se columns
specpool_long <- specpool_long %>%
  select(study_sea, index, estimate, se, n)

print(specpool_long)

# # era/study/survey colors
# label_colors <- c(
#   "si_1978_bohol_sea" = "#024072",  # SI Bohol Sea: navy blue
#   "su_2022_bohol_sea" = "#800000",   # SU Bohol Sea: maroon
#   "si_1978_sulu_sea"  = "#0072CE",  # SI Sulu Sea: blue
#   "su_2022_sulu_sea"  = "#E90303"  # SU Sulu Sea: red
# )

# Human-friendly x labels for the 4 groups
x_labels <- c(
  "si_1978_bohol_sea" = "Historical\nBohol Sea",
  "su_2022_bohol_sea" = "Modern\nBohol Sea",
  "si_1978_sulu_sea"  = "Historical\nSulu Sea",
  "su_2022_sulu_sea"  = "Modern\nSulu Sea"
)

# Dotplot with Error Bars (Recommended)
ggplot(
  filter(specpool_long, index %in% c("Species", "chao", "jack1", "jack2", "boot")),
  aes(x = study_sea, y = estimate, color = study_sea)
) +
  geom_point(position = position_dodge(width = 0.7), size = 3) +
  geom_errorbar(
    aes(ymin = estimate - se, ymax = estimate + se),
    width = 0.2,
    position = position_dodge(width = 0.7),
    na.rm = TRUE
  ) +
  facet_wrap(~ index, scales = "free_y") +
  theme_classic(base_family = "Times New Roman", base_size = 12) +
  theme(legend.position = "bottom", "right") +
  labs(
    y = "Richness Estimate",
    color = "Era - Sea"
  )

# Set factors in desired order
specpool_long <- specpool_long %>%
  mutate(
    study_sea = factor(study_sea, levels = c(
      "si_1978_bohol_sea", "su_2022_bohol_sea",
      "si_1978_sulu_sea", "su_2022_sulu_sea"),
      labels = c("Historical Bohol Sea", "Modern Bohol Sea",
                 "Historical Sulu Sea", "Modern Sulu Sea")
    ),
    index = factor(index, levels = c("Species", "chao", "jack1", "jack2", "boot"),
                   labels = c("Observed S", "Chao", "Jackknife 1", "Jackknife 2", "Bootstrap"))
  )


# Filter first
df_plot <- filter(specpool_long, index %in% c("Observed S", "Chao", "Jackknife 1", "Jackknife 2", "Bootstrap"))

#### PLOT ####
# dot plot with se when available. index and group correctly ordered. 
# free y-axis for each index
ggplot(
  df_plot,
  aes(x = study_sea, y = estimate, color = study_sea)
) +
  geom_point(size = 3) +
  geom_errorbar(
    aes(ymin = estimate - se, ymax = estimate + se),
    width = 0.20,
    na.rm = TRUE
  ) +
  facet_wrap(~ index, nrow = 2, ncol = 3, scales = "free_y") +
  scale_color_manual(
    name = "Survey × Sea",
    values = c(
      "Historical Bohol Sea" = "#024072",
      "Modern Bohol Sea"     = "#800000",
      "Historical Sulu Sea"  = "#0072CE",
      "Modern Sulu Sea"      = "#E90303"
    )
  ) +
  labs(
    y = "Estimated Species Richness",
    x = NULL
  ) +
  theme_classic(base_family = "Times New Roman", base_size = 12) +
  theme(
    axis.text.x  = element_blank(),    # remove x tick labels
    axis.ticks.x = element_blank(),    # remove x ticks
    axis.title.x = element_blank(),    # remove x axis title
    axis.line.x  = element_blank(),
    legend.position = c(0.98, 0.15),   # adjust as needed
    legend.justification = c(1, 0),
    legend.background = element_rect(fill = "white", color = "black"),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold")
  )

# # Save plot
# ggsave("../figures/si_su_duplicates/specpool_species_study_sea_freey.png", width = 6.5, height = 8, units = "in", dpi = 300)

# DOT PLOT with se when available. index and group correctly ordered. 
# set limits and breaks for all y-axes
ggplot(
  df_plot,
  aes(x = study_sea, y = estimate, color = study_sea)
) +
  geom_point(size = 3) +
  geom_errorbar(
    aes(ymin = estimate - se, ymax = estimate + se),
    width = 0.20,
    na.rm = TRUE
  ) +
  facet_wrap(~ index, nrow = 2, ncol = 3, scales = "fixed") +   # or omit 'scales' for default fixed
  scale_color_manual(
    name = "Survey × Sea",
    values = c(
      "Historical Bohol Sea" = "#024072",
      "Modern Bohol Sea"     = "#800000",
      "Historical Sulu Sea"  = "#0072CE",
      "Modern Sulu Sea"      = "#E90303"
    )
  ) +
  scale_y_continuous(
    limits = c(395, 900),
    breaks = seq(400, 900, by = 100)
  ) +
  labs(
    y = "Estimated Species Richness",
    x = NULL
  ) +
  theme_classic(base_family = "Times New Roman", base_size = 12) +
  theme(
    axis.text.x  = element_blank(),    # remove x tick labels
    axis.ticks.x = element_blank(),    # remove x ticks
    axis.title.x = element_blank(),    # remove x axis title
    axis.line.x  = element_blank(),
    legend.position = c(0.98, 0.15),   # adjust as needed
    legend.justification = c(1, 0),
    legend.background = element_rect(fill = "white", color = "black"),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold")
  )

# Save plot
# ggsave("../figures/si_su_duplicates/specpool_species_study_sea_sety.png", width = 6.5, height = 8, units = "in", dpi = 300)
