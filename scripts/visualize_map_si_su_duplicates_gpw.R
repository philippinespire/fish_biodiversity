## This script can be used to generate maps with human population density data from GPWv4

#### INITIALIZE ####
# set working directory

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#### PACKAGES ####
packages_used <- 
  c("tidyverse",
    "dplyr",
    "raster",
    "sf",
    "Cairo",
    "ggplot2",
    "ggspatial",
    "rnaturalearth",
    "rnaturalearthdata",
    "cowplot",
    "terra",
    "ggspatial"
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

# options(bitmapType = "cairo")  # Set Cairo as the default graphics device


#### READ IN & FORMAT DATA ####

source("wrangle_si_su_data.R")

# habitat filter: 
# Stations SU-22-10_SP-78-19 sampled a lagoon. Stations SU-22-17_SP_78-22 sampled a mangrove.SU station SU-22-21_SP-78-05 experienced a rough sampling environment (current, waves), so should be filtered out.
data_su_metadata <- data_su_metadata %>%
  filter(!station_code %in% c("SU-22-10", "SU-22-17", "SU-22-21"))

count(length(unique(data_su_metadata$station_code)))


# Load the TIFF file of population density in people per square kilometer.
# https://www.earthdata.nasa.gov/data/catalog/sedac-ciesin-sedac-gpwv4-popdens-r11-4.11
gpw_raster <- rast("../data/gpw_v4_population_count_rev11_2020_30_sec.tif")
# Check the raster details (dimensions, resolution, CRS)
gpw_raster

# Load and the Philippine boundary data
# https://github.com/altcoder/philippines-psgc-shapefiles/tree/main
philippines <- st_read("../data/PH_Adm3_MuniCities.shp.shp")
st_crs(philippines)

# Ensure the CRS is in WGS84 (EPSG:4326)
philippines <- st_transform(philippines, crs = 4326)

# Check for geometry validity
invalid_geoms <- philippines[!st_is_valid(philippines), ]

# If there are invalid geometries, fix them
if (nrow(invalid_geoms) > 0) {
  philippines <- st_make_valid(philippines)
}

# Apply a zero-width buffer to clean up geometries
philippines <- st_buffer(philippines, 0)

#### USER-DEFINED AREA OF INTEREST ####

# Set coordinates for area of interest: CEBU STRAIT/CENTAL VISAYAS
lon_min = 116.0 #x_min
lon_max = 127.0 #x_max
lat_min = 4.0 #y_min
lat_max = 20.0 #y_max

# Compute aspect ratio based on the map's geographic extent
lat_range <- lat_max - lat_min  # Difference in latitude
lon_range <- lon_max - lon_min  # Difference in longitude


#### WRANGLE DATA ####

# Define bounding box for Cebu Strait
philippines_coor <- ext(lon_min, lon_max, lat_min, lat_max)

# Crop the raster to this extent
pop_crop <- crop(gpw_raster, philippines)
pop_masked <- mask(pop_crop, philippines)

pop_df <- as.data.frame(pop_masked, xy = TRUE, na.rm = TRUE)
names(pop_df) <- c("lon", "lat", "pop_density")

#### CREATE POP DENSITY BINS ####

# Filter the dataframe based on lon and lat 
pop_philippines_df <- pop_df %>%
  filter(lon >= lon_min, lon <= lon_max, lat >= lat_min, lat <= lat_max)

# Check basic statistics
summary(pop_philippines_df$pop_density)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 0.00    86.99   184.42   398.19   348.06 49570.17 

# Compute quantiles for 5-bin, 6-bin, 7-bin options
quantiles_5bins <- quantile(pop_philippines_df$pop_density, probs = seq(0, 1, length.out = 6))
quantiles_6bins <- quantile(pop_philippines_df$pop_density, probs = seq(0, 1, length.out = 7))
quantiles_7bins <- quantile(pop_philippines_df$pop_density, probs = seq(0, 1, length.out = 8))

# Print quantiles
quantiles_5bins
# 0%          20%          40%          60%          80%         100% 
#   0.00000     20.36521     56.02067    122.48742    282.61389 101473.90625
quantiles_6bins
# 0%    16.66667%    33.33333%          50%    66.66667%    83.33333%         100% 
# 0.00000     16.25375     40.93878     83.30080    158.10896    335.37417 101473.90625 
quantiles_7bins
# 0%    14.28571%    28.57143%    42.85714%    57.14286%    71.42857%    85.71429%         100% 
#   0.00000     13.68381     31.99725     62.99294    109.41577    192.15096    383.75174 101473.90625

# create bin breaks & labels for 5, 6, 7 bins

# create bin breaks & labels for 5, 6, 7 bins
bin_breaks_5 <- c(0, 10, 100, 1000, 10000, 102000)
bin_labels_5 <- c("0 - 10", "10 - 100", "100 - 1,000", "1,000 - 10,000", "10,000 - 102,000")

bin_breaks_6 <- c(0, 10, 50, 300, 1500, 10000, 102000)
bin_labels_6 <- c("0 - 10", "10 - 50", "50 - 300", "300 - 1,500", "1,500 - 10,000", "10,000 - 102,000")

bin_breaks_7 <- c(0, 10, 50, 200, 800, 3000, 15000, 102000)
bin_labels_7 <- c("0 - 10", "10 - 50", "50 - 200", "200 - 800", "800 - 3,000", "3,000 - 15,000", "15,000 - 102,000")

# USED THIS ONE! 3/15/25
# bin_breaks_6 <- c(0, 10, 50, 200, 1000, 5000, Inf)
# bin_labels_6 <- c("0 - 10", "10 - 50", "50 - 200", "200 - 1000", "1000 - 5000", "> 5000")


# Apply 5-bin classification
pop_philippines_df$pop_density_cat_5 <- cut(pop_philippines_df$pop_density, 
                                            breaks = bin_breaks_5, 
                                            labels = bin_labels_5, 
                                            include.lowest = TRUE)

# Apply 6-bin classification
pop_philippines_df$pop_density_cat_6 <- cut(pop_philippines_df$pop_density, 
                                            breaks = bin_breaks_6, 
                                            labels = bin_labels_6, 
                                            include.lowest = TRUE)

# Apply 7-bin classification
pop_philippines_df$pop_density_cat_7 <- cut(pop_philippines_df$pop_density, 
                                            breaks = bin_breaks_7, 
                                            labels = bin_labels_7, 
                                            include.lowest = TRUE)

# Check how many values fall in each bin
table(pop_philippines_df$pop_density_cat_5)
table(pop_philippines_df$pop_density_cat_6)
table(pop_philippines_df$pop_density_cat_7)

# histograms
ggplot(pop_philippines_df, aes(x = pop_density_cat_5, fill = pop_density_cat_5)) +
  geom_bar() +
  scale_fill_viridis_d(name = "Density Bins") +
  labs(title = "Population Density Distribution (5 Bins)",
       x = "Population Density Categories",
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(pop_philippines_df, aes(x = pop_density_cat_6, fill = pop_density_cat_6)) +
  geom_bar() +
  scale_fill_viridis_d(name = "Density Bins") +
  labs(title = "Population Density Distribution (6 Bins)",
       x = "Population Density Categories",
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(pop_philippines_df, aes(x = pop_density_cat_7, fill = pop_density_cat_7)) +
  geom_bar() +
  scale_fill_viridis_d(name = "Density Bins") +
  labs(title = "Population Density Distribution (7 Bins)",
       x = "Population Density Categories",
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#### CREATE COLOR PALETTE ####

# Custom breaks for population density
# pop_breaks <- c(0, 1, 10, 100, 1000, Inf)
# pop_labels <- c("0 - 1", "1 - 10", "10 - 100", "100 - 1,000", "> 1,000")

# 5 bin color palette
pop_colors_5 <- c("#e3c7c6",  # Lighter beige-pink (was #ccaead)
                  "#ffe57f",  # Softer yellow (was #ffdc58)
                  "#f79b74",  # Lighter orange (was #eb804e)
                  "#d14970",  # Less intense red (was #b32957)
                  "#B02F79")

# 6 bin color palette
# pop_colors_6 <- c("#ccaead", "#ffdc58", "#eb804e", "#b32957", "#7d1d53", "#451a40")
pop_colors_6 <- c("#e3c7c6",  # Lighter beige-pink (was #ccaead)
                  "#ffe57f",  # Softer yellow (was #ffdc58)
                  "#f79b74",  # Lighter orange (was #eb804e)
                  "#d14970",  # Less intense red (was #b32957)
                  "#B02F79",  # Brighter purple (was #7d1d53)
                  "#702C60")  # Less dark purple (was #451a40)

# 7 bin color palette
pop_colors_7 <- c("#e3c7c6",  # Lighter beige-pink (was #ccaead)
                  "#ffe57f",  # Softer yellow (was #ffdc58)
                  "#f79b74",  # Lighter orange (was #eb804e)
                  "#d14970",  # Less intense red (was #b32957)
                  "#B02F79",  # Brighter purple (was #7d1d53)
                  "#702C60",
                  "#451a40")
  
# pop_colors_7 <- c("#ccaead", "#ffdc58", "#eb804e", "#b32957", "#7d1d53", "#451a40", "#140e26") # too dark

# Create a bin column that converts population density into categorized factor
pop_df$pop_density_cat_5 <- cut(pop_df$pop_density, breaks = bin_breaks_5, labels = bin_labels_5, include.lowest = TRUE)
pop_df$pop_density_cat_6 <- cut(pop_df$pop_density, breaks = bin_breaks_6, labels = bin_labels_6, include.lowest = TRUE)
pop_df$pop_density_cat_7 <- cut(pop_df$pop_density, breaks = bin_breaks_7, labels = bin_labels_7, include.lowest = TRUE)


#### CREATE CITY COORDINATES & LABELS ####
# CITY: Define coordinates for the cities with individual label adjustments
# City population data taken from the Philippine Statistics Authority (PSA) 2020 census data

city_data <- data.frame(
  city = c("Cebu City", "Mandaue", "Lapu-Lapu", "Iloilo", "Bacolod"),
  lon = c(123.89, 123.94, 124, 122.56, 122.95),
  lat = c(10.32, 10.33, 10.31, 10.72, 10.67),
  pop = c(964169, 364116, 497604, 457626, 600783),
  # Custom vertical and horizontal adjustments
  vjust = c(0.8, -0.4, 1, -0.2, -0.1),  # Negative moves text above, positive moves below
  hjust = c(1.1, -0.1, -0.1, -0.2, -0.1),       # Adjust left (0), center (0.5), or right (1)
  angle = c(0, 0, 0, 0, 0)
)

# Greater than 100k. Needs to be adjusted if used. 
# city_data <- data.frame(
#   city = c("Cebu City", "Mandaue", "Lapu-Lapu", "Ormoc", "Tagbilaran", "Dumaguete", 
#            "Iloilo", "Bacolod", "Talisay", "Toledo", "Bayawan"),
#   lon = c(123.89, 123.94, 124, 124.61, 123.85, 123.31, 
#           122.56, 122.95, 123.8491, 123.6414, 122.8044),
#   lat = c(10.32, 10.33, 10.31, 11, 9.65, 9.31, 
#           10.72, 10.67, 10.2447, 10.3792, 9.3648),
#   pop = c(964169, 364116, 497604, 230998, 104976, 134103, 
#           457626, 600783, 263048, 207314, 122747),
#   # Custom vertical and horizontal adjustments
#   vjust = c(0.8, -0.4, 1, 1.4, 1.4, 0, 
#             -0.2, -0.1, 1.1, -0.5, -0.3),  # Negative moves text above, positive moves below
#   hjust = c(1.1, -0.1, -0.1, 0.7, 0.4, -0.1, 
#             -0.2, -0.1, 1.1, 0.1, 0.8),       # Adjust left (0), center (0.5), or right (1)
#   angle = c(0, 0, 0, 0, 0, 0, 
#             0, 0, 0, 0, 0)
# )

# Create 3 bins for the different Highly Urbanized City with at least 200k & annual income of 50M pesos
bin_breaks_city <- c(300000, 400000, 700000, 1000000)
bin_labels_city <- c("300 - 400k", "400 - 700k", "700 - 1,000k")
# Create 4 bins for the different city sizes
# bin_breaks_city <- c(100000, 200000, 400000, 900000, Inf)
# bin_labels_city <- c("100 - 200k", "200 - 400k", "400 - 900k", "900 - 1,000k")

# Categorize cities based on population bins
city_data$pop_bin <- cut(city_data$pop, breaks = bin_breaks_city, labels = bin_labels_city, include.lowest = TRUE)

# Define corresponding point sizes (1, 2, 3, 4) for each population bin
size_map <- c("300 - 400k" = 1.5, "400 - 700k" = 2, "700 - 1,000k" = 2.5)

# size_map <- c("100 - 200k" = 1, "200 - 400k" = 1.5, "400 - 900k" = 2, "900 - 1,000k" = 2.5)

# Assign point sizes based on the bin each city falls into
city_data$point_size <- size_map[as.character(city_data$pop_bin)]


#### CREATE SEA COORDINATES & LABELS ####
# SEA: Define coordinates for the cities with individual label adjustments
sea_data <- data.frame(
  sea = c("Tañon Strait", "Cebu Strait", "Bohol Sea", "Panay Gulf", "Guimaras Strait", "Visayan Sea", "Camotes Sea", "Sulu Sea"),
  lon = c(123.426, 123.754, 124.239, 122.702, 122.824, 123.611, 124.5, 122.733),
  lat = c(10.344, 9.975, 9.305, 10.274, 10.773, 11.271, 10.590, 9.340),
  # Custom vertical and horizontal adjustments
  vjust = c(1, 0, 1, 1, 0.6, 1.3, 1.3, 2),  # Negative moves text above, positive moves below
  hjust = c(0.5, 0.5, 0.5, 0.5, 0.2, 0.9, 0.8, 0.6),   # Adjust left (0), center (0.5), or right (1)
  angle = c(55, 55, 0, 0, 55, 0, 0, 0)
)


#### PLOT MAP ####

# Decide how many bins to use for the map
num_bins = 7

# Create the map
map_philippines_popdensity <- ggplot() +
  # Raster layer with new color scale
  geom_tile(data = pop_philippines_df, aes(x = lon, y = lat, 
                               fill = get(paste0("pop_density_cat_",num_bins)))) +
  
  scale_fill_manual(
    name = "Population Density\n(persons/km²)",  # Same title as City Population legend
    values = get(paste0("pop_colors_", num_bins)), 
    labels = get(paste0("bin_labels_", num_bins)),  # Ensure labels match color categories
    guide = guide_legend(order = 2)  # Ensures it appears after City Population in the legend
  ) +

  # Administrative boundaries
  geom_sf(data = philippines, fill = NA, color = "black", size = 0.3) +
  
  # === NEW: overlay SU station points ===
  # geom_jitter(
  #   data = data_su_metadata,
  #   aes(x = longitude, y = latitude),
  #   inherit.aes = FALSE,
  #   shape = 21,
  #   fill  = "#0072CE",
  #   color = "black",
  #   size  = 2,
  #   stroke = 0.4
  # ) +
  geom_point(
    data = data_su_metadata,
    aes(x = longitude, y = latitude),
    position = position_jitter(width = 0.04, height = 0.04),
    inherit.aes = FALSE,
    shape   = 21,
    fill    = "#0072CE",
    color   = "black",
    size    = 3,
    stroke  = 0.4
  ) +

  
  # Scale bar (positioned correctly for new plot size)
  annotation_scale(location = "br", 
                   width_hint = 0.22,  # Adjusted for new width
                   height = unit(0.25, "cm"), 
                   text_cex = 1) +  # Slightly increased text size
  
  # Compass (adjusted to fit properly)
  annotation_north_arrow(
    location = "br", which_north = "true",
    pad_x = unit(7, "cm"), pad_y = unit(1.5, "cm"),
    style = north_arrow_fancy_orienteering()
  ) +
  
  # Black outline for the map area
  geom_rect(aes(xmin = lon_min, xmax = lon_max, ymin = lat_min, ymax = lat_max), 
            fill = NA, color = "black", linewidth = 0.8) +  # Black border
  
  # Crop the map
  coord_sf(xlim = c(lon_min, lon_max), ylim = c(lat_min, 20), expand = FALSE) +
  
  # Remove all titles
  labs(x = NULL, y = NULL, title = NULL, subtitle = NULL, caption = NULL) +
  
  # Customize theme for axis ticks, labels, and legend
  theme_classic() +
  theme(
    panel.background = element_rect(fill = "aliceblue", color = NA),  # Light blue background
    panel.grid = element_blank(),  # Remove grid lines
    axis.text = element_text(size = 8),  # Set axis label font size to 10
    axis.ticks.length = unit(0.2, "cm"),  # Make tick marks appear outside
    axis.ticks = element_line(color = "black"),  # Ensure ticks are visible
    legend.position = "right", # c(0.90, 0.25),  # left/right = 0, 1; bottom/top = 0, 1
    legend.box = "vertical",
    legend.spacing.y = (unit(0.5, 'cm')),
    legend.text = element_text(size = 10),  # Set legend font size to 11
    legend.title = element_text(size = 10),  # Ensure title matches legend font size
    legend.background = element_rect(fill = "white"), #, color = "black", linewidth = 0.8),  # Black border around legend
    axis.text.x = element_text(margin = margin(t = 3)),  # Adjust spacing for tick labels
    axis.text.y = element_text(margin = margin(r = 3))
  ) +

  # Set axis breaks every 0.5 degrees
  scale_x_continuous(breaks = seq(116, 126, by = 2.0)) +
  scale_y_continuous(breaks = seq(4, 20, by = 2.0))

# Print the final map
print(map_philippines_popdensity)


#### MAINTAIN ASPECT RATIO ####

# Convert longitude difference to real-world distance
# Approximate scaling: 1° longitude ≈ cos(latitude) * 111 km
mean_latitude <- mean(c(lat_min, lat_max))  # Average latitude of the map
lon_to_km <- cos(mean_latitude * pi / 180) * 111  # Scaling factor

# Aspect ratio (height/width) calculation
aspect_ratio <- (lat_range * 111) / (lon_range * lon_to_km)

# Define fixed width and calculate height to maintain aspect ratio
plot_width <- 6.5  # Inches
plot_height <- plot_width * aspect_ratio  # Maintain correct aspect ratio


#### SAVE MAP ####

# Get current date and replace dashes with underscores
current_date <- gsub("-", "_", Sys.Date())

# Save the plot with fixed dimensions
ggsave(paste0("../figures/si_su_duplicates/map_philippines_popdensity_bin7_", current_date, ".png"), 
       plot = map_philippines_popdensity, 
       width = plot_width, 
       height = plot_height, 
       units = "in", 
       dpi = 300)

