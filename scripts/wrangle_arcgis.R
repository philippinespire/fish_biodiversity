#### Initialize ####

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tidyverse)
library(janitor)
library(magrittr)
library(measurements)
library(lubridate)
library(rgdal)
library(raster)
library(sf)
library(rgeos)
library(maptools)
library(tmap)

#### USER DEFINED VARIABLES ####
#inFilePath = "../data/SU-SI_Duplicates(1).xlsx"
inZipFilePath = "../data/PhL_Province_Pop.zip"
inDirPath = "../data/PhL_Province_Pop/"
inFilePath = "../data/PhL_Province_Pop.zip"


#### READ IN DATA ####

# this is all xyz coords with no pop info... 
# need to find other data source
unzip(inFilePath)

arcgis <- readOGR(dsn = inDirPath)

#### FIND NeAREST PolyGon #### 
mydf <- structure(list(longitude = c(128.6979, 153.0046, 104.3261, 124.9019, 
                                     126.7328, 153.2439, 142.8673, 152.689), latitude = c(-7.4197, 
                                                                                          -4.7089, -6.7541, 4.7817, 2.1643, -5.65, 23.3882, -5.571)), .Names = c("longitude", 
                                                                                                                                                                 "latitude"), class = "data.frame", row.names = c(NA, -8L))


### Get long and lat from your data.frame. Make sure that the order is in lon/lat.

station_pts <-
  data_cas_si_su %>%
  dplyr::select(study,
                adjusted_longitude,
                adjusted_latitude) %>%
  distinct() %>%
  dplyr::select(-study) %>%
  drop_na() %>%
  as.data.frame()

station_data <-
  data_cas_si_su %>%
  dplyr::select(study,
                adjusted_longitude,
                adjusted_latitude) %>%
  distinct() %>%
  drop_na() %>%
  dplyr::select(-adjusted_longitude,
                -adjusted_latitude) %>%
  as.data.frame()

longlatStr <- "+proj=longlat"
station_pts_longlat <- 
  SpatialPoints(station_pts,
                CRS(longlatStr))

utmStr <- "+proj=utm +zone=%d +datum=NAD83 +units=m +no_defs +ellps=GRS80"
station_pts_utm <- 
  spTransform(station_pts_longlat,
              CRS(sprintf(utmStr, 51)))

arcgis_utm <- 
  spTransform(arcgis,
              CRS(sprintf(utmStr, 51)))

n <- length(station_pts_utm)
nearest_polygon <- character(n)
dist_nearest_polygon <- numeric(n)

## For each point, find name of nearest polygon (in this case, Belgian cantons)
for (i in seq_along(nearest_polygon)) {
  gDists <- gDistance(station_pts_utm[i,], 
                      arcgis_utm, 
                      byid=TRUE)
  nearest_polygon[i] <- arcgis_utm$ISO_SUB[which.min(gDists)]
  dist_nearest_polygon[i] <- min(gDists)
}

nearest_province <- 
  data.frame(nearest_polygon, 
             dist_nearest_polygon)

arcgis_utm_tibble <- 
  fortify(arcgis_utm, 
          region='ISO_SUB') %>%
  mutate(ISO_SUB = str_remove(group,
                              '\\.[0-9]*')) %>%
  left_join(arcgis@data,
            by = "ISO_SUB") %>%
  mutate(TOTPOP_CY = as.numeric(TOTPOP_CY),
         population = as.numeric(population)) %>%
  dplyr::select(-aggregatio)


arcgis_utm_tibble %>%
  ggplot() + 
  aes(x = long, 
      y = lat, 
      group = group,
      fill = population) +
  geom_polygon(color='black') +
  scale_fill_gradient(low = "white",
                      high = "black") +
  geom_point(data = as_tibble(station_pts_utm@coords) %>%
               bind_cols(nearest_province,
                         station_data),
             aes(x=adjusted_longitude,
                 y=adjusted_latitude,
                 color = nearest_polygon,
                 shape = study),
             inherit.aes = FALSE,
             size = 5) +
  coord_quickmap()


plot(station_pts_utm, pch=16, col="red")
text(station_pts_utm, 1:44, pos=3)
plot(arcgis_utm, add=TRUE)
text(arcgis_utm, p$ISO_SUB, cex=0.7)

plot(arcgis_utm, pch=16)
text(station_pts_utm, 1:44, pos=3)
plot(arcgis_utm, add=TRUE)
text(arcgis_utm, p$ISO_SUB, cex=0.7)


##  First project data into a planar coordinate system (here UTM zone 32)
p <- shapefile(system.file("external/lux.shp", package="raster"))
p2 <- as(1.5*extent(p), "SpatialPolygons")
proj4string(p2) <- proj4string(p)
pts <- spsample(p2-p, n=10, type="random")
plot(pts, pch=16, cex=.5,col="red")
plot(p, col=colorRampPalette(blues9)(12), add=TRUE)

utmStr <- "+proj=utm +zone=%d +datum=NAD83 +units=m +no_defs +ellps=GRS80"
crs <- CRS(sprintf(utmStr, 32))
pUTM <- spTransform(p, crs)
ptsUTM <- spTransform(pts, crs)

## Set up containers for results
n <- length(ptsUTM)
nearestCanton <- character(n)
distToNearestCanton <- numeric(n)

## For each point, find name of nearest polygon (in this case, Belgian cantons)
for (i in seq_along(nearestCanton)) {
  gDists <- gDistance(ptsUTM[i,], pUTM, byid=TRUE)
  nearestCanton[i] <- pUTM$NAME_2[which.min(gDists)]
  distToNearestCanton[i] <- min(gDists)
}

## Check that it worked
data.frame(nearestCanton, distToNearestCanton)
#       nearestCanton distToNearestCanton
# 1             Wiltz           15342.222
# 2        Echternach            7470.728
# 3            Remich           20520.800
# 4          Clervaux            6658.167
# 5        Echternach           22177.771
# 6          Clervaux           26388.388
# 7           Redange            8135.764
# 8            Remich            2199.394
# 9  Esch-sur-Alzette           11776.534
# 10           Remich           14998.204

plot(pts, pch=16, col="red")
text(pts, 1:10, pos=3)
plot(p, add=TRUE)
text(p, p$NAME_2, cex=0.7)



#### PLOTS ####

arcgis@data %>%
  tibble() %>%
  ggplot() +
  aes(x=as.numeric(population)) +
  geom_histogram()

arcgis@data %>%
  tibble() %>%
  ggplot() +
  aes(y=as.numeric(population),
      x=Shape__Are) +
  geom_point()

bubble(trees['HEIGHT'], col=rgb(0.5,0.5,1,0.5))
bubble(arcgis['ID'], col=rgb(0.5,0.5,1,0.5))

