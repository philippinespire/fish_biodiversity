library(sf)
library(ggplot2)

# Load the Shapefile into R
mpa <- st_read("../data/gis/marineprotectedareas/MarineProtectedAreas/MarineProtectedAreas.shp")
summary(mpa)

ggplot() +
  geom_sf(data = mpa) +
  ggtitle("Marine Protected Areas")


library(foreign)

# Load the dbf file into R
dbf <- read.dbf("../data/gis/marineprotectedareas/MarineProtectedAreas/MarineProtectedAreas.dbf")
