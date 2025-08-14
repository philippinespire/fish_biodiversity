#### Initialize ####

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#### PACKAGES ####
packages_used <- 
  c("tidyverse",
    "janitor",
    "magrittr",
    "lubridate",
    "rgdal",
    "raster",
    "rgeos",
    "readr",
    "maptools",
    "scales"
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


#### USER DEFINED VARIABLES ####
source("wrangle_arcgis_si_su.R")
source("distance_calculations_mpa.R")


#### MAP FUNCTIONS ####

make_map <- 
  function(map_shape_data = arcgis_tibble,
           waypoint_data = data_human_pop,
           pop_char = quo(TOTPOP_CY),
           min_long = 120.5,
           max_long = 124.3,
           min_lat = 8.5,
           max_lat = 14){
    map_out <-
      map_shape_data %>%
      ggplot() + 
      aes(x = long, 
          y = lat, 
          group = group,
          fill = !!pop_char) +
      geom_polygon(color='black') +
      scale_fill_gradient(low = "white",
                          high = "black") +
      # here is where we pull in the station by station data
      geom_point(data = waypoint_data,
                 aes(x=long,
                     y=lat,
                     color = study,
                     # size = dist_nearest_polygon,
                     shape = study),
                 inherit.aes = FALSE,
                 size = 2,
                 stroke = 2) +
      scale_shape_manual(values = c(0,1,2)) +
      # scale_shape_manual(values = c(3,4,8)) +
      
      labs(x="Longitude",
           y="Latitude") +
      coord_quickmap(xlim = c(min_long,
                              max_long),
                     ylim = c(min_lat,
                              max_lat))
    
    return(map_out)
  }



make_philippines_map <- 
  function(
    map_shape_data = arcgis_tibble,
    pop_char       = quo(TOTPOP_CY),
    min_long       = NA,
    max_long       = NA,
    min_lat        = NA,
    max_lat        = NA
  ){
    ggplot(map_shape_data, aes(x = long, y = lat, group = group, fill = !!pop_char)) +
      geom_polygon(color = "black") +
      scale_fill_gradient(
        name   = "Total Human /n Population",   # new legend title
        low    = "white",
        high   = "black",
        labels = comma                       # use scales::comma()
      ) +
      labs(
        x = "Longitude",
        y = "Latitude"
      ) +
      coord_quickmap(
        xlim = c(min_long, max_long),
        ylim = c(min_lat,  max_lat)
      ) +
      theme_minimal() +
      theme(
        legend.position       = c(0.98, 0.98),      # top-right inside
        legend.justification  = c("right", "top"),   # anchor at panel top-right
        legend.background     = element_rect(fill = alpha("white", 0.7), color = NA)
      )
}


make_philippines_map()



make_philippines_map <- 
  function(
    map_shape_data = arcgis_tibble,
    pop_char       = quo(TOTPOP_CY),
    min_long       = NA,
    max_long       = NA,
    min_lat        = NA,
    max_lat        = NA
  ){
    ggplot(map_shape_data, aes(x = long, y = lat, group = group, fill = !!pop_char)) +
      geom_polygon(color = "black") +
      scale_fill_gradient(low = "white", high = "black") +
      labs(
        x = "Longitude",
        y = "Latitude"
      ) +
      coord_quickmap(
        xlim = c(min_long, max_long),
        ylim = c(min_lat,  max_lat)
      ) +
      theme_minimal() +
      theme(
        legend.position       = c(0.98, 0.98),      # top-right inside
        legend.justification  = c("right", "top"),   # anchor at panel top-right
        legend.background     = element_rect(fill = alpha("white", 0.7), color = NA)
      )
  }


make_philippines_map()


make_map_stations <- 
  function(map_shape_data = arcgis_tibble,
           waypoint_data = data_human_pop,
           pop_char = quo(TOTPOP_CY),
           min_long = 120.5,
           max_long = 124.3,
           min_lat = 8.5,
           max_lat = 14){
    map_out <-
      map_shape_data %>%
      ggplot() + 
      aes(x = long, 
          y = lat, 
          group = group,
          fill = !!pop_char) +
      geom_polygon(color='black') +
      scale_fill_gradient(low = "white",
                          high = "black") +
      # here is where we pull in the station by station data
      geom_point(data = waypoint_data,
                 aes(x=long,
                     y=lat,
                     color = study,
                     # size = dist_nearest_polygon,
                     shape = study),
                 inherit.aes = FALSE,
                 size = 2,
                 stroke = 2) +
      scale_shape_manual(values = c(0,1,2)) +
    # scale_shape_manual(values = c(3,4,8)) +
    
    labs(x="Longitude",
         y="Latitude") +
      coord_quickmap(xlim = c(min_long,
                              max_long),
                     ylim = c(min_lat,
                              max_lat))
    
    return(map_out)
  }

# visualize survey sites on heatmap of human pop in each province w/ mpas

make_map_mpa <- 
  function(map_shape_data = arcgis_tibble,
           waypoint_data = data_human_pop,
           mpa_data = data_mpa,
           pop_char = quo(TOTPOP_CY),
           min_long = 120.5,
           max_long = 124.3,
           min_lat = 8.5,
           max_lat = 14){
    map_out <-
      map_shape_data %>%
      ggplot() + 
      aes(x = long, 
          y = lat, 
          group = group,
          fill = !!pop_char) +
      geom_polygon(color='black') +
      scale_fill_gradient(low = "white",
                          high = "black") +
      # here is where we pull in the station by station data
      geom_point(data = waypoint_data,
                 aes(x=long,
                     y=lat,
                     color = study,
                     # size = dist_nearest_polygon,
                     shape = study),
                 inherit.aes = FALSE,
                 size = 2,
                 stroke = 2) +
      scale_shape_manual(values = c(0,1,2)) +
      # show closest mpa
      geom_point(data = mpa_data,
                 aes(x=long,
                     y=lat,
                     # shape = study
                 ),
                 color = "red",
                 shape = 8,
                 inherit.aes = FALSE,
                 size = 1,
                 stroke = 2) +
      # scale_shape_manual(values = c(3,4,8)) +
      
      labs(x="Longitude",
           y="Latitude") +
      coord_quickmap(xlim = c(min_long,
                              max_long),
                     ylim = c(min_lat,
                              max_lat))
    
    return(map_out)
  }


#### VISUALIZE MAPS ####

#default map
make_map()
make_map_stations()
make_map(pop_char = quo(log10(TOTPOP_CY/AREA)))

#default map_mpa
make_map_mpa()
make_map_mpa(pop_char = quo(log10(TOTPOP_CY/AREA)))


#north sulu sea visayas
make_map(min_long = 120.8,
         max_long = 124,
         min_lat = 8.8,
         max_lat = 11)

#visayas
make_map(min_long = 122.8,
         max_long = 124,
         min_lat = 8.8,
         max_lat = 10)

# north sulu sea
make_map(min_long = 120.8,
         max_long = 121.4,
         min_lat = 10.8,
         max_lat = 11)

# northern most cluster
make_map(min_long = 120.8,
         max_long = 120.95,
         min_lat = 13.625,
         max_lat = 13.825)

# whole map
make_map(min_long = NA,
         max_long = NA,
         min_lat = NA,
         max_lat = NA)



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


