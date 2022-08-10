#### INITIALIZATION ####
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tidyverse)
library(janitor)
library(readxl)
# install.packages("maps")
# install.packages("viridis")
require(maps)
require(viridis)
theme_set(
  theme_void()
)
# install.packages("vegan")
# install.packages("ggvegan")
library(vegan)
# install.packages("remotes")
library(remotes)
# remotes::install_github("gavinsimpson/ggvegan")
library(ggvegan)
source("wrangle_cas_si_su_data.R")


data <- data_cas_si_su

#### VEGANIZATION ####
prep_vegan_fixed <- function(data_fixed=data_cas_si_su %>%
                               drop_na(adjusted_latitude)){
  data %>%
    # filter by lowest_tax_cat (remove family)
    rename(taxon = verified_identification) %>%
    filter(specimen_count > 0) %>%
    group_by(taxon,
             station_code, #add everything but specimen_count
             study,
             field_number,
             # lowest_tax_cat
    ) %>%
    summarize(sum_specimen_count = sum(specimen_count, 
                                       na.rm = TRUE)) %>%
    ungroup() %>%
    # convert tibble from long to wide format
    pivot_wider(names_from = taxon,
                values_from = sum_specimen_count,
                values_fill = 0) %>%
    clean_names() %>%
    # sort by op_code
    arrange(study,
            station_code) %>%
    drop_na(station_code)
}

data_fixed_vegan <-
  prep_vegan_fixed() %>%
  dplyr::select(-station_code:-field_number
                #:-lowest_tax_cat
  )


#Vegan all data
prep_vegan <- function(data=data_cas_si_su){
  data %>%
  # filter by lowest_tax_cat (remove family)
  rename(taxon = verified_identification) %>%
  filter(specimen_count > 0) %>%
  group_by(taxon,
           station_code, #add everything but specimen_count
           study,
           field_number,
           # lowest_tax_cat
           ) %>%
  summarize(sum_specimen_count = sum(specimen_count, 
                                     na.rm = TRUE)) %>%
  ungroup() %>%
  # convert tibble from long to wide format
  pivot_wider(names_from = taxon,
              values_from = sum_specimen_count,
              values_fill = 0) %>%
  clean_names() %>%
  # sort by op_code
  arrange(study,
          station_code) %>%
    drop_na(station_code)
  }

data_cas_si_su_vegan <-
  prep_vegan() %>%
  dplyr::select(-station_code:-field_number
                #:-lowest_tax_cat
                )

#Vegan cas_2016 data
# prep_vegan_cas <- function(data=data_cas_si_su){
#   data %>%
#     rename(taxon = verified_identification) %>%
#     dplyr::filter(specimen_count > 0) %>%
#     group_by(taxon,
#              station_code, #add everything but specimen_count
#              study,
#              field_number,
#              lowest_tax_cat) %>%
#     summarize(sum_specimen_count = sum(specimen_count)) %>%
#     ungroup() %>%
#     # convert tibble from long to wide format
#     pivot_wider(names_from = taxon,
#                 values_from = sum_specimen_count,
#                 values_fill = 0) %>%
#     clean_names() %>%
#     # sort by station_code
#     arrange(study,
#             station_code) %>%
#     drop_na(station_code)
# }    

data_casvegan <-
  data_cas_si_su %>%
  dplyr::filter(study == "cas_2016") %>%
  prep_vegan() %>%
  dplyr::select(-station_code:-field_number)

#vegan SI data
# prep_vegan_si <- function(data=data_cas_si_su){
#   data %>%
#     rename(taxon = verified_identification) %>%
#     dplyr::filter(specimen_count > 0) %>%
#     group_by(taxon,
#              station_code, #add everything but specimen_count
#              study,
#              field_number,
#              lowest_tax_cat) %>%
#     summarize(sum_specimen_count = sum(specimen_count)) %>%
#     ungroup() %>%
#     # convert tibble from long to wide format
#     pivot_wider(names_from = taxon,
#                 values_from = sum_specimen_count,
#                 values_fill = 0) %>%
#     clean_names() %>%
#     # sort by station_code
#     arrange(study,
#             station_code) %>%
#     drop_na(station_code)
# }    

data_sivegan <-
  data_cas_si_su %>%
  dplyr::filter(study == "si_1978") %>%
  prep_vegan() %>%
  dplyr::select(-station_code:-field_number)

#vegan su_2022 data
# prep_vegan_su <- function(data=data_cas_si_su){
#   data %>%
#     rename(taxon = verified_identification) %>%
#     dplyr::filter(specimen_count > 0) %>%
#     group_by(taxon,
#              station_code, #add everything but specimen_count
#              study,
#              field_number,
#              lowest_tax_cat) %>%
#     summarize(sum_specimen_count = sum(specimen_count)) %>%
#     ungroup() %>%
#     # convert tibble from long to wide format
#     pivot_wider(names_from = taxon,
#                 values_from = sum_specimen_count,
#                 values_fill = 0) %>%
#     clean_names() %>%
#     # sort by station_code
#     arrange(study,
#             station_code) %>%
#     drop_na(station_code)
# }    

data_suvegan <- 
  data_cas_si_su %>%
  dplyr::filter(study == "su_2022") %>%
  prep_vegan() %>%
  dplyr::select(-station_code:-field_number)

#### PART 2: .env ####
#all data
data_cas_si_su_vegan.env <-
  prep_vegan() %>%
  dplyr::select(station_code:field_number)

#cas 2016 data
data_cas_vegan.env <-
  data_cas_si_su %>%
  dplyr::filter(study == "cas_2016") %>%
  prep_vegan() %>%
  dplyr::select(station_code:field_number)

#si data
data_si_vegan.env <-
  data_cas_si_su %>%
  dplyr::filter(study == "si_1978") %>%
  prep_vegan() %>%
  dplyr::select(station_code:field_number)

#su data
data_su_vegan.env <-
  data_cas_si_su %>%
  dplyr::filter(study == "su_2022") %>%
  prep_vegan() %>%
  dplyr::select(station_code:field_number)

#### ATTACH ####
#all data
attach(data_cas_si_su_vegan.env)
data_vegan <- data_cas_si_su_vegan 
data_vegan.env <- data_cas_si_su_vegan.env
attach(data_vegan.env)

#CAS 2016
attach(data_cas_vegan.env)

#si
attach(data_si_vegan.env)

#su
attach(data_su_vegan.env)

#### DCA PLOT ####
#all data------------------
ord <- decorana(data_vegan)  
ord
summary(ord)
#boring plot
plot(ord)

#fancier plot
plot(ord, type = "n")
points(ord, display = "sites", cex = 0.8, pch=21, col="black", bg="yellow")
text(ord, display = "spec", cex=0.7, col="red")

#fanciest plot
plot(ord, disp="sites", type="n")
ordihull(ord, study, col=1:3, lwd=3)
ordiellipse(ord, study, col=1:3, kind = "ehull", lwd=3)
ordiellipse(ord, study, col=1:3, draw="polygon")
points(ord, disp="sites", pch=21, col=1:3, bg="yellow", cex=1.3)
ordispider(ord, study, col=1:3, label = TRUE)

#cas 2016 data-------------
ord <- decorana(data_casvegan)  
ord
summary(ord)
#boring plot
plot(ord)

#fancier plot
plot(ord, type = "n")
points(ord, display = "sites", cex = 0.8, pch=21, col="black", bg="yellow")
text(ord, display = "spec", cex=0.7, col="red")

#fanciest plot
plot(ord, disp="sites", type="n")
ordihull(ord, study, col=1:3, lwd=3)
ordiellipse(ord, study, col=1:3, kind = "ehull", lwd=3)
ordiellipse(ord, study, col=1:3, draw="polygon")
points(ord, disp="sites", pch=21, col=1:3, bg="yellow", cex=1.3)
ordispider(ord, study, col=1:3, label = TRUE)

#si data--------------------
ord <- decorana(data_sivegan)  
ord
summary(ord)
#boring plot
plot(ord)

#fancier plot
plot(ord, type = "n")
points(ord, display = "sites", cex = 0.8, pch=21, col="black", bg="yellow")
text(ord, display = "spec", cex=0.7, col="red")

#fanciest plot
plot(ord, disp="sites", type="n")
ordihull(ord, study, col=1:3, lwd=3)
ordiellipse(ord, study, col=1:3, kind = "ehull", lwd=3)
ordiellipse(ord, study, col=1:3, draw="polygon")
points(ord, disp="sites", pch=21, col=1:3, bg="yellow", cex=1.3)
ordispider(ord, study, col=1:3, label = TRUE)

#su data----------------------
ord <- decorana(data_suvegan)  
ord
summary(ord)
#boring plot
plot(ord)

#fancier plot
plot(ord, type = "n")
points(ord, display = "sites", cex = 0.8, pch=21, col="black", bg="yellow")
text(ord, display = "spec", cex=0.7, col="red")

#fanciest plot
plot(ord, disp="sites", type="n")
ordihull(ord, study, col=1:3, lwd=3)
ordiellipse(ord, study, col=1:3, kind = "ehull", lwd=3)
ordiellipse(ord, study, col=1:3, draw="polygon")
points(ord, disp="sites", pch=21, col=1:3, bg="yellow", cex=1.3)
ordispider(ord, study, col=1:3, label = TRUE)

#### NMDS PLOT ####
#all data---------------------
ord <- metaMDS(data_vegan) #find and record rogue station codes in ord plot
ord
summary(ord)
#fanciest plot
plot(ord, disp="sites", type="n")
ordihull(ord, study, col=1:3, lwd=3)
ordiellipse(ord, study, col=1:3, kind = "ehull", lwd=3)
ordiellipse(ord, study, col=1:3, draw="polygon")
points(ord, disp="sites", pch=21, col=1:3, bg=1:3, cex=1.3)
text(ord, disp="sites", pch=21, col=1:3, bg=1:3, cex=1.3)
ordispider(ord, study, col=1:3, label = TRUE)

ord.fit <- 
  envfit(ord ~ study, 
         data=data_vegan.env, 
         perm=999,
         na.rm = TRUE)
ord.fit
plot(ord, dis="site")
ordiellipse(ord, study, col=1:3, kind = "ehull", lwd=3)
plot(ord.fit, col=1:3)

ordisurf(ord, study, add=TRUE)

#cas 2016 data------------------
ord <- metaMDS(data_casvegan)
ord
summary(ord)
#fanciest plot
plot(ord, disp="sites", type="n")
ordihull(ord, study, col=1:3, lwd=3)
ordiellipse(ord, study, col=1:3, kind = "ehull", lwd=3)
ordiellipse(ord, study, col=1:3, draw="polygon")
points(ord, disp="sites", pch=21, col=1:3, bg=1:3, cex=1.3)
text(ord, disp="sites", pch=21, col=1:3, bg=1:3, cex=1.3)
ordispider(ord, study, col=1:3, label = TRUE)

ord.fit <- 
  envfit(ord ~ study, 
         data=data_cas_vegan.env, 
         perm=999,
         na.rm = TRUE)
ord.fit
plot(ord, dis="site")
ordiellipse(ord, study, col=1:3, kind = "ehull", lwd=3)
plot(ord.fit, col=1:3)

ordisurf(ord, study, add=TRUE)

#si data------------------------
ord <- metaMDS(data_sivegan)
ord
summary(ord)
#fanciest plot
plot(ord, disp="sites", type="n")
ordihull(ord, study, col=1:3, lwd=3)
ordiellipse(ord, study, col=1:3, kind = "ehull", lwd=3)
ordiellipse(ord, study, col=1:3, draw="polygon")
points(ord, disp="sites", pch=21, col=1:3, bg=1:3, cex=1.3)
text(ord, disp="sites", pch=21, col=1:3, bg=1:3, cex=1.3)
ordispider(ord, study, col=1:3, label = TRUE)

ord.fit <- 
  envfit(ord ~ study, 
         data=data_si_vegan.env, 
         perm=999,
         na.rm = TRUE)
ord.fit
plot(ord, dis="site")
ordiellipse(ord, study, col=1:3, kind = "ehull", lwd=3)
plot(ord.fit, col=1:3)

#ordisurf(ord, study, add=TRUE) got an error

#su data---------------------
ord <- metaMDS(data_suvegan)
ord
summary(ord)
#fanciest plot
plot(ord, disp="sites", type="n")
ordihull(ord, study, col=1:3, lwd=3)
ordiellipse(ord, study, col=1:3, kind = "ehull", lwd=3)
ordiellipse(ord, study, col=1:3, draw="polygon")
points(ord, disp="sites", pch=21, col=1:3, bg=1:3, cex=1.3)
text(ord, disp="sites", pch=21, col=1:3, bg=1:3, cex=1.3)
ordispider(ord, study, col=1:3, label = TRUE)

ord.fit <- 
  envfit(ord ~ study, 
         data=data_su_vegan.env, 
         perm=999,
         na.rm = TRUE)
ord.fit
plot(ord, dis="site")
ordiellipse(ord, study, col=1:3, kind = "ehull", lwd=3)
plot(ord.fit, col=1:3)

#ordisurf(ord, study, add=TRUE) gave an error

#### ANOVA ####
#all data
ord <- cca(data_vegan ~ study, data=data_vegan.env)
ord
plot(ord, dis="site")
points(ord, disp="site", pch=21, col=1:3, bg="yellow", cex=1.3)
ordiellipse(ord, study, col=1:3, kind = "ehull", lwd=3)

anova(ord, by="term", permutations=999)
anova(ord, by="mar", permutations=999)
anova(ord, by="axis", permutations=999)

#cas 2016 data
ord <- cca(data_casvegan ~ station_code, data=data_cas_vegan.env) #error given
ord
plot(ord, dis="site")
points(ord, disp="site", pch=21, col=1:3, bg="yellow", cex=1.3)
ordiellipse(ord, study, col=1:3, kind = "ehull", lwd=3)

anova(ord, by="term", permutations=999)
anova(ord, by="mar", permutations=999)
anova(ord, by="axis", permutations=999)

#si data
ord <- cca(data_sivegan ~ station_code, data=data_si_vegan.env) #error given
ord
plot(ord, dis="site")
points(ord, disp="site", pch=21, col=1:3, bg="yellow", cex=1.3)
ordiellipse(ord, study, col=1:3, kind = "ehull", lwd=3)

anova(ord, by="term", permutations=999)
anova(ord, by="mar", permutations=999)
anova(ord, by="axis", permutations=999)

#su data
ord <- cca(data_suvegan ~ station_code, data=data_su_vegan.env) #error given
ord
plot(ord, dis="site")
points(ord, disp="site", pch=21, col=1:3, bg="yellow", cex=1.3)
ordiellipse(ord, study, col=1:3, kind = "ehull", lwd=3)

anova(ord, by="term", permutations=999)
anova(ord, by="mar", permutations=999)
anova(ord, by="axis", permutations=999)

#If there are covariates that we are not interested in 
#testing the effect of, but we want to account for their impact
#on the response variables, we can partial out these covariates
ord <- cca(data_vegan ~ depth_m + site + Condition(bait_type), 
           data=data_vegan.env)
anova(ord, by="term", permutations=999)