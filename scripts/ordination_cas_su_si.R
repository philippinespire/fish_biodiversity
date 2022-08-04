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

prep_vegan <- function(data=data_cas_si_su){
  data %>%
  # filter by lowest_tax_cat (remove family)
  rename(taxon = verified_identification) %>%
  filter(specimen_count > 0) %>%
  group_by(taxon,
           station_code, #add everything but specimen_count
           study,
           field_number,
           lowest_tax_cat) %>%
  summarize(sum_specimen_count = sum(specimen_count)) %>%
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
  dplyr::select(-station_code:-lowest_tax_cat)


#### PART 2 ####

data_cas_si_su_vegan.env <-
  prep_vegan() %>%
  dplyr::select(station_code:lowest_tax_cat)


#### ATTACH ####
attach(data_cas_si_su_vegan.env)

data_vegan <- data_cas_si_su_vegan 
data_vegan.env <- data_cas_si_su_vegan.env

attach(data_vegan.env)

#### DCA PLOT ####
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


#### NMDS PLOT ####
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

#### ANOVA ####
ord <- cca(data_vegan ~ study, data=data_vegan.env)
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