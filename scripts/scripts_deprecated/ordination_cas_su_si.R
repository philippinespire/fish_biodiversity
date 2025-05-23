#### INITIALIZATION ####
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


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
library(tidyverse)
source("wrangle_cas_si_su_data.R")
source("veganize_data.R")

# data <- data_cas_si_su
# data_fixed <- data_cas_si_su %>%
#               drop_na(latitude)



#### VEGANIZATION ####
data_cas_si_su_vegan <-
  data_cas_si_su %>%
  prep_vegan() %>%
  dplyr::select(-station_code:-study)

data_cas_si_su_vegan.env <-
  data_cas_si_su %>%
  prep_vegan() %>%
  dplyr::select(station_code:study)

#vegan CAS data
data_cas_vegan <-
  data_cas_si_su %>%
  dplyr::filter(study == "cas_2016") %>%
  prep_vegan() %>%
  dplyr::select(-station_code:-study)

data_cas_vegan.env <-
  data_cas_si_su %>%
  dplyr::filter(study == "cas_2016") %>%
  prep_vegan() %>%
  dplyr::select(station_code:study)

#vegan SI data
data_si_vegan <-
  data_cas_si_su %>%
  dplyr::filter(study == "si_1978") %>%
  prep_vegan() %>%
  dplyr::select(-station_code:-study)

data_si_vegan.env <-
  data_cas_si_su %>%
  dplyr::filter(study == "si_1978") %>%
  prep_vegan() %>%
  dplyr::select(station_code:study)

#vegan su_2022 data
data_su_vegan <- 
  data_cas_si_su %>%
  dplyr::filter(study == "su_2022") %>%
  prep_vegan() %>%
  dplyr::select(-station_code:-study)

data_su_vegan.env <-
  data_cas_si_su %>%
  dplyr::filter(study == "su_2022") %>%
  prep_vegan() %>%
  dplyr::select(station_code:study)



#### ATTACH ####
#all data
data_vegan <- data_cas_si_su_vegan 
data_vegan.env <- data_cas_si_su_vegan.env
attach(data_vegan.env)


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

attach(data_vegan.env)
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

detach(data_vegan.env)

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