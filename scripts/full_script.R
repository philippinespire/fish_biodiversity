#### INITIALIZATION ####

# install.packages("maps")
# install.packages("viridis")
# install.packages("geosphere")
# devtools::install_github("singmann/afex@master")
# devtools::install_github("eclarke/ggbeeswarm")
# install.packages("multcomp")
# install.packages("multcompView")
# install.packages("performance")
# install.packages("optimx")
# install.packages("effects")
# install.packages("ggeffects")
# install.packages("afex")
# install.packages("ggbeeswarm")
# install.packages("prediction")
# install.packages("fitdistrplus")
# install.packages("rlang")
# install.packages("emmeans")

library(ggbiplot)
library(geosphere)
library(readxl)
library(janitor)
library(purrr)
library(magrittr)
library(lubridate)
library(readr)
library(maps)
library(viridis)
library(vegan)
library(ggvegan)
library(remotes)
library(devtools)
library(fitdistrplus)
library(emmeans)
library(multcomp)
library(multcompView)
library(ggeffects)
library(rlang)
library(afex)
library(ggbeeswarm)
library(performance)
library(optimx)
library(effects)
library(prediction)
library(tidyverse)


#### USER DEFINED VARIABLES ####

wrangle_si_data_path = "./wrangleStationData_SI.R"
wrangle_su_si_data_path = "./wrangle_SU-SI_DuplicatesNewData.R"
wrangle_cas_data_path = "./wrangle_cas_data.R"

#### READ IN DATA ####
source(wrangle_si_data_path)
source(wrangle_su_si_data_path)
source(wrangle_cas_data_path)
source("distance_calculations.R")
source("ordination_cas_su_si.R")
source("wrangle_cas_si_su_data.R")
source("EstimateR.R")

