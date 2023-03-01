#### Setup stuff ####
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#### User Defined Variables ####
data_path = "../../../Old Dominion University/LOPEZ, IVÁN - Philippines_PIRE_project/Rotenone_Database/ecosystem_database/allencoralatlas/VIP-20230127134513.zip"

#### PACKAGES ####
packages_used <- 
  c("geojsonR", # have to install from github, messes up tidyverse code, have to add dplyr:: to several commands
    "tidyverse",
    "janitor")

packages_to_install <- 
  packages_used[!packages_used %in% installed.packages()[,1]]

if (length(packages_to_install) > 0) {
  install.packages(packages_to_install, 
                   Ncpus = Sys.getenv("NUMBER_OF_PROCESSORS") - 1)
}

lapply(packages_used, 
       require, 
       character.only = TRUE)
