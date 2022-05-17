

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# install.packages("tidyverse")
# install.packages("readxl")

library(tidyverse)
library(readxl)
library(janitor)

data_roving <-
  read_excel("RotenoneRovingCombined.xlsx",
             sheet = "Roving") %>%
  rename(species=...1) %>%
  clean_names()

data_rotenone <-
  read_excel("RotenoneRovingCombined.xlsx",
             sheet = "Rotenone")

data_combined <-
  read_excel("RotenoneRovingCombined.xlsx",
             sheet = "Combined")
