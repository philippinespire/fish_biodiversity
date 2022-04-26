

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tidyverse)
library(readxl)

data_roving <-
  read_excel("RotenoneRovingCombined.xlsx",
             sheet = "Roving")
