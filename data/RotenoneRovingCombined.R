

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# install.packages("tidyverse")
# install.packages("readxl")

library(tidyverse)
library(readxl)

data_roving <-
  read_excel("RotenoneRovingCombined.xlsx",
             sheet = "Roving")

data_rotenone <-
  read_excel("RotenoneRovingCombined.xlsx",
             sheet = "Rotenone")

data_combined <-
  read_excel("RotenoneRovingCombined.xlsx",
             sheet = "Combined")
