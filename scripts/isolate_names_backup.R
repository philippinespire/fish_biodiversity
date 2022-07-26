#### Initialization ####
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)

#### Read in Data ####
x <- read_excel("Book2.xlsx")
y <- read_excel("Contemporary_confirmed_names.xlsx")
xy <- semi_join(x,y) %>%
  select(original_id)
z <- anti_join(x, y)
write_excel_csv(z, "Leftover_names.csv")
