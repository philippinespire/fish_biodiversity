# set working directory
setwd("C:/Users/whale/Downloads")


install.packages("rfishbase")
install.packages("openxlsx")
library(magrittr)
library(dplyr)
library(rfishbase)
library(openxlsx)
library(taxize)
options(taxize_entrez_key = "5ba4403576b9a39707d3711d4a9fc3b2c208")

# read in data
data <- read.csv("C:/Users/whale/Downloads/SU-SI_Duplicates.csv")


clean_colnames <- function(df) {
  names(df) <- names(df) %>% 
    gsub(" ", "_", .) %>%           # Replace spaces with underscores
    tolower() %>%                   # Convert to lowercase
    gsub("[^a-z0-9_]", "", .) %>%   # Remove special characters
    gsub("^\\d+", "", .)            # Remove leading numbers
  
  return(df)
}

data <- clean_colnames(data)

cols_to_keep <- c("lot_id", "catalognumber", "specimencount", "identification", 
                  "datecollected", "municipality", "preservationmethod", "samples_retained")

data <- data[, cols_to_keep, drop = FALSE]

data <- data[!((is.na(data$preservationmethod) | data$preservationmethod == "") & 
                 (is.na(data$samples_retained) | data$samples_retained == "")), ]


write.xlsx(data, "C:/Users/whale/Downloads/rotenone_samples.xlsx", rowNames = FALSE)
