#### INITIALIZE ####
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# PACKAGES
#install.packages("rvest")
#install.packages("RCurl")
library(rvest)
library(RCurl)
library(stringr)
library(tidyverse)

ftp_url <- "ftp://cran.r-project.org/pub/R/web/packages/BayesMixSurv/"

get_files <- getURL(ftp_url, dirlistonly = TRUE)

extracted_filenames <- str_split(get_files, "\r\n")[[1]]

extracted_html_filenames <-unlist(str_extract_all(extracted_filenames, ".+(.html)"))

extracted_html_filenames

FTPDownloader <- function(filename, folder, handle)

dir.create(folder, showWarnings = FALSE)
  
fileurl <- str_c(ftp_url, extracted_html_filenames)
  
  if (!file.exists(str_c(folder, "/", extracted_html_filenames))) {
    
file_name <- try(getURL(fileurl, curl = getCurlHandle))
    
    write(file_name, str_c(folder, "/", extracted_html_filenames))
    
    Sys.sleep(1)
    
  }
}
  

#### P MPA DATABASE ####

ftp_url2 <- "http://www.database.mpasupportnetwork.com/"

get_files2 <- getURL(ftp_url, dirlistonly = TRUE)

get_files2

extracted_filenames2 <- str_split(get_files2, "\r\n")[[1]]

extracted_html_filenames2 <-unlist(str_extract_all(extracted_filenames2, ".+(.html)"))

extracted_html_filenames2
