#### INITIALIZE ####

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# install.packages("tidyverse")
# install.packages("readxl")

library(tidyverse)
library(readxl)
library(janitor)
library(purrr)

#### USER DEFINED VARIABLES ####
spRichnessFile = "../data/PracticeEstimateSResult.txt"
siteCompFile = "../data/PracticeEstimateSResultSpecies.txt"
siteMetaDataFile = "../data/station_info.xlsx"
dataDir = "../data"


dataDir = str_replace(dataDir,
                      "\\/$",
                      "")

#### READ IN COUNT DATA ####

data_sp_richness <-
  read_tsv(spRichnessFile,
           skip =  3,
           col_names = TRUE) %>%
  clean_names() %>%
  rename_with(.cols = contains("percent_ci_lower"),
              .fn = ~str_replace(.,
                                 "_percent_ci_lower_bound",
                                 "_ci_lw")) %>%
  rename_with(.cols = contains("percent_ci_upper"),
              .fn = ~str_replace(.,
                                 "_percent_ci_upper_bound",
                                 "_ci_up")) %>%
  select(-x46) 

#### READ IN SITE COMPARISON DATA ####

data_site_comp <-
  read_tsv(siteCompFile,
           skip =  3,
           col_names = TRUE) %>%
  clean_names() %>%
  rename_with(.cols = contains("abundance_based"),
              .fn = ~str_remove(.,
                                 "_abundance_based")) %>%
  select(-x19)

#### DATA VISUALIZE ####
data_sp_richness %>%
  ggplot(aes(x=samples,
             y=s_est)) +
  geom_point() +
  geom_errorbar(aes(ymin = s_est_95_ci_lw,
                    ymax = s_est_95_ci_up),
                width = 0.2) +
  labs(title = str_to_upper("SI 1978-79: Species Abundance"),
       subtitle = "Rotenone Stations Only",
       x = "SAMPLING SITES",
       y = "S ESTIMATE") +
  theme_classic()
  
data_sp_richness %>%
  pivot_longer(cols = s_est:s_est_95_ci_up,
               names_to = "s_est_cat",
               values_to = "s_est") %>%
  ggplot(aes(x=samples,
             y=s_est,
             color = s_est_cat)) +
  geom_point() +
  labs(title = str_to_upper("SI 1978-79: Species Abundance"),
       subtitle = "Rotenone Stations Only",
       x = "SAMPLING SITES",
       y = "S ESTIMATE") +
  theme_classic()


data_site_comp %>%
  ggplot(aes(y=first_sample,
             x=second_sample,
             fill = bray_curtis)) +
  geom_tile() +
  
  labs(title = str_to_upper("SI 1978-79: Bray-Curtis"),
       subtitle = "Rotenone Stations Only",
       x = "SAMPLING SITES",
       y = "SAMPLING SITES") +
  theme_classic()

