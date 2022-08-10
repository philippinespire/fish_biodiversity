#### INITIALIZATION ####
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tidyverse)
library(magrittr)
library(janitor)
# install.packages("fitdistrplus")
# 
# install.packages("rlang")
# install.packages("emmeans")
library(devtools)
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

source("wrangle_cas_si_su_data.R")
source("ordination_cas_su_si.R")

data_fixed <- data_cas_si_su %>%
  drop_na(adjusted_latitude)

prep_vegan_fixed <- function(data_fixed=data_cas_si_su %>%
                               drop_na(adjusted_latitude)){
  data_fixed %>%
    # filter by lowest_tax_cat (remove family)
    rename(taxon = verified_identification) %>%
    filter(specimen_count > 0) %>%
    group_by(taxon,
             station_code, #add everything but specimen_count
             study,
             field_number,
             # lowest_tax_cat
    ) %>%
    summarize(sum_specimen_count = sum(specimen_count, 
                                       na.rm = TRUE)) %>%
    ungroup() %>%
    # convert tibble from long to wide format
    pivot_wider(names_from = taxon,
                values_from = sum_specimen_count,
                values_fill = 0) %>%
    clean_names() %>%
    # sort by op_code
    arrange(study,
            station_code) %>%
    drop_na(station_code)
}

data_fixed_vegan <-
  prep_vegan_fixed() %>%
  dplyr::select(-station_code:-field_number
                #:-lowest_tax_cat
  )

model <<- 
  glm(formula = data_vegan ~ total_length_mm + location, 
      family = distribution_family,
      data = data)

#### USER DEFINED VARIABLES ####

# you can make a default theme for your publication's figures.  This makes things easier for you. 
# feel free to customize as necessary
theme_myfigs <- 
  theme_classic() +
  theme(panel.background = element_rect(fill = 'white', 
                                        color = 'white'),
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(color="grey95", 
                                          size=0.25),
        panel.border = element_blank(),
        axis.text.y = element_text(size = 9, 
                                   color = 'black'),
        axis.text.x = element_text(size = 9, 
                                   color = 'black'),
        # axis.title.x = element_blank(),
        axis.title.y = element_text(size = 10, 
                                    color = 'black'),
        plot.title = element_text(size = 10, 
                                  color = 'black'),
        plot.subtitle = element_text(size = 9, 
                                     color = 'black'),
        plot.caption = element_text(size = 9, 
                                    color = 'black', 
                                    hjust = 0),
        legend.text = element_text(size = 9, 
                                   color = 'black'),
        legend.title = element_text(size = 9, 
                                    color = 'black'),
        legend.background = element_blank(),
        legend.position="right"
  )


#error - NaN's not allowed if 'na.rm' is FALSE
p<-estaccumR(data_fixed_vegan, permutations = 100)
p.plot<-plot(p, display = c("chao","ace"))
p.plot

data_estaccumR_plot <-
  p$chao %>%
  # t() %>%
  as_tibble() %>%
  dplyr::mutate(N = row_number()) %>%
  pivot_longer(cols = starts_with("V"),
               names_to = "permutation") %>%
  group_by(N) %>%
  dplyr::summarize(chao_mean = mean(value),
                   chao_ci_lower = quantile(value,
                                            probs = 0.025),
                   chao_ci_upper = quantile(value,
                                            probs = 0.975))

data_estaccumR_plot

data_estaccumR_plot %>%
  ggplot(aes(x=N,
             y=chao_mean)) +
  geom_ribbon(aes(ymin=chao_ci_lower,
                  ymax=chao_ci_upper),
              fill = "grey") +
  geom_line() +
  theme_classic()
