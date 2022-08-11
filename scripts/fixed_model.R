#### INITIALIZATION ####
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

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
library(tidyverse)

source("wrangle_cas_si_su_data.R")
source("ordination_cas_su_si.R")
source("EstimateR.R")

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



data %>%
  pivot_longer(cols = c(contains("specimen_count"),
               names_to = "verified_identification")) %>%
  ggplot(aes(x=value,
             fill = station_code)) +
  geom_histogram() +
  theme_classic() +
  # theme(axis.text.x = element_text(angle = 0, 
  #                                  hjust=0.5)) +
  facet_grid(location ~ metric,
             scales = "free_x")

# data %>%
#   pivot_longer(cols = c(contains("_mm"),
#                         contains("_g")),
#                names_to = "metric") %>%
#   ggplot(aes(x=value,
#              fill = factor(stage_clean))) +
#   geom_histogram() +
#   theme_classic() +
#   # theme(axis.text.x = element_text(angle = 0, 
#   #                                  hjust=0.5)) +
#   facet_grid(location ~ metric,
#              scales = "free_x")

# I'm noticing that the left skewed distribution of `weight_of_gonads_g` is quite different from the other metrics
# it may have to be handled differently

# visualize statistical distributions (see fitdistrplus: An R Package for Fitting Distributions, 2020)
#  vis_dists() is a function that I made above in the FUNCTIONS section.  It accepts the tibble and column name to visualize.
#  vis_dists() creates three figures
# vis_dists(data,
#           "total_length_mm")
# vis_dists(data,
#           "standard_length_mm")
# vis_dists(data,
#           "weight_g")
# # results in error making third plot because some values are zero and some of the distibutions are incompatible with zeros in data
# vis_dists(data,
#           "weight_of_gonads_g")
# # results in error making third plot because some values are zero and some of the distibutions are incompatible with zeros in data
# vis_dists(data,
#           "female_male")



#### Make Visualization of Hypothesis Test ####
data %>%
  ggplot(aes(y=specimen_count,
             x = station_code,
             color = study)) +
  geom_point(size = 5) +
  geom_smooth(formula = "y ~ x", 
              method = "glm", 
              method.args = list(family="quasibinomial"), 
              se = T) +
  theme_classic() +
  facet_grid(location ~ .)

distribution_family = "poisson"
alpha_sig = 0.05

model <<- 
  glm(formula = s_chao1 ~ pc1_mpa_infl*study, 
      family = "poisson",
      data = est_S)

summary(model)


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
