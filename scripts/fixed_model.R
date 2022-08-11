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

data_fixed <- 
  data_cas_si_su %>%
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
est_S %>%
  ggplot(aes(x = pc1_mpa_infl, 
             y = s_chao1,
             color = study)) +
  geom_point() +
  geom_errorbar(aes(ymin = s_chao1 - se_chao1,
                    ymax = s_chao1 + se_chao1)) +
  geom_smooth(se = FALSE,
              method = "lm") +
  theme_classic()


#### FIT FIXED EFFECT MODEL S vs pc1_mpa_infl ####

distribution_family = "poisson"
alpha_sig = 0.05

model <<- 
  glm(formula = s_chao1 ~ pc1_mpa_infl * study, 
      family = distribution_family,
      data = est_S)

summary(model)

exp(coef(model))


#### Conduct A priori contrast tests for differences among sites ####

# now we move on to finish the hypothesis testing.  Are there differences between the sites?
# estimated marginal means 



emmeans_model <<-
  emmeans(model,
          ~ pc1_mpa_infl*study,
          alpha = alpha_sig)

# emmeans back transformed to the original units of response var
summary(emmeans_model,      
        type="response")

# contrasts between sites
contrast(regrid(emmeans_model), # emmeans back transformed to the original units of response var
         method = 'pairwise', 
         simple = 'each', 
         combine = FALSE, 
         adjust = "bh")


#### Group Sites Based on Model Results ####

groupings_model <<-
  multcomp::cld(emmeans_model, 
                alpha = alpha_sig,
                Letters = letters,
                type="response",
                adjust = "bh") %>%
  as.data.frame %>%
  mutate(group = str_remove_all(.group," "),
         group = str_replace_all(group,
                                 "(.)(.)",
                                 "\\1,\\2")) %>%
  rename(response = 3)

groupings_model             # these values are back transformed, groupings based on transformed


# i noticed that the emmeans from groupings don't match those from emmeans so this is the table to use for making the figure
# the emmeans means and conf intervals match those produced by afex_plot, so I think those are what we want
groupings_model_fixed <<-
  summary(emmeans_model,      # emmeans back transformed to the original units of response var
          type="response") %>%
  tibble() %>%
  left_join(groupings_model %>%
              dplyr::select(-response:-asymp.UCL),
            # by = c(str_replace(fixed_vars,
            #                    "[\\+\\*]",
            #                    '" , "'))) %>%
            by = c("pc1_mpa_infl",
                   "study")) %>%
  rename(response = 3)

groupings_model_fixed       # cld messes up back transformation, this takes values from emmeans and groupings from cld

#### Visualize Estimated Marginal Means Output With Group Categories ####

p <- 
  groupings_model_fixed %>%
  ggplot(aes(x=study,
             y=response,
             fill = study)) +
  geom_col(position = "dodge",
           color = "black") +
  # scale_fill_manual(values = c("lightgrey",
  #                              "white"),
  #                   labels = c('Pre-Screen', 
  #                              'Post-Screen')) +
  # geom_point(data = data,
  #            aes(x = location,
  #                y = !!response_var,
  #                color = location
  #            ),
  #            position = position_dodge(width = 0.9),
  #            # color = "grey70",
#            # shape = 1,
#            size = 1)
geom_errorbar(aes(ymin=asymp.LCL,
                  ymax=asymp.UCL),
              width = 0.2,
              color = "grey50",
              # size = 1,
              position = position_dodge(width=0.9)) +
  guides(color = "none",
         shape = "none") +   #remove color legend
  geom_text(aes(label=group),
            position = position_dodge(width=0.9),
            vjust = -0.5,
            hjust = -0.15,
            size = 8 / (14/5)) +  # https://stackoverflow.com/questions/25061822/ggplot-geom-text-font-size-control
  theme_myfigs +
  # ylim(ymin, 
  #      ymax) +
  labs(x = "",
       y = "Estimated Species Richness (Chao1)") +
  theme(legend.position=c(0.1,0.9),  
        legend.title=element_blank())

p


#### Visualize Fixed Effect Model Fit (Response Var vs Continuous X Var by Group) ####

# this generates a tibble with the model predictions that can be plotted
# however, it does not do a good job of showing us where the model is extrapolating 
emmeans_ggpredict <- 
  ggemmeans(model,
            terms = c("pc1_mpa_infl [all]",
                      "study")) 
# compatible with ggplot
# shows models, but extrapolates beyond observations
plot(emmeans_ggpredict) +
  #this is our custom plot theme defined in USER DEFINED VARIABLES
  theme_myfigs


# the next several blocks of code will only show us predictions within the ranges of observation by location

# this way uses ggpredict, which has some nice features
#make a tibble that has the max and min continuous xvar for each categorical xvar
min_max_xvar <-  
  est_S %>%
  rename(x = pc1_mpa_infl,
         group = study) %>%
  group_by(group) %>%
  filter(x == max(x) |
           x == min(x)) %>%
  dplyr::select(group,
                x) %>%
  arrange(group,
          x) %>%
  distinct() %>%
  mutate(min_max = case_when(row_number() %% 2 == 0 ~ "max_x",
                             TRUE ~ "min_x")) %>%
  pivot_wider(names_from = min_max,
              values_from = x)

# then use that tibble to filter the object made by ggpredict and plot
emmeans_ggpredict %>%
  left_join(min_max_xvar) %>% 
  filter(x >= min_x,
         x <= max_x) %>% 
  plot() +
  #add in our observed values of female_male
  geom_jitter(data = est_S,
              aes(x = pc1_mpa_infl,
                  y = s_chao1,
                  color = study),
              size = 3,
              inherit.aes = FALSE,
              width = 0,
              height = 0.02) +
  theme_myfigs





#### FIT FIXED EFFECT MODEL S vs DISTANCE TO NEAREST MPA####

distribution_family = "poisson"
alpha_sig = 0.05

model <<- 
  glm(formula = s_chao1 ~ station_mpa_distance_km * study, 
      family = distribution_family,
      data = est_S)

summary(model)

exp(coef(model))


#### Conduct A priori contrast tests for differences among sites ####

# now we move on to finish the hypothesis testing.  Are there differences between the sites?
# estimated marginal means 



emmeans_model <<-
  emmeans(model,
          ~ station_mpa_distance_km*study,
          alpha = alpha_sig,
          cov.reduce = range)

# emmeans back transformed to the original units of response var
summary(emmeans_model,      
        type="response")

# contrasts between sites
contrast(regrid(emmeans_model), # emmeans back transformed to the original units of response var
         method = 'pairwise', 
         simple = 'each', 
         combine = FALSE, 
         adjust = "bh")


#### Group Sites Based on Model Results ####

groupings_model <<-
  multcomp::cld(emmeans_model, 
                alpha = alpha_sig,
                Letters = letters,
                type="response",
                adjust = "bh") %>%
  as.data.frame %>%
  mutate(group = str_remove_all(.group," "),
         group = str_replace_all(group,
                                 "(.)(.)",
                                 "\\1,\\2")) %>%
  rename(response = 3)

groupings_model             # these values are back transformed, groupings based on transformed


# i noticed that the emmeans from groupings don't match those from emmeans so this is the table to use for making the figure
# the emmeans means and conf intervals match those produced by afex_plot, so I think those are what we want
groupings_model_fixed <<-
  summary(emmeans_model,      # emmeans back transformed to the original units of response var
          type="response") %>%
  tibble() %>%
  left_join(groupings_model %>%
              dplyr::select(-response:-asymp.UCL),
            # by = c(str_replace(fixed_vars,
            #                    "[\\+\\*]",
            #                    '" , "'))) %>%
            by = c("station_mpa_distance_km",
                   "study")) %>%
  rename(response = 3) %>%
  filter(station_mpa_distance_km < 100)

groupings_model_fixed       # cld messes up back transformation, this takes values from emmeans and groupings from cld

#### Visualize Estimated Marginal Means Output With Group Categories ####

p <- 
  groupings_model_fixed %>%
  ggplot(aes(x=study,
             y=response,
             fill = study)) +
  geom_col(position = "dodge",
           color = "black") +
  # scale_fill_manual(values = c("lightgrey",
  #                              "white"),
  #                   labels = c('Pre-Screen', 
  #                              'Post-Screen')) +
  # geom_point(data = data,
  #            aes(x = location,
  #                y = !!response_var,
  #                color = location
  #            ),
  #            position = position_dodge(width = 0.9),
  #            # color = "grey70",
#            # shape = 1,
#            size = 1)
geom_errorbar(aes(ymin=asymp.LCL,
                  ymax=asymp.UCL),
              width = 0.2,
              color = "grey50",
              # size = 1,
              position = position_dodge(width=0.9)) +
  guides(color = "none",
         shape = "none") +   #remove color legend
  # geom_text(aes(label=group),
  #           position = position_dodge(width=0.9),
  #           vjust = -0.5,
  #           hjust = -0.15,
  #           size = 8 / (14/5)) +  # https://stackoverflow.com/questions/25061822/ggplot-geom-text-font-size-control
  theme_myfigs +
  # ylim(ymin, 
  #      ymax) +
  labs(x = "",
       y = "Estimated Species Richness (Chao1)") +
  theme(legend.position=c(0.1,0.9),  
        legend.title=element_blank())

p


#### Visualize Fixed Effect Model Fit (Response Var vs Continuous X Var by Group) ####

# this generates a tibble with the model predictions that can be plotted
# however, it does not do a good job of showing us where the model is extrapolating 
emmeans_ggpredict <- 
  ggemmeans(model,
            terms = c("station_mpa_distance_km [all]",
                      "study")) 
# compatible with ggplot
# shows models, but extrapolates beyond observations
plot(emmeans_ggpredict) +
  #this is our custom plot theme defined in USER DEFINED VARIABLES
  theme_myfigs


# the next several blocks of code will only show us predictions within the ranges of observation by location

# this way uses ggpredict, which has some nice features
#make a tibble that has the max and min continuous xvar for each categorical xvar
min_max_xvar <-  
  est_S %>%
  rename(x = station_mpa_distance_km,
         group = study) %>%
  group_by(group) %>%
  filter(x == max(x) |
           x == min(x)) %>%
  dplyr::select(group,
                x) %>%
  arrange(group,
          x) %>%
  distinct() %>%
  mutate(min_max = case_when(row_number() %% 2 == 0 ~ "max_x",
                             TRUE ~ "min_x")) %>%
  pivot_wider(names_from = min_max,
              values_from = x)

# then use that tibble to filter the object made by ggpredict and plot
emmeans_ggpredict %>%
  left_join(min_max_xvar) %>% 
  filter(x >= min_x,
         x <= max_x) %>% 
  plot() +
  #add in our observed values of female_male
  geom_jitter(data = est_S,
              aes(x = station_mpa_distance_km,
                  y = s_chao1,
                  color = study),
              size = 3,
              inherit.aes = FALSE,
              width = 0,
              height = 0.02) +
  theme_myfigs


