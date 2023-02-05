

#### functions ####
prep_vegan <- 
  function(data=data_cas_si_su){
    data %>%
      # filter by lowest_tax_cat (remove family)
      dplyr::rename(taxon = verified_identification) %>%
      dplyr::filter(specimen_count > 0,
                    !is.na(latitude)) %>% 
      group_by(taxon,
               station_code, 
               study,
               depth_m) %>%
      dplyr::summarize(sum_specimen_count = sum(specimen_count, 
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

data_vegan <-
  prep_vegan() %>%
  dplyr::select(-station_code:-depth_m)

data_vegan.env <-
  prep_vegan() %>%
  dplyr::select(station_code:depth_m)

