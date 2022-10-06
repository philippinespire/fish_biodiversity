# scripts

scripts to process and analyze the fish biodiversity data cellected by the SI, SU, and CAS expeditions

---

## `RotenoneRovingCombined.R`

Reads in data from 20teens shared by Kent Carpenter, but we don't have station data, only fish counts.

## `wrangleStationData_SI.R`

Wrangle fish biodiversity data from 1978-9 Smithsonian Expedition

Smithsonian db was queried, we read in the results of three queries, remove duplicated rows, remove columns with no data

We removed all rows with no field_number because we determined that 30 were likely otter trawls i the Stingray V, and the other NA record was from Ayungon while the site collected on the same day was farther south near Bais.

### `MPA_map_script.R` and `Site_Map_Script.R`

Scripts to create the map for mpa and site locations respectively

### `full_script.R` 

In progress script that runs everything at once

### `ordination_cas_su_si.R`

* All vegan variables for each era are made here
* Ordination, DCA, NMDS and ANOVA plots

### `data_si_pca.R` and `data_su_pca.R`

PCA Plots using si and su collections

### `EstimateR.R`

est_S plots, estaccumR plots

### `fixed_model.R`

Makes est_S model

Sources from wrangle_cas_si_su_data, ordination_cas_su_si.R, and Estimate.R

### `permanova.R`

Permanova and adonis2

### `Rarefaction.R`
adonis 2 for each era
