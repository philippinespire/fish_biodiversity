# scripts

scripts to process and analyze the fish biodiversity data cellected by the SI, SU, and CAS expeditions

---


## `wrangle_cas_si_su_data.R`

### Data from all era's binded and columns irrelevant dropped
#### Sourced from:
*   `scripts/wrangleStationData_SI.R`
*   `scripts/wrangle_SU-SI_DuplicatesNewData.R`
*   `scripts/wrangle_cas_data.R`


### `wrangleStationData_SI.R`
Wrangle fish biodiversity data from 1978-9 Smithsonian Expedition

Smithsonian db was queried, we read in the results of three queries, remove duplicated rows, remove columns with no data

Uses files `/data/station_info.xlsx` , `SI/Coordinates/Coordinate_Conversions.xlsx` and `/SI/Collections_Data`


### `wrangle_SU-SI_DuplicatesNewData.R`
Wrangle fish biodiversity data from 2022

Uses files `/data/SU-SI_Duplicates_20220808.xlsx` and `/data/All_confirmed_names.xlsx`


### `wrangle_cas_data.R`
Wrangle fish biodiversity data from 2016

Uses files `/CAS/CAS fish count by site PH 2016.xlsx` , `/CAS/CAS-Fishes-VIP2016-localities.xlsx` and `/data/All_confirmed_names.xlsx`


---

## `RotenoneRovingCombined.R`

Reads in data from 20teens shared by Kent Carpenter, but we don't have station data, only fish counts.



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

### `Species_richness_rarefaction.R`



### `ProcessStationData_SI.R`
* Makes EstimateS File for 78-79
* Makes EstimateS File by depth for 78-79
