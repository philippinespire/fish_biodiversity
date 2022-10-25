# scripts

scripts to process and analyze the fish biodiversity data cellected by the SI, SU, and CAS expeditions

---


## `wrangle_cas_si_su_data.R`

### Data from all era's binded and columns irrelevant dropped
Sourced from:
*   `scripts/wrangleStationData_SI.R`
*   `scripts/wrangle_SU-SI_DuplicatesNewData.R`
*   `scripts/wrangle_cas_data.R`


### Source Information


#### `wrangleStationData_SI.R`
Wrangle fish biodiversity data from 1978-9 Smithsonian Expedition

Smithsonian db was queried, we read in the results of three queries, remove duplicated rows, remove columns with no data

Sourced from:
* `/data/station_info.xlsx` 
* `SI/Coordinates/Coordinate_Conversions.xlsx` 
* `/SI/Collections_Data`
* `/data/All_confirmed_names.xlsx`



#### `wrangle_SU-SI_DuplicatesNewData.R`
Wrangle fish biodiversity data from 2022

Sourced from:
* `/data/SU-SI_Duplicates_20220808.xlsx` 
* `/data/All_confirmed_names.xlsx`



#### `wrangle_cas_data.R`
Wrangle fish biodiversity data from 2016

Sourced from:
* `/CAS/CAS fish count by site PH 2016.xlsx` 
* `/CAS/CAS-Fishes-VIP2016-localities.xlsx` 
* `/data/All_confirmed_names.xlsx`


---


## `EstimateR.R`
### Est_S and estaccumR analyses and plots

Sourced from: 
* `wrangle_cas_si_su_data.R`
* `distance_calculations_mpa.R`
* `ordination_cas_su_si.R`


---


## `permanova.R`
### Makes permanova vegan variables 

Need to run all lines of `/scripts/wrangleStationData_SI.R` first for permanova vegan variables to work 


---


## `Rarefaction.R`
### Vegan and vegan.env variables created - adonis2 analyses for each era

Sourced from: `wrangle_cas_si_su_data.R`


---


## `ordination_cas_su_si.R`
### Making DCA, NMDS and Ord plots

Sourced from: `wrangle_cas_si_su_data.R`


---


## `fixed_model.R`
### Visualizations of hypothesis test (est_S model and emmeans_model)

Sourced from: 
* `wrangle_cas_si_su_data.R`
* `ordination_cas_su_si.R`
* `EstimateR.R`


--- 


## `RotenoneRovingCombined.R`

Reads in data from 20teens shared by Kent Carpenter, but we don't have station data, only fish counts.

We removed all rows with no field_number because we determined that 30 were likely otter trawls i the Stingray V, and the other NA record was from Ayungon while the site collected on the same day was farther south near Bais.


---


### `MPA_map_script.R` and `Site_Map_Script.R`

Scripts to create the map for mpa and site locations respectively


---


### `full_script.R` 

In progress script that runs everything at once


---


### `ordination_cas_su_si.R`

* All vegan variables for each era are made here
* Ordination, DCA, NMDS and ANOVA plots


---


### `data_si_pca.R` and `data_su_pca.R`

PCA Plots using si and su collections


---


### `fixed_model.R`

Makes est_S model

Sources from wrangle_cas_si_su_data, ordination_cas_su_si.R, and Estimate.R


---


### `Species_richness_rarefaction.R`
Unfinished

---


### `ProcessStationData_SI.R`
Need to run all lines of `/scripts/wrangleStationData_SI.R` first for permanova vegan variables to work 
* Makes EstimateS combatible File for 78-79
* Makes EstimateS combatible File by depth for 78-79
