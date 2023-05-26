# fish_biodiversity

the purpose of this repo is to centralize all data pertaining to Philippines PIRE fish biodiversity surveys.

---

## Directories

* `SI`: 1978-79 Smithsonian Expedition
* `SU`: 2019-present PIRE Expeditions
* `CAS`: 2016 Cal Academy of Sciences Expedition
* `data`: 20teens data from Kent Carpenter
* `figures`: Plots made from analyses
* `literature`: Background information on community similarity and how to statistically analyze it
* `rr_proposal`: Contains proposal graphic mockup explaining PCA
* `scripts`: R codes that wrangle data, and create all graphics and analyses

## Notes

Rpackage to do what [EstimateS](https://www.robertkcolwell.org/pages/estimates) does: Vegan

---

## Useful Links

This is where all the data is that John Whalen has organized.  Dirs with data that we analyze from these places should be transferred to this repo and not modified.

Google Drive → Philippines PIRE → [Rotenone_Database](https://drive.google.com/drive/folders/1n1yQ6lLybuoX6wJJQ5qRgplAQ09lOYHq?usp=sharing)

[Rotenone_stations](https://www.google.com/maps/d/edit?mid=1xB7u9XTp1JbzET8mogF0nwA_v32L6aGO&usp=sharing)

[Contemporary duplicates](https://drive.google.com/drive/folders/1XW8LRXK4yhUjJNjUEyFE8794wDhy-6hk?usp=sharing)

[Original data sheets](https://drive.google.com/drive/folders/1Y6GeNfWD62MisoEUMpenO09ZsY03Fqcl?usp=sharing)

[References](https://drive.google.com/drive/folders/1UDOhV8IWjNOvagBF-UQNpalLqe9fr30t?usp=sharing)

[EstimateS](https://www.robertkcolwell.org/pages/estimates)

---

## Wrangle and Assemble Full Data Set

These are instructions if you are reading in the data for the first time.  Once you have done all the following steps successfully, you can just run the whole `fish_biodiveristy/scripts/wangle_cas_si_su_data.R` script.

1. Clone this repo to your computer
2. Open RStudio
    * make sure you are using `R 4.2.1` and a 2022 version of R Studio
3. Make sure you have all of the dependencies and they are up to date 2022 versions
    ```r
    library(tidyverse)
    library(readxl)
    library(janitor)
    library(purrr)
    library(magrittr)
    library(measurements)
    library(lubridate)
    library(readr)
    library(devtools)
    #install_github("decisionpatterns/tidyimpute")
    library(tidyimpute)
    ```
4. Open `fish_biodiversity/scripts/wrangle_cas_si_su_data.R` in R Studio
   * `alt-o` to collapse sections
   * run `INITIALIZE` and `USER DEFINED VARIABLES` sections
      * make sure there were no errors
   * Under `READ IN DATA` run each line one by one checking for errors
      * You can expect to see "warnings", but if you see "error" then goto step 2
   * Run each subsequent command in this script making sure there are no errors
5. The result should be a single tibble named `data_cas_si_su_mpa_pop`
   * We remove most of the columns of data from the data sets.  If we want to retain an additional column from the original data for each study
      * the column should be named the same in all three data sets
      * modify the `select()` command that removes the column of interest
6. Open `fish_biodiversity/scripts/distance_calculations_mpa.R` in R Studio

7. Open `fish_biodiversity/scripts/wrangle_arcgis.R` in R Studio
