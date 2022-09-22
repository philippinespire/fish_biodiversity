# fish_biodiversity

the purpose of this repo is to centralize all data pertaining to Philippines PIRE fish biodiversity surveys.

---

## Directories

* SI: 1978-79 Smithsonian Expedition
* SU: 2019-present PIRE Expeditions
* CAS: 2016 Cal Academy of Sciences Expedition
* data: 20teens data from Kent Carpenter

## Notes

Rpackage to do what [EstimateS](https://www.robertkcolwell.org/pages/estimates) does: Vegan

---

## Useful Links

This is where all the data is.  We probably need to spend some time searching and transferring data from google to this repo

Google Drive → Philippines PIRE → [Rotenone_Database](https://drive.google.com/drive/folders/1n1yQ6lLybuoX6wJJQ5qRgplAQ09lOYHq?usp=sharing)

[Rotenone_stations](https://www.google.com/maps/d/edit?mid=1xB7u9XTp1JbzET8mogF0nwA_v32L6aGO&usp=sharing)

[Contemporary duplicates](https://drive.google.com/drive/folders/1XW8LRXK4yhUjJNjUEyFE8794wDhy-6hk?usp=sharing)

[Original data sheets](https://drive.google.com/drive/folders/1Y6GeNfWD62MisoEUMpenO09ZsY03Fqcl?usp=sharing)

[References](https://drive.google.com/drive/folders/1UDOhV8IWjNOvagBF-UQNpalLqe9fr30t?usp=sharing)

[EstimateS](https://www.robertkcolwell.org/pages/estimates)

---

## Wrangle and Assemble Full Data Set

1. Clone this repo to your computer
2. Make sure you have all of the dependencies
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
