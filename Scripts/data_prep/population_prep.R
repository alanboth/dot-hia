### Data is from 2016 Australian census (retrieve using Table Builder) . These data was chosen, instead of projections for baseline  year given that census data is detailed in age
### (5-yr age groups up to 95-100 needed) and location, other data sources are not. 

library(readr)
library(readxl)
library(stringr)
GetPopulation <- function(population_data, location){
   # population_data="Data/original/abs/population_census.xlsx"
   # location="Greater Melbourne" # (other options)
  
  population <- readxl::read_xlsx(population_data, sheet = "Data Sheet 0", range = "B9:K49") %>%
    dplyr::select(age, sex, location) %>%
  rowwise() %>%
  dplyr::rename(population = location) %>%
  mutate(from_age = as.numeric(str_split(age,'-')[[1]][1])) %>%
  mutate(to_age = as.numeric(str_split(age,'-')[[1]][2])) %>%
  # using rowwise() turns the dataframe into a tibble
  data.frame() %>%
  mutate(age_cat = from_age + 2) %>%
  mutate(sex_age_cat = paste(tolower(sex), age_cat, sep="_")) %>%
  dplyr::select(sex_age_cat, population)
  return(population)}