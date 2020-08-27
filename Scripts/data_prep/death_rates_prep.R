## Mortality rates are from the 3222.0 Population Projections, Australia, 2017 (base) – 2066.
### Original data probability rates are converted to rates as these are the requiered inputs for the PMSLT
### Rates are per age and sex group with projections up to year 2066 based on medium improvements in life expectancy from ABS.

suppressPackageStartupMessages(library(dplyr)) # for manipulating data
suppressPackageStartupMessages(library(readxl)) # for reading excel files

rm (list = ls())
GetDeathsRates <- function(deaths, location){

options(scipen=999)
deaths="Data/Population and deaths/projections.xls"
location= "Australia" #"Victoria" or any state of interest.

data_male <- readxl::read_xls(deaths, sheet = "Table 5", range = "A7:K5107") %>% 
  select(Year, Age, location) %>% 
  mutate(rate = -(log(1-.[3]))) %>% # convert to rates 
  mutate(join_variable = case_when(Year < 2067 ~ paste(Year, Age, sep = "_"),
                                   Year >= 2067 ~ paste("2066", Age, sep = "_"))) %>% # to then create dataset by age cohort
  select(rate, join_variable)

data_female <- readxl::read_xls(deaths, sheet = "Table 5", range = "A7:B5107") %>% 
  bind_cols(readxl::read_xls(deaths, sheet = "Table 5", range = "M7:U5107")) %>%
  select(Year, Age, location) %>% 
  mutate(rate = -(log(1-1-.[,3]))) %>% # convert to rates 
  mutate(join_variable = case_when(Year < 2067 ~ paste(Year, Age, sep = "_"),
                                   Year >= 2067 ~ paste("2066", Age, sep = "_"))) %>% # to then create dataset by age cohort
  select(rate, join_variable)

#Age cohorts

age_cohort =seq(from = 17, to = 100, by = 5)


### MALES
# Create list with all death rates for males from 17 to 97 age cohorts

males_deaths <- list()

index <- 1

for (age in age_cohort) {

year=seq(from =2017, to = (2017  + (100-age)), by = 1) # min age modelled is 17 and we assume that cohort live up to 100
age = seq(age, to = 100, by =1)
sex ="male"

males_deaths[[index]] <- data.frame(year, age, sex) %>%  
  mutate(join_variable = case_when(year < 2067 ~ paste(year, age, sep = "_"),
                                   year >= 2067 ~ paste("2066", age, sep = "_"))) %>% # to then create dataset by age cohort. Projected data upto 2066
  left_join(data_male) %>%
  mutate(age_cohort = .[1,2])
  

index <- index +1
}
deaths_males <- plyr::ldply(males_deaths, rbind)
### FEMALES
# Create list with all death rates for males from 17 to 97 age cohorts

females_deaths <- list()

index <- 1

for (age in age_cohort) {
  
  year=seq(from =2017, to = (2017  + (100-age)), by = 1) # min age modelled is 17 and we assume that cohort live up to 100
  age = seq(age, to = 100, by =1)
  sex ="male"
  
  females_deaths[[index]] <- data.frame(year, age, sex) %>%  
    mutate(join_variable = case_when(year < 2067 ~ paste(year, age, sep = "_"),
                                     year >= 2067 ~ paste("2066", age, sep = "_"))) %>% # to then create dataset by age cohort. Projected data upto 2066
    left_join(data_female)  %>%
    mutate(age_cohort = paste(age))  %>%
    mutate(age_cohort = .[1,2])
  
  index <- index +1
}

deaths_females <- plyr::ldply(females_deaths, rbind)

females_deaths <- NULL
males_deaths <- NULL
return(list(deaths_males, deaths_females))

}