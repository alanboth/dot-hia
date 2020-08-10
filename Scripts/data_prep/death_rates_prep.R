## Mortality rates are from the 3222.0 Population Projections, Australia, 2017 (base) – 2066.
### Original data probability rates are converted to rates as these are the requiered inputs for the PMSLT
### Rates are per age and sex group with projections up to year 2066 based on medium improvements in life expectancy from ABS.

suppressPackageStartupMessages(library(dplyr)) # for manipulating data
suppressPackageStartupMessages(library(readxl)) # for reading excel files

rm (list = ls())
GetDeathsRates <- function(deaths){

options(scipen=999)
# deaths="Data/Population and deaths/projections.xls"

data_male <- readxl::read_xls(deaths, sheet = "Table 5", range = "A7:J5107") %>% 
  select(Year, Age, Victoria) %>% 
  mutate(rate = -(log(1-Victoria))) %>% # convert to rates 
  mutate(join_variable = case_when(Year < 2067 ~ paste(Year, Age, sep = "_"),
                                   Year >= 2067 ~ paste("2066", Age, sep = "_"))) %>% # to then create dataset by age cohort
  select(rate, join_variable)

data_female <- readxl::read_xls(deaths, sheet = "Table 5", range = "A7:B5107") %>% 
  bind_cols(readxl::read_xls(deaths, sheet = "Table 5", range = "M7:U5107")) %>%
  select(Year, Age, Victoria) %>% 
  mutate(rate = -(log(1-Victoria))) %>% # convert to rates 
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

##### OLD CODE
### All cause deaths rates from Victoria to parametrise the life table
### Data is from: https://www.abs.gov.au/AUSSTATS/abs@.nsf/DetailsPage/3302.02018?OpenDocument
### Compare rates to those generated in life tables for Victoria

### Get data
### The same can be done with GBD data over three years to reproduce life tables
### Need to add trends in deaths rates

suppressPackageStartupMessages(library(dplyr)) # for manipulating data


calculateDeathRates <- function(population_deaths_location) {
   # population_deaths_location="Data/Population and deaths/population_deaths.csv"
  
    population_deaths <- read.csv(population_deaths_location,fileEncoding="UTF-8-BOM")
  
  # deaths_rates <- population_deaths %>%
  #   ### Ages to numeric. Need to convert to a character first
  #   mutate(Age=as.numeric(as.character(Age)))
  
  ### Keep data for Victoria, one year age intervals, last three years of data,
  ### population numbers and deaths numbers
  ### Add age group 100 and repeat value for 99, Victoria data has no deaths data for 100.
  
  deaths_rates <- population_deaths %>% 
    dplyr::filter(States.and.Territories == "Victoria",
                  Measure %in% c("Population", "Deaths"),
                  Age %in% c(0:99),
                  Time %in% c(2016:2018), 
                  Sex != "Persons") %>%
    ### Ages to numeric. Need to convert to a character first
    mutate(Age=as.numeric(as.character(Age)))
  
  deaths_rates_ave <- deaths_rates  %>%
    dplyr::filter(Measure %in% "Deaths") %>%
    dplyr::group_by(Age, Sex)  %>%
    dplyr::summarize(average = mean(Value)) %>% 
    dplyr::mutate(age_sex = paste(Age, Sex, sep = "_")) 
  
  population <- deaths_rates %>%
    dplyr::filter(Measure %in% "Population", Time %in% 2017) %>%
    dplyr::mutate(age_sex = paste(Age, Sex, sep = "_")) %>% 
    dplyr::select(Time, age_sex, Value) 
  
  
  deaths_rates_final <- population %>%
    left_join(deaths_rates_ave, by = "age_sex") %>% 
    dplyr::mutate(rate = average/Value) %>%
    dplyr::select(age=Age, sex=Sex, rate)
  
  ### Add repeated column for age 100 (when doing Aus wide not needed as data has age 100)
  
  deaths_rates_final_add_row <- deaths_rates_final %>%
    filter(age==99) 
  deaths_rates_final_add_row$age <- 100
  
  deaths_rates_final <- deaths_rates_final %>% rbind(deaths_rates_final_add_row) 
  
  
  return(deaths_rates_final)
    

}
