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
