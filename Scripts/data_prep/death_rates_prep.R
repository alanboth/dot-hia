### All cause deaths rates from Victoria to parametrise the life table
### Data is from: https://www.abs.gov.au/AUSSTATS/abs@.nsf/DetailsPage/3302.02018?OpenDocument
### Compare rates to those generated in life tables for Victoria

rm (list = ls())
options(scipen=999)
library(readr)
library(dplyr)
library(stringr)
library(ggplot2)

### Get data
### The same can be done with GBD data over three years to reproduce life tables
### Need to add trends in deaths rates

deaths_rates <- read_csv(paste0(getwd(), "/Data/Population and deaths/population_deaths.csv"))

### Ages to numeric
deaths_rates$Age <- as.numeric(deaths_rates$Age)

### Keep data for Victoria, one year age intervals, last three years of data, population numbers and deaths numbers

deaths_rates <- deaths_rates %>% 
  dplyr::filter(`States and Territories` == "Victoria", Measure %in% c("Population", "Deaths"), Age %in% c(0:99), Time %in% c(2016:2018), 
                Sex != "Persons")

deaths_rates_ave <- deaths_rates %>%
                      group_by(Age, Sex, Measure)  %>%
                      summarize(average = mean(Value))  

population <- dplyr::filter(deaths_rates_ave, Measure %in% "Population") %>% 
              dplyr::mutate(age_sex = paste(Age, Sex, sep = "_"))

deaths <- dplyr::filter(deaths_rates_ave, Measure %in% "Deaths") %>% 
  dplyr::mutate(age_sex = paste(Age, Sex, sep = "_"))


deaths_rates_final <- population %>% left_join(deaths, by = "age_sex") %>% 
                      dplyr::mutate(rate = average.y/average.x) %>%
                      dplyr::select(Age.x, Sex.x, rate) %>%
                      dplyr::rename(age = Age.x, 
                                    sex = Sex.x)

write_csv(deaths_rates_final, (paste0(getwd(), "/Data/Processed/deaths_melbourne.csv")))

### Graph to check data
dat_males <- filter(deaths_rates_final, sex == "Males")

ggplot(data=dat_males, aes(x=age, y=rate, group=1)) +
  geom_line()

  

