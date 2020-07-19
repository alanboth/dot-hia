### Prepare gbd data from IHME for ithimr

suppressPackageStartupMessages(library(dplyr)) # for manipulating data
suppressPackageStartupMessages(library(stringr)) # for splitting strings


### We need to prepare data for ITHIMR 

# GBD file needs to have the following columns: 
# age (=label, e.g. 15-49)
# sex (=male or female)
# measure
# cause (GBD_DATA$cause matches DISEASE_INVENTORY$GBD_name)
# metric
# burden
# min_age (=number, e.g. 15)
# max_age (=number, e.g. 49)

calculateGBDandPopulation <- function(gbd_melbourne_ithimr_location,population_melbourne_abs_location) {
  # gbd_melbourne_ithimr_location="Data/gbd/gbd_melbourne_ithimr.csv"
  # population_melbourne_abs_location="Data/Processed/population_melbourne_abs.csv"
  
  # Tidy POPULATION to match ITHIMR code
  population <- read.csv(population_melbourne_abs_location, as.is=T,
                         fileEncoding="UTF-8-BOM") %>%
    # population should be numeric
    mutate(population = as.numeric(gsub(",", "", population))) %>%
    # some ages have space at the end
    mutate(age = gsub(" ", "", age)) %>%
    mutate(age = gsub("-", " to ", age)) %>%
    mutate(sex2 = tolower(sex))
  
  # Tidy GBD_DATA to match ITHIMR
  gbd_data <- read.csv(gbd_melbourne_ithimr_location, as.is=T,
                       fileEncoding="UTF-8-BOM") %>%
    filter(age != "Under 5" & age != "5 to 9" & age != "10 to 14") %>%
    mutate(age = ifelse(age=="95 plus", "95 to 120", age)) %>%
    rowwise() %>%
    mutate(min_age = as.numeric(str_split(age,' to ')[[1]][1])) %>%
    mutate(max_age = as.numeric(str_split(age,' to ')[[1]][2])) %>%
    data.frame() %>%
    mutate(sex=tolower(sex)) %>%
    mutate(burden=val) %>%
    mutate(cause_name=cause) %>%
    mutate(age_name=age) %>%
    left_join(population%>%select(sex2,age,population),
              by=c("sex"="sex2","age"="age")) %>%
    # order by cause, otherwise issue with function ithimrsetup
    arrange(sex,age,measure,cause)
  
  # Change format age to match ITHIMR code
  population <- population %>%
    select(-sex2) %>%
    mutate(age = gsub(" to ", "-", age))
  
  return(list(gbd_data,population))
  
}


