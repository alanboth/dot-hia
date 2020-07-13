### Prepare gbd data from IHME for ithimr


# rm (list = ls())

suppressPackageStartupMessages(library(dplyr)) # for manipulating data
suppressPackageStartupMessages(library(stringr)) # for tidying data

# library(readr)
# library(dplyr)
# library(stringr)

source("Scripts/functions_mslt.R")

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

### DATA 

GBD_DATA <- read_csv(paste0(getwd(),"/Data/gbd/gbd_melbourne_ithimr.csv")) 
POPULATION <- read_csv(paste0(getwd(),"/Data/Processed/population_melbourne_abs.csv"))

### Tidy GBD_DATA to match ITHIMR
GBD_DATA <- GBD_DATA %>% dplyr::filter(age != "Under 5" & age != "5 to 9" & age != "10 to 14")
GBD_DATA$age[GBD_DATA$age == "95 plus"] <- "95 to 120" 
GBD_DATA$min_age <- as.numeric(sapply(GBD_DATA$age,function(x)str_split(x,' to ')[[1]][1]))
GBD_DATA$max_age <- as.numeric(sapply(GBD_DATA$age,function(x)str_split(x,' to ')[[1]][2]))
GBD_DATA$sex <- tolower(GBD_DATA$sex)
GBD_DATA$burden <- GBD_DATA$val
GBD_DATA$cause_name <- GBD_DATA$cause
GBD_DATA$age_name <- GBD_DATA$age

### Tidy POPULATION to match ITHIMR code

POPULATION$age[POPULATION$age == "95 plus"] <- "95-120"    
POPULATION$age <- gsub("-", " to ", POPULATION$age)

### Create variable to left join population to GBD file
POPULATION$sex_age <- tolower(paste(POPULATION$sex, POPULATION$age, sep = "_"))

GBD_DATA$sex_age <- tolower(paste(GBD_DATA$sex, GBD_DATA$age, sep = "_"))
GBD_DATA <- left_join(GBD_DATA, dplyr::select(POPULATION, c('sex_age', "population")), by = "sex_age")

### order by cause, otherwise issue with function ithimrsetup
GBD_DATA <- GBD_DATA %>% dplyr::arrange(sex, age, measure, cause)


### Change format age to match ITHIMR code
POPULATION$age <- gsub(" to ", "-", POPULATION$age)

### Drop sex_age columns
POPULATION <- POPULATION %>% dplyr::select( -sex_age)
GBD_DATA <- GBD_DATA %>% dplyr::select( -sex_age)

#### ALAN: Not SURE HOW THESE ARE SAVED
### Save files
write_csv(GBD_DATA, paste0(getwd(), "/Data/Processed/gbd_melbourne.csv"))

write_csv(POPULATION, paste0(getwd(), "/Data/Processed/population_melbourne.csv"))

