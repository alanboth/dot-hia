# ---- chunk-INTRO: ITHIM-R exposures ----

rm (list = ls())
# library(ithimr)
library(srvyr)
library(survey)
library(psych)
library(DescTools)
library(Rmisc)
library(FSA)
library(plyr)
library(boot)
library(VIM)
library(naniar)
library(dplyr)
library(readr)
library(tidyverse)
library(CompLognormal)
library(pracma)
library(goft)
library(tidyr)
library(utils)
library(taRifx)
library(naniar)
library(data.table)
library(stringi)


options(scipen=999)


### Create inputs as per ITHMR (https://github.com/ITHIM/ITHIM-R) for: trips_CITY.csv, injuries_CITY.csv, pa_CITY.csv
### AND AIR POLLUTION?
### Create synthetic population

# ---- chunk-1: Create trips data  ----  
### Columns ITHIMR: 
#### -One row per trip (or stage of trip)
#### -Minimal columns: participant_id, age, sex, trip_mode, trip_duration (or trip_distance)
#### -Other columns: stage_mode, stage_duration (or stage_distance)
### Should include people without trips. FOR NOW INCLUDES ONLY THOSE WITH TRIPS, NOT SURE IF IT INCLUDES PEOPLE WITHOUT TRIPS, I DELETED SOME NAS WHICH MAY REPRESENT PEOPLE
### WITHOUT TRIPS
### VISTA Survey for Melbourne greater area for time period 2017-18
### One day (weekday or weekend)
### Final product trips_melbourne where each row is a stage

hh_VISTA <- read_csv(paste0(getwd(), "/Data/Travel survey/VISTA 12-18/H_VISTA_1218_V1.csv"))
# jte_VISTA <- read_csv(paste0(getwd(), "/Data/Travel survey/VISTA 12-18/JTE_VISTA1218_V1.csv"))
# jtw_VISTA <- read.csv(paste0(getwd(), "/Data/Travel survey/VISTA 12-18/JTW_VISTA1218_V1.csv"))
person_VISTA <- read_csv(paste0(getwd(), "/Data/Travel survey/VISTA 12-18/P_VISTA1218_V1.csv"))
# stop_VISTA <- read.csv(paste0(getwd(), "/Data/Travel survey/VISTA 12-18/S_VISTA1218_V1.csv"))
trip_VISTA <- read_csv(paste0(getwd(), "/Data/Travel survey/VISTA 12-18/T_VISTA1218_V1.csv"))


#### First check NAs for weights in the household, person and trips datasets


hh_weight_NA <- hh_VISTA %>% dplyr::filter(is.na(WDHHWGT) & is.na(WEHHWGT))
pers_weight_NA <- person_VISTA %>% dplyr::filter(is.na(WDPERSWGT) & is.na(WEPERSWGT))
trips_weight_NA <- trip_VISTA %>% dplyr::filter(is.na(WDTRIPWGT) & is.na(WETRIPWGT))

hh_person <- left_join(person_VISTA, hh_VISTA, by = c("HHID"))


###

trips_melbourne  <- left_join(trip_VISTA, hh_person, by = c("PERSID")) %>% dplyr::filter(SurveyPeriod == "2017-18" & (HomeSubRegion != "Geelong" | HomeSubRegion != "Other")) %>%
                                                                                              dplyr::select("HHID.x", "PERSID", "AGE", 
                                                                                              "SEX", "TRIPID","DayType", "SurveyPeriod", "HomeSubRegion", "TRIPNO", 
                                                                                              "LINKMODE", "MODE1", "MODE2", "MODE3", "MODE4", "MODE5", "MODE6", "MODE7", 
                                                                                              "MODE8", "MODE9", "TIME1", "TIME2", "TIME3", "TIME4", "TIME5", "TIME6", "TIME7", 
                                                                                              "TIME8", "TIME9", "DIST1", "DIST2", "DIST3", "DIST4","DIST5", "DIST6", "DIST7",
                                                                                               "DIST8", "DIST9", "TRAVTIME", "TRIPPURP", "WDPERSWGT", "WEPERSWGT", "CUMDIST", 
                                                                                              "DESTLGA", "ORIGLGA", "HOMELGA") %>% 
                                                                                              mutate(WDPERSWGT = replace_na(WDPERSWGT, "")) %>%
                                                                                              mutate(WEPERSWGT = replace_na(WEPERSWGT, "")) %>%
                                                                                              tidyr::unite("participant_wt", WDPERSWGT:WEPERSWGT , na.rm = TRUE, remove = TRUE, sep = "") %>% 
                                                                                              mutate_at("participant_wt", funs(as.numeric)) %>%
                                                                                              mutate_at(vars(starts_with("TIME")),funs(as.numeric)) %>%   
                                                                                              mutate_at(vars(starts_with("DIST")),funs(as.numeric)) 
                                                                                             



### Replace all character "N/A" with NA
trips_melbourne[ trips_melbourne == "N/A" ] <- NA 

### Convert to data table to be able to convert multiple columns (MODE1,2.., TIME, DIST) to MODE, TIME and DIST columns. 
### Creates a variable called "variable" which indicates mode, time and dist variable
trips_melb_dt <- data.table(trips_melbourne)

### Wide to long to represent all modes in rows (with time and dist)

#### BELEN: this is not working as I thought, seems to repeat TIME and DIST

colA = paste("MODE", 1:9, sep = "")
colB = paste("TIME", 1:9, sep = "")
colC = paste("DIST", 1:9, sep = "")
trips_melb_dt_long <-  melt(trips_melb_dt, measure = list(colA, colB, colC), value.name = c("MODE", "TIME", "DIST"))


### Back to data.frame

trips_melbourne <- as.data.frame(trips_melb_dt_long)
### Replace mode 13 and 14 with NA
trips_melbourne$MODE[trips_melbourne$MODE == "13" | trips_melbourne$MODE == "14"] <- NA    
### Sort by person id

trips_melbourne <- trips_melbourne[order(trips_melbourne$TRIPID, trips_melbourne$PERSID),]

### Keep only if complete cases for  MODE, DIST and TIME and participant_wt(I AM UNSURE WHY THERE ARE NA WEIGHTS)

trips_melbourne  <-subset(trips_melbourne, (!is.na(trips_melbourne$MODE) & !is.na(trips_melbourne$TIME) & !is.na(trips_melbourne$DIST) & !is.na(trips_melbourne$participant_wt)))

### Name to match ITHIMR

trips_melbourne$participant_id <- trips_melbourne$PERSID ### Keep original to use in to append total walking and cycling time and distance to PERSONS
names(trips_melbourne)[names(trips_melbourne)=="HHID.x"] <- "household_id"
names(trips_melbourne)[names(trips_melbourne)=="AGE"] <- "age" ## age1 here because age will match the age cats from pa data
names(trips_melbourne)[names(trips_melbourne)=="SEX"] <- "sex"
names(trips_melbourne)[names(trips_melbourne)=="TRIPID"] <- "trip_id"
names(trips_melbourne)[names(trips_melbourne)=="MODE"] <- "trip_mode"
names(trips_melbourne)[names(trips_melbourne)=="TIME"] <- "trip_duration"
names(trips_melbourne)[names(trips_melbourne)=="DIST"] <- "trip_distance"
names(trips_melbourne)[names(trips_melbourne)=="TRIPPURP"] <- "trip_purpose"
names(trips_melbourne)[names(trips_melbourne)=="DayType"] <- "day_type"

### Separate participant id into year, hh_id and participant_id

trips_melbourne <- separate(trips_melbourne, participant_id, into = c(NA, "year", NA, "cluster_id", "household_id", NA, "participant_id"), sep = c(1, 3, 4, 6, -3,-2))

trips_melbourne$trip_id <- seq.int(nrow(trips_melbourne))



### Create age gropus to match with PA data

trips_melbourne <- trips_melbourne %>%  dplyr::mutate(age_cat = case_when(age < 5 ~ 1,
                                                       age >= 5 & age <=9 ~ 2,
                                                       age >= 10 & age <=14 ~ 3,
                                                       age >= 15 & age <=17 ~ 4, 
                                                       age >= 18 & age <=19 ~ 5,
                                                       age >= 20 & age <=24 ~ 6,
                                                       age >= 25 & age <=29 ~ 7, 
                                                       age >= 30 & age <=34 ~ 8, 
                                                       age >= 35 & age <=39 ~ 9, 
                                                       age >= 40 & age <=44 ~ 10,
                                                       age >= 45 & age <=49 ~ 11, 
                                                       age >= 50 & age <=54 ~ 12, 
                                                       age >= 55 & age <=59 ~ 13, 
                                                       age >= 60 & age <=64 ~ 14, 
                                                       age >= 65 & age <=69 ~ 15,
                                                       age >= 70 & age <=74 ~ 16, 
                                                       age >= 75 & age <=79 ~ 17,
                                                       age >= 80 & age <=84 ~ 18,
                                                       age >= 85 ~ 19))

#### Only keep adults to match NHS_pa data, drop categories 1,2 and 3

trips_melbourne <- trips_melbourne %>% dplyr::filter(age_cat>3)

### Do not include age_cat as ithim is doing its own synthetic populaton and this causes issues to have age_cat

trips_melbourne <- trips_melbourne %>% dplyr::select(PERSID, cluster_id, household_id, participant_id, age, sex, year, trip_id,
                                                     trip_purpose, participant_wt, trip_mode, trip_duration, trip_distance, 
                                                    day_type)

trips_melbourne$day_type <- tolower(trips_melbourne$day_type)
trips_melbourne$sex[trips_melbourne$sex =="M"] <- 'male'
trips_melbourne$sex[trips_melbourne$sex =="F"] <- 'female'


trips_melbourne$trip_mode[which(trips_melbourne$trip_mode == "Vehicle Driver")] = "car"
trips_melbourne$trip_mode[which(trips_melbourne$trip_mode == "Vehicle Passenger")] = "car"
trips_melbourne$trip_mode[which(trips_melbourne$trip_mode == "Taxi")] = "car"
trips_melbourne$trip_mode[which(trips_melbourne$trip_mode == "School Bus")] = "bus"
trips_melbourne$trip_mode[which(trips_melbourne$trip_mode == "Public Bus")] = "bus"
trips_melbourne$trip_mode[which(trips_melbourne$trip_mode == "Walking")] = "pedestrian"

trips_melbourne <- mutate_each(trips_melbourne, funs(tolower))

### Create numeric id

trips_melbourne$participant_id <- cumsum(!duplicated(trips_melbourne[1]))

### Character numbers to numerics
cols.num <- c("age","year", "trip_id", "trip_duration", "trip_distance", "participant_wt")
trips_melbourne[cols.num] <- sapply(trips_melbourne[cols.num],as.numeric)
sapply(trips_melbourne, class)



write_csv(trips_melbourne, (paste0(getwd(), "/Data/Processed/trips_melbourne.csv")))
write_rds(trips_melbourne, (paste0(getwd(), "/Data/Processed/trips_melbourne.Rds")))

