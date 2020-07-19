# ---- chunk-INTRO: ITHIM-R exposures ----

# rm (list = ls())
# library(ithimr)


# library(srvyr)
# library(survey)
# library(psych)
# library(DescTools)
# library(Rmisc)
# library(FSA)
# library(plyr)
# library(boot)
# library(VIM)
# library(naniar)
# library(dplyr)
# library(readr)
# library(tidyverse)
# library(CompLognormal)
# library(pracma)
# library(goft)
# library(tidyr)
# library(utils)
# library(taRifx)
# library(naniar)
# library(data.table)
# library(stringi)


suppressPackageStartupMessages(library(dplyr)) # for manipulating data
suppressPackageStartupMessages(library(tidyr)) # for pivoting data


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

# other VISTA datasets. Not used.
# jte_VISTA <- read_csv("Data/Travel survey/VISTA 12-18/JTE_VISTA1218_V1.csv")
# jtw_VISTA <- read.csv("Data/Travel survey/VISTA 12-18/JTW_VISTA1218_V1.csv")
# stop_VISTA <- read.csv("Data/Travel survey/VISTA 12-18/S_VISTA1218_V1.csv")

calculateVistaTrips <- function(hh_VISTA_location,person_VISTA_location,trip_VISTA_location) {
  hh_VISTA_location="Data/Travelsurvey/VISTA12-18/H_VISTA_1218_V1.csv"
  person_VISTA_location="Data/Travelsurvey/VISTA12-18/P_VISTA1218_V1.csv"
  trip_VISTA_location="Data/Travelsurvey/VISTA12-18/T_VISTA1218_V1.csv"
  
  
<<<<<<< HEAD
  hh_VISTA <- read.csv(hh_VISTA_location,as.is=T, fileEncoding="UTF-8-BOM") %>%
    dplyr::select(HHID,SurveyPeriod,DayType,WDHHWGT,WEHHWGT,HomeSubRegion,HOMELGA) %>%
    filter(HHID!="") # some rows were completely blank
  person_VISTA <- read.csv(person_VISTA_location,as.is=T, fileEncoding="UTF-8-BOM") %>%
    dplyr::select(PERSID,HHID,AGE,SEX,WDPERSWGT,WEPERSWGT)
  trip_VISTA <- read.csv(trip_VISTA_location,as.is=T, fileEncoding="UTF-8-BOM") %>%
=======
  hh_VISTA <- read.csv(hh_VISTA_location,as.is=T,fileEncoding="UTF-8-BOM") %>%
    dplyr::select(HHID,SurveyPeriod,DayType,WDHHWGT,WEHHWGT,HomeSubRegion,HOMELGA) %>%
    filter(HHID!="") # some rows were completely blank
  person_VISTA <- read.csv(person_VISTA_location,as.is=T,fileEncoding="UTF-8-BOM") %>%
    dplyr::select(PERSID,HHID,AGE,SEX,WDPERSWGT,WEPERSWGT)
  trip_VISTA <- read.csv(trip_VISTA_location,as.is=T,fileEncoding="UTF-8-BOM") %>%
>>>>>>> 104a6d303f290af1e7bc9567dac7adf43147c6b8
    dplyr::select(TRIPID,PERSID,HHID,TRIPNO,CUMDIST,TRAVTIME,ORIGLGA,DESTLGA,
                  TRIPPURP,LINKMODE,
                  MODE1,MODE2,MODE3,MODE4,MODE5,MODE6,MODE7,MODE8,MODE9,
                  DIST1,DIST2,DIST3,DIST4,DIST5,DIST6,DIST7,DIST8,DIST9,
                  TIME1,TIME2,TIME3,TIME4,TIME5,TIME6,TIME7,TIME8,TIME9,
                  WDTRIPWGT,WETRIPWGT)
  
  
  #### First check NAs for weights in the household, person and trips datasets
  # hh_weight_NA <- hh_VISTA %>% dplyr::filter(is.na(WDHHWGT) & is.na(WEHHWGT))
  # pers_weight_NA <- person_VISTA %>% dplyr::filter(is.na(WDPERSWGT) & is.na(WEPERSWGT))
  # trips_weight_NA <- trip_VISTA %>% dplyr::filter(is.na(WDTRIPWGT) & is.na(WETRIPWGT))
  
  
  hh_person <- left_join(person_VISTA, hh_VISTA, by = "HHID")
  
  
  trips_melbourne  <- left_join(trip_VISTA, hh_person, by = c("PERSID","HHID") ) %>%
    dplyr::filter(SurveyPeriod == "2017-18" &
                    (HomeSubRegion != "Geelong" | HomeSubRegion != "Other")) %>%
    dplyr::select(HHID, PERSID, AGE, SEX, TRIPID, DayType, SurveyPeriod, 
                  HomeSubRegion, TRIPNO, LINKMODE, MODE1, MODE2, MODE3, MODE4, 
                  MODE5, MODE6, MODE7, MODE8, MODE9, TIME1, TIME2, TIME3, TIME4, 
                  TIME5, TIME6, TIME7, TIME8, TIME9, DIST1, DIST2, DIST3, DIST4,
                  DIST5, DIST6, DIST7, DIST8, DIST9, TRAVTIME, TRIPPURP, 
                  WDPERSWGT, WEPERSWGT, CUMDIST, DESTLGA, ORIGLGA, HOMELGA) %>% 
    rowwise() %>% # want to sum across rows, not down columns
    mutate(participant_wt = sum(as.numeric(WDPERSWGT),as.numeric(WEPERSWGT),na.rm=T)) %>%
    dplyr::select(-WDPERSWGT,-WEPERSWGT) %>%
    as.data.frame()
  # these take too long to run, will do conversion to numeric later on
  # mutate_at(vars(starts_with("TIME")),funs(as.numeric)) %>%   
  # mutate_at(vars(starts_with("DIST")),funs(as.numeric)) 
  
  
  
  
  ### Replace all character "N/A" with NA
  trips_melbourne[ trips_melbourne == "N/A" ] <- NA 
  
  # all of the data that isn't MODE,TIME,DIST
  trips_melbourne_tripid <- trips_melbourne %>%
    dplyr::select(HHID, PERSID, AGE, SEX, TRIPID, DayType, SurveyPeriod, 
                  HomeSubRegion, TRIPNO, LINKMODE, TRAVTIME, TRIPPURP, 
                  participant_wt, CUMDIST, DESTLGA, ORIGLGA, HOMELGA) %>%
    ### Keep only if complete cases for MODE, DIST and TIME and participant_wt(I AM UNSURE WHY THERE ARE NA WEIGHTS)
    filter(!is.na(participant_wt))
  
  # pivot MODE,TIME,DIST from wide to long and create a stop number
  trips_melbourne_mode <- trips_melbourne %>%
    dplyr::select(TRIPID,MODE1:MODE9) %>%
    pivot_longer(MODE1:MODE9,names_to="STOP",values_to="MODE") %>%
    filter(!is.na(MODE)) %>%
    mutate(STOP=as.numeric(gsub("MODE", "",STOP)))
  trips_melbourne_time <- trips_melbourne %>%
    dplyr::select(TRIPID,TIME1:TIME9) %>%
    mutate_at(vars(starts_with("TIME")),as.numeric) %>%
    pivot_longer(TIME1:TIME9,names_to="STOP",values_to="TIME") %>%
    filter(!is.na(TIME)) %>%
    mutate(STOP=as.numeric(gsub("TIME", "",STOP)))
  trips_melbourne_dist <- trips_melbourne %>%
    dplyr::select(TRIPID,DIST1:DIST9) %>%
    mutate_at(vars(starts_with("DIST")),as.numeric) %>%
    pivot_longer(DIST1:DIST9,names_to="STOP",values_to="DIST") %>%
    filter(!is.na(DIST)) %>%
    mutate(STOP=as.numeric(gsub("DIST", "",STOP)))
  
  # merge all of the data back into a single long table.
  trips_melbourne_long <- trips_melbourne_tripid %>%
    inner_join(trips_melbourne_mode, by="TRIPID") %>%
    inner_join(trips_melbourne_time, by=c("TRIPID","STOP")) %>%
    inner_join(trips_melbourne_dist, by=c("TRIPID","STOP"))
  
  
  trips_melbourne <- trips_melbourne_long %>%
    ### Replace mode 13 and 14 with NA
    filter(MODE != "13" & MODE != "14") %>%
    ### Sort by person id
    dplyr::arrange(PERSID,TRIPID,STOP) %>%
    ### Name to match ITHIMR
    dplyr::rename(household_id=HHID,
                  age=AGE,
                  sex=SEX,
                  trip_id=TRIPID,
                  trip_mode=MODE,
                  trip_duration=TIME,
                  trip_distance=DIST,
                  trip_purpose=TRIPPURP,
                  day_type=DayType) %>%
    dplyr::mutate(participant_id=PERSID) %>%
    ### Separate participant id into year, hh_id and participant_id
    separate(participant_id, into=c(NA,"year",NA,"cluster_id","household_id",NA,
                                    "participant_id"), sep = c(1, 3, 4, 6, -3,-2)) %>%
    # assign a unique id for each trip
    mutate(trip_id=row_number())
  
  
  ### Create age groups to match with PA data
  trips_melbourne <- trips_melbourne %>%
    dplyr::mutate(age_cat = case_when(age <   5             ~  1,
                                      age >=  5 & age <=  9 ~  2,
                                      age >= 10 & age <= 14 ~  3,
                                      age >= 15 & age <= 17 ~  4, 
                                      age >= 18 & age <= 19 ~  5,
                                      age >= 20 & age <= 24 ~  6,
                                      age >= 25 & age <= 29 ~  7, 
                                      age >= 30 & age <= 34 ~  8, 
                                      age >= 35 & age <= 39 ~  9, 
                                      age >= 40 & age <= 44 ~ 10,
                                      age >= 45 & age <= 49 ~ 11, 
                                      age >= 50 & age <= 54 ~ 12, 
                                      age >= 55 & age <= 59 ~ 13, 
                                      age >= 60 & age <= 64 ~ 14, 
                                      age >= 65 & age <= 69 ~ 15,
                                      age >= 70 & age <= 74 ~ 16, 
                                      age >= 75 & age <= 79 ~ 17,
                                      age >= 80 & age <= 84 ~ 18,
                                      age >= 85             ~ 19))
  
  trips_melbourne <- trips_melbourne %>%
    #### Only keep adults to match NHS_pa data, drop categories 1,2 and 3
    dplyr::filter(age_cat>3) %>%
    ### Do not include age_cat as ithim is doing its own synthetic population and this causes issues to have age_cat
    dplyr::select(PERSID, cluster_id, household_id, participant_id, age, sex, year, trip_id,
                  trip_purpose, participant_wt, trip_mode, trip_duration, trip_distance, 
                  day_type) %>%
    dplyr::mutate(PERSID=tolower(PERSID)) %>%
    dplyr::mutate(year=as.numeric(year)) %>%
    dplyr::mutate(trip_purpose=tolower(trip_purpose)) %>%
    dplyr::mutate(day_type=tolower(day_type)) %>%
    dplyr::mutate(sex=case_when(sex=="M" ~ 'male', sex=="F" ~ 'female')) %>%
    dplyr::mutate(trip_mode=case_when(trip_mode=="Vehicle Driver" ~ 'car', 
                                      trip_mode=="Vehicle Passenger" ~ 'car', 
                                      trip_mode=="Taxi" ~ 'car', 
                                      trip_mode=="School Bus" ~ 'bus', 
                                      trip_mode=="Public Bus" ~ 'bus', 
                                      trip_mode=="Walking" ~ 'pedestrian',
                                      # if meeting none of these criteria, keep original value
                                      TRUE ~ tolower(trip_mode)))
  
  
  ### Create numeric id
  trips_melbourne <- trips_melbourne %>%
    rename(persid=PERSID) %>%
    group_by(persid) %>%
    mutate(participant_id=group_indices()) %>%
    ungroup()
  
  return(trips_melbourne)
}

# write.csv(trips_melbourne, "Data/Processed/trips_melbourne.csv", row.names=F, quote=F)
# write_rds(trips_melbourne, "Data/Processed/trips_melbourne.Rds")

