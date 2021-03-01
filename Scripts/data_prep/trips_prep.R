##### Generate trips data 
suppressPackageStartupMessages(library(dplyr)) # for manipulating data
suppressPackageStartupMessages(library(tidyr)) # for pivoting data


options(scipen=999)

### 1) calculateVistaTrips: each row is a stage
###  data used for: ithimr (https://github.com/ITHIM/ITHIM-R), scenarios DoT Melbourne and descriptives stages

### 2) calculateVistaTripsDescriptives: each row is main mode of travel
#### data used for descriptive statistics only


### 3) Calculate speed walking and cycling by age and sex (quantiles and mean (sd))

######################################  1) calculateVistaTrips ############################################################
### Columns ITHIMR: 
#### -One row per trip (or stage of trip)
#### -Minimal columns: participant_id, age, sex, trip_mode, trip_duration (or trip_distance)
#### -Other columns: stage_mode, stage_duration (or stage_distance)
### Should include people without trips. FOR NOW INCLUDES ONLY THOSE WITH TRIPS, NOT SURE IF IT INCLUDES PEOPLE WITHOUT TRIPS, I DELETED SOME NAS WHICH MAY REPRESENT PEOPLE
### WITHOUT TRIPS
### VISTA Survey for Melbourne greater area for time period 2017-18
### One day (weekday or weekend)
### Final product trips_melbourne where each row is a stage



calculateVistaTrips <- function(hh_VISTA_location,person_VISTA_location,trip_VISTA_location) {
   # hh_VISTA_location="Data/Travelsurvey/VISTA12-18/H_VISTA_1218_V1.csv"
   # person_VISTA_location="Data/Travelsurvey/VISTA12-18/P_VISTA1218_V1.csv"
   # trip_VISTA_location="Data/Travelsurvey/VISTA12-18/T_VISTA1218_V1.csv"



  hh_VISTA <- read.csv(hh_VISTA_location,as.is=T, fileEncoding="UTF-8-BOM") %>%
    dplyr::select(HHID,SurveyPeriod,DayType,WDHHWGT,WEHHWGT,HomeSubRegion,HOMELGA) %>%
    filter(HHID!="") # some rows were completely blank
  person_VISTA <- read.csv(person_VISTA_location,as.is=T, fileEncoding="UTF-8-BOM") %>%
    dplyr::select(PERSID,HHID,AGE,SEX,WDPERSWGT,WEPERSWGT)
  trip_VISTA <- read.csv(trip_VISTA_location,as.is=T, fileEncoding="UTF-8-BOM") %>%

    dplyr::select(TRIPID,PERSID,HHID,TRIPNO,CUMDIST,TRAVTIME,ORIGLGA,DESTLGA,
                  TRIPPURP,LINKMODE,
                  MODE1,MODE2,MODE3,MODE4,MODE5,MODE6,MODE7,MODE8,MODE9,
                  DIST1,DIST2,DIST3,DIST4,DIST5,DIST6,DIST7,DIST8,DIST9,
                  TIME1,TIME2,TIME3,TIME4,TIME5,TIME6,TIME7,TIME8,TIME9,
                  WDTRIPWGT,WETRIPWGT)
    

  hh_person <- left_join(person_VISTA, hh_VISTA, by = "HHID")
  
  
  trips_melbourne  <- left_join(trip_VISTA, hh_person, by = c("PERSID","HHID") ) %>%
    dplyr::filter(SurveyPeriod == "2017-18" &
                    (HomeSubRegion != "Geelong" | HomeSubRegion != "Other")) %>%
    dplyr::select(HHID, PERSID, AGE, SEX, TRIPID, DayType, SurveyPeriod, 
                  HomeSubRegion, TRIPNO, LINKMODE, MODE1, MODE2, MODE3, MODE4, 
                  MODE5, MODE6, MODE7, MODE8, MODE9, TIME1, TIME2, TIME3, TIME4, 
                  TIME5, TIME6, TIME7, TIME8, TIME9, DIST1, DIST2, DIST3, DIST4,
                  DIST5, DIST6, DIST7, DIST8, DIST9, TRAVTIME, TRIPPURP, 
                  WDPERSWGT, WEPERSWGT, CUMDIST, DESTLGA, ORIGLGA, HOMELGA, WDTRIPWGT,WETRIPWGT) %>% 
    dplyr::filter(AGE>15) %>%
    rowwise() %>% # want to sum across rows, not down columns %>%
    mutate(participant_wt = sum(as.numeric(WDPERSWGT),as.numeric(WEPERSWGT),na.rm=T)) %>%
    mutate(trips_wt = sum(as.numeric(WDTRIPWGT),as.numeric(WETRIPWGT),na.rm=T)) %>%
    dplyr::select(-WDPERSWGT,-WEPERSWGT, -WDTRIPWGT, -WETRIPWGT) %>%
    as.data.frame()
  
  ### Replace all character "N/A" with NA
  trips_melbourne[ trips_melbourne == "N/A" ] <- NA 
  
  # all of the data that isn't MODE,TIME,DIST
  trips_melbourne_tripid <- trips_melbourne %>%
    dplyr::select(HHID, PERSID, AGE, SEX, TRIPID, DayType, SurveyPeriod, 
                  HomeSubRegion, TRIPNO, LINKMODE, TRAVTIME, TRIPPURP, 
                  participant_wt, trips_wt, CUMDIST, DESTLGA, ORIGLGA, HOMELGA) %>%
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
                  trip_purpose, participant_wt, trips_wt, trip_mode, trip_duration, trip_distance, 
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
    dplyr::rename(persid=PERSID) %>%
    group_by(persid) %>%
    mutate(participant_id=group_indices()) %>%
    ungroup()
  ### Create number of trips per person
  trips_melbourne <- trips_melbourne %>%
  group_by(persid) %>%
    dplyr::mutate(trip_id_2 = 1:dplyr::n()) %>%
    ungroup() %>% ## Add age groups c( "15 to 19", "20 to 39", "40 to 64", "65 plus", "all"),
    mutate(age_group = as.factor(case_when(age >=  15 & age <=  19 ~  "15 to 19",
                                           age >=  20 & age <=  39 ~  "20 to 39",
                                           age >= 40 & age <= 64 ~  "40 to 64",
                                           age >= 65             ~ "65 plus"))) %>%
    dplyr::mutate(sex =as.factor(sex)) %>%
    dplyr::mutate(age_group=as.factor(age_group)) %>%
    dplyr::mutate(dist_cat=as.factor(case_when(trip_distance < 1 ~ "<1km",
                                               trip_distance >= 1 & trip_distance <= 2 ~ "1-2km", 
                                                trip_distance <= 5 & trip_distance > 2 ~ "3-5km", 
                                                trip_distance <=10 & trip_distance > 5 ~ "6-10km",
                                                trip_distance > 10 ~ ">10km"))) %>%
    dplyr::mutate(trip_purpose=as.factor(case_when(trip_purpose=="social" ~ "Leisure",
                                                   trip_purpose=="recreational" ~ "Leisure",
                                                   trip_purpose=="buy something" ~ "Shopping",
                                                   trip_purpose=="education" ~ "Education",
                                                   trip_purpose=="pick-up or drop-off someone"  ~ "Other",
                                                   trip_purpose=="pick-up or deliver something"  ~ "Other",
                                                   trip_purpose=="unknown purpose (at start of day)" ~ "Other",
                                                   trip_purpose=="other purpose" ~ "Other",
                                                   trip_purpose=="at or go Home"  ~ "Other",
                                                   trip_purpose=="change mode"  ~ "Other",
                                                   trip_purpose=="accompany someone"   ~ "Other",
                                                   trip_purpose=="personal business"  ~ "Other",
                                                   trip_purpose=="at or go home"  ~ "Other",
                                                   trip_purpose=="not stated"   ~ "Other",
                                                   trip_purpose=="work related"   ~ "Work",
                                                   TRUE ~ trip_purpose))) %>%
    
    dplyr::mutate(day_type =as.factor(day_type))
  
  
  return(trips_melbourne)
}

######################################  2) calculateVistaTripsDescriptives ############################################################
### Uses LINMODE (main mode of trip) instead of stages of the trip.
calculateTripsDescriptives <- function(hh_VISTA_location,person_VISTA_location,trip_VISTA_location) {
   
  # hh_VISTA_location="Data/Travelsurvey/VISTA12-18/H_VISTA_1218_V1.csv"
  # person_VISTA_location="Data/Travelsurvey/VISTA12-18/P_VISTA1218_V1.csv"
  # trip_VISTA_location="Data/Travelsurvey/VISTA12-18/T_VISTA1218_V1.csv"

  hh_VISTA <- read.csv(hh_VISTA_location,as.is=T, fileEncoding="UTF-8-BOM") %>%
    dplyr::select(HHID,SurveyPeriod,DayType,WDHHWGT,WEHHWGT,HomeSubRegion,HOMELGA) %>%
    filter(HHID!="") # some rows were completely blank
  person_VISTA <- read.csv(person_VISTA_location,as.is=T, fileEncoding="UTF-8-BOM") %>%
    dplyr::select(PERSID,HHID,AGE,SEX,WDPERSWGT,WEPERSWGT)
  trip_VISTA <- read.csv(trip_VISTA_location,as.is=T, fileEncoding="UTF-8-BOM") %>%
    
    dplyr::select(TRIPID,PERSID,HHID,TRIPNO,CUMDIST,TRAVTIME,ORIGLGA,DESTLGA,
                  TRIPPURP,LINKMODE,
                  MODE1,MODE2,MODE3,MODE4,MODE5,MODE6,MODE7,MODE8,MODE9,
                  DIST1,DIST2,DIST3,DIST4,DIST5,DIST6,DIST7,DIST8,DIST9,
                  TIME1,TIME2,TIME3,TIME4,TIME5,TIME6,TIME7,TIME8,TIME9,
                  WDTRIPWGT,WETRIPWGT) 
  
  trip_VISTA$WDTRIPWGT <- as.numeric(trip_VISTA$WDTRIPWGT)
  
  trip_VISTA$WETRIPWGT <- as.numeric(trip_VISTA$WETRIPWGT)
  
  hh_person <- left_join(person_VISTA, hh_VISTA, by = "HHID")
  
  
  trips_melbourne  <- left_join(trip_VISTA, hh_person, by = c("PERSID","HHID") ) %>%
    dplyr::filter(SurveyPeriod == "2017-18" &
                    (HomeSubRegion != "Geelong" | HomeSubRegion != "Other")) %>%
    dplyr::select(HHID, PERSID, AGE, SEX, TRIPID, DayType, SurveyPeriod, 
                  HomeSubRegion, TRIPNO, LINKMODE, MODE1, MODE2, MODE3, MODE4, 
                  MODE5, MODE6, MODE7, MODE8, MODE9, TIME1, TIME2, TIME3, TIME4, 
                  TIME5, TIME6, TIME7, TIME8, TIME9, DIST1, DIST2, DIST3, DIST4,
                  DIST5, DIST6, DIST7, DIST8, DIST9, TRAVTIME, TRIPPURP, 
                  WDPERSWGT, WEPERSWGT, CUMDIST, DESTLGA, ORIGLGA, HOMELGA, WDTRIPWGT,WETRIPWGT) %>% 
    dplyr::filter(AGE>15) %>%
    rowwise() %>% # want to sum across rows, not down columns %>%
    mutate(trips_wt = sum(as.numeric(WDTRIPWGT),as.numeric(WETRIPWGT),na.rm=T)) %>%  ## Add age groups
    mutate(age_group = as.factor(case_when(AGE >=  15 & AGE <=  19 ~  "15 to 19",
                                            AGE >=  20 & AGE <=  39 ~  "20 to 39",
                                            AGE >= 40 & AGE <= 64 ~  "40 to 64",
                                            AGE >= 65  ~ "65 plus"))) %>%
    dplyr::rename(sex=SEX) %>%
    mutate(sex=case_when(sex=="M" ~ 'male', sex=="F" ~ 'female')) %>% # group modes as per trips file
    dplyr::mutate(trip_mode=as.factor(case_when(LINKMODE=="Vehicle Driver" ~ 'car', 
                                               LINKMODE=="Vehicle Passenger" ~ 'car', 
                                               LINKMODE=="Taxi" ~ 'car', 
                                               LINKMODE=="School Bus" ~ 'public.transport', 
                                               LINKMODE=="Public Bus" ~ 'public.transport', 
                                               LINKMODE=="Train" ~ 'public.transport',
                                               LINKMODE=="Tram" ~ 'public.transport',
                                               LINKMODE=="Motorcycle" ~ 'other',
                                               # if meeting none of these criteria, keep original value
                                               TRUE ~ tolower(LINKMODE)))) %>%
    dplyr::mutate(day_type =as.factor(DayType)) %>%
    dplyr::mutate(sex =as.factor(sex)) %>%
    dplyr::mutate(age_group=as.factor(age_group)) %>%
    dplyr::mutate(dist_cat=as.factor(case_when(CUMDIST < 1 ~ "<1km",
                                                CUMDIST >= 1 & CUMDIST <= 2 ~ "1-2km", 
                                                CUMDIST <= 5 & CUMDIST > 2 ~ "3-5km", 
                                                CUMDIST <=10 & CUMDIST > 5 ~ "6-10km",
                                                CUMDIST > 10 ~ ">10km"))) %>%
    dplyr::mutate(TRIPPURP=as.factor(case_when(TRIPPURP=="Social" ~ "Leisure",
                                                   TRIPPURP=="Recreational" ~ "Leisure",
                                                   TRIPPURP=="Buy Something" ~ "Shopping",
                                                   TRIPPURP=="Education" ~ "Education",
                                                   TRIPPURP=="Pick-up or Drop-off Someone"  ~ "Other",
                                                   TRIPPURP=="Pick-up or Deliver Something"  ~ "Other",
                                                   TRIPPURP=="Unknown purpose (at start of day)" ~ "Other",
                                                   TRIPPURP=="Other Purpose" ~ "Other",
                                                   TRIPPURP=="At or Go Home"  ~ "Other",
                                                   TRIPPURP=="Change Mode"  ~ "Other",
                                                   TRIPPURP=="Accompany Someone"   ~ "Other",
                                                   TRIPPURP=="Personal Business"  ~ "Other",
                                                   TRIPPURP=="Not Stated"   ~ "Other",
                                                   TRIPPURP=="Work Related"   ~ "Work",
                                                   TRUE ~ TRIPPURP))) %>%
    dplyr::rename(trip_distance = CUMDIST, trip_duration = TRAVTIME, trip_purpose = TRIPPURP, persid=PERSID, trip_id_2=TRIPNO)
  
  return(trips_melbourne)
  }

######################################### 3) Calculate speeds walking and cycling ##################################################
#### To calculate time spent walking and cycling
CalculateAgeSexSpeed <- function(in_data){

# in_data="Data/processed/trips_melbourne.csv"

trips_melbourne <- read.csv(in_data,as.is=T,fileEncoding="UTF-8-BOM") ## Add age groups to facilitate selection above and matching  

### Use weighted data


#### SPEEDs by age and sex groups
SPEED_WALK <- dplyr::filter(trips_melbourne, trip_mode == "pedestrian") %>%
  mutate(speed_walk=trip_distance*60/trip_duration)

# Exclude 0 speed, some values have time but not distance

SPEED_WALK <- dplyr::filter(SPEED_WALK, trip_distance != 0)

# Check distribution
density_walk <- ggplot(SPEED_WALK, aes(speed_walk)) + geom_density()
# density_walk


cum_density_walk <- ggplot(SPEED_WALK, aes(speed_walk)) + stat_ecdf(geom = "step")
# cum_density_walk

SPEED_WALK <-  SPEED_WALK  %>%
  srvyr::as_survey_design(weights = trips_wt)

SPEED_CYCLE <- dplyr::filter(trips_melbourne, trip_mode == "bicycle") %>%
  mutate(speed_cycle=trip_distance*60/trip_duration)

# Exclude 0 speed, some values have time but not distance

SPEED_CYCLE <- dplyr::filter(SPEED_CYCLE, trip_distance != 0)
# Check distribution
density_cycle <- ggplot(SPEED_CYCLE, aes(speed_cycle)) + geom_density()
# density_cycle

cum_density_cycle <- ggplot(SPEED_CYCLE, aes(speed_cycle)) + stat_ecdf(geom = "step")
# cum_density_cycle

SPEED_CYCLE <-  SPEED_CYCLE  %>%
  srvyr::as_survey_design(weights = trips_wt)

### Calculate weighted statistics

SPEED_WALK <- SPEED_WALK %>% 
  group_by(sex, age_group,
           .drop = FALSE) %>%
  dplyr::summarize(mean= srvyr::survey_mean(speed_walk),
                   quantiles= srvyr::survey_quantile(speed_walk,  c(.25,.5,.75),ci=TRUE)) %>% 
  mutate(activity = "walking")

SPEED_CYCLE <- SPEED_CYCLE %>% 
  group_by(sex, age_group,
           .drop = FALSE) %>%
  dplyr::summarize(mean= srvyr::survey_mean(speed_cycle),
                   quantiles= srvyr::survey_quantile(speed_cycle,  c(.25,.5,.75),ci=TRUE)) %>% 
                     mutate(activity = "bicycle")

SPEEDS <- rbind(SPEED_WALK, SPEED_CYCLE)

return(SPEEDS)
}

