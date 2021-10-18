# loading libraries and functions -----------------------------------------
### Clean Global Environment
rm (list = ls())

### Install library
# library(devtools)
# devtools::install_local("~/Downloads/ITHIM-R-master")

### Avoid scientific notation
options(scipen=999)

# First, you need to convert the matrices to SA2 for ease of use.
# source("Scripts/convert_dot_matricies.R")
# outputs are in ./matricies folder

# distance=km, time=hours

# scenario_vehicle_measure
# scenario: base (baseline), pt.train (frequent trains), pt.full (frequent bus and trains)
# vehicle: car, pt.drive, pt.walk
# measure: count, distance, time

# SCRIPT LOCATION                   | FUNCTION            | OUTPUT
# Scripts/data_prep/synthetic_pop.R | calculateTravelData | Data/Processed/travel_data.csv
# Scripts/data_prep/trips_prep.R    | calculateVistaTrips | Data/processed/trips_melbourne.csv

suppressPackageStartupMessages(library(dplyr)) # for manipulating data
suppressPackageStartupMessages(library(tidyr)) # for manipulating data
suppressPackageStartupMessages(library(purrr)) # for nested dataframes

### Travel data people used in mslt_code to generate matched population
source("Scripts/data_prep/synthetic_pop.R")

calculateVistaTripsDOT <- function(hh_VISTA_location,person_VISTA_location,trip_VISTA_location) {
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
    mutate(across(c(WDPERSWGT,WEPERSWGT,WDTRIPWGT,WETRIPWGT), function(x) replace_na(as.numeric(x), 0))) %>%
    mutate(participant_wt = WDPERSWGT + WEPERSWGT) %>%
    mutate(trips_wt = WDTRIPWGT + WETRIPWGT) %>%
    dplyr::select(-WDPERSWGT,-WEPERSWGT, -WDTRIPWGT, -WETRIPWGT)
  
  ### Replace all character "N/A" with NA
  trips_melbourne[ trips_melbourne == "N/A" ] <- NA 
  
  # Identify different modes to PT (identify driving the rest will be assigned to walking)                            
  trips_pt.modes <- trips_melbourne %>%
    filter(if_any(starts_with("MODE"), ~ . %in% c("Public Bus", "School Bus", "Train", "Tram" ))) %>% 
    mutate(has.car = if_any(starts_with("MODE"), ~ . %in% c("Vehicle Driver", "Vehicle Passenger", "Taxi"))) %>% 
    mutate(pt.mode= ifelse(has.car, "pt.drive", "pt.walk")) %>% 
    dplyr::select(TRIPID, pt.mode)
  
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
    inner_join(trips_melbourne_dist, by=c("TRIPID","STOP")) %>% 
    left_join(trips_pt.modes, by="TRIPID") %>% # Join pt.mode to only pt leg of pt trips
    mutate(pt.mode = ifelse(MODE %in% c("Public Bus", "School Bus", "Train", "Tram" ), pt.mode, "NA"))
  
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
                  day_type, lga_name=ORIGLGA, pt.mode) %>%
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
    dplyr::mutate(day_type = as.factor(day_type))
  
  
  return(trips_melbourne)
}

chooseTrips <- function(base_choices,scen_car,scen_pt.drive,scen_pt.walk) {
  # base_choices = trips_melbourne_scenarios
  # scen_car = as.numeric(dot_base_mode[1]- trips_melbourne_proportions[1,2])
  # scen_pt.drive = as.numeric(dot_base_mode[2]- trips_melbourne_proportions[2,2])
  # scen_pt.walk = as.numeric(dot_base_mode[3]- trips_melbourne_proportions[3,2])
  
  # Count number of trips by mode: car, pt.walk and pt.drive (drive to station)
  base_car <- base_choices%>%filter(trip_mode_base=='car')%>%nrow()
  base_pt.drive <- base_choices%>%filter(trip_mode_base=='pt.drive')%>%nrow()
  base_pt.walk <- base_choices%>%filter(trip_mode_base=='pt.walk')%>%nrow()
  
  
  # trip numbers for each mode
  base_trips_car <- base_choices %>% filter(trip_mode_base=="car") %>% pull(row_id)
  base_trips_pt.drive <- base_choices %>% filter(trip_mode_base=="pt.drive") %>% pull(row_id)
  base_trips_pt.walk <- base_choices %>% filter(trip_mode_base=="pt.walk") %>% pull(row_id)
  
  
  removed_trips <- c()
  if(scen_car<0) removed_trips <- c(removed_trips, base_trips_car[(1+base_car+scen_car):base_car])
  if(scen_pt.drive<0) removed_trips <- c(removed_trips, base_trips_pt.drive[(1+base_pt.drive+scen_pt.drive):base_pt.drive])
  if(scen_pt.walk<0) removed_trips <- c(removed_trips, base_trips_pt.walk[(1+base_pt.walk+scen_pt.walk):base_pt.walk])
  
  if(length(removed_trips)==0) {
    choices <- base_choices %>%
      mutate(trip_mode_scen=trip_mode_base)
    return(choices)
  }
  
  trip_mode_scen <- c()
  if(scen_car>0) trip_mode_scen <- c(trip_mode_scen, rep("car",scen_car))
  if(scen_pt.drive>0) trip_mode_scen <- c(trip_mode_scen, rep("pt.drive",scen_pt.drive))
  if(scen_pt.walk>0) trip_mode_scen <- c(trip_mode_scen, rep("pt.walk",scen_pt.walk))
  
  scen_choices <- data.frame(row_id=sample(removed_trips, length(removed_trips), replace=F),
                             trip_mode_scen=trip_mode_scen,
                             stringsAsFactors=F)
  
  choices <- base_choices %>%
    left_join(scen_choices, by="row_id") %>%
    mutate(trip_mode_scen=ifelse(is.na(trip_mode_scen),trip_mode_base,trip_mode_scen))
  return(choices)
  
}

calculateScenarioTrips <- function(trips_melbourne,
                                   lga_df,
                                   output_location) {
  
  # trips_melbourne="Data/processed/trips_melbourne_dot.csv"
  # lga_df="Data/processed/DOT_df.csv"
  # output_location="./scenarios_dot"
  
  # in case the directory hasn't been made yet
  dir.create(output_location, recursive=TRUE, showWarnings=FALSE)
  dir.create(paste0(output_location,"/scenarioTrips"), recursive=TRUE, showWarnings=FALSE)
  dir.create(paste0(output_location,"/personTravel"), recursive=TRUE, showWarnings=FALSE)
  
  
  # in case it's a location and not a dataframe
  if(is.character(trips_melbourne)) {
    trips_melbourne <- read.csv(trips_melbourne,as.is=T,fileEncoding="UTF-8-BOM")
  }
  if(is.character(lga_df)) {
    lga_df <- read.csv(lga_df,as.is=T,fileEncoding="UTF-8-BOM")
  }
  
  print(paste0("trips_melbourne length: ",nrow(trips_melbourne)))
  print(paste0("lga_df length: ",nrow(lga_df)))
  
  # Table format: scenario, mode, distance, time
  # Mode is either car, pt.drive (driving to PT), or pt.walk (walking to PT)
  scenario_time_and_distance <- lga_df %>%
    dplyr::select(scenario,mode,distance,time) %>%
    # time in hours
    mutate(time=distance/(1.4*3.6))
  
  trips_melbourne <- trips_melbourne %>%
    dplyr::mutate(trip_mode=case_when(trip_mode=="pedestrian" ~ 'walking', 
                                      trip_mode=="bus" ~ 'public.transport', 
                                      trip_mode=="tram" ~ 'public.transport', 
                                      trip_mode=="train" ~ 'public.transport',
                                      trip_mode=="motorcycle" ~ 'other',
                                      TRUE ~ tolower(trip_mode))) %>% ## Add age groups to facilitate selection above and matching  
    dplyr::rename(trip_mode_base     = trip_mode,
                  trip_duration_base = trip_duration,
                  trip_distance_base = trip_distance) %>%
    dplyr::mutate(trip_duration_base = trip_duration_base/60) %>% 
    dplyr::filter(trip_distance_base!=0)
  
  # all the trips we don't change (i.e., non car/PT work commutes)
  trips_melbourne_unchanged <- trips_melbourne %>%
    filter(!trip_mode_base%in%c('car','public.transport') | trip_purpose!="Work") %>% 
    mutate(trip_mode_scen=trip_mode_base,
           trip_duration_scen=trip_duration_base,
           trip_distance_scen=trip_distance_base) %>%
    mutate(dist_cat=NA) %>%
    dplyr::select(
      persid,cluster_id,household_id,participant_id,age,sex,year,trip_id,trip_purpose,
      participant_wt,trips_wt,trip_mode_base,trip_duration_base,trip_distance_base,
      day_type,trip_id_2,age_group,dist_cat,trip_mode_scen,trip_distance_scen,
      trip_duration_scen
    )
  
  # work trips --------------------------------------------------------------
  trips_melbourne_scenarios_car <- trips_melbourne %>%
    filter(trip_mode_base%in%c('car') & trip_purpose=="Work") %>% 
    dplyr::select(-pt.mode)
  
  trips_melbourne_scenarios_pt <- trips_melbourne %>%
    filter(trip_mode_base%in%c('public.transport') & trip_purpose=="Work") %>%
    # Assign the public transport to pt.walk pt.drive 
    mutate(trip_mode_base= pt.mode) %>% 
    dplyr::select(-pt.mode)
  
  trips_melbourne_scenarios <- bind_rows(trips_melbourne_scenarios_car,
                                         trips_melbourne_scenarios_pt) %>%
    mutate(row_id=row_number())
  
  
  getModeCount <- function(total_pop,percent_car,percent_pt.drive,percent_pt.walk) {
    # total_pop=1000;percent_car=0.89246215;percent_pt.drive=0.03018683;percent_pt.walk=0.07735101
    count_car=round(percent_car*total_pop)
    count_pt.drive=round(percent_pt.drive*total_pop)
    count_pt.walk=round(percent_pt.walk*total_pop)
    new_pop=count_car+count_pt.drive+count_pt.walk
    count_car=count_car+(total_pop-new_pop)
    return(c(count_car,count_pt.drive,count_pt.walk))
    # result<-data.frame(trip_mode_scenario=c("car","pt.drive","pt.walk"),
    #                    count_scenario=c(count_car,count_pt.drive,count_pt.walk))
  }
  
  ## Make DoT baseline and scenarios (VISTA distance)
  # DOT base (VISTA distance)
  trips_melbourne_proportions <- trips_melbourne_scenarios %>%
    group_by(trip_mode_base) %>%
    summarise(count_baseline=n())
  
  dot_base_mode <- getModeCount(sum(trips_melbourne_proportions$count_baseline),
                                0.89246215,0.03018683,0.07735101)
  
  base_dot <- chooseTrips(base_choices = trips_melbourne_scenarios,
                          scen_car = as.numeric(dot_base_mode[1]- trips_melbourne_proportions[1,2]),
                          scen_pt.drive = as.numeric(dot_base_mode[2]- trips_melbourne_proportions[2,2]),
                          scen_pt.walk = as.numeric(dot_base_mode[3]- trips_melbourne_proportions[3,2]))
  
  # join on dot_baseline scenario's distance and duration. (trip_duration_scen,trip_distance_scen)
  # hardcode distance and duration. It's always 1km for pt.drive,  1.5km for pt.walk (and then work out the time for that).
  # remove trip_mode_base,trip_duration_base,trip_distance_base, rename the scenario ones as base.
  base_dot <-base_dot %>%
    dplyr::mutate(trip_distance_scen=case_when(trip_mode_scen=="car" ~ 10, 
                                               trip_mode_scen=="pt.drive" ~ 0.73, 
                                               trip_mode_scen=="pt.walk" ~ 1.25)) %>%
    mutate(trip_duration_scen=trip_distance_scen/(1.4*3.6)) %>%
    mutate(trip_mode_base=trip_mode_scen,
           trip_duration_base=trip_duration_scen,
           trip_distance_base=trip_distance_scen) %>%
    dplyr::select(-trip_mode_scen,-trip_duration_scen,-trip_distance_scen)
  
  # PT full (VISTA distance)
  pt_full_mode <- getModeCount(sum(dot_base_mode),
                               0.86940174,0.04165867,0.08893959)
  
  pt_full <- chooseTrips(base_choices = base_dot,
                         scen_car = as.numeric(pt_full_mode[1]-  dot_base_mode[1]),
                         scen_pt.drive = as.numeric(pt_full_mode[2]- dot_base_mode[2]),
                         scen_pt.walk = as.numeric(pt_full_mode[3]- dot_base_mode[3]))
  
  pt_full <- pt_full %>%
    dplyr::mutate(trip_distance_scen=case_when(trip_mode_scen=="car" ~ 10, 
                                               trip_mode_scen=="pt.drive" ~ 0.73, 
                                               trip_mode_scen=="pt.walk" ~ 1.25)) %>%
    mutate(trip_duration_scen=trip_distance_scen/(1.4*3.6))
  
  # PT train (VISTA distance)
  pt_train_mode <- getModeCount(sum(dot_base_mode),
                                0.87431739,0.04269600,0.08298661)
  
  pt_train <- chooseTrips(base_choices = base_dot,
                          scen_car = as.numeric(pt_train_mode[1]-  dot_base_mode[1]),
                          scen_pt.drive = as.numeric(pt_train_mode[2]- dot_base_mode[2]),
                          scen_pt.walk = as.numeric(pt_train_mode[3]- dot_base_mode[3]))
  
  pt_train <- pt_train %>%
    dplyr::mutate(trip_distance_scen=case_when(trip_mode_scen=="car" ~ 10, 
                                               trip_mode_scen=="pt.drive" ~ 0.73, 
                                               trip_mode_scen=="pt.walk" ~ 1.25)) %>%
    mutate(trip_duration_scen=trip_distance_scen/(1.4*3.6)) 
  
  ## Make final scenarios (VISTA distance)
  # PT full (VISTA distance)
  pt_full_scenario_final <- bind_rows(
    trips_melbourne_unchanged,
    pt_full) %>% 
    mutate(dist_cat=NA) %>%
    dplyr::mutate(trip_duration_base_hrs = trip_duration_base * 7) %>%
    dplyr::mutate(trip_duration_scen_hrs = trip_duration_scen * 7)
  
  # PT train (VISTA distance)
  pt_train_scenario_final <- bind_rows(
    trips_melbourne_unchanged,
    pt_train) %>% 
    mutate(dist_cat=NA) %>%
    dplyr::mutate(trip_duration_base_hrs = trip_duration_base * 7) %>%
    dplyr::mutate(trip_duration_scen_hrs = trip_duration_scen * 7)
  
  write.csv(pt_full_scenario_final, paste0(output_location,"/scenarioTrips/dotFull(VISTA).csv"), row.names=F, quote=T)
  write.csv(pt_train_scenario_final, paste0(output_location,"/scenarioTrips/dotTrain(VISTA).csv"), row.names=F, quote=T)
  
  # need to change pt.walk and pt.drive to walking
  pt_full_scenario_walking <- pt_full_scenario_final %>%
    mutate(trip_mode_base=ifelse(trip_mode_base%in%c("pt.walk","pt.drive"),"walking",trip_mode_base)) %>%
    mutate(trip_mode_scen=ifelse(trip_mode_scen%in%c("pt.walk","pt.drive"),"walking",trip_mode_scen))
  
  pt_train_scenario_walking <- pt_train_scenario_final %>%
    mutate(trip_mode_base=ifelse(trip_mode_base%in%c("pt.walk","pt.drive"),"walking",trip_mode_base)) %>%
    mutate(trip_mode_scen=ifelse(trip_mode_scen%in%c("pt.walk","pt.drive"),"walking",trip_mode_scen))
  
  ### 2.1) Create data set with VISTA people and allocate baseline and scenario trips to them
  persons_travel_pt_full <- calculatePersonsTravelScenario(
    travel_data_location="./Data/processed/travel_data.csv", ## BZ: generated in script runInputsMelbourneExposure.R 
    scenario_location=pt_full_scenario_walking)
  write.csv(persons_travel_pt_full, paste0(output_location,"/personTravel/dotFull(VISTA).csv"), row.names=F, quote=T)
  
  persons_travel_pt_train <- calculatePersonsTravelScenario(
    travel_data_location="./Data/processed/travel_data.csv", ## BZ: generated in script runInputsMelbourneExposure.R 
    scenario_location=pt_train_scenario_walking)
  write.csv(persons_travel_pt_train, paste0(output_location,"/personTravel/dotTrain(VISTA).csv"), row.names=F, quote=T)
  
  #### 2.2) Match NHS people to VISTA people based on age, sex, ses, work status and whether they walk for transport
  persons_matched_pt_full <- calculatePersonsMatch(
    pa_location="./Data/processed/persons_pa.csv", ## BZ: generated in script runInputsMelbourneExposure.R 
    persons_travel_location=persons_travel_pt_full  #"Data/processed/persons_travel.csv"
  ) %>% dplyr::mutate(scen="dotFull") %>% 
    dplyr::filter(work_status == "employed")
  write.csv(persons_matched_pt_full, paste0(output_location,"/dotFull(VISTA).csv"), row.names=F, quote=T)
  
  persons_matched_pt_train <- calculatePersonsMatch(
    pa_location="./Data/processed/persons_pa.csv", ## BZ: generated in script runInputsMelbourneExposure.R 
    persons_travel_location=persons_travel_pt_train  #"Data/processed/persons_travel.csv"
  ) %>% dplyr::mutate(scen="dotTrain") %>% 
    dplyr::filter(work_status == "employed")
  write.csv(persons_matched_pt_train, paste0(output_location,"/dotTrain(VISTA).csv"), row.names=F, quote=T)
  
  
  ## Make DoT baseline and scenarios (DOT distance) 
  # Change distance and time to DOT 
  pt_full_DOT <- pt_full %>%
    dplyr::mutate(trip_distance_base=case_when(trip_mode_base=="car" ~ 9.406, 
                                               trip_mode_base=="pt.drive" ~ 1.351, 
                                               trip_mode_base=="pt.walk" ~ 1.163)) %>%
    dplyr::mutate(trip_distance_scen=case_when(trip_mode_scen=="car" ~ 9.443, 
                                               trip_mode_scen=="pt.drive" ~ 0.867, 
                                               trip_mode_scen=="pt.walk" ~ 1.201)) %>%
    mutate(trip_duration_base=trip_distance_scen/(1.4*3.6),
           trip_duration_scen=trip_distance_scen/(1.4*3.6))
  
  
  pt_train_DOT <- pt_train %>%
    dplyr::mutate(trip_distance_base=case_when(trip_mode_base=="car" ~ 9.406, 
                                               trip_mode_base=="pt.drive" ~ 1.351, 
                                               trip_mode_base=="pt.walk" ~ 1.163)) %>%
    dplyr::mutate(trip_distance_scen=case_when(trip_mode_scen=="car" ~ 9.444, 
                                               trip_mode_scen=="pt.drive" ~ 0.891, 
                                               trip_mode_scen=="pt.walk" ~ 1.234)) %>%
    mutate(trip_duration_base=trip_distance_scen/(1.4*3.6),
           trip_duration_scen=trip_distance_scen/(1.4*3.6))
  
  ## Make final scenarios (DOT distance)
  # PT full (DOT distance)
  pt_full_scenario_final_DOT <- bind_rows(
    trips_melbourne_unchanged,
    pt_full_DOT) %>% 
    mutate(dist_cat=NA) %>%
    dplyr::mutate(trip_duration_base_hrs = trip_duration_base * 7) %>%
    dplyr::mutate(trip_duration_scen_hrs = trip_duration_scen * 7)
  
  # PT train (DOT distance)
  pt_train_scenario_final_DOT <- bind_rows(
    trips_melbourne_unchanged,
    pt_train_DOT) %>% 
    mutate(dist_cat=NA) %>%
    dplyr::mutate(trip_duration_base_hrs = trip_duration_base * 7) %>%
    dplyr::mutate(trip_duration_scen_hrs = trip_duration_scen * 7)
  
  write.csv(pt_full_scenario_final_DOT, paste0(output_location,"/scenarioTrips/dotFull(DOT).csv"), row.names=F, quote=T)
  write.csv(pt_train_scenario_final_DOT, paste0(output_location,"/scenarioTrips/dotTrain(DOT).csv"), row.names=F, quote=T)
  
  # need to change pt.walk and pt.drive to walking
  pt_full_scenario_walking_DOT <- pt_full_scenario_final_DOT %>%
    mutate(trip_mode_base=ifelse(trip_mode_base%in%c("pt.walk","pt.drive"),"walking",trip_mode_base)) %>%
    mutate(trip_mode_scen=ifelse(trip_mode_scen%in%c("pt.walk","pt.drive"),"walking",trip_mode_scen))
  
  pt_train_scenario_walking_DOT <- pt_train_scenario_final_DOT %>%
    mutate(trip_mode_base=ifelse(trip_mode_base%in%c("pt.walk","pt.drive"),"walking",trip_mode_base)) %>%
    mutate(trip_mode_scen=ifelse(trip_mode_scen%in%c("pt.walk","pt.drive"),"walking",trip_mode_scen))
  
  ### 2.1) Create data set with VISTA people and allocate baseline and scenario trips to them
  persons_travel_pt_full_DOT <- calculatePersonsTravelScenario(
    travel_data_location="./Data/processed/travel_data.csv", ## BZ: generated in script runInputsMelbourneExposure.R 
    scenario_location=pt_full_scenario_walking_DOT)
  write.csv(persons_travel_pt_full_DOT, paste0(output_location,"/personTravel/dotFull(DOT).csv"), row.names=F, quote=T)
  
  persons_travel_pt_train_DOT <- calculatePersonsTravelScenario(
    travel_data_location="./Data/processed/travel_data.csv", ## BZ: generated in script runInputsMelbourneExposure.R 
    scenario_location=pt_train_scenario_walking_DOT)
  write.csv(persons_travel_pt_train_DOT, paste0(output_location,"/personTravel/dotTrain(DOT).csv"), row.names=F, quote=T)
  
  #### 2.2) Match NHS people to VISTA people based on age, sex, ses, work status and whether they walk for transport
  persons_matched_pt_full_DOT <- calculatePersonsMatch(
    pa_location="./Data/processed/persons_pa.csv", ## BZ: generated in script runInputsMelbourneExposure.R 
    persons_travel_location=persons_travel_pt_full_DOT  #"Data/processed/persons_travel.csv"
  ) %>% dplyr::mutate(scen="dotFull") %>% 
    dplyr::filter(work_status == "employed")
  write.csv(persons_matched_pt_full_DOT, paste0(output_location,"/dotFull(DOT).csv"), row.names=F, quote=T)
  
  persons_matched_pt_train_DOT <- calculatePersonsMatch(
    pa_location="./Data/processed/persons_pa.csv", ## BZ: generated in script runInputsMelbourneExposure.R 
    persons_travel_location=persons_travel_pt_train_DOT  #"Data/processed/persons_travel.csv"
  ) %>% dplyr::mutate(scen="dotTrain") %>% 
    dplyr::filter(work_status == "employed")
  write.csv(persons_matched_pt_train_DOT, paste0(output_location,"/dotTrain(DOT).csv"), row.names=F, quote=T)
  
}

# calculating results -----------------------------------------------------

### Trips data used in mslt_code and ithim-r
# source("Scripts/data_prep/trips_prep.R")
trips_melbourne <- calculateVistaTripsDOT(
  hh_VISTA_location="Data/Travelsurvey/VISTA12-18/H_VISTA_1218_V1.csv",
  person_VISTA_location="Data/Travelsurvey/VISTA12-18/P_VISTA1218_V1.csv",
  trip_VISTA_location="Data/Travelsurvey/VISTA12-18/T_VISTA1218_V1.csv"
)
write.csv(trips_melbourne, "Data/processed/trips_melbourne_dot.csv", row.names=F, quote=F)

calculateScenarioTrips(
  trips_melbourne="Data/processed/trips_melbourne_dot.csv",
  lga_df="Data/processed/DOT_df.csv",
  output_location="./scenarios_dot"
)
