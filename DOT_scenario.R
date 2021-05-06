# loading libraries and functions -----------------------------------------


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
                  day_type, lga_name=ORIGLGA) %>%
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
    # dplyr::mutate(dist_cat=as.factor(case_when(trip_distance < 1 ~ "<1km",
    #                                            trip_distance >= 1 & trip_distance <= 2 ~ "1-2km", 
    #                                            trip_distance <= 5 & trip_distance > 2 ~ "3-5km", 
    #                                            trip_distance <=10 & trip_distance > 5 ~ "6-10km",
    #                                            trip_distance > 10 ~ ">10km"))) %>%
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

chooseBase <- function(lga_name,count,car,pt.drive,pt.walk) {
  # lga_name='Banyule (C)';count=67;car=62;pt.drive=1;pt.walk=4
  base_choices <- data.frame(lga_name=lga_name,
                             lga_id=sample(1:count, count, replace=F),
                             trip_mode_base=c(rep("car",car),rep("pt.drive",pt.drive),rep("pt.walk",pt.walk)),
                             stringsAsFactors=F) %>%
    arrange(lga_id)
  return(base_choices)
}

chooseTrips <- function(base_choices, lga_name,scen_car,scen_pt.walk,scen_pt.drive) {
  # lga_name=pt_full$lga_name[1]; scen_car=pt_full$scen_car[1]
  # scen_pt.walk=pt_full$scen_pt.walk[1]; scen_pt.drive=pt_full$scen_pt.drive[1]
  # base_choices=pt_full$data[[1]]
  
  base_car <- base_choices%>%filter(trip_mode_base=='car')%>%nrow()
  base_pt.walk <- base_choices%>%filter(trip_mode_base=='pt.walk')%>%nrow()
  base_pt.drive <- base_choices%>%filter(trip_mode_base=='pt.drive')%>%nrow()
  # count <- base_car+base_pt.walk+base_pt.drive
  
  # trip numbers for each mode
  base_trips_car <- base_choices %>% filter(trip_mode_base=="car") %>% pull(lga_id)
  base_trips_pt.walk <- base_choices %>% filter(trip_mode_base=="pt.walk") %>% pull(lga_id)
  base_trips_pt.drive <- base_choices %>% filter(trip_mode_base=="pt.drive") %>% pull(lga_id)
  
  removed_trips <- c()
  if(scen_car<0) removed_trips <- c(removed_trips, base_trips_car[(1+base_car+scen_car):base_car])
  if(scen_pt.walk<0) removed_trips <- c(removed_trips, base_trips_pt.walk[(1+base_pt.walk+scen_pt.walk):base_pt.walk])
  if(scen_pt.drive<0) removed_trips <- c(removed_trips, base_trips_pt.drive[(1+base_pt.drive+scen_pt.drive):base_pt.drive])
  
  if(length(removed_trips)==0) {
    choices <- base_choices %>%
      mutate(trip_mode_scen=trip_mode_base)
    return(choices)
  }
  
  trip_mode_scen <- c()
  if(scen_car>0) trip_mode_scen <- c(trip_mode_scen, rep("car",scen_car))
  if(scen_pt.walk>0) trip_mode_scen <- c(trip_mode_scen, rep("pt.walk",scen_pt.walk))
  if(scen_pt.drive>0) trip_mode_scen <- c(trip_mode_scen, rep("pt.drive",scen_pt.drive))
  
  scen_choices <- data.frame(lga_id=sample(removed_trips, length(removed_trips), replace=F),
                             trip_mode_scen=trip_mode_scen,
                             stringsAsFactors=F)
  
  choices <- base_choices %>%
    left_join(scen_choices, by="lga_id") %>%
    mutate(trip_mode_scen=ifelse(is.na(trip_mode_scen),trip_mode_base,trip_mode_scen))
  return(choices)
}

# attaches distance and time values from scenario_time_and_distance to an intervention
getDistanceAndTime <- function(df, scen_name) {
  # scen_name="pt.full"; df=pt_full_scenario
  
  bl <- scenario_time_and_distance %>%
    filter(scenario=='base') %>%
    dplyr::select(lga_name,trip_mode_base=mode,trip_duration_base=time,trip_distance_base=distance)
  scen <- scenario_time_and_distance %>%
    filter(scenario==scen_name) %>%
    dplyr::select(lga_name,trip_mode_scen=mode,trip_duration_scen=time,trip_distance_scen=distance)
  
  output <- df %>%
    inner_join(bl, by=c("lga_name","trip_mode_base")) %>%
    inner_join(scen, by=c("lga_name","trip_mode_scen"))
  
  return(output)
}

calculateScenarioTrips <- function(trips_melbourne,
                                   lga_df,
                                   output_location) {
  
  # trips_melbourne="Data/processed/trips_melbourne_dot.csv"
  # lga_df="Data/processed/lga_df.csv"
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
  
  scenario_data <- lga_df %>%
    dplyr::select(-lga_code) %>%
    pivot_longer(cols=base_car_count:pt.train_pt.walk_time.sd,
                 names_to=c("scenario", "mode", "measure"),
                 names_sep="_") %>%
    pivot_wider(id_cols=c(lga_name,scenario,mode),
                names_from=measure,
                values_from=value) %>%
    group_by(lga_name,scenario) %>%
    mutate(prop=count/sum(count,na.rm=T)) %>%
    ungroup() %>%
    # sum LGAs are named slightly differently
    mutate(lga_name=ifelse(lga_name=="Kingston (C) (Vic.)","Kingston (C)",lga_name)) %>%
    mutate(lga_name=ifelse(lga_name=="Melton (C)","Melton (S)",lga_name))
  
  scenario_proportions <- scenario_data %>%
    dplyr::select(lga_name,scenario,mode,prop) %>%
    group_by(lga_name,scenario) %>%
    arrange(-prop) %>%
    mutate(rank=row_number()) %>%
    ungroup() %>%
    arrange(lga_name,scenario,mode)
  # %>%
  #   pivot_wider(id_cols=lga_name,
  #               names_from=c(scenario,mode),
  #               values_from=c(prop,rank))
  
  # Table format: sa2, scenario, mode, distance, time
  # Mode is either car, pt.drive (driving to PT), or pt.walk (walking to PT)
  scenario_time_and_distance <- scenario_data %>%
    dplyr::select(lga_name,scenario,mode,distance=distance.mean,time=time.mean) %>%
    # time in hours
    mutate(time=time/60)
  
  trips_melbourne <- trips_melbourne %>%
    dplyr::mutate(trip_mode=case_when(trip_mode=="pedestrian" ~ 'walking', 
                                      trip_mode=="bus" ~ 'public.transport', 
                                      trip_mode=="tram" ~ 'public.transport', 
                                      trip_mode=="train" ~ 'public.transport',
                                      trip_mode=="motorcycle" ~ 'other',
                                      TRUE ~ tolower(trip_mode))) %>% ## Add age groups to facilitate selection above and matching  
    # only interested in trips that are within Greater Melbourne
    filter(lga_name%in%scenario_data$lga_name) %>%
    dplyr::rename(trip_mode_base     = trip_mode,
                  trip_duration_base = trip_duration,
                  trip_distance_base = trip_distance) %>%
    dplyr::mutate(trip_duration_base = trip_duration_base/60) %>% 
    dplyr::filter(trip_distance_base!=0)
  
  # trips_melbourne$trip_mode%>%unique()
  
  
  # setdiff(
  #   scenario_data$lga_name%>%unique()%>%sort(),
  #   trips_melbourne$lga_name%>%unique()%>%sort()
  # )
  # View(trips_melbourne_unchanged%>%group_by(lga_name)%>%summarise(count=n()))
  # View(scenario_data%>%
  #        select(lga_name,scenario,mode,prob)%>%
  #        pivot_wider(id_cols=c(lga_name,mode),names_from=scenario,values_from=prob)
  #      )
  
  
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
  trips_melbourne_scenarios <- trips_melbourne %>%
    filter(trip_mode_base%in%c('car','public.transport') & trip_purpose=="Work") %>%
    group_by(lga_name) %>%
    mutate(lga_id=row_number()) %>%
    ungroup()
  
  trips_melbourne_proportions <- trips_melbourne_scenarios %>%
    group_by(lga_name) %>%
    summarise(count=n()) %>%
    left_join(scenario_proportions, by="lga_name") %>%
    mutate(people=round(prop*count)) %>%
    group_by(lga_name,count,scenario) %>%
    mutate(rounded_total=sum(people)) %>%
    ungroup() %>%
    mutate(people=ifelse(rank==1,people+(count-rounded_total),people)) %>%
    dplyr::select(lga_name,count,scenario,mode,people) %>%
    pivot_wider(id_cols=c(lga_name,count),
                names_from=c(mode,scenario),
                values_from=people)
  
  
  baseline <- inner_join(
    trips_melbourne_proportions,
    mapply(
      chooseBase,
      trips_melbourne_proportions$lga_name,
      trips_melbourne_proportions$count,
      trips_melbourne_proportions$car_base,
      trips_melbourne_proportions$pt.drive_base,
      trips_melbourne_proportions$pt.walk_base,
      SIMPLIFY=F) %>%
      bind_rows() %>%
      group_by(lga_name) %>%
      nest(),
    by="lga_name"
  )
  
  pt_full <- baseline %>%
    mutate(scen_car      = car_pt.full      - car_base,
           scen_pt.drive = pt.drive_pt.full - pt.drive_base,
           scen_pt.walk  = pt.walk_pt.full  - pt.walk_base,
    ) %>%
    dplyr::select(lga_name,scen_car,scen_pt.walk,scen_pt.drive,data)
  pt_full_scenario <- pt_full %>%
    mutate(data = pmap(list(data, lga_name,scen_car,scen_pt.walk,scen_pt.drive), chooseTrips)) %>%
    dplyr::select(lga_name,data) %>%
    unnest(cols="data") %>%
    getDistanceAndTime("pt.full") %>%
    inner_join(
      trips_melbourne_scenarios%>%dplyr::select(-trip_mode_base,-trip_duration_base,-trip_distance_base),
      by=c("lga_name","lga_id")
    ) %>% 
    mutate(dist_cat=NA) %>%
    dplyr::select(
      persid,cluster_id,household_id,participant_id,age,sex,year,trip_id,trip_purpose,
      participant_wt,trips_wt,trip_mode_base,trip_duration_base,trip_distance_base,
      day_type,trip_id_2,age_group,dist_cat,trip_mode_scen,trip_distance_scen,
      trip_duration_scen
    )
  
  pt_full_scenario_final <- bind_rows(
    trips_melbourne_unchanged,
    pt_full_scenario) %>% 
    mutate(dist_cat=NA) %>%
    dplyr::mutate(trip_duration_base_hrs = trip_duration_base * 7) %>%
    dplyr::mutate(trip_duration_scen_hrs = trip_duration_scen * 7)
  
  
  pt_train <- baseline %>%
    mutate(scen_car      = car_pt.train      - car_base,
           scen_pt.drive = pt.drive_pt.train - pt.drive_base,
           scen_pt.walk  = pt.walk_pt.train  - pt.walk_base,
    ) %>%
    dplyr::select(lga_name,scen_car,scen_pt.walk,scen_pt.drive,data)
  pt_train_scenario <- pt_train %>%
    mutate(data = pmap(list(data, lga_name,scen_car,scen_pt.walk,scen_pt.drive), chooseTrips)) %>%
    dplyr::select(lga_name,data) %>%
    unnest(cols="data") %>%
    getDistanceAndTime("pt.train") %>%
    inner_join(
      trips_melbourne_scenarios%>%dplyr::select(-trip_mode_base,-trip_duration_base,-trip_distance_base),
      by=c("lga_name","lga_id")
    ) %>% 
    mutate(dist_cat=NA) %>%
    dplyr::select(
      persid,cluster_id,household_id,participant_id,age,sex,year,trip_id,trip_purpose,
      participant_wt,trips_wt,trip_mode_base,trip_duration_base,trip_distance_base,
      day_type,trip_id_2,age_group,dist_cat,trip_mode_scen,trip_distance_scen,
      trip_duration_scen
    )
  
  pt_train_scenario_final <- bind_rows(
    trips_melbourne_unchanged,
    pt_train_scenario) %>% 
    mutate(dist_cat=NA) %>%
    dplyr::mutate(trip_duration_base_hrs = trip_duration_base * 7) %>%
    dplyr::mutate(trip_duration_scen_hrs = trip_duration_scen * 7)
  
  write.csv(pt_full_scenario_final, paste0(output_location,"/scenarioTrips/dotFull.csv"), row.names=F, quote=T)
  write.csv(pt_train_scenario_final, paste0(output_location,"/scenarioTrips/dotTrain.csv"), row.names=F, quote=T)
  
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
    scenario_location=pt_full_scenario_walking
  )
  write.csv(persons_travel_pt_full, paste0(output_location,"/personTravel/dotFull.csv"), row.names=F, quote=T)
  persons_travel_pt_train <- calculatePersonsTravelScenario(
    travel_data_location="./Data/processed/travel_data.csv", ## BZ: generated in script runInputsMelbourneExposure.R 
    scenario_location=pt_train_scenario_walking
  )
  write.csv(persons_travel_pt_train, paste0(output_location,"/personTravel/dotTrain.csv"), row.names=F, quote=T)
  
  #### 2.2) Match NHS people to VISTA people based on age, sex, ses, work status and whether they walk for transport
  persons_matched_pt_full <- calculatePersonsMatch(
    pa_location="./Data/processed/persons_pa.csv", ## BZ: generated in script runInputsMelbourneExposure.R 
    persons_travel_location=persons_travel_pt_full  #"Data/processed/persons_travel.csv"
  ) %>% dplyr::mutate(scen="dotFull")
  write.csv(persons_matched_pt_full, paste0(output_location,"/dotFull.csv"), row.names=F, quote=T)
  persons_matched_pt_train <- calculatePersonsMatch(
    pa_location="./Data/processed/persons_pa.csv", ## BZ: generated in script runInputsMelbourneExposure.R 
    persons_travel_location=persons_travel_pt_train  #"Data/processed/persons_travel.csv"
  ) %>% dplyr::mutate(scen="dotTrain")
  write.csv(persons_matched_pt_train, paste0(output_location,"/dotTrain.csv"), row.names=F, quote=T)
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
  lga_df="Data/processed/lga_df.csv",
  output_location="./scenarios_dot"
)