# Note: you need to have the private version of the VISTA data to run this.
# trip_VISTA_location, person_VISTA_location CANNOT BE MADE PUBLIC DUE TO PRIVACY CONCERNS!
# srl_df_location CANNOT BE MADE PUBLIC, IT BELONGS TO THE DEPARTMENT OF TRANSPORT!
# the output of this code can however be made public.

person_VISTA_location <- '/home/alan/Projects/virtual-population/VISTA/RMIT VISTA 2012-16 Pack/P_VISTA12-16_v1.sav' 
trip_VISTA_location <- '/home/alan/Projects/virtual-population/VISTA/RMIT VISTA 2012-16 Pack/T_VISTA12_16_v1.sav' 
srl_df_location <- './Data/processed/sa2_df.csv'
mb_location <- "./Data/vitm/mb.sqlite"
speed_location <- "./Data/processed/speed_trips_melbourne.csv"

# hh_VISTA_location <- '/home/alan/Projects/virtual-population/VISTA/RMIT VISTA 2012-16 Pack/H_VISTA12-16_v1.sav' 


# load libraries and functions --------------------------------------------
suppressPackageStartupMessages(library(haven)) # for spss reading
suppressPackageStartupMessages(library(sf)) # for spatial things
# suppressPackageStartupMessages(library(lwgeom)) # for advanced spatial things
suppressPackageStartupMessages(library(dplyr)) # for manipulating data
suppressPackageStartupMessages(library(tidyr)) # for manipulating data


calculateTripsSRL <- function(person_VISTA_location,trip_VISTA_location,mb_location) {
  mb <- st_read(mb_location) %>%
    st_drop_geometry()
  
  person_VISTA <- read_sav(person_VISTA_location) %>%
    as.data.frame() %>%
    mutate(WDPERSWGT=CW_WDPERSWGT_SA3,
           WEPERSWGT=CW_WEPERSWGT_SA3) %>%
    dplyr::select(PERSID,AGE,SEX,WDPERSWGT,WEPERSWGT) %>%
    mutate(SEX=as.character(SEX),
           SEX=case_when(SEX=="1" ~ 'male', SEX=="2" ~ 'female')) %>%
    replace_na(list(WDPERSWGT=0,WEPERSWGT=0)) %>%
    mutate(participant_wt = as.numeric(WDPERSWGT+WEPERSWGT)) %>%
    mutate(age = as.numeric(AGE)) %>%
    mutate(PERSID = as.character(PERSID)) %>%
    dplyr::select(PERSID,age,sex=SEX,participant_wt)
  
  
  trip_VISTA <- read_sav(trip_VISTA_location) %>%
    as.data.frame() %>%
    dplyr::select(TRIPID,PERSID,HHID,TRIPNO,CUMDIST,TRAVTIME,TRAVDOW,
                  # TRIPPURP,
                  LINKMODE,ORIGMESHBLOCK,DESTMESHBLOCK,ORIGPURP1,
                  DESTPURP1,TRIPTIME,
                  Mode1,Mode2,Mode3,Mode4,Mode5,Mode6,Mode7,Mode8,Mode9,
                  WDTRIPWGT=CW_WDTRIPWGT_SA3,WETRIPWGT=CW_WETRIPWGT_SA3) %>%
    mutate(PERSID = as.character(PERSID)) %>%
    # limit to trips within Greater Melbourne
    filter(ORIGMESHBLOCK %in% mb$mb_code16 & DESTMESHBLOCK %in% mb$mb_code16) %>%
    dplyr::select(-DESTMESHBLOCK) %>%
    # only want valid modes. Also excluding planes and other
    filter(LINKMODE %in% 1:10) %>%
    filter(DESTPURP1 > 0)
  
  trip_VISTA_cleaned <- trip_VISTA %>%
    mutate(day_type=ifelse(TRAVDOW<=5,"weekday","weekend day")) %>%
    dplyr::select(-TRAVDOW) %>%
    replace_na(list(WETRIPWGT=0,WDTRIPWGT=0)) %>%
    mutate(trips_wt = WETRIPWGT+WDTRIPWGT) %>%
    dplyr::select(-WETRIPWGT,-WDTRIPWGT) %>%
    mutate(has_walk=ifelse(Mode1==4 | Mode2==4 | Mode3==4 | Mode4==4 | Mode5==4 | 
                             Mode6==4 | Mode7==4 | Mode8==4 | Mode9==4,T,F)) %>%
    mutate(has_pt=ifelse(Mode1 %in% 7:10 | Mode2 %in% 7:10 | Mode3 %in% 7:10 | 
                           Mode4 %in% 7:10 | Mode5 %in% 7:10 | Mode6 %in% 7:10 | 
                           Mode7 %in% 7:10 | Mode8 %in% 7:10 | Mode9 %in% 7:10,T,F)) %>%
    mutate(has_car=ifelse(Mode1 %in% c(1:3,6) | Mode2 %in% c(1:3,6) | 
                            Mode3 %in% c(1:3,6) | Mode4 %in% c(1:3,6) | 
                            Mode5 %in% c(1:3,6) | Mode6 %in% c(1:3,6) | 
                            Mode7 %in% c(1:3,6) | Mode8 %in% c(1:3,6) | 
                            Mode9 %in% c(1:3,6),T,F)) %>%
    dplyr::select(-Mode1,-Mode2,-Mode3,-Mode4,-Mode5,-Mode6,-Mode7,-Mode8,-Mode9) %>%
    mutate(work_trip=ifelse(ORIGPURP1==7 | DESTPURP1==7,T,F)) %>%
    dplyr::select(-ORIGPURP1)
  
  # categorise trip purpose and mode
  trip_VISTA_cleaned2 <- trip_VISTA_cleaned %>%
    mutate(trip_purpose=case_when(
      DESTPURP1==1 ~ "change mode",
      DESTPURP1==2 ~ "accompany someone",
      DESTPURP1==3 ~ "buy something",
      DESTPURP1==4 ~ "pick-up or deliver something",
      DESTPURP1==5 ~ "pick-up or drop-off someone",
      DESTPURP1==6 ~ "education",
      DESTPURP1==7 ~ "work related",
      DESTPURP1==8 ~ "at or go home",
      DESTPURP1==9 ~ "personal business",
      DESTPURP1==10 ~ "social",
      DESTPURP1==11 ~ "recreational",
      DESTPURP1==12 ~ "other purpose"
    )) %>%
    dplyr::select(-DESTPURP1) %>%
    mutate(trip_mode=NA,
           trip_mode=ifelse(LINKMODE %in% c(1:3,6),"car",trip_mode),
           trip_mode=ifelse(LINKMODE==4,"pedestrian",trip_mode),
           trip_mode=ifelse(LINKMODE==5,"bicycle",trip_mode),
           trip_mode=ifelse(LINKMODE==7,"public transport",trip_mode),
           trip_mode=ifelse(LINKMODE==8,"public transport",trip_mode),
           trip_mode=ifelse(LINKMODE %in% c(9:10),"public transport",trip_mode),
           trip_mode=ifelse(has_walk==T & has_pt==T,"pt.walk",trip_mode),
           trip_mode=ifelse(has_car==T & has_pt==T,"pt.drive",trip_mode)
    ) %>%
    dplyr::select(-LINKMODE,-has_walk,-has_pt,-has_car)
  
  # Add SA2 region, rename variables, add trip_id_2
  trip_VISTA_cleaned3 <- trip_VISTA_cleaned2 %>%
    inner_join(mb,by=c("ORIGMESHBLOCK"="mb_code16")) %>%
    rename(orig_sa2=sa2_main16) %>%
    dplyr::select(-ORIGMESHBLOCK) %>%
    group_by(PERSID) %>%
    dplyr::mutate(trip_id_2 = 1:dplyr::n()) %>%
    ungroup() %>%
    rename(trip_distance=CUMDIST, trip_duration=TRAVTIME)
  
  # Add ages, calculate age_group, distance_cat, reorder variables
  trip_VISTA_cleaned4 <- trip_VISTA_cleaned3 %>%
    inner_join(person_VISTA,by="PERSID") %>%
    mutate(age_group = case_when(age <  18             ~   "0 to 17",
                                 age >= 18 & age <= 40 ~  "18 to 40",
                                 age >= 41 & age <= 65 ~  "41 to 65",
                                 age >= 65             ~   "65 plus")) %>%
    dplyr::mutate(dist_cat=
                    case_when(trip_distance <   2                      ~  "< 2km", 
                              trip_distance <=  5 & trip_distance >= 2 ~  "2-5km", 
                              trip_distance <= 10 & trip_distance >  5 ~ "6-10km",
                              trip_distance >  10                      ~  ">10km")) %>%
    mutate(trip_id=row_number()) %>%
    dplyr::mutate(participant_id=PERSID) %>%
    ### Separate participant id into year, hh_id and participant_id
    separate(participant_id, into=c(NA,"year",NA,"cluster_id","household_id",NA,
                                    "participant_id"), sep = c(1, 3, 4, 6, -3,-2)) %>%
    mutate(persid=tolower(PERSID)) %>%
    dplyr::select(persid,cluster_id,household_id,participant_id,age,sex,year,
                  trip_id,trip_purpose,participant_wt,trips_wt,trip_mode,
                  trip_duration,trip_distance,day_type,trip_id_2,age_group,
                  dist_cat,work_trip,orig_sa2) %>%
    mutate(trip_duration=as.numeric(trip_duration),
           participant_wt=as.numeric(participant_wt),
           trips_wt=as.numeric(trips_wt),
           trip_distance=as.numeric(trip_distance))
  
  return(trip_VISTA_cleaned4)
}

calculateScenarioSRL <- function(scenarioName,workTripsProportions,srl_trips,srl_time_and_distance) {
  # scenarioName='pt.train'
  
  scenarioProportions <- workTripsProportions %>%
    filter(scenario%in%c('base',scenarioName)) %>%
    mutate(scenario=ifelse(scenario==scenarioName,'scen',scenario)) %>%
    pivot_wider(names_from=c(scenario,mode),values_from=people) %>%
    mutate(scen_car=scen_car-base_car,
           scen_pt.walk=scen_pt.walk-base_pt.walk,
           scen_pt.drive=scen_pt.drive-base_pt.drive) %>%
    dplyr::select(orig_sa2,sa2_count,base_car,base_pt.walk,base_pt.drive,
                  scen_car,scen_pt.walk,scen_pt.drive)
  
  chooseTrips <- function(orig_sa2,sa2_count,base_car,base_pt.walk,base_pt.drive,
                          scen_car,scen_pt.walk,scen_pt.drive) {
    # orig_sa2=1234
    # sa2_count=30
    # base_car=15
    # base_pt.walk=5
    # base_pt.drive=10
    # scen_car=0
    # scen_pt.walk=0
    # scen_pt.drive=0
    base_choices <- data.frame(orig_sa2=orig_sa2,
                               trip=sample(1:sa2_count, sa2_count, replace=F),
                               trip_mode_base=c(rep("car",base_car),rep("pt.walk",base_pt.walk),rep("pt.drive",base_pt.drive)),
                               stringsAsFactors=F) %>%
      arrange(trip)
    base_trips_car <- base_choices %>% filter(trip_mode_base=="car") %>% pull(trip)
    base_trips_pt.walk <- base_choices %>% filter(trip_mode_base=="pt.walk") %>% pull(trip)
    base_trips_pt.drive <- base_choices %>% filter(trip_mode_base=="pt.drive") %>% pull(trip)
    
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
    
    scen_choices <- data.frame(trip=sample(removed_trips, length(removed_trips), replace=F),
                               trip_mode_scen=trip_mode_scen,
                               stringsAsFactors=F)
    
    choices <- base_choices %>%
      left_join(scen_choices, by="trip") %>%
      mutate(trip_mode_scen=ifelse(is.na(trip_mode_scen),trip_mode_base,trip_mode_scen))
    return(choices)
  }
  
  set.seed(20200910)
  workTripsMode <- mapply(
    chooseTrips,scenarioProportions$orig_sa2,scenarioProportions$sa2_count,
    scenarioProportions$base_car,scenarioProportions$base_pt.walk,
    scenarioProportions$base_pt.drive,scenarioProportions$scen_car,
    scenarioProportions$scen_pt.walk,scenarioProportions$scen_pt.drive,
    SIMPLIFY=F) %>% bind_rows()
  
  currentTimeAndDistance <- srl_time_and_distance %>%
    filter(scenario%in%c('base',scenarioName)) %>%
    mutate(scenario=ifelse(scenario==scenarioName,'scen',scenario)) #%>%
    # pivot_wider(names_from=scenario,values_from=c(distance,time)) %>%
    
  
  workTripsFinal <- workTrips %>%
    inner_join(workTripsMode, by=c("orig_sa2"="orig_sa2","trip"="trip")) %>%
    dplyr::select(-trip_mode,-trip_duration,-trip_distance) %>%
    inner_join(currentTimeAndDistance%>%filter(scenario=="base")%>%dplyr::select(-scenario),
               by=c("orig_sa2"="sa2","trip_mode_base"="mode")) %>%
    rename(trip_duration_base=time,trip_distance_base=distance) %>%
    inner_join(currentTimeAndDistance%>%filter(scenario=="scen")%>%dplyr::select(-scenario),
               by=c("orig_sa2"="sa2","trip_mode_scen"="mode")) %>%
    rename(trip_duration_scen=time,trip_distance_scen=distance) %>%
    dplyr::select(-orig_sa2,-trip)
  
  nonWorkTrips <- srl_trips %>%
    filter(work_trip==F | (work_trip==T & !trip_mode %in% c("car","pt.walk","pt.drive"))) %>%
    dplyr::select(-orig_sa2,-work_trip) %>%
    mutate(trip_mode_base=trip_mode,
           trip_mode_scen=trip_mode,
           trip_distance_base=trip_distance,
           trip_duration_base=trip_duration,
           trip_distance_scen=trip_distance,
           trip_duration_scen=trip_duration) %>%
    dplyr::select(-trip_mode,-trip_distance,-trip_duration) %>%
    as.data.frame()
  
  allTrips <- bind_rows(workTripsFinal,nonWorkTrips)
  
  return(allTrips)
}

reformatScenarioSRL <- function(trips_srl_scenario,speed_location) {
  speed <- read.csv(speed_location,as.is=T,fileEncoding="UTF-8-BOM")
  #### create column in trips_melbourne with speed data for age and sex (use median)
  walk_speed <- speed %>% dplyr::filter(activity=="walking") %>% rename(walk_mean_speed = mean) %>% dplyr::select(age_group, sex, walk_mean_speed)
  cycle_speed <- speed %>% dplyr::filter(activity=="bicycle") %>% rename(cycle_mean_speed = mean) %>% dplyr::select(age_group, sex, cycle_mean_speed)
  
  tripsReformatted <- trips_srl_scenario %>%
    dplyr::mutate(trip_mode_base=case_when(trip_mode_base=="pedestrian" ~ 'walking', 
                                           trip_mode_base=="bus" ~ 'public.transport', 
                                           trip_mode_base=="tram" ~ 'public.transport', 
                                           trip_mode_base=="train" ~ 'public.transport',
                                           trip_mode_base=="motorcycle" ~ 'other',
                                           TRUE ~ tolower(trip_mode_base))) %>%
    dplyr::mutate(trip_mode_scen=case_when(trip_mode_scen=="pedestrian" ~ 'walking', 
                                           trip_mode_scen=="bus" ~ 'public.transport', 
                                           trip_mode_scen=="tram" ~ 'public.transport', 
                                           trip_mode_scen=="train" ~ 'public.transport',
                                           trip_mode_scen=="motorcycle" ~ 'other',
                                           TRUE ~ tolower(trip_mode_scen))) %>%
    dplyr::mutate(trip_duration_base = trip_duration_base/60) %>% 
    dplyr::mutate(trip_duration_scen = trip_duration_scen/60)
  
  tripsReformatted2 <- tripsReformatted %>%
    inner_join(walk_speed, by=c("age_group", "sex")) %>%
    inner_join(cycle_speed, by=c("age_group", "sex")) %>%
    ### Replace with age and sex walking and cycling speed
    mutate(trip_duration_base=ifelse(trip_mode_base=="bicycle",
                                     trip_distance_base/cycle_mean_speed,
                                     trip_duration_base)) %>%
    mutate(trip_duration_scen=ifelse(trip_mode_scen=="bicycle",
                                     trip_distance_scen/cycle_mean_speed,
                                     trip_duration_scen)) %>%
    mutate(trip_duration_base=ifelse(trip_mode_base=="walking",
                                     trip_distance_base/walk_mean_speed,
                                     trip_duration_base)) %>%
    mutate(trip_duration_scen=ifelse(trip_mode_scen=="walking",
                                     trip_distance_scen/walk_mean_speed,
                                     trip_duration_scen)) %>%
    # dplyr::select(-cycle_mean_speed,-walk_mean_speed) %>%
    dplyr::mutate(trip_duration_base_hrs = trip_duration_base * 7) %>% ### Alan I modified here after discussing with James.
    dplyr::mutate(trip_duration_scen_hrs = trip_duration_scen * 7) %>%
    dplyr::select(persid,cluster_id,household_id,participant_id,age,sex,year,
                  trip_id,trip_purpose,participant_wt,trips_wt,trip_mode_base,
                  trip_duration_base,trip_distance_base,day_type,trip_id_2,
                  age_group,dist_cat,walk_mean_speed,cycle_mean_speed,
                  trip_mode_scen,trip_distance_scen,trip_duration_scen,
                  trip_duration_base_hrs,trip_duration_scen_hrs)
  return(tripsReformatted2)
}


# calculations ------------------------------------------------------------


srl_df <- read.csv(srl_df_location,as.is=T, fileEncoding="UTF-8-BOM") %>%
  pivot_longer(cols=base_car_count:pt.train_pt.walk_time.sd,
               names_to = c("scenario","mode","measure"),
               names_sep="_")

# calculate the proportion of car, pt.drive, and pt.walk for the scenarios.
# also determine the rank of each proportion (1 for largest, 3 for smallest)
srl_proportions <- srl_df %>%
  filter(measure=="count") %>%
  group_by(sa2,scenario) %>%
  mutate(total=sum(value)) %>%
  ungroup() %>%
  mutate(prop=value/total) %>%
  dplyr::select(sa2,scenario,mode,prop) %>%
  group_by(sa2,scenario) %>%
  arrange(-prop) %>%
  mutate(rank=row_number()) %>%
  ungroup() %>%
  pivot_wider(names_from=c(scenario,mode),values_from=c(prop,rank)) %>%
  as.data.frame()

# Table format: sa2, scenario, mode, distance, time
# Mode is either car, pt.drive (driving to PT), or pt.walk (walking to PT)
srl_time_and_distance <- srl_df %>%
  filter(measure%in%c("distance.mean","time.mean")) %>%
  pivot_wider(names_from=c(measure),values_from=value) %>%
  rename(distance=distance.mean) %>%
  rename(time=time.mean) %>%
  as.data.frame()

srl_trips <- calculateTripsSRL(
  person_VISTA_location=person_VISTA_location,
  trip_VISTA_location=trip_VISTA_location,
  mb_location=mb_location
)


# work trips --------------------------------------------------------------
workTrips <- srl_trips %>%
  filter(work_trip==T & trip_mode %in% c("car","pt.walk","pt.drive")) %>%
  group_by(orig_sa2) %>%
  mutate(trip=row_number()) %>%
  ungroup() %>%
  dplyr::select(-work_trip) %>%
  as.data.frame()

workTripsProportions <- workTrips %>%
  group_by(orig_sa2) %>%
  summarise(sa2_count=max(trip,na.rm=T)) %>%
  ungroup() %>%
  left_join(srl_proportions, by=c("orig_sa2"="sa2")) %>%
  mutate(people=round(prop*sa2_count)) %>%
  group_by(orig_sa2,sa2_count,scenario) %>%
  mutate(rounded_total=sum(people)) %>%
  ungroup() %>%
  mutate(people=ifelse(rank==1,people+(sa2_count-rounded_total),people)) %>%
  dplyr::select(orig_sa2,sa2_count,scenario,mode,people)



# calculate scenarios -----------------------------------------------------
scenarioPTtrain <- calculateScenarioSRL(
  scenarioName='pt.train',
  workTripsProportions,
  srl_trips,
  srl_time_and_distance)

scenarioPTfull <- calculateScenarioSRL(
  scenarioName='pt.full',
  workTripsProportions,
  srl_trips,
  srl_time_and_distance)

scenarioPTtrainFinal <- reformatScenarioSRL(
  trips_srl_scenario=scenarioPTtrain,
  speed_location=speed_location
)

scenarioPTfullFinal <- reformatScenarioSRL(
  trips_srl_scenario=scenarioPTfull,
  speed_location=speed_location
)


# export trips ------------------------------------------------------------
# # in case the directory hasn't been made yet
# dir.create('./', recursive=TRUE, showWarnings=FALSE)

write.csv(scenarioPTtrainFinal, "Data/processed/trips_melbourne_scenarios_pt_train.csv", row.names=F, quote=T)
write.csv(scenarioPTfullFinal, "Data/processed/trips_melbourne_scenarios_pt_full.csv", row.names=F, quote=T)


# reformat trips ----------------------------------------------------------


tripsPTtrain <- scenarioPTtrainFinal %>%
  mutate(trip_mode_base=ifelse(trip_mode_base%in%c('pt.walk','pt.drive'),'walking',trip_mode_base)) %>%
  mutate(trip_mode_scen=ifelse(trip_mode_scen%in%c('pt.walk','pt.drive'),'walking',trip_mode_scen)) %>%
  mutate(trip_mode_base=ifelse(trip_mode_base=='public transport','public.transport',trip_mode_base)) %>%
  mutate(trip_mode_scen=ifelse(trip_mode_scen=='public transport','public.transport',trip_mode_scen))
  
tripsPTfull <- scenarioPTfullFinal %>%
  mutate(trip_mode_base=ifelse(trip_mode_base%in%c('pt.walk','pt.drive'),'walking',trip_mode_base)) %>%
  mutate(trip_mode_scen=ifelse(trip_mode_scen%in%c('pt.walk','pt.drive'),'walking',trip_mode_scen)) %>%
  mutate(trip_mode_base=ifelse(trip_mode_base=='public transport','public.transport',trip_mode_base)) %>%
  mutate(trip_mode_scen=ifelse(trip_mode_scen=='public transport','public.transport',trip_mode_scen))

scenarioPurposes <- c("Leisure","Shopping","Work related",
                      "Pick-up or drop-off someone/something","personal business",
                      "Other","accompany someone","education","at or go home")


a<-unique(tripsPTtrain$trip_purpose)%>%sort()
b<-unique(scenario_trips$trip_purpose)%>%as.character()%>%sort()


source("Scripts/data_prep/synthetic_pop.R")

### 2.1) Create data set with VISTA people and allocate baseline and scenario trips to them
persons_travel <- calculatePersonsTravelScenario(
  travel_data_location="Data/processed/travel_data.csv", ## BZ: generated in script runInputsMelbourneExposure.R 
  scenario_location=tripsPTtrain#"Data/processed/trips_melbourne_scenarios.csv" ### BZ: Generated in step 1
)
# write.csv(persons_travel, "Data/processed/persons_travel.csv", row.names=F, quote=T)


#### 2.2) Match NHS people to VISTA people based on age, sex, ses, work status and whether they walk for transport
persons_matched <- calculatePersonsMatch(
  pa_location="Data/processed/persons_pa.csv", ## BZ: generated in script runInputsMelbourneExposure.R 
  persons_travel_location=persons_travel   #"Data/processed/persons_travel.csv"
)
write.csv(persons_matched, "scenarios/scenario_1/matched_pop_1.csv", row.names=F, quote=T)
persons_matched <- read.csv("scenarios/scenario_1/matched_pop_1.csv", as.is=T, fileEncoding="UTF-8-BOM")
