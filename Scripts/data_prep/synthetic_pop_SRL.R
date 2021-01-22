# SYNTHETIC POPULATION OF VISTA PERSONS AND NHS PERSONS
suppressPackageStartupMessages(library(dplyr)) # for manipulating data
suppressPackageStartupMessages(library(tidyr)) # for pivoting data

# TRAVEL DATA PERSON FILE
## Join VISTA persons to VISTA Households
calculateTravelData <- function(hh_VISTA_location,person_VISTA_location,ses_index_location) {
  # hh_VISTA_location="Data/Travelsurvey/VISTA12-18/H_VISTA_1218_V1.csv"
  # person_VISTA_location="Data/Travelsurvey/VISTA12-18/P_VISTA1218_V1.csv"
  # ses_index_location="Data/Travelsurvey/ABS SEIFA/ses.csv"
  
  hh_VISTA <- read.csv(hh_VISTA_location,as.is=T,fileEncoding="UTF-8-BOM") %>%
    dplyr::select(HHID,SurveyPeriod,HomeSubRegion,HOMEPC) %>%
    filter(HHID!="") # some rows were completely blank
  
  person_VISTA <- read.csv(person_VISTA_location,as.is=T, fileEncoding="UTF-8-BOM") %>%
    dplyr::select(PERSID, HHID, AGE, SEX, FULLTIMEWORK, ANYWORK, STUDYING, ANZSCO1,
                  ANZSIC1, WDPERSWGT, WEPERSWGT)
  ## Add SEIFA-IRSD
  ses_index <- read.csv(ses_index_location,as.is=T, fileEncoding="UTF-8-BOM") %>% 
    rename_all(~c("HOMEPC","ses")) %>%
    filter(!is.na(HOMEPC))
  
  ## Add SEIFA-IRSD
  ses_index <- read.csv(ses_index_location,as.is=T,fileEncoding="UTF-8-BOM") %>%
    rename_all(~c("HOMEPC","ses")) %>%
    filter(!is.na(HOMEPC))
  
  ### Join persons and household, keep data for greater Melb only and create unique weights
  persons_travel <- left_join(person_VISTA, hh_VISTA, by = "HHID") %>%  ### Alan: here I am now excluding 2016-2017 and 20017-18, so using same years as sceanrio
    filter(SurveyPeriod != "2017-18" &
             (HomeSubRegion != "Geelong" | HomeSubRegion != "Other")) %>%  ## Alan: do you 
    rowwise() %>% # want to sum across rows, not down columns
    mutate(participant_wt = sum(as.numeric(WDPERSWGT),as.numeric(WEPERSWGT),na.rm=T)) %>%
    dplyr::select(-WDPERSWGT,-WEPERSWGT) %>%
    as.data.frame() %>%
    inner_join(ses_index, by="HOMEPC") %>%
    ### Create age category as persons_pa is only available by age groups
    rename(age=AGE) %>%
    mutate(age_group = case_when(age <   5             ~  1,
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
                                 age >= 85             ~ 19)) %>%
    mutate(age_group_2 = case_when(age <   5             ~  2, ### for pifs calculations Melbourne model
                                 age >=  5 & age <=  9 ~  7,
                                 age >= 10 & age <= 14 ~  12,
                                 age >= 15 & age <= 17 ~  17, 
                                 age >= 18 & age <= 19 ~  17,
                                 age >= 20 & age <= 24 ~  22,
                                 age >= 25 & age <= 29 ~  27, 
                                 age >= 30 & age <= 34 ~  32, 
                                 age >= 35 & age <= 39 ~  37, 
                                 age >= 40 & age <= 44 ~ 42,
                                 age >= 45 & age <= 49 ~ 47, 
                                 age >= 50 & age <= 54 ~ 52, 
                                 age >= 55 & age <= 59 ~ 57, 
                                 age >= 60 & age <= 64 ~ 62, 
                                 age >= 65 & age <= 69 ~ 67,
                                 age >= 70 & age <= 74 ~ 72, 
                                 age >= 75 & age <= 79 ~ 77,
                                 age >= 80 & age <= 84 ~ 82,
                                 age >= 85 & age <= 89 ~ 87,
                                 age >= 90 & age <= 94 ~ 92,
                                 age >=95 ~ 97)) %>%
    #### Only keep adults over 18
    filter(age_group>4) %>%
    ### Sex to match persons_pa sex variable
    rename(sex=SEX) %>%
    mutate(sex=case_when(sex=="M" ~ 'male', sex=="F" ~ 'female')) %>%
    ### Employment status to match persons_pa work_status variable (LFSBC)
    mutate(work_status = ifelse(ANYWORK=="Yes",'employed','unemployed')) %>%
    ### Classification of occupation (ANZSCO1)
    mutate(occupation_cat = ifelse(ANZSCO1=="Missing/Refused",NA,ANZSCO1)) %>%
    ### Classification for industry
    mutate(industry_cat = ifelse(ANZSIC1=="Missing/Refused",NA,ANZSIC1)) %>%
    ### Change observations to match pa data
    mutate(study_full = case_when(STUDYING=="Part-time TAFE/Uni" ~ "Part time",
                                  STUDYING=="Full-time TAFE/Uni" ~ "Full time")) %>%
    ### Change observations to match pa data
    rename(work_full = FULLTIMEWORK) %>%
    rename(persid = PERSID) %>%
    rename(hhid = HHID) %>%
    dplyr::select(persid,hhid,age,age_group, age_group_2, sex,work_status,work_full,study_full,
                  occupation_cat,industry_cat,SurveyPeriod,HomeSubRegion,HOMEPC,
                  participant_wt,ses)
  
  return(persons_travel)
  
}


### trips_melbourne
calculatePersonsTravelScenario <- function(travel_data_location,scenario_location) {
  travel_data_location="Data/processed/travel_data.csv"
  scenario_location="Data/processed/trips_melbourne_scenarios_srl.csv"
    
  ### Original set
    # "Data/processed/trips_melbourne_scenarios.csv"

  
  travel_data <- read.csv(travel_data_location,as.is=T, fileEncoding="UTF-8-BOM")
  
  trips_melbourne <- read.csv(scenario_location,as.is=T, fileEncoding="UTF-8-BOM") %>%
    mutate(persid=toupper(persid))
  
  ### Create total duration and distance for all modes, rather long process here. 
  ### The intervention will change the trips file (scenario trips file) which in 
  ### turn will feed onto the persons file. 
  
  #### Scenario
  total_trips_time_dist_base <- trips_melbourne %>%
    dplyr::select(persid, trip_mode_base, trip_duration_base_hrs, trip_distance_base) %>%
    # group by person and travel mode
    dplyr::group_by(persid, trip_mode_base) %>%
    # find the total distance and time traveled for each person by travel mode
    dplyr::summarise(time_base=sum(trip_duration_base_hrs),
                     distance_base=sum(trip_distance_base)) %>%
    # expand time_base and distance_base to separate pairs of columns for each
    # travel mode
    pivot_wider(names_from=trip_mode_base,values_from=c(time_base, distance_base)) %>%
    # rearrange the columns
    dplyr::select(persid,time_base_car,distance_base_car,
                  time_base_walking,distance_base_walking,
                  'time_base_public transport','distance_base_public transport',
                  time_base_pt.walk,distance_base_pt.walk,
                  time_base_pt.drive,distance_base_pt.drive,
                  time_base_bicycle,distance_base_bicycle)
  
  #### Scenario
  total_trips_time_dist_scen <- trips_melbourne %>%
    dplyr::select(persid, trip_mode_scen, trip_duration_scen_hrs, trip_distance_scen) %>%
    # group by person and travel mode
    dplyr::group_by(persid, trip_mode_scen) %>%
    # find the total distance and time traveled for each person by travel mode
    dplyr::summarise(time_scen=sum(trip_duration_scen_hrs),
                     distance_scen=sum(trip_distance_scen)) %>%
    # expand time_scen and distance_scen to separate pairs of columns for each
    # travel mode
    pivot_wider(names_from=trip_mode_scen,values_from=c(time_scen, distance_scen)) %>%
    # rearrange the columns
    dplyr::select(persid,time_scen_car,distance_scen_car,
                  time_scen_walking,distance_scen_walking,
                  'time_scen_public transport','distance_scen_public transport',
                  time_scen_pt.walk,distance_scen_pt.walk,
                  time_scen_pt.drive,distance_scen_pt.drive,
                  time_scen_bicycle,distance_scen_bicycle)
  
  ### Do this last appending all
  persons_travel <- travel_data %>%
    left_join(total_trips_time_dist_base, by = "persid") %>%
    left_join(total_trips_time_dist_scen, by = "persid")
  
  # ### Create walking yes or no
  persons_travel <- persons_travel %>%
    mutate(walk_base = case_when(is.na(time_base_walking) ~ "No",
                                 time_base_walking > 0  ~ "Yes")) %>%
    mutate(walk_scen = case_when(is.na(time_scen_walking) ~ "No",
                                 time_scen_walking > 0  ~ "Yes"))  %>%
    
  ### Create walking min of 2 (to improve matching)
  mutate(walk_base_min = case_when(time_base_walking <=2 ~ "No",
                                   is.na(time_base_walking) ~ "No",
                                   time_base_walking > 2  ~ "Yes")) %>%
    mutate_if(is.numeric, ~replace(., is.na(.), 0))
  #not needed
  # names(persons_travel)[1:95] <- RemoveAllWs(tolower(names(persons_travel)[1:95]))
  
  return(persons_travel)
}




# PA PERSON AND HOUSEHOLD FILE
### Create variables to match with travel survey and for pa analysis
calculatePersonsPA <- function(pa_location,hh_location) {
  # pa_location="Data/Physical activity/NHS2017-18_CSV/NHS17SPB.csv"
  # hh_location="Data/Physical activity/NHS2017-18_CSV/NHS17HHB.csv"

  
#### PA variables
  
  # EXLWTIME, #Total minutes undertaken exercise for fitness, recreation, sport or transport in last week
  # EXLWTIM2,	#Total minutes did exercise or walked for transport in last week (vig x 2)
  # EXFSRMIN,	#Total minutes walked for fitness, recreation or sport in last week
  # EXTRAMIN,	#Total minutes spent walking for transport in last week
  # EXWLKTME,	#Total minutes spent walking for exercise and transport last week
  # EXLWMMIN,	#Total minutes undertaken moderate exercise last week
  # EXLWVMIN,	#Total minutes undertaken vigorous exercise last week
  # WPAMMIN,	#Total minutes undertaken moderate workplace physical activity last week
  # WPAVMIN,	#Total minutes undertaken vigorous workplace physical activity last week
  # WPAV2MIN,	#Total minutes undertaken vigorous workplace physical activity last week (x 2)
  # WPATMIN,	#Total minutes undertaken all workplace physical activity last week
  # WPATMIN2,	#Total minutes undertaken all workplace physical activity last week (vig x 2)
  # MODMINS,	#Total minutes undertaken all moderate physical activity last week
  # VIGMINS,	#Total minutes undertaken all vigorous physical activity last week
  # PAMINS,	  #Total minutes undertaken all physical activity last week
  # PAMINS2,	#Total minutes undertaken all physical activity last week (vig x 2)
  # EXLWKTNO, #Number of times walked for 10 minutes or more for transport in the last week
  # EXNUDAYW, #Number of days exercised for fitness, recreation, sport and to get to and from places in last week
  # EXNUDST,  #Number of days did strength or toning activities in the last week
  # EXWLKTME, #Total minutes spent walking for exercise and transport last week
  # EXNUDTH, #Number of days exercised for at least 30 minutes in the last week
  
  
  pa <- read.csv(pa_location,as.is=T, fileEncoding="UTF-8-BOM") %>%
    
    dplyr::select(ABSHIDB, SEX, LFSBC, OCCUP13B, ANZSICBC, USHRWKB, STDYFTPT, AGEB,
                  EXTRAMIN, EXLWMMIN, EXLWVMIN, WPAMMIN, WPAVMIN, MODMINS, VIGMINS, EXFSRMIN,
                  EXLWKTNO, EXNUDAYW, EXNUDST, EXWLKTME, EXNUDTH, NHIFINWT) %>%
    mutate_all(funs(type.convert(replace(., .== 99997, NA)))) %>%
    mutate_all(funs(type.convert(replace(., .== 99998, NA))))
  
 hh <- read.csv(hh_location,as.is=T, fileEncoding="UTF-8-BOM") %>% 
   dplyr::select(ABSHIDB, STATE16, SA1SF2DN, INCDECU1)
  
  persons_pa <- left_join(pa, hh, by="ABSHIDB") %>%
    ### Sex to match persons_pa
    dplyr::rename(sex = SEX) %>%
    mutate(sex = case_when(sex == 1 ~ "male",
                           sex == 2 ~ "female")) %>%
    ### Work status (reclassify "Not in the workforce" coded as 3 and NA coded 
    ### as 0 to "No" to match NHS PA)
    mutate(work_status = case_when(LFSBC == 0 ~ "unemployed",
                                   LFSBC == 1 ~ "employed",
                                   LFSBC == 2 ~ "unemployed",
                                   LFSBC == 3 ~ "unemployed")) %>%
    ### Classification of occupation (OCCUP13B) see Physical Activity-Basic 
    ### CURF.xls for codes references
    mutate(occupation_cat = case_when(OCCUP13B == "0" ~ "Not in Work Force",
                                      OCCUP13B == "1" ~ "Managers",
                                      OCCUP13B == "2" ~ "Professional",
                                      OCCUP13B == "3" ~ "Technicians and Trades Worker", 
                                      OCCUP13B == "4" ~ "Community and Personal Service Workers",
                                      OCCUP13B == "5" ~ "Clerical and Administrative Worker",
                                      OCCUP13B == "6" ~ "Sales Workers", 
                                      OCCUP13B == "7" ~ "Machinery Operators and Drivers", 
                                      OCCUP13B == "8" ~ "Labourers", 
                                      OCCUP13B == "9" ~ "NA")) %>%
    mutate(occupation_cat = ifelse(occupation_cat=="NA",NA,occupation_cat)) %>%
    ### Classification of industry (ANZSICBC) see Physical Activity-Basic CURF.xls
    ### for codes references
    mutate(industry_cat = case_when(ANZSICBC == 1 ~ "Agriculture, Forestry and Fishing",
                                    ANZSICBC == 2 ~ "Mining",
                                    ANZSICBC == 3 ~ "Manufacturing",
                                    ANZSICBC == 4 ~ "Electricity, Gas, Water and Waste Services",
                                    ANZSICBC == 5 ~ "Construction",
                                    ANZSICBC == 6 ~ "Wholesale Trade",
                                    ANZSICBC == 7 ~ "Retail Trade",
                                    ANZSICBC == 8 ~ "Accommodation and Food Services",
                                    ANZSICBC == 9 ~ "Transport, Postal and Warehousing",
                                    ANZSICBC == 10 ~ "Information Media and Telecommunications",
                                    ANZSICBC == 11 ~ "Financial and Insurance Services",
                                    ANZSICBC == 12 ~ "Rental, Hiring and Real Estate Services",
                                    ANZSICBC == 13 ~ "Professional, Scientific and Technical Services",
                                    ANZSICBC == 14 ~ "Administrative and Support Services",
                                    ANZSICBC == 15 ~ "Public Administration and Safety",
                                    ANZSICBC == 16 ~ "Education and Training",
                                    ANZSICBC == 17 ~ "Health Care and Social Assistance",
                                    ANZSICBC == 18 ~ "Arts and Recreation Services",
                                    ANZSICBC == 19 ~ "Other Services",
                                    ANZSICBC == 25 ~ "Inadequately described",
                                    ANZSICBC == 26 ~ "Not in Work Force")) %>%
    ### Create work_full (yes or no) to indicate whether ind work full time or 
    ### not. Derive from USHRWKB
    mutate(work_full = case_when(USHRWKB == 0 ~ "NA",
                                 USHRWKB == 1 | USHRWKB == 2 | USHRWKB == 3 ~ "No",
                                 USHRWKB >= 4 ~ "Yes")) %>%
    mutate(work_full = ifelse(work_full=="NA",NA,work_full)) %>%
    ### Original variable values: 0. Not applicable 1. Studying full-time
    ### 2. Studying part-time 3. Not studying
    mutate(study_full = case_when(STDYFTPT  == 0 | STDYFTPT  == 3 ~ "NA",
                                  STDYFTPT == 1 ~ "Full time",
                                  STDYFTPT  == 2 ~ "Part time")) %>%
    mutate(study_full = ifelse(study_full=="NA",NA,study_full)) %>%
    dplyr::rename(age_group = AGEB) %>%
    dplyr::rename(ses = SA1SF2DN) %>%
    dplyr::rename(state = STATE16) %>%
    mutate(mod_total_hr = MODMINS/60) %>%
    mutate(vig_total_hr = VIGMINS/60) %>%
    mutate(mod_leis_hr = EXLWMMIN/60) %>% 
    mutate(vig_leis_hr = EXLWVMIN/60) %>%
    mutate(mod_work_hr = WPAMMIN/60) %>% 
    mutate(vig_work_hr = WPAVMIN/60) %>%
    mutate(walk_rc = EXFSRMIN/60) %>%
    mutate(walk_trans = EXTRAMIN/60) %>%
    mutate(walk_base = case_when(EXTRAMIN == 0 ~ "No",
                                 EXTRAMIN > 0  ~ "Yes")) %>%
    mutate(walk_base_min = case_when(walk_trans < 2 ~ "No",
                                     walk_trans >= 2  ~ "Yes")) %>%
    ### Add whether participants meet PA guidelines (difference for adults and 
    ### older adults)
    mutate(pa_guide_adults =ifelse((EXNUDAYW >=5 & EXNUDST >=2 & 
                                      (EXWLKTME + EXLWMMIN + EXLWVMIN*2) >= 150),
                                   "Yes", "No")) %>%
    mutate(pa_guide_older_adults = ifelse(EXNUDAYW >= 5 & EXNUDTH >=5, "Yes", "No")) %>%
    
    dplyr::filter(age_group >4) %>%
    
    
    ## Add age group variable
    mutate(age_group_scen = case_when(age_group == 5 ~ "15 to 19",
                                      age_group == 6 ~ "20 to 24",
                                      age_group == 7 ~ "25 to 29", 
                                      age_group == 8 ~ "30 to 34", 
                                      age_group == 9 ~ "35 to 39",
                                      age_group == 10 ~ "40 to 44",
                                      age_group == 11 ~ "45 to 49", 
                                      age_group == 12 ~ "50 to 54",
                                      age_group == 13 ~ "55 to 59",
                                      age_group == 14 ~ "60 to 64", 
                                      age_group == 15 ~ "65 to 69",
                                      age_group == 16 ~ "70 to 74",
                                      age_group == 17 ~ "75 to 79", 
                                      age_group == 18 ~ "80 to 84",
                                      age_group == 19 ~ "85 +")) %>%
    
    mutate(dem_index = case_when(age_group == 5 & sex == "male" ~  1,
                                 age_group == 6 & sex == "male" ~  3,
                                 age_group == 7 & sex == "male" ~  5,
                                 age_group == 8 & sex == "male" ~  7,
                                 age_group == 9 & sex == "male" ~  9,
                                 age_group == 10 & sex == "male" ~ 11,
                                 age_group == 11 & sex == "male" ~  13, 
                                 age_group == 12 & sex == "male" ~  15,
                                 age_group == 13 & sex == "male" ~ 17,
                                 age_group == 14 & sex == "male" ~ 19, 
                                 age_group == 15 & sex == "male" ~ 21,
                                 age_group == 16 & sex == "male" ~ 23, 
                                 age_group == 17 & sex == "male" ~ 25, 
                                 age_group == 18 & sex == "male" ~ 27,
                                 age_group == 19 & sex == "male" ~ 29,
                                 age_group == 5 & sex == "female" ~  2,
                                 age_group == 6 & sex == "female" ~  4,
                                 age_group == 7 & sex == "female" ~  6,
                                 age_group == 8 & sex == "female" ~  8,
                                 age_group == 9 & sex == "female" ~  10,
                                 age_group == 10 & sex == "female" ~  12,
                                 age_group == 11 & sex == "female" ~  14, 
                                 age_group == 12 & sex == "female" ~  16,
                                 age_group == 13 & sex == "female" ~ 18,
                                 age_group == 14 & sex == "female" ~ 20, 
                                 age_group == 15 & sex == "female" ~ 22,
                                 age_group == 16 & sex == "female" ~ 24, 
                                 age_group == 17 & sex == "female" ~ 26, 
                                 age_group == 18 & sex == "female" ~ 28,
                                 age_group == 19 & sex == "female" ~ 30)) %>%  
    
    
    dplyr::rename(participant_wt = NHIFINWT) %>%
    
    dplyr::select(ABSHIDB, age_group, age_group_scen, sex, ses, walk_base, work_status, participant_wt, dem_index, age_group_scen, 
                  mod_total_hr, vig_total_hr, mod_leis_hr, vig_leis_hr, mod_work_hr, vig_work_hr, walk_rc, walk_trans, EXTRAMIN, walk_base_min,
                  pa_guide_adults, pa_guide_older_adults) %>%
    mutate_if(is.numeric, ~replace(., is.na(.), 0)) ### Replace NAs with zeros, then in descriptive stats all taken into acccount, not only those with values
  
  return(persons_pa)
}



##### Randomly allocate leisure time, work time and walk for transport variables from persons_pa to persons_travel

# I am aiming to randomly assign the following variables from dataset persons_pa
# to dataset persons_travel (both datasets are attached): 
### mod_total_hr
###  vig_total_hr
### mod_leis_hr
### vig_leis_hr
### mod_work_hr
### vig_work_hr
### walk_rc
### walk_trans
### walk_base

calculatePersonsMatch <- function(pa_location,persons_travel_location) {
  # pa_location="Data/processed/persons_pa.csv"
  # persons_travel_location="Data/processed/persons_travel.csv"


  persons_pa <- read.csv(pa_location,as.is=T, fileEncoding="UTF-8-BOM")
  persons_travel <- persons_travel #read.csv(persons_travel_location,as.is=T, fileEncoding="UTF-8-BOM")
  
  # This joins the two tables based on the match variables. This is all of the 
  # possible group combinations. We then assign a unique per group number.
  # Some people have no viable matches, so using a left join means that these
  # people will have NA for their values. I'd suggest using wider age ranges.
  
  persons_matched <- left_join(persons_travel, persons_pa,
                               by=c("age_group", "sex", "ses", "walk_base", "walk_base_min", "work_status")) %>% 
    group_by(persid) %>%
    # group_number is a unique number (1:n) for each of a persid's possible matches
    dplyr::mutate(group_number=row_number()) %>%
    ungroup()
  
  # This we then find the group size for each person and pick a random number
  # between 1 and the group size.
  # Some groups are large, some only have one member so they're not really random
  # set.seed ensures that the randomization will produce the same results each
  # time, MUST BE REMOVED FOR PRODUCTION!
  set.seed(12)
  persons_matched_random<- persons_matched %>%
    dplyr::group_by(persid) %>%
    dplyr::summarize(group_size=dplyr::n()) %>%
    ungroup() %>%
    rowwise() %>%
    mutate(random_sample=round(runif(1, min=1, max=group_size)))
  
  # Making the final table
  persons_matched_final <- persons_matched_random %>%
    left_join(persons_matched,
              by=c("persid"="persid","random_sample"="group_number")) 
  
  # Check matches
  
  check_match <- 1-(sum(is.na(persons_matched_final$walk_trans))/nrow(persons_matched_final))
  
  cat(paste0("The percentage of sucessful matches is: ",
             round(check_match*100,2),"%"))
 
  
  ### Add demographic groups to match with ITHIMR style code
  
  persons_matched_final<- persons_matched_final %>%
    mutate(dem_index = case_when(age <= 19              & sex ==   "male" ~  1,
                                 age >= 20 & age <=  24 & sex ==   "male" ~  3,
                                 age >= 25 & age <=  29 & sex ==   "male" ~  5,
                                 age >= 30 & age <=  34 & sex ==   "male" ~  7,
                                 age >= 35 & age <=  39 & sex ==   "male" ~  9,
                                 age >= 40 & age <=  44 & sex ==   "male" ~  11,
                                 age >= 45 & age <=  49 & sex ==   "male" ~  13,
                                 age >= 50 & age <=  54 & sex ==   "male" ~  15, 
                                 age >= 55 & age <=  59 & sex ==   "male" ~  17,
                                 age >= 60 & age <=  64 & sex ==   "male" ~ 19,
                                 age >= 65 & age <=  69 & sex ==   "male" ~ 21, 
                                 age >= 70 & age <=  74 & sex ==   "male" ~ 23,
                                 age >= 75 & age <=  79 & sex ==   "male" ~ 25, 
                                 age >= 80 & age <=  84 & sex ==   "male" ~ 27, 
                                 age >= 85 & age <=  89 & sex ==   "male" ~ 29,
                                 age >= 90 & age <=  94 & sex ==   "male" ~ 31,
                                 age >= 95 & age <= 120 & sex ==   "male" ~ 33,
                                 age <= 19              & sex == "female" ~ 2,
                                 age >= 20 & age <=  24 & sex == "female" ~ 4,
                                 age >= 25 & age <=  29 & sex == "female" ~ 6,
                                 age >= 30 & age <=  34 & sex == "female" ~ 8,
                                 age >= 35 & age <=  39 & sex == "female" ~ 10,
                                 age >= 40 & age <=  44 & sex == "female" ~ 12,
                                 age >= 45 & age <=  49 & sex == "female" ~ 14,
                                 age >= 50 & age <=  54 & sex == "female" ~ 16,
                                 age >= 55 & age <=  59 & sex == "female" ~ 18,
                                 age >= 60 & age <=  64 & sex == "female" ~ 20,
                                 age >= 65 & age <=  69 & sex == "female" ~ 22,
                                 age >= 70 & age <=  74 & sex == "female" ~ 24,
                                 age >= 75 & age <=  79 & sex == "female" ~ 26,
                                 age >= 80 & age <=  84 & sex == "female" ~ 28,
                                 age >= 85 & age <=  89 & sex == "female" ~ 30,
                                 age >= 90 & age <=  94 & sex == "female" ~ 32,
                                 age >= 95 & age <= 120 & sex == "female" ~ 34)) %>%
    
    ### participant_w present in both PA and travel data frame, we are inteterested in the travel weights
    rename(participant_wt = participant_wt.x)
  
  ### Select variables ### Keep participant_wt for travel survey
  
  persons_matched_final <- persons_matched_final %>%
    dplyr::select(persid, participant_wt, age, sex, ses, dem_index, work_status, age_group_scen, age_group_2,
                  occupation_cat, industry_cat, work_full, study_full,
                  mod_total_hr, vig_total_hr, mod_leis_hr, vig_leis_hr, mod_work_hr, vig_work_hr, walk_rc, walk_trans, walk_base,
                  pa_guide_adults, pa_guide_older_adults,
                  time_base_car , distance_base_car,
                  time_base_walking, distance_base_walking, 
                  time_base_public.transport, distance_base_public.transport,
                  time_base_other, distance_base_other,
                  time_base_bicycle, distance_base_bicycle,
                  time_scen_car, distance_scen_car,
                  time_scen_walking, distance_scen_walking, 
                  time_scen_public.transport, distance_scen_public.transport,
                  time_scen_other, distance_scen_other,
                  time_scen_bicycle, distance_scen_bicycle,
                  walk_base, walk_scen)
  
  return(persons_matched_final)
}
# write_csv(persons_matched_final, "Data/processed/matched_pop.csv")



