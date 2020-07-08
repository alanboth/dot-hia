# SYNTHETIC POPULATION OF VISTA PERSONS AND NHS PERSONS
rm (list = ls())
#Load package.
library(rlang)
library(tidyverse)
library(readr)
library(plyr)
library(dplyr)
library(tidyr)
library(survey)
library(srvyr)
source("Scripts/functions_mslt.R")



# TRAVEL DATA PERSON FILE

## Join VISTA persons to VISTA Households
### Get data
hh_VISTA <- read_csv(paste0(getwd(), "/Data/Travel survey/VISTA 12-18/H_VISTA_1218_V1.csv"))
person_VISTA <- read_csv(paste0(getwd(), "/Data/Travel survey/VISTA 12-18/P_VISTA1218_V1.csv"))

### Join persons and household, keep data for greater Melb only and create unique weights

persons_travel <- left_join(person_VISTA, hh_VISTA, by = c("HHID")) %>% 
                  dplyr::filter(SurveyPeriod == "2017-18" & (HomeSubRegion != "Geelong" | HomeSubRegion != "Other")) %>%
                  dplyr::mutate(WDPERSWGT = replace_na(WDPERSWGT, "")) %>%
                  dplyr::mutate(WEPERSWGT = replace_na(WEPERSWGT, "")) %>%
                  tidyr::unite("participant_wt", WDPERSWGT:WEPERSWGT , na.rm = TRUE, remove = TRUE, sep = "") %>% 
                  dplyr::mutate_at("participant_wt", funs(as.numeric))


## Add SEIFA-IRSD

ses_index <- read_csv(paste0(getwd(), "/Data/Travel survey/ABS SEIFA/ses.csv")) %>% drop_na()

persons_travel$ses <- as.numeric(ses_index$`Decile ranking of the POA within Australia`[match(persons_travel$HOMEPC, ses_index$`2016 Postal Area (POA) Code`)])



### Create age category as persons_pa is only available by age groups

names(persons_travel)[names(persons_travel)=="AGE"] <- "age1"


persons_travel <- persons_travel %>%  dplyr::mutate(age = dplyr::case_when(age1 < 5 ~ 1,
                                                                      age1 >= 5 & age1 <=9 ~ 2,
                                                                      age1 >= 10 & age1 <=14 ~ 3,
                                                                      age1 >= 15 & age1 <=17 ~ 4, 
                                                                      age1 >= 18 & age1 <=19 ~ 5,
                                                                      age1 >= 20 & age1 <=24 ~ 6,
                                                                      age1 >= 25 & age1 <=29 ~ 7, 
                                                                      age1 >= 30 & age1 <=34 ~ 8, 
                                                                      age1 >= 35 & age1 <=39 ~ 9, 
                                                                      age1 >= 40 & age1 <=44 ~ 10,
                                                                      age1 >= 45 & age1 <=49 ~ 11, 
                                                                      age1 >= 50 & age1 <=54 ~ 12, 
                                                                      age1 >= 55 & age1 <=59 ~ 13, 
                                                                      age1 >= 60 & age1 <=64 ~ 14, 
                                                                      age1 >= 65 & age1 <=69 ~ 15,
                                                                      age1 >= 70 & age1 <=74 ~ 16, 
                                                                      age1 >= 75 & age1 <=79 ~ 17,
                                                                      age1 >= 80 & age1 <=84 ~ 18,
                                                                      age1 >= 85 ~ 19))

#### Only keep adults over 18

persons_travel <- persons_travel %>% dplyr::filter(age>4)

### Sex to match persons_pa sex variable
persons_travel$SEX[persons_travel$SEX =="M"] <- 'male'
persons_travel$SEX[persons_travel$SEX =="F"] <- 'female'


### Employment status to match persons_pa work_status variable (LFSBC)
persons_travel$work_status <- persons_travel$ANYWORK
persons_travel$work_status[persons_travel$work_status =="YES"] <- 'employed'
persons_travel$work_status[persons_travel$work_status =="NO"] <- 'unemployed'


### Classification of occupation (ANZSCO1)
persons_travel$occupation_cat <- persons_travel$ANZSCO1
persons_travel$occupation_cat[persons_travel$occupation_cat =="Missing/Refused"] <- NA

### Classification for industry
persons_travel$industry_cat <- persons_travel$ANZSIC1
persons_travel$industry_cat[persons_travel$industry_cat =="Missing/Refused"] <- NA

### Full time variable
persons_travel$work_full <- persons_travel$FULLTIMEWORK
persons_travel$work_full[persons_travel$work_full == "Part-time TAFE/Uni"] <- "Full time"
persons_travel$work_full[persons_travel$work_full == "Part-time TAFE/Uni"] <- "Part time"
persons_travel$work_full[persons_travel$work_full == "No Study"] <- NA
persons_travel$work_full[persons_travel$work_full == "Something Else"] <- "Other"


### Change observations to match pa data
persons_travel$study_full <- persons_travel$STUDYING
persons_travel$study_full[persons_travel$study_full =="Missing/Refused"] <- NA
persons_travel$study_full[persons_travel$study_full =="No Study"] <- NA
persons_travel$study_full[persons_travel$study_full =="Secondary"] <- NA
persons_travel$study_full[persons_travel$study_full =="Part-time TAFE/Uni"] <- "Part time"
persons_travel$study_full[persons_travel$study_full =="Full-time TAFE/Uni"] <- "Full time"
persons_travel$study_full[persons_travel$study_full =="Something Else"] <- NA


### USE SCENARIOS TRIPS SET
### Add variable with total transport walking and cycling time and distance from trips_melbourne

trips_melbourne <- read_csv(paste0(getwd(), "/Data/Processed/trips_melbourne_scenarios.csv")) 
                  
trips_melbourne$PERSID <- toupper(trips_melbourne$PERSID)


# trips_melbourne$PERSID <- as.factor(trips_melbourne$PERSID) 


### Create total duration and distance for all modes, rather long process here. The intervention will change the trips file (sceanrio trips file)
### which in turn will feed onto the persons file. 



trips_dur_time_base <- list()
index <- 1

for (m in c(unique(trips_melbourne$trip_mode_base))) {
  


data <-   dplyr::select(trips_melbourne, PERSID, trip_mode_base, trip_duration_base, trip_distance_base, day_type) %>%
                    dplyr::filter(trip_mode_base == m) 
 
data2 <- data %>%
group_by(PERSID) %>%
  summarise_at(vars(trip_duration_base),
             list(time_base = sum))

names(data2)[names(data2) == 'time_base'] <-
  paste('time_base', tolower(m), sep = '_')

data3 <- data %>%
  group_by(PERSID) %>%
  summarise_at(vars(trip_distance_base),
               list(distance_base = sum))

names(data3)[names(data3) == 'distance_base'] <-
  paste('distance_base', tolower(m), sep = '_')


trips_dur_time_base[[index]] <- left_join(data2, data3, by = "PERSID")

index <- index + 1

}

total_trips_time_dist_base <- trips_dur_time_base %>% reduce(left_join, by = "PERSID")

#### Scenario

trips_dur_time_scen <- list()
index <- 1

for (m in c(unique(trips_melbourne$trip_mode_scen))) {
  
  
  
  data <-   dplyr::select(trips_melbourne, PERSID, trip_mode_scen, trip_duration_scen, trip_distance_scen, day_type) %>%
    dplyr::filter(trip_mode_scen == m) 
  
  data2 <- data %>%
    group_by(PERSID) %>%
    summarise_at(vars(trip_duration_scen),
                 list(time_scen = sum))
  
  names(data2)[names(data2) == 'time_scen'] <-
    paste('time_scen', tolower(m), sep = '_')
  
  data3 <- data %>%
    group_by(PERSID) %>%
    summarise_at(vars(trip_distance_scen),
                 list(distance_scen = sum))
  
  names(data3)[names(data3) == 'distance_scen'] <-
    paste('distance_scen', tolower(m), sep = '_')
  
  
  trips_dur_time_scen[[index]] <- left_join(data2, data3, by = "PERSID")
  
  index <- index + 1
  
}

total_trips_time_dist_scen <- trips_dur_time_scen %>% reduce(left_join, by = "PERSID")

### Do this last appending all

persons_travel <- persons_travel %>% left_join(total_trips_time_dist_base, by = "PERSID")%>% left_join(total_trips_time_dist_scen, by = "PERSID")

# ### Create walking yes or no

persons_travel <- persons_travel %>% dplyr::mutate(walk_base = case_when(is.na(time_base_pedestrian) ~ "No",
                                                                         time_base_pedestrian > 0  ~ "Yes")) %>%
                             dplyr::mutate(walk_scen = case_when(is.na(time_scen_pedestrian) ~ "No",
                                                                 time_scen_pedestrian > 0  ~ "Yes"))




names(persons_travel)[1:95] <- RemoveAllWs(tolower(names(persons_travel)[1:95]))

# PA PERSON AND HOUSEHOLD FILE
### Create variables to match with travel survey and for pa analysis
persons_pa <- read_csv(paste0(getwd(),"/Data/Physical activity/NHS2017-18_CSV/NHS17SPB.csv"))

hh <-  read_csv(paste0(getwd(),"/Data/Physical activity/NHS2017-18_CSV/NHS17HHB.csv"))

hh <- hh %>% dplyr::select(ABSHIDB, STATE16, SA1SF2DN, INCDECU1)

persons_pa <- left_join(persons_pa, hh,  by = c("ABSHIDB"))


### Sex to match persons_pa
names(persons_pa)[names(persons_pa)=="SEX"] <- "sex"
persons_pa$sex[persons_pa$sex == 1] <- "male"
persons_pa$sex[persons_pa$sex == 2] <- "female"

### Work status (reclassify "Not in the workforce" coded as 3 and NA coded as 0 to "No" to match NHS PA)

persons_pa <-  persons_pa %>%  dplyr::mutate(work_status = case_when(LFSBC == 0 ~ "No",
                                                                        LFSBC == 1 ~ "Yes",
                                                                        LFSBC == 2 ~ "No",
                                                                        LFSBC == 3 ~ "No"))



### Classification of occupation (OCCUP13B) see Physical Activity-Basic CURF.xls for codes references

persons_pa <- persons_pa %>%  dplyr::mutate(occupation_cat = case_when(OCCUP13B == "0" ~ "Not in Work Force",
                                                                       OCCUP13B == "1" ~ "Managers",
                                                                       OCCUP13B == "2" ~ "Professional",
                                                                       OCCUP13B == "3" ~ "Technicians and Trades Worker", 
                                                                       OCCUP13B == "4" ~ "Community and Personal Service Workers",
                                                                       OCCUP13B == "5" ~ "Clerical and Administrative Worker",
                                                                       OCCUP13B == "6" ~ "Sales Workers", 
                                                                       OCCUP13B == "7" ~ "Machinery Operators and Drivers", 
                                                                       OCCUP13B == "8" ~ "Labourers", 
                                                                       OCCUP13B == "9" ~ "NA"))

persons_pa$occupation_cat[persons_pa$occupation_cat =="NA"] <- NA

### Classification of industry (ANZSICBC) see Physical Activity-Basic CURF.xls for codes references

persons_pa <- persons_pa %>%  dplyr::mutate(industry_cat = case_when(ANZSICBC == 1 ~ "Agriculture, Forestry and Fishing",
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
                                                                       ANZSICBC == 26 ~ "Not in Work Force"))


### Create work_full (yes or no) to indicate whether ind work full time or not. Derive from USHRWKB 
persons_pa <- persons_pa %>%  dplyr::mutate(work_full = case_when(USHRWKB == 0 ~ "NA",
                                                                  USHRWKB == 1 | USHRWKB == 2 | USHRWKB == 3 ~ "No",
                                                                  USHRWKB >= 4 ~ "Yes"))

persons_pa$work_full[persons_pa$work_full =="NA"] <- NA


#### Original variable values: 0. Not applicable 1. Studying full-time 2. Studying part-time 3. Not studying

persons_pa <- persons_pa %>%  dplyr::mutate(study_full = case_when(STDYFTPT  == 0 | STDYFTPT  == 3 ~ "NA",
                                                     STDYFTPT == 1 ~ "Full time",
                                                     STDYFTPT  == 2 ~ "Part time"))

persons_pa$study_full[persons_pa$study_full =="NA"] <- NA

                                                                       
### Sort data names and create PA variables. SES here is for SA2 and in travel data for postcode, need to check.
### Moderate = 5 mets and 4 marginal mets, vigorous 7.5 mets and 6.5 marginal mets and walking 3.5 mets and 2.5 marginal mets
persons_pa <- persons_pa  %>%
  dplyr::mutate(age = as.double(AGEB)) %>%
  dplyr::mutate(ses = SA1SF2DN) %>%
  dplyr::mutate(state = STATE16) %>%
  dplyr::mutate(sex = as.character(sex)) %>%
  dplyr::mutate(ltpa_marg_met = (EXLWMMIN*4 + EXLWVMIN*6.5)/60) %>%
  dplyr::mutate(work_marg_met = (WPAMMIN*4 + WPAVMIN*6.5)/60) %>%
  dplyr::mutate(work_ltpa_marg_met = (MODMINS*4 + VIGMINS*6.5 + EXFSRMIN*2.5)/60) %>%
  dplyr::mutate(walk_trans = EXLWKTNO) %>%
  dplyr::mutate(walk_base = case_when(EXLWKTNO == 0 ~ "No",
                                         EXLWKTNO > 0  ~ "Yes"))

### Add whether participants meet PA guidelines (difference for adults and older adults)
persons_pa <- persons_pa  %>%
  dplyr::mutate(pa_guide_adults = ifelse((EXNUDAYW >=5 & EXNUDST >=2 & (EXWLKTME + EXLWMMIN + EXLWVMIN*2) >= 150), "Yes", "No"))


persons_pa <- persons_pa  %>%
  dplyr::mutate(pa_guide_older_adults = ifelse(EXNUDAYW >= 5 & EXNUDTH >=5,  "Yes", "No"))



##### Randomly allocate leisure time, work time and walk for transport variables from persons_pa to persons_travel

# I am aiming to randomly assign the following variables from dataset persons_pa
# to dataset persons_travel (both datasets are attached): 
# ltpa_marg_met
# work_marg_met
# work_ltpa_met
# work_ltpa_marg_met
# walk_trans



persons_pa_match <- persons_pa %>%
  dplyr::mutate(age_range=cut(age, breaks=seq(0,20,4))) 

persons_travel_match <- persons_travel %>%
  dplyr::mutate(age_range=cut(age, breaks=seq(0,20,4)))

# selecting the match variables and the ones we'll join
persons_pa_match <- persons_pa_match %>%
  dplyr::select(age_range, sex, ses, walk_base, work_status, ltpa_marg_met, work_marg_met,
                 work_ltpa_marg_met, walk_trans, pa_guide_adults, pa_guide_older_adults)


# This joins the two tables based on the match variables. This is all of the 
# possible group compbinations. We then assign a unique per group number.
# Some people have no viable matches, so using a left join means that these
# people will have NA for their values. I'd suggest using wider age ranges.
persons_matched <- left_join(persons_travel_match,persons_pa_match,
                               by=c("age_range","sex", "ses", "walk_base", "work_status")) %>% 
  group_by(persid) %>%
  dplyr::mutate(group_number=row_number()) %>%
  ungroup()

# This we then find the group size for each person and pick a random number
# between 1 and the group size.
# Some groups are large, some only have one member so they're not really random
set.seed(12)
persons_matched_random<- persons_matched %>%
  dplyr::group_by(persid) %>%
  dplyr::summarize(group_size=dplyr::n()) %>%
  dplyr::ungroup() %>%
  dplyr::rowwise() %>%
  dplyr::mutate(random_sample=round(runif(1, min=1, max=group_size)))

# Making the final table
persons_matched_final <- persons_matched_random %>%
  left_join(persons_matched,
            by=c("persid"="persid","random_sample"="group_number")) 

# Check matches

check_match <- 1-(sum(is.na(persons_matched_final$walk_trans))/nrow(persons_matched_final))

## Check proportion meeting PA with original data NHS

pa_match_guide_adults<- dplyr::filter(persons_matched_final, age >= 5 & age <=14)

pa_match_guide_adults_meets <-  prop.table(table(persons_matched_final$pa_guide_adults))


pa_match_guide_older_adults<- dplyr::filter(persons_matched_final, age >14)

pa_match_guide_adults_older_meets <-  prop.table(table(pa_match_guide_older_adults$pa_guide_older_adults))

### Add demographic groups to match with ITHIMR style code

persons_matched_final<- persons_matched_final %>%  dplyr::mutate(dem_index = case_when(age1 <= 19 & sex == "male"  ~ 1,
                                                                    age1 >= 20 & age1 <=24 & sex == "male" ~ 2,
                                                                    age1 >= 25 & age1 <=29 & sex == "male" ~ 3,
                                                                    age1 >= 30 & age1 <=34 & sex == "male" ~ 4,
                                                                    age1 >= 35 & age1 <=39 & sex == "male" ~ 5,
                                                                    age1 >= 40 & age1 <=44 & sex == "male" ~ 6,
                                                                    age1 >= 45 & age1 <=49 & sex == "male" ~ 7,
                                                                    age1 >= 50 & age1 <=54 & sex == "male" ~ 8, 
                                                                    age1 >= 55 & age1 <=59 & sex == "male" ~ 9,
                                                                    age1 >= 60 & age1 <=64 & sex == "male" ~ 10,
                                                                    age1 >= 65 & age1 <=69 & sex == "male" ~ 11, 
                                                                    age1 >= 70 & age1 <=74 & sex == "male" ~ 12,
                                                                    age1 >= 75 & age1 <=79 & sex == "male" ~ 13, 
                                                                    age1 >= 80 & age1 <=84 & sex == "male" ~ 14, 
                                                                    age1 >= 85 & age1 <=89 & sex == "male" ~ 15,
                                                                    age1 >= 90 & age1 <=94 & sex == "male" ~ 16,
                                                                    age1 >= 85 & age1 <=120 & sex == "male" ~ 17,
                                                                    age1 <= 19 & sex == "female" ~ 18,
                                                                    age1 >= 20 & age1 <=24 & sex == "female" ~ 19,
                                                                    age1 >= 25 & age1 <=29 & sex == "female" ~ 20,
                                                                    age1 >= 30 & age1 <=34 & sex == "female" ~ 21,
                                                                    age1 >= 35 & age1 <=39 & sex == "female" ~ 22,
                                                                    age1 >= 40 & age1 <=44 & sex ==  "female" ~ 23,
                                                                    age1 >= 45 & age1 <=49 & sex == "female" ~ 24,
                                                                    age1 >= 50 & age1 <=54 & sex == "female" ~ 25,
                                                                    age1 >= 55 & age1 <=59 & sex == "female" ~ 26,
                                                                    age1 >= 60 & age1 <=64 & sex == "female" ~ 27,
                                                                    age1 >= 65 & age1 <=69 & sex == "female" ~ 28,
                                                                    age1 >= 70 & age1 <=74 & sex == "female" ~ 29,
                                                                    age1 >= 75 & age1 <=79 & sex == "female" ~ 30,
                                                                    age1 >= 80 & age1 <=84 & sex =="female" ~ 31,
                                                                    age1 >= 85 & age1 <=89 & sex == "female" ~ 32,
                                                                    age1 >= 90 & age1 <=94 & sex ==  "female" ~ 33,
                                                                    age1 >= 85 & age1 <=120 & sex == "female" ~ 34))

### Select variables 

persons_matched_final <- persons_matched_final %>% dplyr::select(persid, participant_wt, age1, sex, ses, dem_index, work_status, occupation_cat, industry_cat, work_full,            
                                                                study_full, work_ltpa_marg_met, walk_trans, time_base_car, distance_base_car, time_base_pedestrian, distance_base_pedestrian, 
                                                                time_base_train, distance_base_train, time_base_bus, distance_base_bus,  time_base_tram, distance_base_tram, 
                                                                time_base_other, distance_base_other, time_base_bicycle, distance_base_bicycle, time_base_motorcycle, 
                                                                distance_base_motorcycle, time_scen_car, distance_scen_car, time_scen_pedestrian ,distance_scen_pedestrian, 
                                                                time_scen_train ,distance_scen_train ,time_scen_bus, distance_scen_bus ,time_scen_tram , distance_scen_tram,
                                                                time_scen_other ,distance_scen_other ,time_scen_bicycle ,distance_scen_bicycle ,time_scen_motorcycle, 
                                                                distance_scen_motorcycle, walk_base, walk_scen)

write_csv(persons_matched_final, (paste0(getwd(), "/Data/Processed/matched_pop.csv")))
write_rds(persons_matched_final, (paste0(getwd(), "/Data/Processed/matched_pop.Rds")))


