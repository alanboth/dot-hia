### Prepare accidents data from original Victoria data

# rm (list = ls())

# library(readr)
suppressPackageStartupMessages(library(dplyr)) # for manipulating data
suppressPackageStartupMessages(library(tidyr)) # for pivoting data



### Data from here: 
### Helpful tabulation to understand codes: https://public.tableau.com/views/Crashstatdetail/Accidentdatapreview?:embed=y&:display_count=yes&:showTabs=y&:showVizHome=no#1&%3Adisplay_count=yes&%3Atoolbar=no&:render=false
### VEHICLE: 
##### vehicle_ID (A,B, C, etc) represent the number of vehicles in one accident
### PERSON: 
##### person ID (A, B, 01, 02) letters are the same as vehicle ID and represent the driver (cyclist/pedestrian is a driver) numbers represent occupants

calculateInjuries <- function(accident_location,accident_event_location,person_location,vehicle_location) {
  # accident_location="Data/VicRoads Road Injuries/Original_ACCIDENT.csv"
  # accident_event_location="Data/VicRoads Road Injuries/Original_ACCIDENT.csv"
  # person_location="Data/VicRoads Road Injuries/Original_PERSON.csv"
  # vehicle_location="Data/VicRoads Road Injuries/Original_VEHICLE.csv"
  
  ACCIDENT <- read.csv(accident_location,as.is=T) %>%
    select(ACCIDENT_NO, ACCIDENTDATE, NO_OF_VEHICLES, Accident.Type.Desc)
  
  ACCIDENT_EVENT <- read.csv(accident_event_location,as.is=T)
  
  PERSON <- read.csv(person_location,as.is=T) %>%
    select(ACCIDENT_NO, PERSON_ID, VEHICLE_ID, SEX, AGE, Age.Group,
           Inj.Level.Desc, Road.User.Type.Desc)
  
  VEHICLE <- read.csv(vehicle_location,as.is=T) %>%
    select(ACCIDENT_NO, VEHICLE_ID, vehicle_type=Vehicle.Type.Desc) %>%
    mutate(vehicle_type = case_when(vehicle_type == "Motor Cycle" ~ 'motorcycle',
                                    vehicle_type == "Light Commercial Vehicle (Rigid) <= 4.5 Tonnes GVM" ~ 'lightcom',
                                    vehicle_type == "Panel Van" ~ 'van',
                                    vehicle_type == "Bus/Coach" ~ 'bus',
                                    vehicle_type == "Panel Van" ~ 'Van',
                                    vehicle_type == "Unknown" ~ 'unknown',
                                    vehicle_type == "Panel Van" ~ 'Van',
                                    vehicle_type == "Other Vehicle" ~ 'other',
                                    vehicle_type == "Heavy Vehicle (Rigid) > 4.5 Tonnes" ~ 'truck',
                                    vehicle_type == "Prime Mover Only" ~ 'truck',
                                    vehicle_type == "Tram" ~ 'tram',
                                    vehicle_type == "Moped" ~ 'moped',
                                    vehicle_type == "Horse (ridden or drawn)" ~ 'other',
                                    vehicle_type == "Prime Mover B-Triple" ~ 'truck',
                                    vehicle_type == "Plant machinery and Agricultural equipment" ~ 'truck',
                                    vehicle_type == "Car" ~ 'car',
                                    vehicle_type == "Station Wagon" ~ 'car',
                                    vehicle_type == "Prime Mover (No of Trailers Unknown)" ~ 'truck',
                                    vehicle_type == "Bicycle" ~ 'bicycle',
                                    vehicle_type == "Taxi" ~ 'car',
                                    vehicle_type == "Rigid Truck(Weight Unknown)" ~ 'truck',
                                    vehicle_type == "Mini Bus(9-13 seats)" ~ 'car',
                                    vehicle_type == "Mini Bus(9-13 seats)" ~ 'car',
                                    vehicle_type == "Motor Scooter" ~ 'motorcycle',
                                    vehicle_type == "Train" ~ 'train',
                                    vehicle_type == "Not Applicable" ~ 'unknown',
                                    vehicle_type == "Prime Mover B-Double" ~ 'truck',
                                    vehicle_type == "Prime Mover - Single Trailer" ~ 'truck',
                                    vehicle_type == "Quad Bike" ~ 'motorcycle',
                                    vehicle_type == "Parked trailers" ~ 'other',
                                    vehicle_type == "Utility" ~ 'utility')) %>%
    #### Add column with hierarchy
    mutate(hierc = case_when(vehicle_type == "unknow" ~ 1,
                             vehicle_type == "other" ~ 2,
                             vehicle_type == "bicycle" ~ 3,
                             vehicle_type == "moped" ~ 4,
                             vehicle_type == "motorcycle" ~ 5,
                             vehicle_type == "car" ~ 6,
                             vehicle_type == "van" ~ 7,
                             vehicle_type == "utility" ~ 8,
                             vehicle_type == "lightcom" ~ 9,
                             vehicle_type == "bus" ~ 10,
                             vehicle_type == "truck" ~ 11,
                             vehicle_type == "tram" ~ 12,
                             vehicle_type == "train" ~ 13))
  
  injuries_melbourne <- PERSON %>%
    left_join(VEHICLE, by=c("ACCIDENT_NO", "VEHICLE_ID")) %>%
    rename(cas_type=Inj.Level.Desc) %>%
    ### Keep persons with injuries or fatality
    filter(cas_type %in% c("Other injury","Serious injury","Fatality")) %>%
    ### Drop SEX "U", this needs revisions, not sure what sex U means (it's Unknown). 
    filter(SEX %in% c("M","F")) %>%
    mutate(cas_mode=ifelse(is.na(vehicle_type), tolower(Road.User.Type.Desc), tolower(vehicle_type)))
  
  ### Sort vehicle type in VEHICLE FILE to then match with injuires MEL for striking mode

  #### Create data frame from VEHICLES with max hierchacy vehicle involved
  hierc_max <- VEHICLE %>%
    # Keep ACCIDENT_NO that match injuries_melbourne (road accidents with
    # causalities or deaths)
    filter(ACCIDENT_NO %in% injuries_melbourne$ACCIDENT_NO) %>%
    group_by(ACCIDENT_NO) %>%
    slice(which.max(hierc)) %>%
    ungroup() %>%
    select(ACCIDENT_NO, vehicle_type, hierc) %>%
    rename(strike_mode=hierc)
    
  #### ACCIDENT data derive (to derive accidents where there is no other vehicle involved)
  ACCIDENT <- ACCIDENT %>%
    filter(ACCIDENT_NO %in% injuries_melbourne$ACCIDENT_NO)
  
  ##### Join injuries mel (from PERSON data frame), strike_mode_id (from VEHICLE data frame) and ACCIDENT
  injuries_melbourne2 <- injuries_melbourne %>%
    inner_join(hierc_max, by = "ACCIDENT_NO") %>%
    inner_join(ACCIDENT, by = "ACCIDENT_NO") %>% 
    select(ACCIDENT_NO, PERSON_ID, VEHICLE_ID, AGE, SEX, Age.Group, 
           cas_type, vehicle_type.x, hierc, cas_mode, vehicle_type.y,
           strike_mode, ACCIDENTDATE, NO_OF_VEHICLES, Accident.Type.Desc) %>%
    #### Create year and weight data and clean data
    separate(ACCIDENTDATE, into =  c(NA, "year"), sep = c(-4)) %>%
    mutate(strike_mode=vehicle_type.y) %>%
    mutate(year=as.numeric(year)) %>%
    rename(cas_age=AGE) %>%
    rename(cas_gender=SEX) %>%
    rename(event_id=ACCIDENT_NO) %>%
    mutate(cas_gender = case_when(cas_gender == "F" ~ 'female',
                                  cas_gender == "M" ~ 'male')) %>%
    select(event_id, cas_age, cas_gender, cas_mode, strike_mode, year) %>%
    mutate(cas_mode = case_when(cas_mode == "van" ~ "car",
                                cas_mode == "lightcom" ~ "truck",
                                cas_mode == "utility" ~ "car",
                                cas_mode == "moped" ~ "other",
                                cas_mode == "unknown" ~ "other",
                                cas_mode == "pedestrians" ~ "pedestrian",
                                TRUE ~ cas_mode)) %>%
    mutate(strike_mode = case_when(strike_mode == "van" ~ "car",
                                   strike_mode == "lightcom" ~ "truck",
                                   strike_mode == "utility" ~ "car",
                                   strike_mode == "moped" ~ "other",
                                   TRUE ~ strike_mode)) %>%
    ### Remove observations with NAs. NEED to revise, for now. (3823, 300 observations lost)
  drop_na()
  
  return(injuries_melbourne2)
  
  # saveRDS(injuries_melbourne2, "/Data/Processed/injuries_melbourne.Rds")
  # write_csv(injuries_melbourne2, "/Data/Processed/injuries_melbourne.csv")
  
  #### Inspect subset
  # injuries_melbourne_sub <- injuries_melbourne[1:1000,]
}



# ### FOR NOW just model deaths, then add serious injuries
# # injuries_melbourne <- injuries_melbourne %>% dplyr::filter(cas_type == "Serious injury" | cas_type == "Fatality")
# #### Group injuries_melbourne types (add reclassification to technical appenix)
# modes_hierc <- c("train","tram", "truck", "bus", "lightcom", "utility", "van",
#                  "car", "motorcycle", "moped", "bicycle", "other", "unknown")
