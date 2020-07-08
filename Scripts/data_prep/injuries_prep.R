### Prepare accidents data from original Victoria data

rm (list = ls())

library(readr)
library(dplyr)
library(tidyr)


### Data from here: 
### HelpfUl tabulation to understand codes: https://public.tableau.com/views/Crashstatdetail/Accidentdatapreview?:embed=y&:display_count=yes&:showTabs=y&:showVizHome=no#1&%3Adisplay_count=yes&%3Atoolbar=no&:render=false
### VEHICLE: 
##### vehicle_ID (A,B, C, etc) reperesent the number of vehicles in one accident
### PERSON: 
##### person ID (A, B, 01, 02) letters are the same as vehicle ID and represent the driver (cyclist/pedestrian is a driver) numbers represent occupants

ACCIDENT <- read_csv("Data/VicRoads Road Injuries/Original_ACCIDENT.csv")
ACCIDENT_EVENT <- read_csv("Data/VicRoads Road Injuries/Original_ACCIDENT_EVENT.csv")
PERSON <- read_csv("Data/VicRoads Road Injuries/Original_PERSON.csv")
VEHICLE <- read_csv("Data/VicRoads Road Injuries/Original_VEHICLE.csv")


injuries_melbourne <- PERSON %>% left_join(VEHICLE) 

### Keep persons with injuries or fatality
names(injuries_melbourne)[names(injuries_melbourne)=="Inj Level Desc"] <- "cas_type"
injuries_melbourne <- injuries_melbourne %>% dplyr::filter(cas_type == "Fatality")

### FOR NOW just model deaths, then add serious injuries
# injuries_melbourne <- injuries_melbourne %>% dplyr::filter(cas_type == "Serious injury" | cas_type == "Fatality")

### Drop SEX "U", this needs revisions, not sure what sex U means. 

injuries_melbourne <- dplyr::filter(injuries_melbourne, SEX != "U")

names(injuries_melbourne)[names(injuries_melbourne)=="Vehicle Type Desc"] <- "vehicle_type"

#### Group injuires_melbourne types (add reclassification to technical appenix)

injuries_melbourne$vehicle_type[injuries_melbourne$vehicle_type=='Motor Cycle'] <- 'motorcycle'
injuries_melbourne$vehicle_type[injuries_melbourne$vehicle_type =="Light Commercial Vehicle (Rigid) <= 4.5 Tonnes GVM"] <- 'lightcom'
injuries_melbourne$vehicle_type[injuries_melbourne$vehicle_type =="Panel Van"] <- 'van'
injuries_melbourne$vehicle_type[injuries_melbourne$vehicle_type =="Bus/Coach"] <- 'bus'
injuries_melbourne$vehicle_type[injuries_melbourne$vehicle_type =="Panel Van"] <- 'Van'
injuries_melbourne$vehicle_type[injuries_melbourne$vehicle_type =="Unknown"] <- 'unknown'
injuries_melbourne$vehicle_type[injuries_melbourne$vehicle_type =="Panel Van"] <- 'Van'
injuries_melbourne$vehicle_type[injuries_melbourne$vehicle_type =="Other Vehicle"] <- 'other'
injuries_melbourne$vehicle_type[injuries_melbourne$vehicle_type =="Heavy Vehicle (Rigid) > 4.5 Tonnes"] <- 'truck'
injuries_melbourne$vehicle_type[injuries_melbourne$vehicle_type =="Prime Mover Only"] <- 'truck'
injuries_melbourne$vehicle_type[injuries_melbourne$vehicle_type =="Tram"] <- 'tram'
injuries_melbourne$vehicle_type[injuries_melbourne$vehicle_type =="Moped"] <- 'moped'
injuries_melbourne$vehicle_type[injuries_melbourne$vehicle_type =="Horse (ridden or drawn)"] <- 'other'
injuries_melbourne$vehicle_type[injuries_melbourne$vehicle_type =="Prime Mover B-Triple"] <- 'truck'
injuries_melbourne$vehicle_type[injuries_melbourne$vehicle_type =="Plant machinery and Agricultural equipment"] <- 'truck'
injuries_melbourne$vehicle_type[injuries_melbourne$vehicle_type =="Car"] <- 'car'
injuries_melbourne$vehicle_type[injuries_melbourne$vehicle_type =="Station Wagon"] <- 'car'
injuries_melbourne$vehicle_type[injuries_melbourne$vehicle_type =="Prime Mover (No of Trailers Unknown)"] <- 'truck'
injuries_melbourne$vehicle_type[injuries_melbourne$vehicle_type =="Bicycle"] <- 'bicycle'
injuries_melbourne$vehicle_type[injuries_melbourne$vehicle_type =="Taxi"] <- 'car'
injuries_melbourne$vehicle_type[injuries_melbourne$vehicle_type =="Rigid Truck(Weight Unknown)"] <- 'truck'
injuries_melbourne$vehicle_type[injuries_melbourne$vehicle_type =="Mini Bus(9-13 seats)"] <- 'car'
injuries_melbourne$vehicle_type[injuries_melbourne$vehicle_type =="Mini Bus(9-13 seats)"] <- 'car'
injuries_melbourne$vehicle_type[injuries_melbourne$vehicle_type =="Motor Scooter"] <- 'motorcycle'
injuries_melbourne$vehicle_type[injuries_melbourne$vehicle_type =="Train"] <- 'train'
injuries_melbourne$vehicle_type[injuries_melbourne$vehicle_type =="Not Applicable"] <- 'unknown'
injuries_melbourne$vehicle_type[injuries_melbourne$vehicle_type =="Prime Mover B-Double"] <- 'truck'
injuries_melbourne$vehicle_type[injuries_melbourne$vehicle_type =="Prime Mover - Single Trailer"] <- 'truck'
injuries_melbourne$vehicle_type[injuries_melbourne$vehicle_type =="Quad Bike"] <- 'motorcycle'
injuries_melbourne$vehicle_type[injuries_melbourne$vehicle_type =="Parked trailers"] <- 'other'
injuries_melbourne$vehicle_type[injuries_melbourne$vehicle_type =="Utility"] <- 'utility'

#### Add column with hierchacy

modes_hierc <- c("train","tram", "truck", "bus", "lightcom", "utility", "van", "car", "motorcycle", "moped", "bicycle", "other", "unknow")



injuries_melbourne <- injuries_melbourne %>%  dplyr::mutate(hierc = case_when(vehicle_type == "unknow" ~ 1,
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





injuries_melbourne <- injuries_melbourne %>% dplyr::select(ACCIDENT_NO, PERSON_ID, VEHICLE_ID, AGE, SEX, "Age Group", cas_type, "Road User Type Desc", 
                                               vehicle_type, hierc) %>%
                                                mutate(cas_mode=ifelse(is.na(vehicle_type), tolower(`Road User Type Desc`), tolower(vehicle_type)))

### Sort vehicle type in VEHICLE FILE to then match with injuires MEL for striking mode

names(VEHICLE)[names(VEHICLE)=="Vehicle Type Desc"] <- "vehicle_type"

VEHICLE$vehicle_type[VEHICLE$vehicle_type=='Motor Cycle'] <- 'motorcycle'
VEHICLE$vehicle_type[VEHICLE$vehicle_type =="Light Commercial Vehicle (Rigid) <= 4.5 Tonnes GVM"] <- 'lightcom'
VEHICLE$vehicle_type[VEHICLE$vehicle_type =="Panel Van"] <- 'van'
VEHICLE$vehicle_type[VEHICLE$vehicle_type =="Bus/Coach"] <- 'bus'
VEHICLE$vehicle_type[VEHICLE$vehicle_type =="Panel Van"] <- 'Van'
VEHICLE$vehicle_type[VEHICLE$vehicle_type =="Unknown"] <- 'unknown'
VEHICLE$vehicle_type[VEHICLE$vehicle_type =="Panel Van"] <- 'Van'
VEHICLE$vehicle_type[VEHICLE$vehicle_type =="Other Vehicle"] <- 'other'
VEHICLE$vehicle_type[VEHICLE$vehicle_type =="Heavy Vehicle (Rigid) > 4.5 Tonnes"] <- 'truck'
VEHICLE$vehicle_type[VEHICLE$vehicle_type =="Prime Mover Only"] <- 'truck'
VEHICLE$vehicle_type[VEHICLE$vehicle_type =="Tram"] <- 'tram'
VEHICLE$vehicle_type[VEHICLE$vehicle_type =="Moped"] <- 'moped'
VEHICLE$vehicle_type[VEHICLE$vehicle_type =="Horse (ridden or drawn)"] <- 'other'
VEHICLE$vehicle_type[VEHICLE$vehicle_type =="Prime Mover B-Triple"] <- 'truck'
VEHICLE$vehicle_type[VEHICLE$vehicle_type =="Plant machinery and Agricultural equipment"] <- 'truck'
VEHICLE$vehicle_type[VEHICLE$vehicle_type =="Car"] <- 'car'
VEHICLE$vehicle_type[VEHICLE$vehicle_type =="Station Wagon"] <- 'car'
VEHICLE$vehicle_type[VEHICLE$vehicle_type =="Prime Mover (No of Trailers Unknown)"] <- 'truck'
VEHICLE$vehicle_type[VEHICLE$vehicle_type =="Bicycle"] <- 'bicycle'
VEHICLE$vehicle_type[VEHICLE$vehicle_type =="Taxi"] <- 'car'
VEHICLE$vehicle_type[VEHICLE$vehicle_type =="Rigid Truck(Weight Unknown)"] <- 'truck'
VEHICLE$vehicle_type[VEHICLE$vehicle_type =="Mini Bus(9-13 seats)"] <- 'car'
VEHICLE$vehicle_type[VEHICLE$vehicle_type =="Mini Bus(9-13 seats)"] <- 'car'
VEHICLE$vehicle_type[VEHICLE$vehicle_type =="Motor Scooter"] <- 'motorcycle'
VEHICLE$vehicle_type[VEHICLE$vehicle_type =="Train"] <- 'train'
VEHICLE$vehicle_type[VEHICLE$vehicle_type =="Not Applicable"] <- 'unknown'
VEHICLE$vehicle_type[VEHICLE$vehicle_type =="Prime Mover B-Double"] <- 'truck'
VEHICLE$vehicle_type[VEHICLE$vehicle_type =="Prime Mover - Single Trailer"] <- 'truck'
VEHICLE$vehicle_type[VEHICLE$vehicle_type =="Quad Bike"] <- 'motorcycle'
VEHICLE$vehicle_type[VEHICLE$vehicle_type =="Parked trailers"] <- 'other'
VEHICLE$vehicle_type[VEHICLE$vehicle_type =="Utility"] <- 'utility'

#### Add column with hierchacy

VEHICLE <- VEHICLE %>%  dplyr::mutate(hierc = case_when(vehicle_type == "unknow" ~ 1,
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


#### Create data frame from VEHICLES with max hierchacy vehicle involved

hierc_max <- VEHICLE %>% group_by(ACCIDENT_NO) %>% dplyr::filter(row_number(hierc)==dplyr::n()) %>% 
  dplyr::select(ACCIDENT_NO, vehicle_type, hierc)
names(hierc_max)[names(hierc_max)=="hierc"] <- "strike_mode"


#### Keep ACCIDENT_NO which match injuries_melbourne (road accidents with causalities or deaths)

#### hier_max data derive from VEHICLES
data.1.ID <- paste(hierc_max$ACCIDENT_NO)

keep.these.ID <- paste(injuries_melbourne$ACCIDENT_NO)

strike_mode_id <- hierc_max[data.1.ID %in% keep.these.ID,]



#### ACCIDENT data derive (to derive accidents where there is no other vehicle involved)


data.2.ID <- paste(ACCIDENT$ACCIDENT_NO)

ACCIDENT <- ACCIDENT[data.2.ID %in% keep.these.ID,]



##### Join injuires mel (from PERSON data frame), strike_mode_id (from VEHICLE data frame) and ACCIDENT

injuries_melbourne <- injuries_melbourne %>% inner_join(strike_mode_id, by = "ACCIDENT_NO") %>% inner_join(ACCIDENT, by = "ACCIDENT_NO")

#### Create year and weight data and clean data

injuries_melbourne <- injuries_melbourne %>% dplyr::select(ACCIDENT_NO, PERSON_ID, VEHICLE_ID, AGE, SEX, "Age Group", 
                                                           cas_type, vehicle_type.x, hierc, cas_mode, vehicle_type.y,
                                                           strike_mode, ACCIDENTDATE, NO_OF_VEHICLES, "Accident Type Desc") %>%
                                                          separate(ACCIDENTDATE, into =  c(NA, "year"), sep = c(-4))

# injuries_melbourne$strike_mode <- ifelse(injuries_melbourne$NO_OF_VEHICLES == 1, "none", injuries_melbourne$vehicle_type.y)
injuries_melbourne$strike_mode <- injuries_melbourne$vehicle_type.y

injuries_melbourne$year <- as.numeric(injuries_melbourne$year)


names(injuries_melbourne)[names(injuries_melbourne) == "AGE"] <- "cas_age"
names(injuries_melbourne)[names(injuries_melbourne) == "SEX"] <- "cas_gender"
names(injuries_melbourne)[names(injuries_melbourne) == "ACCIDENT_NO"] <- "event_id"

injuries_melbourne$cas_gender[injuries_melbourne$cas_gender =="F"] <- 'female'
injuries_melbourne$cas_gender[injuries_melbourne$cas_gender =="M"] <- 'male'

injuries_melbourne <- injuries_melbourne %>% dplyr::select(event_id, cas_age, cas_gender, cas_mode, strike_mode, year)


injuries_melbourne$cas_mode[which(injuries_melbourne$cas_mode == "van")] = "car"
injuries_melbourne$cas_mode[which(injuries_melbourne$cas_mode == "lightcom")] = "truck"
injuries_melbourne$cas_mode[which(injuries_melbourne$cas_mode == "utility")] = "car"
injuries_melbourne$cas_mode[which(injuries_melbourne$cas_mode == "moped")] = "other"
injuries_melbourne$cas_mode[which(injuries_melbourne$cas_mode == "unknown")] = "other"
injuries_melbourne$cas_mode[which(injuries_melbourne$cas_mode == "pedestrians")] = "pedestrian"


injuries_melbourne$strike_mode[which(injuries_melbourne$strike_mode == "van")] = "car"
injuries_melbourne$strike_mode[which(injuries_melbourne$strike_mode == "lightcom")] = "truck"
injuries_melbourne$strike_mode[which(injuries_melbourne$strike_mode == "utility")] = "car"
injuries_melbourne$strike_mode[which(injuries_melbourne$strike_mode == "moped")] = "other"

### Remove observations with NAs. NEED to revise, for now. (3823, 300 observations lost)

injuries_melbourne <- injuries_melbourne %>% drop_na()


saveRDS(injuries_melbourne, (paste0(getwd(), "/Data/Processed/injuries_melbourne.Rds")))

write_csv(injuries_melbourne, (paste0(getwd(), "/Data/Processed/injuries_melbourne.csv")))


#### Inspect subset

injuries_melbourne_sub <- injuries_melbourne[1:1000,]



