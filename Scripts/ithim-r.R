
rm (list = ls())

### Get packages to run code
library(devtools)
# install_github("ITHIM/ITHIM-R")
library(ithimr)
library(readr)
library(data.table)
library(dplyr)

### Prepare data
#### GBD data and population
source("Scripts/data_prep/ithim_gbd_prep.R")
# this outputs a list containing gbd_melbourne and population_melbourne
GBDandPopulation <- calculateGBDandPopulation(
  gbd_melbourne_ithimr_location="Data/original/gbd/gbd_ithimr.csv",
  population_melbourne_abs_location="Data/original/abs/population_melbourne_abs.csv"
)
gbd_melbourne <- GBDandPopulation[[1]]
population_melbourne <- GBDandPopulation[[2]]
write.csv(gbd_melbourne,"Data/processed/ithimr/gbd_melbourne.csv", row.names=F, quote=T)
write.csv(population_melbourne,"Data/processed/ithimr/population_melbourne.csv", row.names=F, quote=T)
 



### Run ITHIM set up

setup_call_summary_filename <- 'setup_call_summary.txt"'
parameters <- ithimr::run_ithim_setup(
  seed = 1,
  setup_call_summary_filename = 'setup_call_summary.txt',
  PATH_TO_LOCAL_DATA = "Data/input/ithimr/",
  AGE_RANGE = c(15,120),
  TEST_WALK_SCENARIO=T, 
  ADD_WALK_TO_BUS_TRIPS=F,
  CITY='melbourne',
  ADD_TRUCK_DRIVERS = F,
  ADD_BUS_DRIVERS = F,
  
  # PM_TRANS_SHARE = 0.4,
  # PM_CONC_BASE = 20,
  
  speeds =  list( ### Added manually, get from data
    car=21,
    pedestrian=4.8,
    bicycle=14.5,
    motorcycle=25,1,
    bus=21,
    tram=28,
    rail=35,
    other=21),
  
  emission_inventory <- list( ### added manually
    bus=0,
    bus_driver=0.82,
    car=0.228,
    taxi=0.011,
    walking=0,
    bicycle=0,
    motorcycle=0.011,
    truck=0.859,
    big_truck=0.711,
    other=0.082)
  #   
  # NSAMPLES = 1,
  # MMET_WALKING = c((2.53),(1.2)),
  # MMET_CYCLING = c((4.63),(1.2)),
  # PM_CONC_BASE = c((50), (1.3)),
  # PM_TRANS_SHARE = c(5,20)
  )
# 
# INJURY_REPORTING_RATE = c(8,3), 
# 
# CHRONIC_DISEASE_SCALAR = c((1), (1.2)),  
# 
# BACKGROUND_PA_SCALAR = c((1), (1.2)),   
# 
# BUS_TO_PASSENGER_RATIO = c(20,600),
# 
# TRUCK_TO_CAR_RATIO = c(3,10),
# 
# DISTANCE_SCALAR_CAR_TAXI = c(1,(1.2)),
# 
# DISTANCE_SCALAR_MOTORCYCLE = c(1,(1.2)),
# 
# DISTANCE_SCALAR_WALKING = c(1,(1.2)),
# 
# DISTANCE_SCALAR_CYCLING = c(1,(1.2)),
# 
# DISTANCE_SCALAR_PT = c(1,(1.2)),
# 
# PA_DOSE_RESPONSE_QUANTILE = T,  
# 
# AP_DOSE_RESPONSE_QUANTILE = T,
# 
# DAY_TO_WEEK_TRAVEL_SCALAR = 7,#c(20,3),
# 
# SIN_EXPONENT_SUM= c((1.5),(1.1)),
# 
# CASUALTY_EXPONENT_FRACTION = c(15,15),
# 
# EMISSION_INVENTORY_CONFIDENCE = 0.5,
# 
# BACKGROUND_PA_CONFIDENCE = 0.5)

# saving and restoring the parameters
saveRDS(parameters, "Data/Processed/parameters.rds")
parameters <- readRDS("Data/Processed/parameters.rds")
### Check inputs
ithimr::summarise_ithim_inputs(parameters)                                
                                
### Run ITHIM deterministic outcomes
parameters$outcomes<- run_ithim(parameters, seed = 1)


### Run ITHIM uncertainty 

parameters$outcomes <- mclapply(1:10, FUN=run_ithim, ithim_object=parameters,
                                mc.cores = ifelse(Sys.info()[['sysname']] == "Windows",  1,  10))

for(i in 1:NSAMPLES) print(length(ithim_object$outcomes[[i]]))

plot(ithim_object$parameters$AP_DOSE_RESPONSE_QUANTILE_GAMMA_cvd_ihd,sapply(ithim_object$outcomes,function(x)sum(x$hb$deaths[,10])))

### Calculate PIFs from baseline and scenario levels for physical activity, air pollution and injuries to use in MSLT


### ALAN THIS IS AN EXAMPLE OF HOW THE HEALTH BURDEN 2 FUNCTION SUMMARISES THE PIFS
### Calculate health burden by age groups using metahit function
#### Calculate RRs per person for physical activity (mmets) and air pollution for baseline and scenario
RR_PA_calculations <- ithimr::gen_pa_rr(parameters[["outcomes"]][["mmets"]])
RR_AP_calculations <- ithimr::gen_ap_rr(parameters[["outcomes"]][["pm_conc_pp"]])

#### Calculate combined RRs for air pollution and physical activity
RR_PA_AP_calculations <- ithimr::combined_rr_ap_pa(RR_PA_calculations,RR_AP_calculations) %>%
  dplyr::mutate(dem_index = group_indices(., age_cat, sex)) ### Added here to add 

### Calculate PIFs by age and sex for air pollution and physical activity combined
#### health_burden2 was created by Rob J for metahit
pifs_pa_ap <- health_burden_2(RR_PA_AP_calculations)

### Calculate difference in mortality rates for injuries to use in mslt

pif_injuries_deaths <- parameters[["outcomes"]][["injuries"]] %>% dplyr::select(age_cat, sex, sex_age, dem_index, Deaths, scenario) %>%
                                                                  dplyr::rename(age = age_cat) %>%
                                                                  pivot_wider(names_from = scenario, values_from = Deaths) %>%
                                                                  left_join(DEMOGRAPHIC, by = c("age", "sex")) %>%
                                                                  dplyr::mutate(death_rate_base = Baseline/population) %>%
                                                                  dplyr::mutate(death_rate_base = walking/population) %>%
                                                                  dplyr::mutate(deaths_injuries_diff = ((walking/population) - (Baseline/population)))





##### UP TO HERE





#### To do, exand Pifs, but road injuries PIFs missing, need to add them on. See how to use ITHIM functions to generate pifs by mode


pif <- pifs
pif$age_cat <- pif$age
pif$age <- 0
pif$age [pif$age_cat =="16-19"] <- 17
pif$age [pif$age_cat =="20-24"] <- 22
pif$age [pif$age_cat =="25-29"] <- 27
pif$age [pif$age_cat =="30-34"] <- 32
pif$age [pif$age_cat =="35-39"] <- 37
pif$age [pif$age_cat =="40-44"] <- 42
pif$age [pif$age_cat =="45-49"] <- 47
pif$age [pif$age_cat =="50-54"] <- 52
pif$age [pif$age_cat =="55-59"] <- 57
pif$age [pif$age_cat =="60-64"] <- 62
pif$age [pif$age_cat =="65-69"] <- 67
pif$age [pif$age_cat =="70-74"] <- 72
pif$age [pif$age_cat =="75-79"] <- 77
pif$age [pif$age_cat =="80-84"] <- 82
pif$age [pif$age_cat =="85-89"] <- 87
pif$age [pif$age_cat =="90-94"] <- 92
pif$age [pif$age_cat =="95-120"] <- 97

## Change names to get rid of risk factors combinations in the name (BEST IF I DO NOT HAVE TO DO THIS MANUALLY)

names(pif)[names(pif) == "scen_pif_pa_ap_noise_no2_ihd"] <- "pif_ihd"
names(pif)[names(pif) == "scen_pif_pa_ap_stroke"] <- "pif_stroke"
names(pif)[names(pif) == "scen_pif_pa_colon" ] <- "pif_colon"
names(pif)[names(pif) == "scen_pif_pa_t2d"] <- "pif_t2d"
names(pif)[names(pif) == "scen_pif_pa_endo"] <- "pif_endo"
names(pif)[names(pif) == "scen_pif_pa_ap_lc"] <- "pif_lc"
names(pif)[names(pif) == "scen_pif_ap_lri"] <- "pif_lri"
names(pif)[names(pif) == "scen_pif_ap_copd"] <- "pif_copd"
names(pif)[names(pif) == "scen_pif_pa_breast"] <- "pif_breast"

### Belen: finish road injuries

# names(pif)[names(pif) == "scen_cyclist_Fatal"] <- "pif_cyclist_deaths"
# names(pif)[names(pif) == "scen_pedestrian_Fatal"] <- "pif_pedestrian_deaths"
# names(pif)[names(pif) == "scen_cyclist_Serious"] <- "pif_cyclist_ylds"
# names(pif)[names(pif) == "scen_pedestrian_Serious"] <- "pif_pedestrian_ylds"
# names(pif)[names(pif) == "scen_car/taxi_Fatal"] <- "pif_motor_deaths"
# names(pif)[names(pif) == "scen_motorcycle_Fatal"  ] <- "pif_motorcyclist_deaths"
# names(pif)[names(pif) == "scen_car/taxi_Serious"] <- "pif_motor_ylds"
# names(pif)[names(pif) == "scen_motorcycle_Serious"  ] <- "pif_motorcyclist_ylds"


## Repeat pif lri for deaths and ylds

pif$pif_lri_deaths <- pifs$scen1_pif_ap_lri
pif$pif_lri_ylds <- pifs$scen1_pif_ap_lri

### mslt_df names are not matching pifs names, need to change this, preferably, not manually

#### MANUALLY TO CHECK THAT IT WORKS FOR ROAD INJURIES

#### TO DO BElen



p <- pif[pif$sex == "male",]

p <- pif[pif$sex == "male",]

outage <- min(p$age):100

ind <- findInterval(outage, p$age)
pif_expanded <- p[ind,]
pif_expanded$age <- outage

p_1 <- pif[pif$sex == "female",]

outage <- min(p_1$age):100

ind <- findInterval(outage, p_1$age)
pif_expanded_1 <- p_1[ind,]
pif_expanded_1$age <- outage


pif_expanded <- rbind(pif_expanded, pif_expanded_1)


