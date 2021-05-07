# ------ DoT model

#### Differences with AUO model
# Trips: working trips only. 
# Matched population: working population only
# MSLT population: working population from 16 to 65 years of age. 

suppressPackageStartupMessages(library(doParallel))

### Clean Global Environment
rm (list = ls())

### Avoid scientific notation
options(scipen=999)

#### Functions for model
source("Scripts/data_prep/mmet_pp.R")
source("Scripts/ithim-r_wrappers.R")
source("Scripts/data_prep/population_prep.R")


### Outputs location (select your local drive)
scenarioLocation      <- "./scenarios_dot"
scenarioTripsLocation <- "./scenarios_dot/scenarioTrips"
# outputLocation        <- "/home/alan/DATA/dot-hia/dot-outputs-raw"
# combinedLocation      <- "/home/alan/DATA/dot-hia/dot-outputs-combined"
# combinedLocationMMETS <- "/home/alan/DATA/dot-hia/dot-outputs-combined-mmets"
# summarisedLocation    <- "/home/alan/DATA/dot-hia/dot-outputs-summarised"
# finalLocation         <- "output/dot-outputs"
outputLocation        <- "C:/dot-hia/output/dot-outputs-raw"
combinedLocation      <- "C:/dot-hia/output/dot-outputs-combined"
combinedLocationMMETS <- "C:/dot-hia/output/dot-outputs-combined-mmets"
summarisedLocation    <- "C:/dot-hia/output/dot-outputs-summarised"
finalLocation         <- "C:/dot-hia/output/dot-outputs"

### Scenarios 
scenarios_dot <- data.frame(scenario=c("dotFull","dotTrain")) %>%
  mutate(scenario_location=paste0(scenarioLocation,"/",scenario,".csv")) %>%
  mutate(trips_location=paste0(scenarioTripsLocation,"/",scenario,".csv")) %>%
  mutate(output_location=paste0(outputLocation,"/",scenario))


### Run model
print(paste0("iterating through ",nrow(scenarios_dot)," scenarios at ",Sys.time()))
for (i in 1:nrow(scenarios_dot)){
  
  number_cores <- max(1,floor(as.integer(detectCores())*0.8))
  cl <- makeCluster(number_cores)
  cat(paste0("About to start processing results in parallel, using ",number_cores," cores\n"))
  persons_matched=read.csv(scenarios_dot[i,]$scenario_location,as.is=T, fileEncoding="UTF-8-BOM")
  seeds<-1:1
  registerDoParallel(cl)
  start_time = Sys.time()
  results <- foreach::foreach(seed_current=seeds,
                              .combine=rbind,
                              .verbose=F,
                              .packages=c("dplyr","tidyr","stringr","readr","readxl","data.table","srvyr"),
                              .export=c("calculateMMETSperPerson","CalculationModel","gen_pa_rr_wrapper",
                                        "GetParamters","GetPopulation","GetStDevRR","health_burden_2",
                                        "RunDisease","RunLifeTable")
  ) %dopar%
    CalculationModel(seed=seed_current,
                     output_location=scenarios_dot[i,]$output_location,
                     persons_matched)
  end_time = Sys.time()
  end_time - start_time
  stopCluster(cl)
  cat(paste0("\n scenario ",i,"/",nrow(scenarios_dot)," complete at ",Sys.time(),"\n"))
}


#### Save combined outputs (for all scenarios and iterations)
##### Create directories if not created
dir.create(combinedLocation, recursive=TRUE, showWarnings=FALSE)

print(paste0("merging ",nrow(scenarios_dot)," scenario outputs into single file at ",Sys.time()))


# Function to combine outputs
combineOutputs <- function(inputDirectory,outputFile) {
  output_df_files<-list.files(inputDirectory,pattern="*.csv",full.names=T)
  output_df<-lapply(output_df_files,read.csv,header=T) %>%
    bind_rows(.id="run") %>%
    mutate(run=as.integer(run))
  saveRDS(output_df, file=outputFile)
}

### Combine outputs and save (outputs raw here "C:/dot-hia/output/melbourne-outputs-raw" get combined to here "C:/dot-hia/output/melbourne-outputs-combined")

for (i in 1:nrow(scenarios_dot)){
  combineOutputs(paste0(scenarios_dot[i,]$output_location,'/output_df'),
                 paste0(combinedLocation,"/",scenarios_dot[i,]$scenario,".rds"))
  cat(paste0("\n combined scenario ",i,"/",nrow(scenarios_dot)," complete at ",Sys.time(),"\n"))
}

 ### Summarize outputs and save for each scenario ("C:/dot-hia/output/melbourne-outputs-summarised")
#### Saved items include: output_df_aggregate, output_life_expectancy_change, output_life_year_change, output_disease_change

print(paste0("summarising ",nrow(scenarios_dot)," scenario outputs at ",Sys.time()))
for (i in 1:nrow(scenarios_dot)){
  output_df <- readRDS(paste0(combinedLocation,"/",scenarios_dot[i,]$scenario,".rds"))
  summariseOutputs(scenario_location=
                     paste0(summarisedLocation,"/",scenarios_dot[i,]$scenario),
                   output_df)
  cat(paste0("\n combined scenario ",i,"/",nrow(scenarios_dot)," complete at ",Sys.time(),"\n"))
}

### Summarise transport (customised to account for work trips only)
summariseTransport <- function(inputFile,scenario_name="default") {
  # inputFile=scenarios_Melb[i,]$trips_location
  data <- read.csv(inputFile,as.is=T, fileEncoding="UTF-8-BOM") %>%
     dplyr::filter(trip_purpose=="Work") %>% ## Filter work trips only
    dplyr::select(participant_wt,age,sex,trip_mode_base,trip_mode_scen) %>%
    mutate(agegroup= case_when(
      age>=15 & age<=19 ~'15-19',
      age>=20 & age<=39 ~'20-39',
      age>=40 & age<=64 ~'40-64',
      age>=65           ~'65plus')) %>%
    dplyr::select(age=agegroup,sex,participant_wt,bl=trip_mode_base,sc=trip_mode_scen)
  
  
  dataAll <- bind_rows(
    data,
    data%>%mutate(age='all'),
    data%>%mutate(sex='all'),
    data%>%mutate(age='all',sex='all')
  ) %>% pivot_longer(cols = c("bl", "sc"),
                     names_to = "scenario",
                     values_to = "mode")%>%
    mutate(scen=scenario_name) %>%
    as.data.frame()
  
  return(dataAll)
}
### Save to melbourne-outputs
print(paste0("summarising transport modes ",nrow(scenarios_dot)," scenario outputs at ",Sys.time()))
scenarioTrips<-NULL
for (i in 1:nrow(scenarios_dot)){
  scenarioTripsCurrent<-summariseTransport(scenarios_dot[i,]$trips_location,
                                           scenarios_dot[i,]$scenario)
  scenarioTrips<-bind_rows(scenarioTrips,scenarioTripsCurrent) %>%
    dplyr::filter(participant_wt!=0) ## Some weights had 0 value
  cat(paste0("\n combined transport scenario ",i,"/",nrow(scenarios_dot)," complete at ",Sys.time(),"\n"))
}



### Two options for transport trips, weighted and unweigthed, trying to find out issue with walking changing when scenarios does not change
#### Filter work trips only


scenario_trips_weighted <- list()
iage <- c(unique(scenarioTrips$age))
isex <- c(unique(scenarioTrips$sex))
iscenario <- c(unique(scenarioTrips$scenario))
iscen <- c(unique(scenarioTrips$scen))
index <- 1
for (a in iage){
  for (s in isex){
    for (sc in iscenario){
      for (scena in iscen) {
        data <- scenarioTrips %>%
        dplyr::filter(age==a, sex==s, scenario==sc, scen==scena)
        mode_share <- data %>%
          srvyr::as_survey_design(weights = participant_wt) %>%
                    group_by(mode) %>%
                    dplyr::summarize(prop= srvyr::survey_mean(na.rm = T),
                                     trips = srvyr::survey_total(na.rm = T),
                                     surveyed = srvyr::unweighted(dplyr::n())) %>%
          ungroup() %>%
          dplyr::mutate(age=a,
                        sex=s,
                        scenario=sc,
                        scen=scena) %>%
          dplyr::select(age, sex, scenario, scen, prop, mode)
        scenario_trips_weighted[[index]] <- mode_share
        index <- index +1}}}}

scenarioTrips<- bind_rows(scenario_trips_weighted)
dir.create(finalLocation, recursive=TRUE, showWarnings=FALSE)
saveRDS(scenarioTrips,paste0(finalLocation,"/output_transport_modes.rds"))

# 
#### Minutes per week walking and cycling
#### Create combined file (and add all age groups and sex)
PA_files<-list.files(scenarioLocation,pattern="*.csv",full.names=T)
data<-lapply(PA_files,read.csv,header=T) %>%
  bind_rows() %>%
  dplyr::select(participant_wt,age,sex,ses,mod_total_hr:scen) %>%
  mutate(agegroup= case_when(
    age>=15 & age<=19 ~'15-19',
    age>=20 & age<=39 ~'20-39',
    age>=40 & age<=64 ~'40-64',
    age>=65           ~'65plus')) %>%
  dplyr::select(participant_wt,age=agegroup,sex,ses,mod_total_hr:scen)
dataAll <- bind_rows(
  data,
  data%>%mutate(age='all'),
  data%>%mutate(sex='all'),
  data%>%mutate(age='all',sex='all'))

### Create weighted stats
PA_weighted  <- dataAll %>%
  srvyr::as_survey_design(weights = participant_wt)%>%
  group_by(age, sex, scen) %>%
  dplyr::summarize(walk_base= srvyr::survey_mean(time_base_walking*60),
                   walk_scen = srvyr::survey_mean(time_scen_walking*60),
                   cycle_base= srvyr::survey_mean(time_base_bicycle*60),
                   cycle_scen = srvyr::survey_mean(time_scen_bicycle*60))

saveRDS(PA_weighted, file=paste0(finalLocation, "/PAall.rds"))

### Meets guidelines
PA_weighted  <- dataAll %>% 
  dplyr::mutate(meets_pa_base=ifelse((time_base_walking + walk_rc + time_base_bicycle*2 + mod_leis_hr + vig_leis_hr*2)*60 >= 150, 1, 0), 
                meets_pa_scen= ifelse((time_scen_walking + walk_rc + time_scen_bicycle*2 + mod_leis_hr + vig_leis_hr*2)*60>= 150, 1, 0))

PA_guide_weighted  <- PA_weighted %>% 
  srvyr::as_survey_design(weights = participant_wt)%>%
  group_by(age, sex, scen) %>%
  dplyr::summarize(meets_base= srvyr::survey_mean(meets_pa_base, na.rm = T), 
                   meets_scen = srvyr::survey_mean(meets_pa_scen, na.rm = T))
saveRDS(PA_guide_weighted, file=paste0(finalLocation, "/PAallGuide.rds"))

# combine scenarios into single files 

combineScenarios <- function(summarisedLocation,name) {
  scenario_names<-data.frame(scen=list.files(summarisedLocation)) %>%
    mutate(id=row_number())
  file_locations<-list.files(summarisedLocation,
                             pattern=name,
                             full.names=T,recursive=T)
  output_df<-lapply(file_locations,read.csv,header=T) %>%
    bind_rows(.id="id") %>%
    mutate(id=as.integer(id))
  output_df<-inner_join(scenario_names,output_df,by='id')%>%
    dplyr::select(-id)
  return(output_df)
}

output_df_agg_all <- combineScenarios(summarisedLocation,name="output_df_agg.csv") %>%
  rename(age=age_group_final,sex=Gender)
output_diseases_change <- combineScenarios(summarisedLocation,name="output_diseases_change.csv") %>%
  rename(age=age_group_final,sex=Gender)
output_life_expectancy_change <- combineScenarios(summarisedLocation,name="output_life_expectancy_change.csv") %>%
  rename(age=age_group_final,sex=Gender)
output_life_years_change <- combineScenarios(summarisedLocation,name="output_life_years_change.csv") %>%
  rename(age=age_group_final,sex=Gender)

# in case the directory hasn't been made yet (save here ".melbourne_outputs")
dir.create(finalLocation, recursive=TRUE, showWarnings=FALSE)
saveRDS(output_df_agg_all,paste0(finalLocation,"/output_df_agg.rds"))
saveRDS(output_diseases_change,paste0(finalLocation,"/output_diseases_change.rds"))
saveRDS(output_life_expectancy_change,paste0(finalLocation,"/output_life_expectancy_change.rds"))
saveRDS(output_life_years_change,paste0(finalLocation,"/output_life_years_change.rds"))




