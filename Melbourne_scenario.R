
suppressPackageStartupMessages(library(dplyr))
# suppressPackageStartupMessages(library(readr))
# suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(srvyr)) 
suppressPackageStartupMessages(library(forcats))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(scales))
suppressPackageStartupMessages(library(ggeasy))
suppressPackageStartupMessages(library(ggridges))
suppressPackageStartupMessages(library(stringr))




maxDistanceWalk <- c(0,1,2)
maxDistanceCycle <- c(2,5,10)
# recreational, essential, all trips
tripPurpose <- c("commuting", "all")
"Leisure,Shopping,Work,Education,Other"
# Belen, we need to work out what should go in each category here
tripPurposeFull <- c("Work,Education",
                     "Leisure,Shopping,Work,Education,Other")

tripPurposeDF <- data.frame(purpose=tripPurpose,
                            purpose_full=tripPurposeFull)

scenarioPurposes <- c("Leisure","Shopping","Work","Education","Other")


scenarios_Melb <- crossing(data.frame(max_walk=maxDistanceWalk),
                           data.frame(max_cycle=maxDistanceCycle),
                           data.frame(purpose=tripPurpose)) %>%
  filter(max_walk<max_cycle) %>%
  inner_join(tripPurposeDF) %>%
  mutate(scenario=paste0(purpose,"_",max_walk,"_",max_cycle))

write.csv(scenarios_Melb, "scenarios_for_melbourne.csv", row.names=F, quote=T)


### Steps: 1) Generate trip set with baseline and scenario trips, 2) Generate persons_matched 

source("Scripts/scenarios_MEL.R")
source("Scripts/data_prep/synthetic_pop.R")

# iterate through each entry in scenarios_Melb, creating the matched population
print(paste0("iterating through ",nrow(scenarios_Melb)," scenarios at ",Sys.time()))
for (i in 1:nrow(scenarios_Melb)){
  generateMatchedPopulationScenario(
    output_location="./scenarios",
    scenario_name=scenarios_Melb[i,]$scenario,
    in_data="Data/processed/trips_melbourne.csv",
    in_speed="Data/processed/speed_trips_melbourne.csv",
    max_walk=scenarios_Melb[i,]$max_walk,
    max_cycle=scenarios_Melb[i,]$max_cycle,
    purpose=scenarios_Melb[i,]$purpose_full
  )
  cat(paste0("\n scenario ",i,"/",nrow(scenarios_Melb)," complete at ",Sys.time(),"\n"))
}

