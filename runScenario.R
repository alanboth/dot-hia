# usedPackages<-c('devtools','ithimr','readr','data.table','dplyr','tidyverse',
#                 'knitr','kableExtra','citr','ggpubr','ggplot2','arsenal',
#                 'janitor','conflicted','rlist','reshape','reshape2','zoo',
#                 'stringi','srvyr')
# suppressPackageStartupMessages(library(readr)) # for reading/writing data
suppressPackageStartupMessages(library(dplyr)) # for manipulating data
suppressPackageStartupMessages(library(tidyr)) # for pivoting data
rm(list = ls())
library(devtools)

# ltpa= leisure time physical activity


# Generate death rates for the population
source("Scripts/data_prep/death_rates_prep.R")
death_rates <- calculateDeathRates(
  population_deaths_location="Data/Population and deaths/population_deaths.csv"
)
write.csv(death_rates,"Data/Processed/deaths_melbourne.csv", row.names=F, quote=F)


source("Scripts/data_prep/ithim_gbd_prep.R")
# this outputs a list containing gbd_melbourne and population_melbourne
GBDandPopulation <- calculateGBDandPopulation(
  gbd_melbourne_ithimr_location="Data/gbd/gbd_melbourne_ithimr.csv",
  population_melbourne_abs_location="Data/Processed/population_melbourne_abs.csv"
)
gbd_melbourne <- GBDandPopulation[[1]]
population_melbourne <- GBDandPopulation[[2]]
write.csv(gbd_melbourne,"Data/Processed/gbd_melbourne.csv", row.names=F, quote=T)
write.csv(population_melbourne,"Data/Processed/population_melbourne.csv", row.names=F, quote=T)

source("Scripts/data_prep/mslt_gbd_prep.R")
# I don't think this is actually needed anymore
disease_names <- calculateDiseaseNames(
  gbd_location="Data/gbd/gbd_melbourne_mslt.csv",
  disease_outcomes_location="Data/Processed/disease_outcomes_lookup.csv"
)
write.csv(disease_names, "Data/Processed/disease_names.csv", row.names=F, quote=T)

gbd_wider <- calculateGBDwider(
  gbd_location="Data/gbd/gbd_melbourne_mslt.csv"
)
write.csv(gbd_wider, "Data/Processed/gbd_wider.csv", row.names=F, quote=T)

# This function needs a good look, I don't understand enough of about health calculations
mslt <- calculateMSLT(
  population_melbourne_location="Data/Processed/population_melbourne.csv",
  deaths_melbourne_location="Data/Processed/deaths_melbourne.csv",
  gbd_wider_location="Data/Processed/gbd_wider.csv",
  dismod_output_cancers="Data/Processed/dismod_output_cancers.csv",
  dismod_output_non_cancers="Data/Processed/dismod_output_non_cancers.csv"
)
write.csv(mslt, "Data/Processed/mslt.csv", row.names=F, quote=T)






# MAIN FUNCTIONS TO RUN A SCENARIO

# Generate trips_melbourne.csv
source("Scripts/data_prep/trips_prep.R")
trips_melbourne <- calculateVistaTrips(
  hh_VISTA_location="Data/Travelsurvey/VISTA12-18/H_VISTA_1218_V1.csv",
  person_VISTA_location="Data/Travelsurvey/VISTA12-18/P_VISTA1218_V1.csv",
  trip_VISTA_location="Data/Travelsurvey/VISTA12-18/T_VISTA1218_V1.csv"
)
write.csv(trips_melbourne, "Data/Processed/trips_melbourne.csv", row.names=F, quote=F)



# Generate trips_melbourne_scenarios.csv
source("Scripts/scenarios.R")
scenario <- calculateScenario(
  trips_melbourne_location="Data/Processed/trips_melbourne.csv"
)
write.csv(scenario, "Data/Processed/trips_melbourne_scenarios.csv", row.names=F, quote=F)



source("Scripts/data_prep/synthetic_pop.R")
travel_data <- calculateTravelData(
  hh_VISTA_location="Data/Travelsurvey/VISTA12-18/H_VISTA_1218_V1.csv",
  person_VISTA_location="Data/Travelsurvey/VISTA12-18/P_VISTA1218_V1.csv",
  ses_index_location="Data/Travelsurvey/ABS SEIFA/ses.csv"
)
# need quotes on since some industry_cat entries have commas 
# e.g.: 'Professional, Scientific and Technical Services'
write.csv(travel_data, "Data/Processed/travel_data.csv", row.names=F, quote=T)

persons_travel <- calculatePersonsTravelScenario(
  travel_data_location="Data/Processed/travel_data.csv",
  scenario_location="Data/Processed/trips_melbourne_scenarios.csv"
)
write.csv(persons_travel, "Data/Processed/persons_travel.csv", row.names=F, quote=T)

persons_pa <- calculatePersonsPA(
  pa_location="Data/Physical activity/NHS2017-18_CSV/NHS17SPB.csv",
  hh_location="Data/Physical activity/NHS2017-18_CSV/NHS17HHB.csv"
)
write.csv(persons_pa, "Data/Processed/persons_pa.csv", row.names=F, quote=F)

persons_matched <- calculatePersonsMatch(
  pa_location="Data/Processed/persons_pa.csv",
  persons_travel_location="Data/Processed/persons_travel.csv"
)
write.csv(persons_matched, "Data/Processed/matched_pop.csv", row.names=F, quote=T)






# OTHER FUNCTIONS, NOT YET REFERENCED IN MAIN CODE
source("Scripts/data_prep/injuries_prep.R")
injuries_melbourne <- calculateInjuries(
  accident_location="Data/VicRoads Road Injuries/Original_ACCIDENT.csv",
  accident_event_location="Data/VicRoads Road Injuries/Original_ACCIDENT.csv",
  person_location="Data/VicRoads Road Injuries/Original_PERSON.csv",
  vehicle_location="Data/VicRoads Road Injuries/Original_VEHICLE.csv"
)
write.csv(injuries_melbourne,"Data/Processed/injuries_melbourne.csv", row.names=F, quote=F)
saveRDS(injuries_melbourne, "Data/Processed/injuries_melbourne.Rds")


# other scripts to work on
"Scripts/data_prep/pa_prep.R"
"Scripts/data_prep/gbd_prep.R"
"Scripts/data_prep/disbayes_process.R"
"Scripts/ithim-r.R"
"Scripts/mslt_code.R"


