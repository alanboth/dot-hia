



suppressPackageStartupMessages(library(dplyr)) # for manipulating data
suppressPackageStartupMessages(library(tidyr)) # for pivoting data
rm(list = ls())
library(devtools)

# MAIN FUNCTIONS TO RUN A SCENARIO

# Generate trips_melbourne.csv
### FIXED, SERVES TO ITHIM R AND METAHIT, MOVE TO DATA PREP
source("Scripts/data_prep/trips_prep.R")
trips_melbourne <- calculateVistaTrips(
  hh_VISTA_location="Data/Travelsurvey/VISTA12-18/H_VISTA_1218_V1.csv",
  person_VISTA_location="Data/Travelsurvey/VISTA12-18/P_VISTA1218_V1.csv",
  trip_VISTA_location="Data/Travelsurvey/VISTA12-18/T_VISTA1218_V1.csv"
)
write.csv(trips_melbourne, "Data/Processed/trips_melbourne.csv", row.names=F, quote=F)


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


