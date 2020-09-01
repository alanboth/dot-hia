
suppressPackageStartupMessages(library(dplyr)) # for manipulating data
suppressPackageStartupMessages(library(tidyr)) # for pivoting data
rm(list = ls())

### Genate fixed inputs for Melbourne for mslt_code and ithim-r


### Trips data used in mslt_code and ithim-r
source("Scripts/data_prep/trips_prep.R")
trips_melbourne <- calculateVistaTrips(
  hh_VISTA_location="Data/Travelsurvey/VISTA12-18/H_VISTA_1218_V1.csv",
  person_VISTA_location="Data/Travelsurvey/VISTA12-18/P_VISTA1218_V1.csv",
  trip_VISTA_location="Data/Travelsurvey/VISTA12-18/T_VISTA1218_V1.csv"
)
write.csv(trips_melbourne, "Data/processed/trips_melbourne.csv", row.names=F, quote=F)

### Travel data people used in mslt_code to generate matched population
source("Scripts/data_prep/synthetic_pop.R")
travel_data <- calculateTravelData(
  hh_VISTA_location="Data/Travelsurvey/VISTA12-18/H_VISTA_1218_V1.csv",
  person_VISTA_location="Data/Travelsurvey/VISTA12-18/P_VISTA1218_V1.csv",
  ses_index_location="Data/Travelsurvey/ABS SEIFA/ses.csv"
)

write.csv(travel_data, "Data/processed/travel_data.csv", row.names=F, quote=T)

### PA data people used in mslt_code to generate matched population
persons_pa <- calculatePersonsPA(
  pa_location="Data/Physical activity/NHS2017-18_CSV/NHS17SPB.csv",
  hh_location="Data/Physical activity/NHS2017-18_CSV/NHS17HHB.csv"
)
write.csv(persons_pa, "Data/Processed/persons_pa.csv", row.names=F, quote=F)



#### SOME ISSUE WITH IT, BUT WE DO NOT NEED IT FOR NOW
### Injuries for ithim-r only
source("Scripts/data_prep/injuries_prep.R")
injuries_melbourne <- calculateInjuries(
  accident_location="Data/VicRoads Road Injuries/Original_ACCIDENT.csv",
  accident_event_location="Data/VicRoads Road Injuries/Original_ACCIDENT.csv",
  person_location="Data/VicRoads Road Injuries/Original_PERSON.csv",
  vehicle_location="Data/VicRoads Road Injuries/Original_VEHICLE.csv"
)
write.csv(injuries_melbourne,"Data/processed/injuries_melbourne.csv", row.names=F, quote=F)
