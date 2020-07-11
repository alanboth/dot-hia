usedPackages<-c('devtools','ithimr','readr','data.table','dplyr','tidyverse',
                'knitr','kableExtra','citr','ggpubr','ggplot2','arsenal',
                'janitor','conflicted','rlist','reshape','reshape2','zoo',
                'stringi','srvyr')


library(dplyr)
library(readr)
library(tidyr)

# ltpa= leisure time physical activity



# Generate death rates for the population
source("Scripts/data_prep/death_rates_prep.R")
read.csv("Data/Population and deaths/population_deaths.csv") %>%
  calculateDeathRates() %>%
  write.csv("Data/Processed/deaths_melbourne.csv", row.names=F, quote=F)



# Generate trips_melbourne.csv
source("Scripts/data_prep/trips_prep.R")
trips_melbourne <- calculateVistaTrips(
  "Data/Travel survey/VISTA 12-18/H_VISTA_1218_V1.csv",
  "Data/Travel survey/VISTA 12-18/P_VISTA1218_V1.csv",
  "Data/Travel survey/VISTA 12-18/T_VISTA1218_V1.csv"
)
write.csv(trips_melbourne, "Data/Processed/trips_melbourne.csv", row.names=F, quote=F)
write_rds(trips_melbourne, "Data/Processed/trips_melbourne.Rds")



"Scripts/scenarios.R"

"Scripts/data_prep/synthetic_pop.R"








"Scripts/data_prep/injuries_prep.R"
"Scripts/data_prep/pa_prep.R"

"Scripts/data_prep/gbd_prep.R"

"Scripts/data_prep/disbayes_process.R"


