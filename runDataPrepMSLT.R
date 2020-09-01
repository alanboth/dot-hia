### Script to generate inputs for mslt_code
#### Generates inputs MSLT_DF for all of Australia for: gbd data (and dismod outputs, externally generated) and deaths

suppressPackageStartupMessages(library(dplyr)) # for manipulating data
suppressPackageStartupMessages(library(tidyr)) # for pivoting data
rm(list = ls())



# Generate death rates with projections for Australia and all states

source("Scripts/data_prep/death_rates_prep.R")

location_assumption <- crossing(data.frame(location=c("New South Wales",	"Victoria",	"Queensland",	"South Australia",	
                                                      "Western Australia",	"Tasmania",	"Northern Territory",	"Australian Capital Territory",	"Australia")),
                                data.frame(assumption=c("medium", "high")))

### Periodic death rates
deaths_periodic <- list()
for (l in c(location_assumption$location)) {
deaths_periodic[[l]] <- GetDeathRatesPeriodic(
  population_deaths_location="Data/original/abs/population_deaths.csv", 
  location = l
)

}

deaths_periodic <- do.call(rbind, deaths_periodic)

write.csv(deaths_periodic, "Data/processed/mslt/deaths_periodic.csv")


### Death rates with projections
deaths_projections <- list()
for (i in 1:nrow(location_assumption)) {
  deaths_projections[[i]] <- GetDeathsRatesProjections( 
    deaths="Data/original/abs/projections.xls", 
    location= location_assumption$location[[i]], 
    assumption= location_assumption$assumption[[i]]
  )
  names(deaths_projections)[i] <- paste(location_assumption$location[i], location_assumption$assumption[i], sep = "_")}

### Create large data set with all data

deaths_mslt <- do.call(rbind, deaths_projections)
deaths_mslt <- do.call(rbind, deaths_mslt)

write.csv(deaths_mslt, "Data/processed/mslt/deaths_projections.csv", row.names=F, quote=T)

source("Scripts/data_prep/mslt_gbd_prep.R")

### Create disease names and save to use in mslt_code
disease_names <- calculateDiseaseNames(
  gbd_location="Data/original/gbd/gbd_mslt.csv",
  disease_outcomes_location="Data/original/ithimr/disease_outcomes_lookup.csv"
)
write.csv(disease_names, "Data/processed/mslt/disease_names.csv", row.names=F, quote=T)



### Generate mslt data frame and save to use in mslt_code
gbd_wider <- calculateGBDwider(
  gbd_location="Data/original/gbd/gbd_mslt.csv"
)
write.csv(gbd_wider, "Data/processed/mslt/gbd_wider.csv", row.names=F, quote=T)


# This function needs a good look, I don't understand enough of about health calculations
source("Scripts/data_prep/mslt_gbd_prep.R")
mslt <- calculateMSLT(
  gbd_wider="Data/processed/mslt/gbd_wider.csv",
  dismod_output_cancers="Data/processed/mslt/dismod_output_cancers.csv",
  dismod_output_non_cancers="Data/processed/mslt/dismod_output_non_cancers.csv"
)
write.csv(mslt, "Data/processed/mslt/mslt_df.csv", row.names=F, quote=T)

source("Scripts/data_prep/trends_prep.R")
trends_diseases <- calculateDiseaseTrends(
  incidence_trends_cancers="Data/original/aihw/cancer_incidence_AIHW_with_projections.xlsx",
  mortality_trends_cancers="Data/original/aihw/cancers_trends_mortality_aihw.xls",
  trends_cvd="Data/original/aihw/cardiovascular_disease_trends_aihw.xlsx",
  grim_books="Data/original/aihw/grim_books_utf8.csv",
  trends_diabetes="Data/original/aihw/diabetes_trends_aihw.xls"
)

mortality_trends_f <- trends_diseases[[1]]
mortality_trends_m <- trends_diseases[[2]]
incidence_trends_f <- trends_diseases[[3]]
incidence_trends_m <- trends_diseases[[4]]
write.csv(incidence_trends_f, "Data/processed/mslt/incidence_trends_f.csv", row.names=F, quote=T)
write.csv(incidence_trends_m, "Data/processed/mslt/incidence_trends_m.csv", row.names=F, quote=T)
write.csv(mortality_trends_f, "Data/processed/mslt/mortality_trends_f.csv", row.names=F, quote=T)
write.csv(mortality_trends_m, "Data/processed/mslt/mortality_trends_m.csv", row.names=F, quote=T)



