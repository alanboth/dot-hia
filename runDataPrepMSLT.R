#### Functions to generate inputs for Melbourne multi-state life table

suppressPackageStartupMessages(library(dplyr)) # for manipulating data
suppressPackageStartupMessages(library(tidyr)) # for pivoting data
rm(list = ls())
library(devtools)

# ltpa= leisure time physical activity


# Generate death rates for the population
source("Scripts/data_prep/death_rates_prep.R")
death_rates <- calculateDeathRates(
  population_deaths="Data/Population and deaths/population_deaths.csv"
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
  population_melbourne="Data/Processed/population_melbourne.csv",
  deaths_melbourne="Data/Processed/deaths_melbourne.csv",
  gbd_wider="Data/Processed/gbd_wider.csv",
  dismod_output_cancers="Data/Processed/dismod_output_cancers.csv",
  dismod_output_non_cancers="Data/Processed/dismod_output_non_cancers.csv"
)
write.csv(mslt, "Data/Processed/mslt/mslt_df.csv", row.names=F, quote=T)

source("Scripts/data_prep/trends_prep.R")
trends_diseases <- calculateDiseaseTrends(
  incidence_trends_cancers="Data/aihw/cancer_incidence_AIHW_with_projections.xlsx",
  mortality_trends_cancers="Data/aihw/trends/cancers_trends_mortality_aihw.xls",
  trends_cvd="Data/aihw/trends/cardiovascular_disease_trends_aihw.xlsx",
  grim_books="Data/aihw/trends/grim_books.csv",
  trends_diabetes="Data/aihw/trends/diabetes_trends_aihw.xls"
)

write.csv(incidence_trends_f, "Data/Processed/mslt/incidence_trends_f.csv", row.names=F, quote=T)
write.csv(incidence_trends_m, "Data/Processed/mslt/incidence_trends_m.csv", row.names=F, quote=T)
write.csv(mortality_trends_f, "Data/Processed/mslt/mortality_trends_f.csv", row.names=F, quote=T)
write.csv(mortality_trends_m, "Data/Processed/mslt/mortality_trends_m.csv", row.names=F, quote=T)
