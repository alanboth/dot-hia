##### Script with graphs, tables and text for presentation in the urban observatory


suppressPackageStartupMessages(library(dplyr)) 
suppressPackageStartupMessages(library(tidyr))


source("./Scripts/graphs_DOT.R")


#### Results to be presented for: 
#1) Change mode share: see./scenarios/trips for all ages and sex. Code example below to generate for age and sex group
#2) Change mmets: see./scenarios/trips for all ages and sex. Code example below to generate for age and sex group
#3) diseases
#4) life expectancy
#5) life years

### Age groups: "15 to 19", "20 to 39", "40 to 64", "65 plus"
### Sex: "male", "female"

### Combinations scenarios for graphs and results
### Scenarios 
scenariosDF <- data.frame(scen=c("dotFull","dotTrain"))


### Combinations age and sex for graphs and results
age_sex_cohorts <- crossing(data.frame(age=c("15-19", "20-39", "40-64", "65plus", "all")),
                            data.frame(sex=c('male', 'female', 'all'))) %>%
  dplyr::mutate(cohort=paste0(age,"_",sex))

dir.create("output/dot-outputs/graphs", recursive=TRUE, showWarnings=FALSE)

# Load data ---------------------------------------------------------------
finalLocation                 <-  "output/dot-outputs"
graphsLocation                <-  "output/dot-outputs/graphs/"
output_df_agg_all             <- readRDS(paste0(finalLocation,"/output_df_agg.rds"))
output_diseases_change        <- readRDS(paste0(finalLocation,"/output_diseases_change.rds"))
output_life_expectancy_change <- readRDS(paste0(finalLocation,"/output_life_expectancy_change.rds"))
output_life_years_change      <- readRDS(paste0(finalLocation,"/output_life_years_change.rds"))
PAall                         <- readRDS(paste0(finalLocation,"/PAall.rds"))
PAallGuide                    <- readRDS(paste0(finalLocation,"/PAallGuide.rds"))
output_transport_modes        <- readRDS(paste0(finalLocation,"/output_transport_modes.rds"))
# Inputs options ----------------------------------------------------------
# age_val: "15-19"  "20-39"  "40-64"  "65plus" "all" 
# sex_val: "all"    "female" "male" 
# scen_val: "dotFull", "dotTrain",

# 1) Transport graph-example ----------------------------------------------
GraphsMode(
  age_val= "all",
  sex_val='all',
  scen_val='dotFull'
)
ggsave(paste0(graphsLocation,"mode_all_all_full.pdf"),width=10,height=6)

GraphsMode(
  age_val= "all",
  sex_val='all',
  scen_val='dotTrain'
)
ggsave(paste0(graphsLocation,"mode_all_all_train.pdf"),width=10,height=6)


GraphsMode(
  age_val= "20-39",
  sex_val='all',
  scen_val='dotFull'
)
ggsave(paste0(graphsLocation,"mode_20-39_all_full.pdf"),width=10,height=6)

# 2) Minutes-text-example --------------------------------------------------(Gus, this should have the text that Lucy is sharing with use. Transport data from above graph and physical activity data for mean walking and cycling from data called in this function)

GetMinutesText(
  age_val= "female",
  sex_val='all',
  scen_val='dotFull'
)

# 3) diseases -------------------------------------------------------------

### Table.
## GUS: from here we want to present a table with age groups, sex, disease name (disease_names variable disease), median, 
## 2.5% and 97.5 % percentiles for diseases incidence (incident cases) and mx (deaths)

## Disease names: brsc: breast cancer, carc: colon and rectum cancers, dmt2: Type 2 diabetes, ishd: ischemic heart disease, strk: ischemic stroke, tbalc:Tracheal, bronchus, and lung cancer, utrc: Uterine cancer
## note males to not have utrc and brsc 
#### GUS: text to go along to explain: Over the life course of the modeled population of adults aged (age_val)
# and sex (sex_val) of xx people (diseaseExmple$population), the model predicts diseaseExample$median (diseaseExample$percentile025, diseaseExample$percentile975)
# cases prevented.
diseasesExample <- diseasesTable(
  age_val='all',
  sex_val='all',
  scen_val='dotFull'
)

#### Graphs for incidence, deaths and life years
###### Percentage change disease incidence over the life course of cohort
diseasesChangeIncidence(
  age_val='all',
  sex_val='all',
  scen_val='dotFull'
)
ggsave(paste0(graphsLocation,"incidenceChange_all_all_full.pdf"),width=10,height=6)

###### Percentage change disease mortality over the life course of cohort
diseasesChangeDeaths(
  age_val='all',
  sex_val='all',
  scen_val='dotFull'
)
ggsave(paste0(graphsLocation,"deathChange_all_all_full.pdf"),width=10,height=6)

### Incidence diseases 
incidenceDiseasesGraph(
  age_val='all',
  sex_val='all',
  scen_val='dotFull'
)
ggsave(paste0(graphsLocation,"incidence_all_all_full.pdf"),width=10,height=6)


incidenceDiseasesGraph(
  age_val='20-39',
  sex_val='all',
  scen_val='dotFull'
)
ggsave(paste0(graphsLocation,"incidence_all_all_full.pdf"),width=10,height=6)

### Mortality diseases
mortalityDiseasesGraph(
  age_val='all',
  sex_val='all',
  scen_val='dotFull'
)
ggsave(paste0(graphsLocation,"mortality_all_all_full.pdf"),width=10,height=6)

### HALYs table

HALYsexample <- HALYsTable(
  age_val='all',
  sex_val='all',
  scen_val='dotFull'
)


### Health adjusted life years (BELEN do table for )
# make like incidence graph
halyGraph(
  age_val='all',
  sex_val='all',
  scen_val='dotFull'
)
ggsave(paste0(graphsLocation,"HALY_all_all_full.pdf"),width=10,height=6)

lyGraph(
  age_val='all',
  sex_val='all',
  scen_val='dotFull'
)
ggsave(paste0(graphsLocation,"LY_all_all_full.pdf"),width=10,height=6)
