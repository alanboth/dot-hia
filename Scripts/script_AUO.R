##### Script with graphs, tables and text for presentation in the urban observatory


suppressPackageStartupMessages(library(dplyr)) # for manipulating data
suppressPackageStartupMessages(library(tidyr))


source("./Scripts/graphs_AUO.R")


#### Results to be presented for: 
#1) Change mode share: see./scenarios/trips for all ages and sex. Code example below to generate for age and sex group
#2) Change mmets: see./scenarios/trips for all ages and sex. Code example below to generate for age and sex group
#3) diseases
#4) life expectancy
#5) life years

### Age groups: "15 to 19", "20 to 39", "40 to 64", "65 plus"
### Sex: "male", "female"

### Combinations scenarios for graphs and results
scenariosDF <- crossing(data.frame(max_walk=c(0,1,2)),
                           data.frame(max_cycle=c(2,5,10)),
                           data.frame(purpose=c("commuting", "all"))) %>%
  filter(max_walk<max_cycle) %>%
  mutate(scen=paste0(purpose,"_",max_walk,"_",max_cycle))


### Combinations age and sex for graphs and results
age_sex_cohorts <- crossing(data.frame(age=c("15-19", "20-39", "40-64", "65plus", "all")),
                            data.frame(sex=c('male', 'female', 'all'))) %>%
  dplyr::mutate(cohort=paste0(age,"_",sex))


# Load data ---------------------------------------------------------------
finalLocation <- "./melbourne-outputs"
output_df_agg_all<-readRDS(paste0(finalLocation,"/output_df_agg.rds"))
output_diseases_change<-readRDS(paste0(finalLocation,"/output_diseases_change.rds"))
output_life_expectancy_change<-readRDS(paste0(finalLocation,"/output_life_expectancy_change.rds"))
output_life_years_change<-readRDS(paste0(finalLocation,"/output_life_years_change.rds"))
output_mmets<-readRDS(paste0(finalLocation,"/output_mmets.rds"))
output_mmets_graph<-readRDS(paste0(finalLocation,"/output_mmets_graph.rds"))
output_transport_modes<-readRDS(paste0(finalLocation,"/output_transport_modes.rds"))

# Inputs options ----------------------------------------------------------
# age_val: "15-19"  "20-39"  "40-64"  "65plus" "all" 
# sex_val: "all"    "female" "male" 
# scen_cal: "all_0_2", "commuting_0_2", all_0_5", "commuting_0_5", "all_0_10", "commuting_0_10", "all_1_2", 
# "commuting_1_2", "all_1_5", "commuting_1_5", "all_1_10", "commuting_1_10", "all_2_5", "commuting_2_5", "all_2_10"       "commuting_2_10"

# 1) Transport graph-example ----------------------------------------------
transportModeGraph(
  age_val= "15-19",
  sex_val='female',
  scen_val='all_2_10'
)


# 2) mmets graph-example --------------------------------------------------
# Note: black lines are error bars
mmetsGraph(
  age_val= "all",
  sex_val='all',
  scen_val='all_2_10'
)

# 3) diseases -------------------------------------------------------------

### Table.
## GUS: from here we want to present a table with age groups, sex, disease name (disease_names variable disease), median, 
## 2.5% and 97.5 % percentiles for diseases incidence (incident cases) and mx (deaths)

## Disease names: brsc: breast cancer, carc: colon and rectum cancers, dmt2: Type 2 diabetes, ishd: ischemic heart disease, strk: ischemic stroke, tbalc:Tracheal, bronchus, and lung cancer, utrc: Uterine cancer
## note males to not have utrc and brsc
diseasesExample <- diseasesTable(
  age_val='all',
  sex_val='all',
  scen_val='all_2_10'
)

#### Graphs for incidence, deaths and life years

### Incidence diseases (WE MAY WANT TO SMOOTH THE GRAPHS A BIT)
incidenceDiseasesGraph(
  age_val='all',
  sex_val='all',
  scen_val='all_2_10'
)

### Mortality diseases
mortalityDiseasesGraph(
  age_val='all',
  sex_val='all',
  scen_val='all_2_10'
)

### Health adjusted life years
halyGraph(
  age_val='all',
  sex_val='all',
  scen_val='all_2_10'
)
