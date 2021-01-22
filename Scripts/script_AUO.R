##### Script with graphs, tables and text for presentation in the urban observatory


suppressPackageStartupMessages(library(dplyr)) # for manipulating data
suppressPackageStartupMessages(library(tidyr))

source("./Scripts/scenarios_MEL.R")
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



# 1) Transport graph-example age group 20 to 39 and males -----------------
# Couldn't get this to work
example_1_trips <- read.csv("./scenarios/trips/trips_all_0_2.csv")
example_1__trips_data <- example_1_trips %>% dplyr::filter(age_group=="20 to 39", sex == "female")

mode_example <- GraphsMode(example_1_trips_data)

mode_example 



# 2) mmets graph-example --------------------------------------------------
# Note: black lines are error bars
mmetsGraph(
  age_val='all',
  sex_val='all',
  scen_val='all_2_10'
)



# 3) diseases -------------------------------------------------------------

### Table.
## GUS: from here we want to present a table with age groups, sex, disease name (disease_names variable disease), median, 
## 2.5% and 97.5 % percentiles for diseases incidence (incident cases) and mx (deaths)

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
