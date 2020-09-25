# ---- chunk-intro ----

### Proportional multi-state life table model to calculate changes in: life years, health-adjusted life years, life expectancy, 
### health-adjusted life expectancy and changes in diseases' incidence and mortality numbers for a changes in transport modes.
### The model is parametrised for the Melbourne Greater Area Population. 
### Detailed explanation can be found in TechnicalDoc.


### Packages to run code
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(readr))
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(srvyr)) 
suppressPackageStartupMessages(library(forcats))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(scales))
suppressPackageStartupMessages(library(ggeasy))
suppressPackageStartupMessages(library(ggridges))

### Clean Global Environment
rm (list = ls())

### Avoid scientific notation
options(scipen=999)

### TO DO: 

## Add uncertainty inputs for marginal mets for NHS inputs
## Add duration intervention or dealyed in adoption

# ---- chunk-1 ----

## CALCULATION ORDER only including physical activity changes

# 1) Inputs MSLT (from runDataPrepMSLT, fixed)
# 2) Run scenarios (use calculateScenario, not fixed)
# 3) Matched population with mets baseline and scenario (use )
# 4) mmets per person (code below, has uncertainty inputs)
# 5) RRs per person (code below, has uncertainty inputs)
# 6) PIFS by age and sex (with function health_burden_2)
# 7) Parameters for Mslt code running
# 8) Run rest

###################################### Choose from #################################################################

### age and sex feed into: calculateScenarioMel, RunLifeTable, RunDisease
i_age_cohort <-  c(17, 22, 27, 32, 37, 42, 47, 52, 57, 62, 67, 72, 77, 82, 87, 92, 97) #c("15 to 19","20 tp 24", "25 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 59", "60 to 64", 
  # "65 to 69", "70 to 74", "75 to 79", "80 to 84", "85 to 89", "90 to 94", "95 to 100")
i_sex <- c('male', 'female')

# sc_duration <- replicate(4,1) %>% append(replicate(80, 0))


###################################### Probabilistic Sensitivity Scenario parameters ################################

## TO DO: see montecarlo file, we need to define distributions, RRs lognormal
NSAMPLES <- 2000 #activate for Monte Carlo simulation
MMET_CYCLING <- 4.63 #c(4.63, (1.2) #lognormal  
MMET_WALKING <- 2.53 #c(2.53, 1.1)  #lognormal 
MMET_MOD <- 3.5 ## TO DO: GET Uncertain parameters
MMET_VIG <- 7 ## TO DO: GET Uncertain parameters


PA_DOSE_RESPONSE_QUANTILE <- F # Generates random numbers for each of the Relative Risk functions
SPEED_WALK <- 5 # ADD AS UNCERTAIN PARAMETERS (GET DISTRIBUTION FROM VISTA)
SPEED_CYCLE <- 20 # ADD AS UNCEARTAIN PARAMETERS

DIABETES_IHD_RR_F <- 2.82 ## c(2.82, CI (2.35, 3.38) get SD from CI
DIABETES_STROKE_RR_F <- 2.28 ## c(2.28) CI (1.93, 2.69) get SD from CI
DIABETES_IHD_RR_M <- 2.16 ## c(2.16, CI (1.82, 2.56) get SD from CI
DIABETES_STROKE_RR_M <- 1.83 ## c(1.83) CI (1.60, 2.08) get SD from CI

############################## 0) Inputs MSLT (from runDataPrepMSLT) ###############################################

### MSLT has fixed inputs and inputs that change by location (death rates and population). Melbourne models uses population for Melbourne
### Greater Area and death rates for Victoria.

### General inputs for all models 
mslt_general="Data/processed/mslt/mslt_df.csv"
MSLT_DF <- read.csv(mslt_general,as.is=T,fileEncoding="UTF-8-BOM")

#### Get death rates: 1) periodic and 2) projections. RunLifeTable has option to choose which death rates to use. Use projections for baseline
#### and periodic for sensitivity analysis.

#### 1) Death rates periodic (no projections, assumes current death rates are observed in the future) are added to mslt for selected location
death_rate_periodic="Data/processed/mslt/deaths_periodic.csv"
death_rate_periodic <- read.csv(death_rate_periodic,as.is=T,fileEncoding="UTF-8-BOM") %>% dplyr::filter(location == "Victoria") %>%
  dplyr::select("sex_age_cat", "mx")
MSLT_DF <- left_join(MSLT_DF, death_rate_periodic)

#### 2) Deaths rates with projections can be used as an option in RunLifeTable function, two assumptions are made by ABS for improvements in life
#### expectancy: high and medium
death_rates="Data/processed/mslt/deaths_projections.csv"
death_rates <- read.csv(death_rates,as.is=T,fileEncoding="UTF-8-BOM") %>% dplyr::filter(location == "Victoria", assumption == "medium")

##### Population
source("Scripts/data_prep/population_prep.R")
population <- GetPopulation(
population_data="Data/original/abs/population_census.xlsx",
location= "Greater Melbourne")
MSLT_DF <- left_join(MSLT_DF, population)
MSLT_DF$age <- as.numeric(MSLT_DF$age)

############################## 1) Run scenarios ###################################################################
#### For Melbourne: trips change, more calculations requiered as below steps to get mmets for RRs calculation
#### The entire dataset is keept with all trips, not only those that change
# Generate trips_melbourne_scenarios.csv
source("Scripts/scenarios_MEL.R")
in_data="Data/processed/trips_melbourne.csv"
in_speed="Data/processed/speed_trips_melbourne.csv"
scenario_trips <- calculateScenarioMel(trips_melbourne = in_data, 
                                       speed = in_speed,
                                       age_input = c("0 to 17", "18 to 40", "41 to 65", "66 plus"),
                                       sex_input = c("male", "female"), 
                                       original_mode = "car" , # Just car trips can be replaced
                                       replace_mode_walk = T,
                                       replace_mode_cycle = T,
                                       distance_replace_walk = "< 2km",  #c(">10km",  "6-10km", "< 2km",  "2-5km"),
                                       distance_replace_cycle = "2-5km",  #c(">10km",  "6-10km", "< 2km",  "2-5km"),
                                       purpose_input = c("Leisure", "Shopping", "Work related", "Pick-up or drop-off someone/something", "personal business",
                                                         "Other", "accompany someone", "education","at or go home")) 


scenario_trips <- scenario_trips %>% mutate_if(sapply(scenario_trips, is.character), as.factor) ## all character to factors for group by analysis
write.csv(scenario_trips, "Data/processed/trips_melbourne_scenarios.csv")
#### Graphs
###### Get weighted data

scenario_trips_weighted <-  scenario_trips  %>%
  srvyr::as_survey_design(weights = trips_wt)

###### Table with baseline and scenario proportion by mode
scenario_trips_mode <- scenario_trips_weighted   %>% 
  group_by(trip_mode_scen,.drop = FALSE) %>%
  dplyr::summarize(prop= srvyr::survey_mean()) %>%
  rename(mode = trip_mode_scen) %>%
  mutate(scen="scenario")

baseline_trips_mode <- scenario_trips_weighted   %>% 
  group_by(trip_mode_base,.drop = FALSE) %>%
  dplyr::summarize(prop= srvyr::survey_mean()) %>%
  rename(mode = trip_mode_base) %>%
  mutate(scen="base") 

data_mode_combo <- rbind(scenario_trips_mode, baseline_trips_mode) %>% mutate(mode = fct_reorder(mode, desc(prop)))

#### Get bar chart modes distribution
bar_chart_combo_sc <- data_mode_combo %>%
  ggplot(aes(x = mode, y = prop)) +
  geom_bar(
    aes(color = scen, fill = scen),
    stat = "identity" , position = "dodge"
  ) + 
  labs(title="Distribution trips baseline and scenario", x="", y="Proportion of all trips") +
  theme_classic() +
  geom_text(aes(label=paste0(round(prop*100,1),"%"), y=prop), size=3)  + 
  theme(plot.title = element_text(hjust = 0.5, size = 12,face="bold"),
        axis.text=element_text(size=10),
        axis.title=element_text(size=10)) +
  theme(legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(colour = "black", size = 10),
        legend.key = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))  +
  scale_y_continuous(labels = percent)


bar_chart_combo_sc
ggsave("output/proportion_modes_sc.png")


############################## 2) Matched population with mets baseline and scenario (from run_Scenario) ##########

source("Scripts/data_prep/synthetic_pop.R")

#### 2.1) Create data set with VISTA people and allocate baseline and scenario trips to them
persons_travel <- calculatePersonsTravelScenario(
  travel_data_location="Data/processed/travel_data.csv", ## BZ: generated in script runInputsMelbourneExposure.R 
  scenario_location="Data/processed/trips_melbourne_scenarios.csv" ### BZ: Generated in step 1
)
write.csv(persons_travel, "Data/processed/persons_travel.csv", row.names=F, quote=T)


#### 2.2) Match NHS people to VISTA people based on age, sex, ses, work status and whether they walk for transport
persons_matched <- calculatePersonsMatch(
  pa_location="Data/processed/persons_pa.csv", ## BZ: generated in script runInputsMelbourneExposure.R 
  persons_travel_location="Data/processed/persons_travel.csv"
)
write.csv(persons_matched, "Data/processed/matched_pop.csv", row.names=F, quote=T)


############################# 3) mmets per person (code below, has uncertainty inputs) ############################

source("Scripts/data_prep/mmet_pp.R")

### change work and time marginal met to minutes and multiply by uncertain mets

mmets_pp_MEL <- calculateMMETSperPerson(
  matched_pop_location = "Data/processed/matched_pop.csv",
  mets = "Data/Physical Activity/met_values.csv" ,
  MMET_CYCLING = MMET_CYCLING,
  MMET_WALKING = MMET_WALKING,
  TOTAL = F
)

### mmets_pp_MEL$diff <- mmets_pp_MEL$base_mmet - mmets_pp_MEL$scen1_mmet # to check for errors. If difference is not negative, there is an error
mmets_pp_MEL <- mmets_pp_MEL %>%
  mutate(age_group = as.factor(case_when(
    age <  18             ~ "0 to 17" ,
    age >= 18 & age <= 40 ~ "18 to 40",
    age >= 41 & age <= 65 ~ "41 to 65",
    age >= 65             ~ "65 plus"))) %>%
mutate(sex =as.factor(sex)) 

mmets_graphs <- mmets_pp_MEL %>% 
  pivot_longer(cols = c("base_mmet", "scen1_mmet"),
               names_to = "scenario", 
               values_to = "mmets") 

scenario.labs <- c("Baseline", "Scenario")
names(scenario.labs) <- c("base_mmet", "scen1_mmet")

##### Graphs for mmets basline and scenario to compare with the dose response curves
mmets <- ggplot(mmets_graphs, aes(x = mmets)) + 
  geom_histogram(bins = 50)  + 
  labs(title="mMET-hours per week baseline and scenario", x="mMETs-hours", y="Frequency") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 12,face="bold"),
        axis.text=element_text(size=10),
        axis.title=element_text(size=10)) +
   facet_grid(. ~scenario, 
              labeller = labeller(scenario = scenario.labs))+
  scale_colour_brewer(type = "seq", palette = "Spectral") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black"))+ 
  theme_classic() 

## Check graph mMET=hours
mmets
########################## 4) RRs per person (code below, has uncertainty inputs) #################################
### Melbourne
source("Scripts/ithim-r_wrappers.R")

### Relative risks of physical inactivity on diseases

RR_PA_calculations_MEL <- gen_pa_rr_wrapper(
  mmets_pp_MEL,
  disease_inventory_location="Data/original/ithimr/disease_outcomes_lookup.csv",
  # location of ithmr default dose response data:
  dose_response_folder=paste0(file.path(find.package('ithimr',lib.loc=.libPaths()), 'extdata/global'),
                              "/dose_response/drpa/extdata"),
  PA_DOSE_RESPONSE_QUANTILE=F
)

###################### 5) PIFS by age and sex (with function health_burden_2) #####################################
source("Scripts/ithim-r_wrappers.R")

# calculate_AP will calculate air pollution, be sure to set false if you don't have air pollution data
# health_burden_2 needs a complete rewrite to be more comprehensible, but works well enough for now

### Melbourne
#### Two options: 1) using simple average by age and sex or 2) using population weights, use second option. 
pif_MEL <- health_burden_2(
  ind_ap_pa_location=RR_PA_calculations_MEL,
  disease_inventory_location="Data/original/ithimr/disease_outcomes_lookup.csv",
  demographic_location="Data/processed/DEMO.csv",
  combined_AP_PA=F,
  calculate_AP=F
) 

# write.csv(pif_MEL[[2]], "SuppDocs/Tables/Pifs.csv" )

pif_MEL_age_sex <- pif_MEL[[2]] %>% dplyr::rename(age=age_group_2) %>%
  dplyr::slice(rep(1:dplyr::n(), each = 5))

age <- rep(seq(16,100,1), times = 2)

pif_MEL_age_sex$age <- age

pif_MEL_age_sex <- pif_MEL_age_sex %>% dplyr::filter(age !=16)

################### 6) Parameters for Mslt code running #######################################################

DISEASE_SHORT_NAMES <- read.csv("Data/processed/mslt/disease_names.csv",as.is=T,fileEncoding="UTF-8-BOM")

### Only include DISEASE_SHORT_NAMES for PA related diseases

disease_inventory_location="Data/original/ithimr/disease_outcomes_lookup.csv"
include <- read.csv(disease_inventory_location,as.is=T,fileEncoding="UTF-8-BOM") %>% 
  dplyr::filter(physical_activity == 1)

DISEASE_SHORT_NAMES <- DISEASE_SHORT_NAMES %>%
  dplyr::filter(acronym %in% include$acronym)


###################### 7) Run rest ##############################################################################
source("Scripts/ithim-r_wrappers.R")

pif_expanded <- pif_MEL_age_sex
  # read.csv("Data/processed/pifs_pa_ap.csv",as.is=T,fileEncoding="UTF-8-BOM")

# ---- chunk-2 ----

## Create baseline life tables

#### Alan, this needs to pick up specific deaths rates for each of the age and sex cohorts (saved in Data/processed/death_rates_males/females)
general_life_table_list_bl <- list()

# dataframe of the age and sex cohorts (crossing just does a cross product)
age_sex_cohorts <- crossing(data.frame(age=i_age_cohort),
                            data.frame(sex=c('male', 'female'))) %>%
  dplyr::mutate(cohort=paste0(age,"_",sex))
# tmp <- RunLifeTable(in_idata = MSLT_DF, in_sex = "male", in_mid_age = 17)
# tmp2 <- RunLifeTable(in_idata = MSLT_DF, in_sex = "male", in_mid_age = 17, death_rates = death_rates)
# 
# 
# tmp <- mapply(RunLifeTable, c(MSLT_DF,MSLT_DF), c('male','female'), c(17,17) )



for (i in 1:nrow(age_sex_cohorts)){
  suppressWarnings(
    general_life_table_list_bl[[i]] <- RunLifeTable(
      in_idata    = MSLT_DF,
      in_sex      = age_sex_cohorts$sex[i],
      in_mid_age  = age_sex_cohorts$age[i],
      death_rates = death_rates
  ))
  names(general_life_table_list_bl)[i] <- age_sex_cohorts$cohort[i]
}

# convert the list of dataframes to single dataframes
general_life_table_bl <- bind_rows(general_life_table_list_bl, .id = "age_group") %>%
  mutate(age_group = as.numeric(gsub("_.*","",age_group)))



# ---- chunk-3 ----


### Change order in disease short_names to start with diabetes. This is important when calculating the scenario disease life tables as diabetes is calculated first to then 
### impact on cardiovascular disease calculations. 

### ALAN, diseases trends should be applied to incidence and case fatality (from here: Data\processed\mslt\incidence_trends_f.csv")
### In the disease trends "Year" means simulation year, not age. 

incidence_trends <- bind_rows(
  read.csv("Data/processed/mslt/incidence_trends_m.csv",as.is=T,fileEncoding="UTF-8-BOM"),
  read.csv("Data/processed/mslt/incidence_trends_f.csv",as.is=T,fileEncoding="UTF-8-BOM")
)

mortality_trends <- bind_rows(
  read.csv("Data/processed/mslt/mortality_trends_m.csv",as.is=T,fileEncoding="UTF-8-BOM"),
  read.csv("Data/processed/mslt/mortality_trends_f.csv",as.is=T,fileEncoding="UTF-8-BOM")
)


disease_cohorts <- DISEASE_SHORT_NAMES %>%
  # Exclude non-diseases, road injuries, and diseases with no pif
  dplyr::filter(is_not_dis == 0 & acronym != 'no_pif' & acronym != 'other' ) %>%
  dplyr::select(sname,acronym,males,females)

# adding the age and sex cohorts:
age_sex_disease_cohorts <- crossing(age_sex_cohorts,disease_cohorts) %>%
  mutate(cohort=paste0(age,'_',sex,'_',sname)) %>%
  # Exclude non-male diseases (and non-female if there were any)
  filter( (sex=='male' & males==1) | (sex=='female' & females==1)) %>%
  dplyr::select(age,sex,sname,acronym,cohort) %>%
  # ishd and strk have the prerequisite disease dmt2
  mutate(prerequsite=ifelse(sname %in% c("ishd","strk"),paste0(age,"_",sex,"_dmt2"),0)) %>%
  # ensuring prequisites are calculated first
  arrange(age,sex,prerequsite,sname)


disease_life_table_list_bl <- list()

for (i in 1:nrow(age_sex_disease_cohorts)){
  disease_life_table_list_bl[[i]] <- RunDisease(
    in_idata         = MSLT_DF,
    in_mid_age       = age_sex_disease_cohorts$age[i],
    in_sex           = age_sex_disease_cohorts$sex[i],
    in_disease       = age_sex_disease_cohorts$sname[i],
    incidence_trends = incidence_trends,
    mortality_trends = mortality_trends
  )
  names(disease_life_table_list_bl)[i] <- age_sex_disease_cohorts$cohort[i]
}

# # ---- chunk-4 ----

###ALAN: you got to here, I continue, please read my notes, my aim was for the code to work. 
## Create scenario life tables with new pifs,includes Diabetes loop. 

### Read disease inventory and only include PA related diseases

disease_relative_risks <- tribble(
  ~sex    , ~prerequsite, ~disease , ~relative_risk       ,
  "male"  ,  "dmt2"     ,  "ishd"  ,  DIABETES_IHD_RR_M   ,
  "female",  "dmt2"     ,  "ishd"  ,  DIABETES_IHD_RR_F   ,
  "male"  ,  "dmt2"     ,  "strk"  ,  DIABETES_STROKE_RR_M,
  "female",  "dmt2"     ,  "strk"  ,  DIABETES_STROKE_RR_F
)

disease_life_table_list_sc <- list()

for (i in 1:nrow(age_sex_disease_cohorts)){
  # i=6
  td1_age_sex <- MSLT_DF %>%
    filter(age >= age_sex_disease_cohorts$age[i] & sex == age_sex_disease_cohorts$sex[i])

  pif_colname <- paste0('pif_',age_sex_disease_cohorts$acronym[i])
  
  pif_disease <- pif_expanded %>%
    filter(age >= age_sex_disease_cohorts$age[i] & sex == age_sex_disease_cohorts$sex[i]) %>%
    dplyr::select(age,sex,pif_colname)
  
  # adjustment for diabetes effect on ihd and stroke
  if(age_sex_disease_cohorts$prerequsite[i] != 0){
    # get name for pif column
    target_disease <- paste0("pif_",age_sex_disease_cohorts$acronym[i])
    # get prerequisite disease cohort name (i.e., age_sex_dmt2 for diabetes)
    dia_col <- age_sex_disease_cohorts$prerequsite[i]
    # select relative risk of disease given diabetes (depends on sex, not age)
    relative_risk <- disease_relative_risks %>%
      filter(sex == age_sex_disease_cohorts$sex[i] &
               disease == age_sex_disease_cohorts$sname[i]) %>%
      pull(relative_risk)
    # (store old pif)
    # old_pif <- pif_disease[[target_disease]]
    # diabetes pif = - { scenario prevalence - baseline prevalence } * (RR - 1)  / { baseline prevalence * (RR - 1) + 1 }
    scenario_prevalence <- disease_life_table_list_sc[[dia_col]]$px
    baseline_prevalence <- disease_life_table_list_bl[[dia_col]]$px
    pif_dia <- -(scenario_prevalence - baseline_prevalence)*(relative_risk-1)/
      (baseline_prevalence * (relative_risk-1) + 1)
    # modify pif for target disease: new pif =  (1 - old pif) * (1 - diabetes pif)
    pif_disease[[target_disease]] <- 1- (1-pif_disease[[target_disease]]) * (1-pif_dia)
    # print(sum(old_pif-pif_disease[[target_disease]]))
  }
  
  ### Multiply for vector for duration scenario
  # pif_disease[,2] <- pif_disease[,2] * sc_duration
  incidence_colname <- paste0('incidence_', age_sex_disease_cohorts$sname[i])
  new_col <- td1_age_sex%>%pull(incidence_colname) * (1 - (pif_disease%>%pull(pif_colname)))
  new_col[is.na(new_col)] <- 0
  td1_age_sex[[incidence_colname]] <- new_col
  
  ## Instead of idata, feed td to run scenarios. Now all diseases are run again, with the effect of diabetes
  ## on cardiovascular diseases taken into account. 
  
  disease_life_table_list_sc[[i]] <- RunDisease(
    in_idata         = td1_age_sex,
    in_sex           = age_sex_disease_cohorts$sex[i],
    in_mid_age       = age_sex_disease_cohorts$age[i],
    in_disease       = age_sex_disease_cohorts$sname[i],
    incidence_trends = incidence_trends,
    mortality_trends = mortality_trends
  )
  names(disease_life_table_list_sc)[i] <- age_sex_disease_cohorts$cohort[i]
}
## Uncomment to check scenario life tables
# View(disease_life_table_list_sc[[3]])


# Calculation of differences between baseline and scenario
for (cohort in age_sex_disease_cohorts$cohort) {
  disease_life_table_list_sc[[cohort]]$diff_inc_disease <-
    disease_life_table_list_sc[[cohort]]$incidence_disease - disease_life_table_list_bl[[cohort]]$incidence_disease
  
  disease_life_table_list_sc[[cohort]]$diff_prev_disease <-
    disease_life_table_list_sc[[cohort]]$px - disease_life_table_list_bl[[cohort]]$px
  
  disease_life_table_list_sc[[cohort]]$diff_mort_disease <-
    disease_life_table_list_sc[[cohort]]$mx - disease_life_table_list_bl[[cohort]]$mx
  
  disease_life_table_list_sc[[cohort]]$diff_pylds_disease <-
    (disease_life_table_list_sc[[cohort]]$px - disease_life_table_list_bl[[cohort]]$px) * 
    (disease_life_table_list_bl[[cohort]]$dw_disease)
}


# convert the list of dataframes to single dataframes
disease_life_table_bl <- bind_rows(disease_life_table_list_bl, .id = "age_sex_disease_cohort") %>%
  mutate(age_sex_disease_cohort = as.numeric(gsub("_.*","",age_sex_disease_cohort))) %>%
  rename(age_group=age_sex_disease_cohort)

disease_life_table_sc <- bind_rows(disease_life_table_list_sc, .id = "age_sex_disease_cohort") %>%
  mutate(age_sex_disease_cohort = as.numeric(gsub("_.*","",age_sex_disease_cohort))) %>%
  rename(age_group=age_sex_disease_cohort)

### BZ: graph to check difference values for incidence, prevalence, mortality and yYLDs diseases
### Uncomment to run
# graphs_check <- list()
# index <- 1
# for(i in 1:length(disease_life_table_list_sc)) {
#   data <- disease_life_table_list_sc[[i]]
#   line_chart_change <-  ggplot(data = data, aes(x = age, y = diff_inc_disease)) +
#   geom_line(aes(color="Incidence")) + 
#   geom_line(data = data, aes(y = diff_prev_disease, color="Prevalence")) +
#   geom_line(data = data, aes(y = diff_mort_disease, color="Mortality")) + 
#     geom_line(data = data, aes(y = diff_pylds_disease, color="pYLDs")) +
#     labs(color="") +
#  labs(x = "Age",
#             title = paste(data[1, "age"], data[1, "sex"], data[1, "disease"], sep=" "),
#       y = "Rates difference") +
#   theme(plot.title = element_text(hjust = 0.5, size = 12,face="bold"),
#         axis.text=element_text(size=10),
#         axis.title=element_text(size=10)) +
#   theme(legend.position = "right",
#         legend.title = element_blank(),
#         legend.text = element_text(colour = "black", size = 10),
#         legend.key = element_blank(),
#         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
#   theme_classic() +
#   geom_hline(yintercept=0, linetype='dashed', color = 'black')
#   ggsave(line_chart_change, file=paste("./SuppDocs/CheckGraphs/diseases/", names(disease_life_table_list_sc[i]), ".png", sep=""), width = 14, height = 10, units = "cm")
#   graphs_check[[i]] <- line_chart_change 
#   
#   # dev.off()
#   index <- index + 1
#   
# }


# ---- chunk-5 ----

## Generate total change in mortality rate to recalculate scenario general life tables


### Sum mortality rate and pylds change scenarios
mx_pylds_sc_total_disease_df <- disease_life_table_sc %>%
  group_by(age_group,sex,age) %>%
  summarise(mortality_sum=sum(diff_mort_disease,na.rm=T),
            pylds_sum=sum(diff_pylds_disease,na.rm=T)) %>%
  ungroup() %>%
  mutate(age_sex_cohort=paste0(age_group,'_',sex))


# ---- chunk-6 ----

## Calculate general life tables with modified mortality and pylds total
## Original mortality rate is modified by the mx_sc_total (total change in mortality from diseases)
## Original pyld rate is modified by the change in each disease pylds

general_life_table_list_sc <- list()

for (i in 1:nrow(age_sex_cohorts)){
  # modify idata's mortality and pyld total for the said scenario
  mx_pylds_sc_total_disease_df_cohort <- mx_pylds_sc_total_disease_df %>%
    filter(age_sex_cohort==age_sex_cohorts$cohort[i]) %>%
    dplyr::select(age,mortality_sum,pylds_sum)
 
### Modify rates in static MSLT  (pylds are always static, mx can include future trends)
   td2 <- MSLT_DF %>%
    filter(sex==age_sex_cohorts$sex[i]) %>%
    left_join(mx_pylds_sc_total_disease_df_cohort,by="age") %>%
    mutate(mx=mx+replace_na(mortality_sum,0),
           pyld_rate=pyld_rate+replace_na(pylds_sum,0)) %>%
    dplyr::select(-mortality_sum,-pylds_sum)

### Modify death rates with future trends
   td3 <- death_rates %>%
     mutate(cohort=paste(age_cohort, sex, sep = "_")) %>% # variable to match change in mortality rates df
     filter(cohort==age_sex_cohorts$cohort[i]) %>%
     left_join(mx_pylds_sc_total_disease_df_cohort) %>%
     mutate(rate=rate+replace_na(mortality_sum,0))%>%
     dplyr::select(-mortality_sum,-pylds_sum)   
   
   
   
  suppressWarnings(
    general_life_table_list_sc[[i]] <- RunLifeTable(
      in_idata    = td2,
      in_sex      = age_sex_cohorts$sex[i],
      in_mid_age  = age_sex_cohorts$age[i],
      death_rates = td3
  ))
  names(general_life_table_list_sc)[i] <- age_sex_cohorts$cohort[i]
}

# convert the list of dataframes to single dataframes
general_life_table_sc <- bind_rows(general_life_table_list_sc, .id = "age_group") %>%
  mutate(age_group = as.numeric(gsub("_.*","",age_group)))

### Graph check, commented out
# 
# graphs_check_lt <- list()
# index <- 1
#  for(i in 1:length(general_life_table_list_bl)) {
#    data_bl <- general_life_table_list_bl[[i]]
#    data_sc <- general_life_table_list_sc[[i]]
#    plot <-  ggplot(data = data_bl, aes(x = age, y = mx)) +
#    geom_line(aes(color="Mortality rate baseline")) +
#    geom_line(data = data_sc, aes(y = mx, color="Mortality rate scenario")) +
#      labs(color="") +
#   labs(x = "Age",
#              title = paste(data_bl[1, "age"], data_bl[1, "sex"], sep=" "),
#        y = "Rates difference") +
#    theme(plot.title = element_text(hjust = 0.5, size = 12,face="bold"),
#          axis.text=element_text(size=10),
#          axis.title=element_text(size=10)) +
#    theme(legend.position = "right",
#          legend.title = element_blank(),
#          legend.text = element_text(colour = "black", size = 10),
#          legend.key = element_blank(),
#          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
#    theme_classic() +
#    geom_hline(yintercept=0, linetype='dashed', color = 'black')
#    ggsave(plot, file=paste("./SuppDocs/CheckGraphs/lifetables/", names(general_life_table_list_bl[i]), ".png", sep=""), width = 14, height = 10, units = "cm")
#    graphs_check_lt[[i]] <- plot
# 
#    # dev.off()
#    index <- index + 1
# }
# ---- chunk-7 ----

## In the following list 'output_life_table', 34 data frames are nested per age and sex cohort

## Outputs are generated following the index order of disease life tables baseline and scenarios where diabetes is first calculated as it impacts on cardiovascular diseases. 

## In the following list 'output_life_table', 34 data frames are nested per age and sex cohort

## Outputs are generated following the index order of disease life tables baseline and scenarios where diabetes is first calculated as it impacts on cardiovascular diseases. 

dia_index <- which(DISEASE_SHORT_NAMES$sname=='dmt2')
dia_order <- c(dia_index,c(1:nrow(DISEASE_SHORT_NAMES))[-dia_index])

disease_sc <- inner_join(disease_life_table_sc %>%
                           dplyr::select(age_group,sex,age,disease,incidence_disease,mx,px),
                         general_life_table_sc %>%
                           dplyr::select(age_group,sex,age,Lx,ex,Lwx,ewx),
                         by=c("age","sex","age_group")) %>%
  mutate(intervention="sc")

disease_bl <- inner_join(disease_life_table_bl %>%
                           dplyr::select(age_group,sex,age,disease,incidence_disease,mx,px),
                         general_life_table_bl %>%
                           dplyr::select(age_group,sex,age,Lx,ex,Lwx,ewx),
                         by=c("age","sex","age_group")) %>%
  mutate(intervention="bl")

disease_combined <- bind_rows(disease_sc,disease_bl) %>%
  pivot_wider(names_from  = intervention,
              values_from = c(incidence_disease,mx,px,Lx,ex,Lwx,ewx)) %>%
  mutate(inc_num_bl   = incidence_disease_bl*(1-px_bl)*Lx_bl,
         inc_num_sc   = incidence_disease_sc*(1-px_sc)*Lx_sc,
         inc_num_diff = inc_num_sc-inc_num_bl,
         mx_num_bl    = mx_bl*Lx_bl,
         mx_num_sc    = mx_sc*Lx_sc,
         mx_num_diff  = mx_num_sc-mx_num_bl) %>%
  pivot_wider(names_from  = disease,
              values_from = incidence_disease_sc:mx_num_diff)

general_lf <- bind_rows(
  general_life_table_sc %>%
    dplyr::select(age_group,sex,age,Lx,ex,Lwx,ewx) %>%
    mutate(intervention="sc"),
  general_life_table_bl %>%
    dplyr::select(age_group,sex,age,Lx,ex,Lwx,ewx) %>%
    mutate(intervention="bl")) %>%
  pivot_wider(names_from  = intervention,
              values_from = c(Lx,ex,Lwx,ewx)) %>%
  mutate(Lx_diff  = Lx_sc-Lx_bl,
         Lwx_diff = Lwx_sc-Lwx_bl,
         ex_diff  = ex_sc-ex_bl,
         ewx_diff = ewx_sc-ewx_bl)


##### Data frames to generate outputs

######## Dataframe with all outputs by age and sex cohort over the simulation years (years of the cohort)
output_df <- inner_join(disease_combined,
                        general_lf,
                        by=c("age","sex","age_group"))


######## Dataframe with all outputs aggregated by year of simlation by sex
output_df_agg_sex  <- output_df   %>% ### Create a simulation year columns
  group_by(age_group, sex, .add=TRUE) %>%
  dplyr::mutate(year = 1:dplyr::n()) %>%
  dplyr::select(sex, year, Lx_bl, Lx_sc, Lx_diff, Lwx_bl, Lwx_sc, Lwx_diff, contains("num")) %>%
  ungroup() %>%
  group_by(year, sex, .add=TRUE) %>% 
  summarise_if(is.numeric, funs(sum)) %>%
  ungroup() 

######## Dataframe with all outputs aggregated by year of simlation all
output_df_agg_all  <- output_df   %>% ### Create a simulation year columns
  group_by(age_group, sex, .add=TRUE) %>%
  dplyr::mutate(year = 1:dplyr::n()) %>%
  dplyr::select(sex, year, Lx_bl, Lx_sc, Lx_diff, Lwx_bl, Lwx_sc, Lwx_diff, contains("num")) %>%
  ungroup() %>%
  group_by(year, .add=TRUE) %>% 
  summarise_if(is.numeric, funs(sum)) %>%
  ungroup() 

# ---- chunk-11 ---- 

# ---- chunk-11.1 Tables ----

### Create age groups variable, easier to read

output_df <- output_df %>%
  mutate(age_group_2 = case_when(
    age_group == 17 ~ "16-19",
    age_group == 22 ~ "20-24",
    age_group == 27 ~ "25-29",
    age_group == 32 ~ "30-34",
    age_group == 37 ~ "35-39",
    age_group == 42 ~ "40-44",
    age_group == 47 ~ "45-49",
    age_group == 52 ~ "50-54",
    age_group == 57 ~ "55-59",
    age_group == 62 ~ "60-64",
    age_group == 67 ~ "65-69",
    age_group == 72 ~ "70-74",
    age_group == 77 ~ "75-79",
    age_group == 82 ~ "80-84",
    age_group == 87 ~ "85-89",
    age_group == 92 ~ "90-94",
    age_group == 97 ~ "95 plus")) %>%
  mutate(cohort=paste(sex, age_group, sep = "_")) %>%
  rename(`Age group` = age_group_2, Gender = sex)

### Population to add to tables
population <- population %>% 
  rename(cohort = sex_age_cat) %>%
  dplyr::filter(cohort %in% unique(output_df$cohort))

# ---- chunk-11.1.1 Table: Life expectancy and health adjusted life expectancy ----

output_life_expectancy_change <- output_df[!duplicated(output_df$cohort), c("Age group", "cohort", "Gender", "ex_bl", "ex_sc", "ewx_bl", "ewx_sc", 
                                                                         "ex_diff", "ewx_diff")] %>%
  dplyr::rename(`Life expectancy at baseline` = ex_bl, 
                `Life expectancy scenario` = ex_sc, 
                `Health adjusted life expectancy baseline` = ewx_bl, 
                `Health adjusted life expectancy scenario` = ewx_sc) %>%
  dplyr::mutate(`Difference in life expectancy in days` = ex_diff * 365, 
                `Difference in health adjusted life expectancy in days` = ewx_diff* 365) %>% 
  mutate_if(is.numeric, round, digits = 3) %>%
  left_join(population)%>%
  dplyr::select(-c(ex_diff, ewx_diff, cohort)) %>%
  relocate(population, .after = Gender)%>%
  rename('Population cohort'=population)

output_life_expectancy_change <- output_life_expectancy_change[order(output_life_expectancy_change$Gender),] 

# ---- chunk-11.1.2 Table: Life years and health adjusted life years ----

output_life_years_change <- output_df %>% 
  group_by(Gender, `Age group`, cohort, .add=TRUE) %>%
  summarise_if(is.numeric, funs(sum)) %>%
  ungroup() %>%
  dplyr::select(`Age group`,cohort, Gender,Lx_diff, Lwx_diff) %>%
  dplyr::rename(`Life years` = Lx_diff, 
                `Health adjusted life years` = Lwx_diff)  %>% 
  mutate_if(is.numeric, round) %>%
  left_join(population) %>%
  relocate(population, .after = Gender)%>%
  rename('Population cohort'=population) %>%
  dplyr::select(-cohort)

# ---- chunk-11.1.2 Table: Diseases deaths, incidence and ylds ----

output_diseases_change <- output_df %>% 
  group_by(Gender, `Age group`, cohort, .add=TRUE) %>%
  summarise_if(is.numeric, funs(sum)) %>% 
  mutate_if(is.numeric, round) %>%
  left_join(population) %>%
  relocate(population, .after = Gender)%>%
  rename('Population cohort'=population) %>%
  dplyr::select(-cohort) %>%
  dplyr::select(`Age group`, Gender, matches("diff_dmt2|diff_ishd|diff_strk|diff_carc|diff_copd|diff_tbalc|diff_brsc|diff_utrc|diff_lri"))


# ---- chunk-11.1.3 Graphs by outcome and simulation year ----
### changes in life years and health-adjusted life years by age and sex cohort over cohorts life course.

# ---- chunk-11.1.4 Graphs for changes in diseases over time  ----

### Incidence

data_f <- dplyr::filter(output_df_agg_sex, sex == "female") %>% dplyr::select("sex", "year", contains("diff"))
data_m <- dplyr::filter(output_df_agg_sex, sex == "male") %>% dplyr::select("sex", "year", contains("diff"))
data_t <- dplyr::filter(output_df_agg_all) %>% dplyr::select("year", contains("diff")) %>%
  mutate(sex ="total")

data <- bind_rows(data_f, data_m, data_t)

plot <- plot <- data %>%
  ggplot(aes(x = year, y = inc_num_diff_brsc)) +
  geom_smooth(method = "loess") +
  geom_smooth(aes(y = inc_num_diff_carc, method = "loess", color="Colon cancer")) +
  geom_smooth(aes(y = inc_num_diff_dmt2, method = "loess", color="Type 2 diabetes")) +
  geom_smooth(aes(y = inc_num_diff_tbalc, method = "loess", color="Lung cancer")) +
  geom_smooth(aes(y = inc_num_diff_utrc, method = "loess", color="Uterine cancer")) +
  geom_smooth(aes(y = inc_num_diff_ishd, method = "loess", color="Ischemic heart disease")) +
  geom_smooth(aes(y = inc_num_diff_strk, method = "loess", color="Stroke")) + 
  labs(x = "Simulation year",
       title = paste("Changes in disease incidence over time"),
       y = "Numbers") +
labs(color="") +
  theme(plot.title = element_text(hjust = 0.5, size = 12,face="bold"),
        axis.text=element_text(size=10),
        axis.title=element_text(size=10)) +
  theme_classic() +
  geom_hline(yintercept=0, linetype='dashed', color = 'black')+
 facet_wrap(. ~ sex) + 
   theme(
     strip.background = element_blank() ) +
  scale_fill_brewer(name = "Sex") +
  theme(legend.position = "bottom",
        legend.text = element_text(colour = "black", size = 8),
        legend.key = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggsave(plot, file=paste0("./output/diseases/incidence.png"), width = 14, height = 10, units = "cm")


### Deaths

plot <- plot <- data %>%
  ggplot(aes(x = year, y = mx_num_diff_brsc)) +
  geom_smooth(method = "loess") +
  geom_smooth(aes(y = mx_num_diff_carc, method = "loess", color="Colon cancer")) +
  geom_smooth(aes(y = mx_num_diff_dmt2, method = "loess", color="Type 2 diabetes")) +
  geom_smooth(aes(y = mx_num_diff_tbalc, method = "loess", color="Lung cancer")) +
  geom_smooth(aes(y = mx_num_diff_utrc, method = "loess", color="Uterine cancer")) +
  geom_smooth(aes(y = mx_num_diff_ishd, method = "loess", color="Ischemic heart disease")) +
  geom_smooth(aes(y = mx_num_diff_strk, method = "loess", color="Stroke")) + 
  labs(x = "Simulation year",
       title = paste("Changes in disease mortality over time"),
       y = "Numbers") +
  labs(color="") +
  theme(plot.title = element_text(hjust = 0.5, size = 12,face="bold"),
        axis.text=element_text(size=10),
        axis.title=element_text(size=10)) +
  theme_classic() +
  geom_hline(yintercept=0, linetype='dashed', color = 'black')+
  facet_wrap(. ~ sex) + 
  theme(
    strip.background = element_blank() ) +
  scale_fill_brewer(name = "Sex") +
  theme(legend.position = "bottom",
        legend.text = element_text(colour = "black", size = 8),
        legend.key = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggsave(plot, file=paste0("./output/diseases/deaths.png"), width = 14, height = 10, units = "cm")



# ---- chunk-11.1.6 Graph for changes in life years -------

data_f <- dplyr::filter(output_df_agg_sex, sex == "female") %>% dplyr::select("sex", "year", "Lx_diff", "Lwx_diff")
data_m <- dplyr::filter(output_df_agg_sex, sex == "male") %>% dplyr::select("sex", "year", "Lx_diff", "Lwx_diff")
data_t <-  dplyr::filter(output_df_agg_all) %>% dplyr::select("year", "Lx_diff", "Lwx_diff")       
        
plot <- data_t %>%
          ggplot(aes(x = year, y = Lx_diff)) +
          geom_smooth(method = "loess", aes(color= "Life years total")) +
          geom_smooth(data = data_t, aes(y = Lwx_diff, method = "loess",  color= "HALYs total")) +
          geom_smooth(data = data_f, aes(y = Lx_diff,  method = "loess",  color= "Life years female")) +
          geom_smooth(data = data_f, aes(y = Lwx_diff,  method = "loess",  color= "HALYs female")) +
          geom_smooth(data = data_m, aes(y = Lx_diff,  method = "loess",  color= "Life years male")) +
          geom_smooth(data = data_m, aes(y = Lwx_diff,  method = "loess",  color= "HALYs male")) +
          labs(x = "Simulation year",
               title = paste("Difference life years and health-adjusted life years"),
               y = "Numbers") +
          labs(color="") +
          theme(plot.title = element_text(hjust = 0.5, size = 12,face="bold"),
                axis.text=element_text(size=10),
                axis.title=element_text(size=10)) +
          theme(legend.position = "right",
                legend.title = element_blank(),
                legend.text = element_text(colour = "black", size = 10),
                legend.key = element_blank(),
                axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
          theme_classic() +
          geom_hline(yintercept=0, linetype='dashed', color = 'black')
         ggsave(plot, file=paste0("./output/LifeYears/lifeyears.png"), width = 14, height = 10, units = "cm")


