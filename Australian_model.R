#### Model for Australia wide and Australian capital cities for changes in physical activity 
#### This model will be embedded in the Urban Observatory

# ---- chunk-intro ----
rm (list = ls())
library(ithimr)
library(dplyr)
library(readr)
library(data.table)
library(tidyr)

options(scipen=999)
### All processing without uncertainty inputs left in other codes, here, all code with uncertainty


#### TO DO: 

## Add uncertainty inputs for marginal mets for NHS inputs

## Check that parameters work for running one age group at the time
## Add population option (what proportion impacted)
## add option for scearios (e.g. change x driving to walking)
## Add time frame into the future for modeling


## CALCULATION ORDER only including physical activity changes
# 1) Inputs MSLT (from runDataPrepMSLT, fixed)
# 2) Run scenarios (use calculateScenario, not fixed)
# 3) Matched population with mets baseline and scenario (use )
# 4) mmets per person (code below, has uncertainty inputs)
# 5) RRs per person (code below, has uncertainty inputs)
# 6) PIFS by age and sex (with function health_burden_2)
# 7) Parameters for Mslt code running
# 8) Run rest

###################################### Probabilistic Sensitivity Scenario parameters ################################

## TO DO: see montecarlo file, we need to define distributions, RRs lognormal
# NSAMPLES <- 2000 #activate for Monte Carlo simulation
MMET_CYCLING <- 4.63 #c(4.63, (1.2) #lognormal  
MMET_WALKING <- 2.53 #c(2.53, 1.1)  #lognormal 
MMET_MOD <- 3.5 ## TO DO: GET Uncertain parameters
MMET_VIG <- 7 ## TO DO: GET Uncertain parameters
DIABETES_IHD_RR_F <- 2.82 ## c(2.82, CI (2.35, 3.38) get SD from CI
DIABETES_STROKE_RR_F <- 2.28 ## c(2.28) CI (1.93, 2.69) get SD from CI
DIABETES_IHD_RR_M <- 2.16 ## c(2.16, CI (1.82, 2.56) get SD from CI
DIABETES_STROKE_RR_M <- 1.83 ## c(1.83) CI (1.60, 2.08) get SD from CI

PA_DOSE_RESPONSE_QUANTILE <- F # Generates random numbers for each of the Relative Risk functions

############################## 0) Inputs MSLT (from runDataPrepMSLT) ###############################################

### MSLT has fixed inputs and inputs that change by location (death rates and population).
### General inputs for all models 
mslt_general="Data/processed/mslt/mslt_df.csv"
MSLT_DF <- read.csv(mslt_general,as.is=T,fileEncoding="UTF-8-BOM")

#### Get death rates: 1) periodic and 2) projections. RunLifeTable has option to choose which death rates to use. Use projections for baseline
#### and periodic for sensitivity analysis.

##### Death rates in Australia are available by age (1-yr intervals) and sex for states and Australia wide. Then, choose from
##### location = "Australia", "Australian Capital Territory", "New South Wales", "Northern Territory", "Queensland",  "South Australia", "Tasmania", "Victoria", "Western Australia"   

#### 1) Death rates periodic (no projections, assumes current death rates are observed in the future) are added to mslt for selected location
death_rate_periodic="Data/processed/mslt/deaths_periodic.csv"
death_rate_periodic <- read.csv(death_rate_periodic,as.is=T,fileEncoding="UTF-8-BOM") %>% dplyr::filter(location == "Australia") %>%
  dplyr::select("sex_age_cat", "mx")
MSLT_DF <- left_join(MSLT_DF, death_rate_periodic)

#### 2) Deaths rates with projections can be used as an option in RunLifeTable function, two assumptions are made by ABS for improvements in life
#### expectancy: high and medium
death_rates="Data/processed/mslt/deaths_projections.csv"
death_rates <- read.csv(death_rates,as.is=T,fileEncoding="UTF-8-BOM") %>% dplyr::filter(location == "Australia", assumption == "medium")

##### Population: filter location population and replace in MSLT_DF. 
##### location: "Greater Sydney", "Greater Melbourne", "Greater Brisbane", "Greater Perth", "Greater Adelaide", "Greater Hobart", "Greater Darwin", 
##### "Australia"
source("Scripts/data_prep/population_prep.R")
population <- GetPopulation(
  population_data="Data/original/abs/population_census.xlsx",
  location= "Australia")
MSLT_DF <- left_join(MSLT_DF, population)

############################## 1) Run scenarios ###################################################################

#### For Australia wide: walking or/and cycling changes, no more calculations requiered to generate mmets for RRs calculations
source("Scripts/scenarios_AUS.R")
source("Scripts/data_prep/synthetic_pop.R")
mmets_pp_Aus <- calculateMMETSperPerson_AUS (pa_location="Data/Physical activity/NHS2017-18_CSV/NHS17SPB.csv", 
                                             hh_location="Data/Physical activity/NHS2017-18_CSV/NHS17HHB.csv",
                                             MMET_CYCLING=MMET_CYCLING,
                                             MMET_WALKING=MMET_CYCLING,
                                             MMET_MOD=MMET_CYCLING,
                                             MMET_VIG=MMET_CYCLING,
                                             SCEN_WALK = 50, # user defined in minutes
                                             SCEN_CYCLE = 0, # user defined in minutes
                                             age_input = c("15 to 19","20 tp 24", "25 to 29", "30 to 34", 
                                                           "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 59", "60 to 64", 
                                                           "65 to 69", "70 to 74", "75 to 79", "80 to 84", "85 +" ), # user defined, default, all ages
                                             sex_input = c("male", "female")) # user defined, default male and female

############################## 2) Get individual RRs ############################################################

source("Scripts/ithim-r_wrappers.R")

### Australia
RR_PA_calculations_AUS <- gen_pa_rr_wrapper(
  mmets_pp_Aus,
  disease_inventory_location="Data/original/ithimr/disease_outcomes_lookup.csv",
  # location of ithmr default dose response data:
  dose_response_folder=paste0(file.path(find.package('ithimr',lib.loc=.libPaths()), 'extdata/global'),
                              "/dose_response/drpa/extdata"),
  PA_DOSE_RESPONSE_QUANTILE=F
)




###################### 3) PIFS by age and sex (with function health_burden_2) #####################################
source("Scripts/ithim-r_wrappers.R")

pif_AUS <- health_burden_2(
   ind_ap_pa_location=RR_PA_calculations_AUS,
   disease_inventory_location="Data/original/ithimr/disease_outcomes_lookup.csv",
   demographic_location="Data/processed/DEMO_AUS.csv",
   combined_AP_PA=F,
   calculate_AP=F
 )

### Australian data for physical activity to derive PIFs is up to age group 85 +, below code to run mslt splits age group 85+. 
### I repeat age group 85+ which are dem_index 29 for male and 31 for female
## ALAN, my code below is not very tidy!

pif_AUS <- pif_AUS[[2]] %>% 
 add_row(filter(., dem_index == c(29, 30))) 
pif_AUS[31,1] <- 31
pif_AUS[32,1] <- 32

pif_AUS <- pif_AUS %>% 
  add_row(filter(., dem_index == c(31, 32)))
pif_AUS[33,1] <- 33
pif_AUS[34,1] <- 34

pif_AUS <- pif_AUS %>% 
  dplyr::slice(rep(1:dplyr::n(), each = 5)) %>% 
  dplyr::mutate(age=rep(seq(16,100,1), times = 2))

###################### 6) Parameters for Mslt code running #######################################################

DISEASE_SHORT_NAMES <- read.csv("Data/processed/mslt/disease_names.csv",as.is=T,fileEncoding="UTF-8-BOM")

### Only include DISEASE_SHORT_NAMES for PA related diseases

disease_inventory_location="Data/original/ithimr/disease_outcomes_lookup.csv"
include <- read.csv(disease_inventory_location,as.is=T,fileEncoding="UTF-8-BOM") %>% 
  dplyr::filter(physical_activity == 1)

DISEASE_SHORT_NAMES <- DISEASE_SHORT_NAMES %>%
  dplyr::filter(acronym %in% include$acronym)


year <- 2017


### Inputs that we can change in a shiny app
i_age_cohort <- c(17, 22, 27, 32, 37, 42, 47, 52, 57, 62, 67, 72, 77, 82, 87, 92, 97)

i_sex <- c('male', 'female')

# sc_duration <- replicate(4,1) %>% append(replicate(80, 0))

###################### 7) Run rest ##############################################################################
source("Scripts/ithim-r_wrappers.R")

pif_expanded <- pif_AUS

# ---- chunk-2 ----

## Create baseline life tables

#### Alan, this needs to pick up specific deaths rates for each of the age and sex cohorts (saved in Data/processed/death_rates_males/females)
general_life_table_list_bl <- list()

# dataframe of the age and sex cohorts (crossing just does a cross product)
age_sex_cohorts <- crossing(data.frame(age=c(17, 22, 27, 32, 37, 42, 47, 52, 57, 62, 67, 72, 77, 82, 87, 92, 97)),
                            data.frame(sex=c('male', 'female'))) %>%
  dplyr::mutate(cohort=paste0(age,"_",sex))
# tmp <- RunLifeTable(in_idata = MSLT_DF, in_sex = "male", in_mid_age = 17)
# tmp2 <- RunLifeTable(in_idata = MSLT_DF, in_sex = "male", in_mid_age = 17, death_rates = death_rates)
# 
# 
# tmp <- mapply(RunLifeTable, c(MSLT_DF,MSLT_DF), c('male','female'), c(17,17) )



# index <- 1

for (i in 1:nrow(age_sex_cohorts)){
  suppressWarnings(
    general_life_table_list_bl[[i]] <- RunLifeTable(in_idata   = MSLT_DF,
                                                    in_sex     = age_sex_cohorts$sex[i],
                                                    in_mid_age = age_sex_cohorts$age[i],
                                                    death_rates= death_rates)
  )
  
  names(general_life_table_list_bl)[i] <- age_sex_cohorts$cohort[i]
}


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
  dplyr::select(sname,males,females)

# adding the age and sex cohorts:
age_sex_disease_cohorts <- crossing(age_sex_cohorts,disease_cohorts) %>%
  mutate(cohort=paste0(age,'_',sex,'_',sname)) %>%
  # Exclude non-male diseases (and non-female if there were any)
  filter( (sex=='male' & males==1) | (sex=='female' & females==1)) %>%
  dplyr::select(age,sex,sname,cohort)

# ensuring we start with diabetes (dmt2)
age_sex_disease_cohorts <- bind_rows(
  age_sex_disease_cohorts %>%
    filter(sname=='dmt2'),
  age_sex_disease_cohorts %>%
    filter(sname!='dmt2')
)

# # testing outputs
# tmp <- RunDisease(in_idata=MSLT_DF, in_mid_age=17, in_sex='male',
#                   in_disease='ishd')
# # modify incidence rates and case fatality rates with trends. 
# tmp2 <- RunDisease(in_idata=MSLT_DF, in_mid_age=17, in_sex='male',
#                    in_disease='ishd',incidence_trends=incidence_trends,
#                    mortality_trends=mortality_trends)


disease_life_table_list_bl <- list()

for (i in 1:nrow(age_sex_disease_cohorts)){
  disease_life_table_list_bl[[i]] <- RunDisease(in_idata         = MSLT_DF,
                                                in_mid_age       = age_sex_disease_cohorts$age[i],
                                                in_sex           = age_sex_disease_cohorts$sex[i],
                                                in_disease       = age_sex_disease_cohorts$sname[i],
                                                incidence_trends = incidence_trends,
                                                mortality_trends = mortality_trends)
  
  names(disease_life_table_list_bl)[i] <- age_sex_disease_cohorts$cohort[i]
}



#### All above works up to here
# # ---- chunk-4 ----

## Create scenario life tables with new pifs,includes Diabetes loop. 

### Read disease inventory and only include PA related diseases


disease_life_table_list_sc <- list()
index <- 1


disease_relative_risks <- list(c(DIABETES_IHD_RR_M,DIABETES_IHD_RR_F),
                               c(DIABETES_STROKE_RR_M,DIABETES_STROKE_RR_F))
##!! diabetes must be calculated before stroke and ihd
ishd_index <- which(DISEASE_SHORT_NAMES$sname=='ishd')
strk_index <- which(DISEASE_SHORT_NAMES$sname=='strk')
dia_index <- which(DISEASE_SHORT_NAMES$sname=='dmt2')
dia_order <- c(dia_index,c(1:nrow(DISEASE_SHORT_NAMES))[-dia_index])
for (iage in i_age_cohort){
  td1_age <- MSLT_DF[MSLT_DF$age>=iage,] 
  pif_disease_age <- pif_expanded[pif_expanded$age>=iage,]
  for (isex in i_sex){
    td1_age_sex <- td1_age[td1_age$sex==isex,]
    pif_disease_age_sex <- pif_disease_age[pif_disease_age$sex==isex,]
    for (d in c(1:nrow(DISEASE_SHORT_NAMES))[dia_order]){
      
      ## Exclude non-males diseases and non-chronic diseases and road injuries and disease with no pif
      if (isex == 'male' && (DISEASE_SHORT_NAMES$disease[d] %in% c('breast cancer', 'uterine cancer'))|| 
          DISEASE_SHORT_NAMES$is_not_dis[d] != 0 || DISEASE_SHORT_NAMES$acronym[d] == 'no_pif' || DISEASE_SHORT_NAMES$acronym[d] == 'other'){
      } else {
        
        
        pif_colname <- paste0('pif_',DISEASE_SHORT_NAMES$acronym[d])
        
        pif_disease <- pif_disease_age_sex[,colnames(pif_disease_age_sex) %in% c('age', 'sex', pif_colname)]
        
        # adjustment for diabetes effect on ihd and stroke
        if(d %in% c(ishd_index,strk_index)){
          # select which disease
          which_disease <- which(c(ishd_index,strk_index)==d)
          # get name for pif column
          target_disease <- c('pif_ihd','pif_stroke')[which_disease]
          # get diabetes label, just made
          dia_col <- paste0(iage,'_',isex,'_dmt2')
          # select relative risk of disease given diabetes (depends on sex, not age)
          relative_risk <- disease_relative_risks[[which_disease]][which(i_sex==isex)]
          # (store old pif)
          old_pif <- pif_disease[[target_disease]]
          # diabetes pif = - { scenario prevalence - baseline prevalence } * (RR - 1)  / { baseline prevalence * (RR - 1) + 1 }
          pif_dia <- -(disease_life_table_list_sc[[dia_col]]$px - disease_life_table_list_bl[[dia_col]]$px)*(relative_risk-1)/
            (disease_life_table_list_bl[[dia_col]]$px * (relative_risk-1) + 1)
          # modify pif for target disease: new pif =  (1 - old pif) * (1 - diabetes pif)
          pif_disease[[target_disease]] <- 1- (1-pif_disease[[target_disease]]) * (1-pif_dia)
          # print(sum(old_pif-pif_disease[[target_disease]]))
          
        }
        
        ### Multiply for vector for duration scenario
        # pif_disease[,2] <- pif_disease[,2] * sc_duration
        
        new_col <- td1_age_sex[[paste('incidence', DISEASE_SHORT_NAMES$sname[d], sep = '_')]] * (1 - (pif_disease[[pif_colname]]))
        
        new_col[is.na(new_col)] <- 0
        td1_age_sex[[paste('incidence', DISEASE_SHORT_NAMES$sname[d], sep = '_')]] <- new_col
        
        
        
        ## Instead of idata, feed td to run scenarios. Now all diseases are run again, with the effect of diabetes
        ## on cardiovarcular diseases taken into account. 
        
        disease_life_table_list_sc_temp <- RunDisease(in_idata = td1_age_sex, in_sex = isex,
                                                      in_mid_age = iage, in_disease = DISEASE_SHORT_NAMES$sname[d])
        
        
        
        disease_life_table_list_sc_temp$diff_inc_disease <-
          disease_life_table_list_sc_temp$incidence_disease -   disease_life_table_list_bl[[index]]$incidence_disease
        disease_life_table_list_sc_temp$diff_prev_disease <-
          disease_life_table_list_sc_temp$px  - disease_life_table_list_bl[[index]]$px
        disease_life_table_list_sc_temp$diff_mort_disease <-
          disease_life_table_list_sc_temp$mx - disease_life_table_list_bl[[index]]$mx
        disease_life_table_list_sc_temp$diff_pylds_disease <-
          (disease_life_table_list_sc_temp$px - disease_life_table_list_bl[[index]]$px) * disease_life_table_list_bl[[index]]$dw_disease
        
        disease_life_table_list_sc[[index]] <- disease_life_table_list_sc_temp
        names(disease_life_table_list_sc)[index] <- paste(iage, isex, DISEASE_SHORT_NAMES$sname[d], sep = '_')
        
        index <- index + 1
      }
    }
  }
}
## Uncomment to check scenario life tables
# View(disease_life_table_list_sc[[3]])

# ---- chunk-5 ----

## Generate total change in mortality rate to recalculate scenario general life tables

### Vector in common by all calculations for change in mx and pylds.

index <- 1
age_sex_cols <- which(colnames(disease_life_table_list_sc[[index]])%in%c('age', 'sex'))

### Sum mortality rate change scenarios (mx_sc_total)

#### Diseases
mx_sc_total_disease <- list()
l_index <- 1
index <- 1
for (iage in i_age_cohort){
  for (isex in i_sex){
    mortality_sum <- NULL
    
    create_new <- T
    
    ## Sum all diseases mortality rates
    
    for (d in 1:nrow(DISEASE_SHORT_NAMES)) {
      if (isex == 'male' && (DISEASE_SHORT_NAMES$disease[d] %in% c('breast cancer', 'uterine cancer'))
          || DISEASE_SHORT_NAMES$is_not_dis[d] != 0 || DISEASE_SHORT_NAMES$acronym[d] == 'no_pif' || DISEASE_SHORT_NAMES$acronym[d] == 'other'){
      }
      else {
        
        # print(paste(isex, DISEASE_SHORT_NAMES$disease[d]))
        
        if (create_new){
          mortality_sum <- disease_life_table_list_sc[[index]][,age_sex_cols]
          mortality_sum$total <- 0
          create_new <- F
          mortality_sum$total <- mortality_sum$total +
            (disease_life_table_list_sc[[index]]$diff_mort_disease)
        }else{
          mortality_sum$total <- mortality_sum$total +
            (disease_life_table_list_sc[[index]]$diff_mort_disease)
        }
        
        # cat(age, ' - ', sex,' - ',  disease,' - ',  index, ' - ', l_index,  '\n')
        index <- index + 1
      }
    }
    mx_sc_total_disease[[l_index]] <- mortality_sum 
    names(mx_sc_total_disease)[l_index] <- paste(iage, isex)
    
    l_index <- l_index + 1
    
  }
}

## YLDs change
### Diseases

pylds_sc_total_disease <- list()
l_index <- 1
index <- 1
for (iage in i_age_cohort){
  for (isex in i_sex){
    pylds_sum <- NULL
    create_new <- T
    
    for (d in 1:nrow(DISEASE_SHORT_NAMES)) {
      if (isex == 'male' && (DISEASE_SHORT_NAMES$disease[d] %in% c('breast cancer', 'uterine cancer'))
          || DISEASE_SHORT_NAMES$is_not_dis[d] != 0 || DISEASE_SHORT_NAMES$acronym[d] == 'no_pif' || DISEASE_SHORT_NAMES$acronym[d] == 'other'){
      }
      else {
        
        if (create_new){
          
          pylds_sum <- disease_life_table_list_sc[[index]][,age_sex_cols]
          pylds_sum$total <- 0
          create_new <- F
          pylds_sum$total <- pylds_sum$total +
            (disease_life_table_list_sc[[index]]$diff_pylds_disease)
        }else{
          pylds_sum$total <- pylds_sum$total +
            (disease_life_table_list_sc[[index]]$diff_pylds_disease)
        }
        
        # cat(age, ' - ', sex,' - ',  disease,' - ',  index, ' - ', l_index,  '\n')
        index <- index + 1
      }
      
    }
    pylds_sc_total_disease[[l_index]] <- pylds_sum
    names(pylds_sc_total_disease)[l_index] <- paste(iage, isex)
    l_index <- l_index + 1
  }
}


# ---- chunk-6 ----

## Calculate general life tables with modified mortality and pylds total
## Original mortality rate is modified by the mx_sc_total (total change in mortality from diseases)
## Original pyld rate is modified by the change in each disease pylds

general_life_table_list_sc <- list()
index <- 1

for (iage in i_age_cohort){
  for (isex in i_sex){
    
    
    # cat('age ', age, ' and sex ', sex, '\n')
    # modify idata's mortality and pyld total for the said scenario
    td2 <- MSLT_DF
    # td2 <- subset(td2, select = -c(mx, pyld_rate))
    td2[td2$age >= iage & td2$sex == isex,][[paste('mx')]] <- general_life_table_list_bl[[index]]$mx + 
      mx_sc_total_disease[[index]]$total
    
    td2[td2$age >= iage & td2$sex == isex,][[paste('pyld_rate')]] <- general_life_table_list_bl[[index]]$pyld_rate + 
      pylds_sc_total_disease[[index]]$total
    
    
    # Instead of idata, feed td to run scenarios
    general_life_table_list_sc[[index]] <- RunLifeTable(in_idata = td2, in_sex = isex, in_mid_age = iage)
    #
    names(general_life_table_list_sc)[index] <- paste(iage, isex, sep = '_')
    
    
    index <- index + 1
  }
}

# ---- chunk-7 ----

## In the following list 'output_life_table', 34 data frames are nested per age and sex cohort

## Outputs are generated following the index order of disease life tables baseline and scenarios where diabates is first calculated as it impacts on cardivascular diseases. 

## In the following list 'output_life_table', 34 data frames are nested per age and sex cohort

## Outputs are generated following the index order of disease life tables baseline and scenarios where diabates is first calculated as it impacts on cardivascular diseases. 

dia_index <- which(DISEASE_SHORT_NAMES$sname=='dmt2')
dia_order <- c(dia_index,c(1:nrow(DISEASE_SHORT_NAMES))[-dia_index])


output_burden <- list()
l_index <- 1
index <- 1

sc_cols <- which(colnames(disease_life_table_list_sc[[index]])%in%c('age', 'sex', 'incidence_disease', 'mx', 'px'))
bl_cols <- which(colnames(disease_life_table_list_bl[[index]])%in%c('incidence_disease', 'mx', 'px'))
l_sc_cols <- which(colnames(general_life_table_list_sc[[l_index]])%in%c('Lx', 'Lwx', 'ex', 'ewx'))
l_bl_cols <- which(colnames(general_life_table_list_bl[[l_index]])%in%c('Lx', 'Lwx', 'ex', 'ewx'))

for (iage in i_age_cohort){
  for (isex in i_sex){
    
    
    # We create a TRUE/FALSE variable for the loop to move into the next disease
    
    create_new <- T
    for (d in c(1:nrow(DISEASE_SHORT_NAMES))[dia_order]){
      if (isex == 'male' && (DISEASE_SHORT_NAMES$disease[d] %in% c('breast cancer', 'uterine cancer'))
          || DISEASE_SHORT_NAMES$is_not_dis[d] != 0 || DISEASE_SHORT_NAMES$acronym[d] == 'no_pif' || DISEASE_SHORT_NAMES$acronym[d] == 'other'){
      }
      else {
        
        if (create_new){
          
          output_burden_sc <- output_burden_sc <- disease_life_table_list_sc[[index]][,sc_cols]
          
          names(output_burden_sc)[names(output_burden_sc) == 'incidence_disease'] <-
            paste('incidence_disease', DISEASE_SHORT_NAMES$sname[d], 'sc', sep = '_')
          names(output_burden_sc)[names(output_burden_sc) == 'mx'] <-
            paste('mx', DISEASE_SHORT_NAMES$sname[d], 'sc', sep = '_')
          names(output_burden_sc)[names(output_burden_sc) == 'px'] <-
            paste('px', DISEASE_SHORT_NAMES$sname[d], 'sc', sep = '_')
          
          output_burden_bl <- disease_life_table_list_bl[[index]][,bl_cols]
          
          names(output_burden_bl)[names(output_burden_bl) == 'incidence_disease'] <-
            paste('incidence_disease', DISEASE_SHORT_NAMES$sname[d], 'bl', sep = '_')
          names(output_burden_bl)[names(output_burden_bl) == 'mx'] <-
            paste('mx', DISEASE_SHORT_NAMES$sname[d], 'bl', sep = '_')
          names(output_burden_bl)[names(output_burden_bl) == 'px'] <-
            paste('px', DISEASE_SHORT_NAMES$sname[d], 'bl', sep = '_')
          
          ## New list to add calculations for changes in burden of disease (incidence and mortality numbers)
          
          output_burden_change <- list()
          
          output_burden_change$inc_num_bl <- disease_life_table_list_bl[[index]]$incidence_disease *
            (1 - disease_life_table_list_bl[[index]]$px) * general_life_table_list_bl[[l_index]]$Lx
          output_burden_change$inc_num_sc <- disease_life_table_list_sc[[index]]$incidence_disease *
            (1 - disease_life_table_list_sc[[index]]$px) * general_life_table_list_sc[[l_index]]$Lx
          output_burden_change$inc_num_diff <- output_burden_change$inc_num_sc - output_burden_change$inc_num_bl
          
          output_burden_change$mx_num_bl <- disease_life_table_list_bl[[index]]$mx * general_life_table_list_bl[[l_index]]$Lx
          output_burden_change$mx_num_sc <- disease_life_table_list_sc[[index]]$mx * general_life_table_list_sc[[l_index]]$Lx
          output_burden_change$mx_num_diff <- output_burden_change$mx_num_sc - output_burden_change$mx_num_bl
          
          names(output_burden_change)[names(output_burden_change) == 'inc_num_bl'] <-
            paste('inc_num_bl', DISEASE_SHORT_NAMES$sname[d], sep = '_')
          names(output_burden_change)[names(output_burden_change) == 'inc_num_sc'] <-
            paste('inc_num_sc', DISEASE_SHORT_NAMES$sname[d], sep = '_')
          names(output_burden_change)[names(output_burden_change) == 'inc_num_diff'] <-
            paste('inc_num_diff', DISEASE_SHORT_NAMES$sname[d], sep = '_')
          names(output_burden_change)[names(output_burden_change) == 'mx_num_bl'] <-
            paste('mx_num_bl', DISEASE_SHORT_NAMES$sname[d], sep = '_')
          names(output_burden_change)[names(output_burden_change) == 'mx_num_sc'] <-
            paste('mx_num_sc', DISEASE_SHORT_NAMES$sname[d], sep = '_')
          names(output_burden_change)[names(output_burden_change) == 'mx_num_diff'] <-
            paste('mx_num_diff', DISEASE_SHORT_NAMES$sname[d], sep = '_')
          
          ## Bind all lists
          
          output_burden_sc <- cbind(output_burden_sc, output_burden_bl)
          output_burden_sc <- cbind(output_burden_sc, output_burden_change)
          
          create_new <- F
          
          ## Here the calculations above are repeated, here is where the F is telling to move into the next disease
          
        }else{
          
          td3 <- disease_life_table_list_sc[[index]][,colnames(disease_life_table_list_sc[[index]])%in%c('incidence_disease', 'mx', 'px')]
          
          names(td3)[names(td3) == 'incidence_disease'] <-
            paste('incidence_disease', DISEASE_SHORT_NAMES$sname[d], 'sc', sep = '_')
          names(td3)[names(td3) == 'mx'] <-
            paste('mx', DISEASE_SHORT_NAMES$sname[d], 'sc', sep = '_')
          names(td3)[names(td3) == 'px'] <-
            paste('px', DISEASE_SHORT_NAMES$sname[d], 'sc', sep = '_')
          
          td4 <- disease_life_table_list_bl[[index]][,colnames(disease_life_table_list_bl[[index]])%in%c('incidence_disease', 'mx', 'px')]
          
          names(td4)[names(td4) == 'incidence_disease'] <-
            paste('incidence_disease', DISEASE_SHORT_NAMES$sname[d], 'bl', sep = '_')
          names(td4)[names(td4) == 'mx'] <-
            paste('mx', DISEASE_SHORT_NAMES$sname[d], 'bl', sep = '_')
          names(td4)[names(td4) == 'px'] <-
            paste('px', DISEASE_SHORT_NAMES$sname[d], 'bl', sep = '_')
          
          output_burden_change2 <- list()
          
          output_burden_change2$inc_num_bl <- disease_life_table_list_bl[[index]]$incidence_disease * (1 - disease_life_table_list_bl[[index]]$px) * 
            general_life_table_list_bl[[l_index]]$Lx
          output_burden_change2$inc_num_sc <- disease_life_table_list_sc[[index]]$incidence_disease * (1 - disease_life_table_list_sc[[index]]$px) * 
            general_life_table_list_sc[[l_index]]$Lx
          output_burden_change2$inc_num_diff <- output_burden_change2$inc_num_sc - output_burden_change2$inc_num_bl
          
          output_burden_change2$mx_num_bl <- disease_life_table_list_bl[[index]]$mx * general_life_table_list_bl[[l_index]]$Lx
          output_burden_change2$mx_num_sc <- disease_life_table_list_sc[[index]]$mx * general_life_table_list_sc[[l_index]]$Lx
          output_burden_change2$mx_num_diff <- output_burden_change2$mx_num_sc - output_burden_change2$mx_num_bl
          
          names(output_burden_change2)[names(output_burden_change2) == 'inc_num_bl'] <-
            paste('inc_num_bl', DISEASE_SHORT_NAMES$sname[d], sep = '_')
          names(output_burden_change2)[names(output_burden_change2) == 'inc_num_sc'] <-
            paste('inc_num_sc', DISEASE_SHORT_NAMES$sname[d], sep = '_')
          names(output_burden_change2)[names(output_burden_change2) == 'inc_num_diff'] <-
            paste('inc_num_diff', DISEASE_SHORT_NAMES$sname[d], sep = '_')
          names(output_burden_change2)[names(output_burden_change2) == 'mx_num_bl'] <-
            paste('mx_num_bl', DISEASE_SHORT_NAMES$sname[d], sep = '_')
          names(output_burden_change2)[names(output_burden_change2) == 'mx_num_sc'] <-
            paste('mx_num_sc', DISEASE_SHORT_NAMES$sname[d], sep = '_')
          names(output_burden_change2)[names(output_burden_change2) == 'mx_num_diff'] <-
            paste('mx_num_diff', DISEASE_SHORT_NAMES$sname[d], sep = '_')
          
          
          ## Bind all lists
          
          output_burden_sc <- cbind(output_burden_sc, td3)
          output_burden_sc <- cbind(output_burden_sc, td4)
          output_burden_sc$age_cohort <- iage
          output_burden_sc <- cbind(output_burden_sc, output_burden_change2)
          
        }
        
        # cat(iage, ' - ', isex,' - ',  disease,' - ',  index, ' - ', l_index,  '\n')
        index <- index + 1
      }
    }
    
    
    ## general_life_table_list_sc and general_life_table_list_bl (Lx)
    
    output_burden_lf_sc <- general_life_table_list_sc[[l_index]][,l_sc_cols]
    
    names(output_burden_lf_sc)[names(output_burden_lf_sc) == 'Lx'] <- paste('Lx', 'sc', sep = '_')
    names(output_burden_lf_sc)[names(output_burden_lf_sc) == 'Lwx'] <- paste('Lwx', 'sc', sep = '_')
    names(output_burden_lf_sc)[names(output_burden_lf_sc) == 'ex'] <- paste('ex', 'sc', sep = '_')
    names(output_burden_lf_sc)[names(output_burden_lf_sc) == 'ewx'] <- paste('ewx', 'sc', sep = '_')
    
    output_burden_lf_bl <- general_life_table_list_bl[[l_index]][,l_bl_cols]
    
    names(output_burden_lf_bl)[names(output_burden_lf_bl) == 'Lx'] <- paste('Lx', 'bl', sep = '_')
    names(output_burden_lf_bl)[names(output_burden_lf_bl) == 'Lwx'] <- paste('Lwx', 'bl', sep = '_')
    names(output_burden_lf_bl)[names(output_burden_lf_bl) == 'ex'] <- paste('ex', 'bl', sep = '_')
    names(output_burden_lf_bl)[names(output_burden_lf_bl) == 'ewx'] <- paste('ewx', 'bl', sep = '_')
    
    ### Difference in life years and health adjusted life years for all evey cohort year
    output_burden_lf_sc$Lx_diff <- general_life_table_list_sc[[l_index]]$Lx - general_life_table_list_bl[[l_index]]$Lx
    output_burden_lf_sc$Lwx_diff <- general_life_table_list_sc[[l_index]]$Lwx - general_life_table_list_bl[[l_index]]$Lwx
    
    ### Difference in life expectancy and health adjusted life expectancy for first year of cohort (show in results)
    output_burden_lf_sc$ex_diff <- general_life_table_list_sc[[l_index]]$ex - general_life_table_list_bl[[l_index]]$ex
    output_burden_lf_sc$ewx_diff <- general_life_table_list_sc[[l_index]]$ewx - general_life_table_list_bl[[l_index]]$ewx
    
    
    output_burden_sc <- cbind(output_burden_sc, output_burden_lf_sc)
    output_burden_sc <- cbind(output_burden_sc, output_burden_lf_bl)
    
    
    output_burden[[l_index]] <- output_burden_sc
    
    names(output_burden)[l_index] <- paste(iage, isex, sep = '_')
    
    
    l_index <- l_index + 1
    
  }
}  

# ---- chunk-11 ---- 

## Generate a data frame for all results

output_df <- plyr::ldply(output_burden, rbind)

output_dir = 'output/'

#### Add within each outcome subfolders for each city region

# ---- chunk-11.1 Tables ----

### Create age groups variable, easier to read

output_df <-  output_df  %>%
  separate(.id, c("Age group", "Gender"), "_", remove = FALSE) 
output_df$`Age group` <- as.numeric(output_df$`Age group`)

output_df$`Age group`[output_df$`Age group` == 17] <-"16-19"
output_df$`Age group`[output_df$`Age group` == 22] <-"20-24"
output_df$`Age group`[output_df$`Age group` == 27] <-"25-29"
output_df$`Age group`[output_df$`Age group` == 32] <-"30-34"
output_df$`Age group`[output_df$`Age group` == 37] <-"35-39"
output_df$`Age group`[output_df$`Age group` == 42] <-"40-44"
output_df$`Age group`[output_df$`Age group` == 47] <-"45-49"
output_df$`Age group`[output_df$`Age group` == 52] <-"50-54"
output_df$`Age group`[output_df$`Age group` == 57] <-"55-59"
output_df$`Age group`[output_df$`Age group` == 62] <-"60-64"
output_df$`Age group`[output_df$`Age group` == 67] <-"65-69"
output_df$`Age group`[output_df$`Age group` == 72] <-"70-74"
output_df$`Age group`[output_df$`Age group` == 77] <-"75-79"
output_df$`Age group`[output_df$`Age group` == 82] <-"80-84"
output_df$`Age group`[output_df$`Age group` == 87] <-"85-89"
output_df$`Age group`[output_df$`Age group` == 92] <-"90-94"
output_df$`Age group`[output_df$`Age group` == 97] <-"95 plus"

# ---- chunk-11.1.1 Life expectancy and health adjusted life expectancy ----
#### Add uncertainty intervals
### Life expectancy is for year one of simulation. For example, change in life expectancy for a female aged 16-19.

output_life_expectancy_change <- output_df[!duplicated(output_df$.id), c("Age group", "Gender", "ex_bl", "ex_sc", "ewx_bl", "ewx_sc", 
                                                                         "ex_diff", "ewx_diff")] %>%
  dplyr::rename(`Life expectancy at baseline` = ex_bl, 
                `Life expectancy scenario` = ex_sc, 
                `Health adjusted life expectancy baseline` = ewx_bl, 
                `Health adjusted life expectancy scenario` = ewx_sc, 
                `Difference in life expectancy` = ex_diff, 
                `Difference in health adjusted life expectancy` = ewx_diff) %>% 
  mutate_if(is.numeric, round, digits = 3)


output_life_expectancy_change <- output_life_expectancy_change[order(output_life_expectancy_change$Gender),]



# ---- chunk-11.1.2 Life years and health adjusted life years ----
#### Add uncertainty intervals
#### Accumulated over the life of the cohort. For example, total life years change for females 16-19 over their life course

output_life_years_change <- output_df %>% 
  group_by(Gender, `Age group`) %>%
  summarise_if(is.numeric, funs(sum)) %>%
  dplyr::select(`Age group`, Gender, Lx_diff, Lwx_diff) %>%
  dplyr::rename(`Life years` = Lx_diff, 
                `Health adjusted life years` = Lwx_diff)  %>% 
  mutate_if(is.numeric, round)




# ---- chunk-11.1.2 Diseases deaths, incidence and ylds (lri) ----
#### Add uncertainty intervals
#### Accumulated over the life of the cohort. For example, total life years change for females 16-19 over their life course

### Vector with diseases (may be a better way for this)

### hard coded matches, best not to
output_diseases_change <- output_df %>% 
  group_by(Gender, `Age group`) %>%
  summarise_if(is.numeric, funs(sum)) %>%
  dplyr::select(`Age group`, Gender, matches("diff_dmt2|diff_ishd|diff_strk|diff_carc|diff_copd|diff_tbalc|diff_brsc|diff_utrc|diff_lri"))





# # ---- chunk-11.2 Graphs ---- CHECK GRAPHS AND WHAT WE WANT
# # ---- chunk-11.2.1 Graphs cohorts age and sex ----
# 
# ### DISEASE DEATHS AND INCIDENCE NUMBERS: graphs by age and sex cohort, over the life course of cohort.  
# 
# #### Define variables names
# bl <- 'num_bl'
# sc <- 'num_sc'
# diff <- 'num_diff'
# i_outcome_d <- c('mx', 'inc')
# 
# 
# for (iage in i_age_cohort){
#   for (isex in i_sex) {
#     for (ioutcome in i_outcome_d) {
#       for (d in 1:nrow(DISEASE_SHORT_NAMES)) {
#         
#         
#         if (isex == 'male' && (DISEASE_SHORT_NAMES$disease[d] %in% c('breast cancer', 'uterine cancer'))
#             || DISEASE_SHORT_NAMES$acronym[d] == 'no_pif' || DISEASE_SHORT_NAMES$acronym[d] == 'other' || DISEASE_SHORT_NAMES$is_not_dis[d] !=0){
#         }
#         else{
#           
#           pdf(paste0(output_dir, 'graphs/cohorts/', DISEASE_SHORT_NAMES$sname[d],'_', isex, '_', iage, '_', ioutcome, '.pdf'),width=5.5,height=4)
#           
#           p_index  <- PlotOutput_compiled(in_data = output_df, in_age = iage, in_population = isex, in_outcomes = c('age', paste(ioutcome, bl, DISEASE_SHORT_NAMES$sname[d], sep = '_'), paste(ioutcome, sc, DISEASE_SHORT_NAMES$sname[d], sep = '_'), paste(ioutcome, diff, DISEASE_SHORT_NAMES$sname[d], sep = '_')), in_legend = ifelse(ioutcome == 'inc', 'Incidence', 'Deaths'), in_disease = DISEASE_SHORT_NAMES$disease[d])
#           
#           
#           ### Pdf is smaller and faster to save than jpeg
#           # ggsave_compiled(p_index, file=paste0(output_dir, DISEASE_SHORT_NAMES$sname[d],'_', isex, '_', iage, '_', ioutcome, '.jpeg'), width = 14, height = 10, units = 'cm')
#           print(p_index)
#           dev.off()
#           
#         }
#       }
#     }
#   }
# }
# 
# 
# ### NON-DISEASE DEATHS AND YLDS NUMBERS: graphs by age and sex cohort, over the life course of cohort.  
# 
# #### Define variables names
# bl <- 'num_bl'
# sc <- 'num_sc'
# diff <- 'num_diff'
# i_outcome_nd <- c('mx', 'ylds')
# 
# 
# for (iage in i_age_cohort){
#   for (isex in i_sex) {
#     for (ioutcome in i_outcome_nd) {
#       for (d in 1:nrow(DISEASE_SHORT_NAMES)) {
#         
#        
#         ## Exclude chronic disease and all-cause mortality and  pyld
#         if (DISEASE_SHORT_NAMES$is_not_dis[d] != 1 || DISEASE_SHORT_NAMES$acronym[d] == 'other' || DISEASE_SHORT_NAMES$acronym[d] == 'no_pif'){
#         }
#         else {
#          
#           pdf(paste0(output_dir, 'graphs/cohorts/', DISEASE_SHORT_NAMES$acronym[d],'_', isex, '_', iage, '_', ioutcome, '.pdf'),width=5.5,height=4)
#            
#           p_index  <- PlotOutput(in_data = output_df, in_age = iage, in_population = isex, in_outcomes = c('age', paste(ioutcome, bl, DISEASE_SHORT_NAMES$acronym[d], sep = '_'), paste(ioutcome, sc, DISEASE_SHORT_NAMES$acronym[d], sep = '_'), paste(ioutcome, diff, DISEASE_SHORT_NAMES$acronym[d], sep = '_')), in_legend = ifelse(ioutcome == 'ylds', 'YLDs', 'Deaths'), in_disease = DISEASE_SHORT_NAMES$acronym[d])
#           
#           # ggsave(p_index, file=paste0(output_dir, DISEASE_SHORT_NAMES$acronym[d],'_', isex, '_', iage, '_', ioutcome, '.jpeg'), width = 14, height = 10, units = 'cm')
#           
#           print(p_index)
#           dev.off()
#           
#         }
#       }
#     }
#   }
# }
# 
# 
# # ---- chunk-11.2.1 Graphs aggreagated cohorts ----
# 
# ### First create aggregated data frames
# 
# i_outcome_d <- c('mx', 'inc')
# i_outcome_nd <- c('mx', 'ylds')
# 
# aggregate_frame_d_males <- list()
# aggregate_frame_d_females <- list()
# 
# index <- 1
# 
# for (ioutcome in i_outcome_d) {
#   for (d in 1:nrow(DISEASE_SHORT_NAMES)) {
#     if (DISEASE_SHORT_NAMES$acronym[d] == 'no_pif' || DISEASE_SHORT_NAMES$acronym[d] == 'other' || DISEASE_SHORT_NAMES$is_not_dis[d] !=0){
#     }
#     else{
#       
#       aggregate_frame_d_males[[index]] <- GenAggregate(in_data = output_df, in_cohorts = 17, 
#                                                        in_population = 'male', in_outcomes = c(paste(ioutcome, 'num', 'bl', 
#                                                                                                      DISEASE_SHORT_NAMES$sname[d], sep = '_'), paste(ioutcome, 'num', 'sc', 
#                                                                                                                                                      DISEASE_SHORT_NAMES$sname[d], sep = '_'), paste(ioutcome, 'num', 'diff', 
#                                                                                                                                                                                                      DISEASE_SHORT_NAMES$sname[d], sep = '_')))
#       
#       aggregate_frame_d_females[[index]] <- GenAggregate(in_data = output_df, in_cohorts = 17, in_population = 'female', 
#                                                          in_outcomes = c(paste(ioutcome, 'num', 'bl', DISEASE_SHORT_NAMES$sname[d], sep = '_'), 
#                                                                          paste(ioutcome, 'num', 'sc', DISEASE_SHORT_NAMES$sname[d], sep = '_'), 
#                                                                          paste(ioutcome, 'num', 'diff', DISEASE_SHORT_NAMES$sname[d], sep = '_')))
#       
#       # Keep totals only
#       aggregate_frame_d_males[[index]] <- aggregate_frame_d_males[[index]] %>% dplyr::select(contains('total'))
#       
#       aggregate_frame_d_females[[index]] <- aggregate_frame_d_females[[index]] %>% dplyr::select(contains('total'))
#       
#       index <- index + 1
#     }
#   }
# }
# 
# 
# #### Non-Diseases: ylds and deaths
# 
# aggregate_frame_nd_males <- list()
# aggregate_frame_nd_females <- list()
# 
# index <- 1
# 
# for (ioutcome in i_outcome_nd) {
#   for (d in 1:nrow(DISEASE_SHORT_NAMES)) {
#     ## Exclude chronic disease and all-cause mortality and  pyld
#     if (DISEASE_SHORT_NAMES$is_not_dis[d] != 1 || DISEASE_SHORT_NAMES$acronym[d] == 'other' || DISEASE_SHORT_NAMES$acronym[d] == 'no_pif'){
#     }
#     else {
#       
#       aggregate_frame_nd_males[[index]] <- GenAggregate(in_data = output_df, in_cohorts = 17, in_population = 'male', 
#                                                         in_outcomes = c(paste(ioutcome, 'num', 'bl', DISEASE_SHORT_NAMES$acronym[d], sep = '_'), 
#                                                                         paste(ioutcome, 'num', 'sc', DISEASE_SHORT_NAMES$acronym[d], sep = '_'), 
#                                                                         paste(ioutcome, 'num', 'diff', DISEASE_SHORT_NAMES$acronym[d], sep = '_')))
#       
#       aggregate_frame_nd_females[[index]] <- GenAggregate(in_data = output_df, in_cohorts = 17, in_population = 'female',
#                                                           in_outcomes = c(paste(ioutcome, 'num', 'bl', DISEASE_SHORT_NAMES$acronym[d], sep = '_'), 
#                                                                           paste(ioutcome, 'num', 'sc', DISEASE_SHORT_NAMES$acronym[d], sep = '_'), 
#                                                                           paste(ioutcome, 'num', 'diff', DISEASE_SHORT_NAMES$acronym[d], sep = '_')))
#       
#       # Remove non-numeric columns starting with age and sex
#       aggregate_frame_nd_males[[index]] <- aggregate_frame_nd_males[[index]] %>% dplyr::select(contains('total'))
#       
#       aggregate_frame_nd_females[[index]] <- aggregate_frame_nd_females[[index]] %>% dplyr::select(contains('total'))
#       
#       index <- index + 1
#     }
#   }
# }
# 
# 
# #### Life years and health adjusted life years
# 
# i_outcome_lys <- c('Lx', 'Lwx')
# 
# aggregate_frame_males_lys <- list()
# aggregate_frame_females_lys <- list()
# 
# for (i in i_outcome_lys){
#   
#   aggregate_frame_males_lys[[i]] <- GenAggregate(in_data = output_df, in_cohorts = 17, in_population = 'male', 
#                                                  in_outcomes = c(paste(i, 'bl', sep = '_'), paste(i, 'sc', sep = '_'), 
#                                                                  paste(i, 'diff',sep = '_')))
#   
#   aggregate_frame_females_lys[[i]] <- GenAggregate(in_data = output_df, in_cohorts = 17, in_population = 'female', 
#                                                    in_outcomes = c(paste(i, 'bl', sep = '_'), 
#                                                                    paste(i,  'sc', sep = '_'), paste(i, 'diff',sep = '_')))
#   
#   
#   aggregate_frame_males_lys[[i]] <- aggregate_frame_males_lys[[i]] %>% dplyr::select(contains('total'))
#   
#   aggregate_frame_females_lys[[i]] <- aggregate_frame_females_lys[[i]] %>% dplyr::select(contains('total'))
#   
# }
# 
# ## Transform lists for diseases, non-diseases and life years to data frames including all aggregated outcomes by sex
# 
# ### Females
# 
# aggregate_females_df <- do.call(cbind, c(aggregate_frame_d_females, aggregate_frame_nd_females, aggregate_frame_females_lys)) %>%
#   mutate(simulation_yr = c(1:84), sex = 'female')
# 
# ## Drop string added to Lx and Lwx column names (not sure why, but happens in this step)
# 
# names(aggregate_females_df) <-  gsub("Lx.total_Lx", "total_Lx", names(aggregate_females_df))
# names(aggregate_females_df) <-  gsub("Lwx.total_Lwx", "total_Lwx", names(aggregate_females_df))
# 
# ### Males
# 
# aggregate_males_df <- do.call(cbind, c(aggregate_frame_d_males, aggregate_frame_nd_males, aggregate_frame_males_lys)) %>% 
#   mutate(simulation_yr = c(1:84), sex = 'male') 
# 
# ## Drop string added to Lx and Lwx column names (not sure why, but happens in this step)
# 
# names(aggregate_males_df) <-  gsub("Lx.total_Lx", "total_Lx", names(aggregate_males_df))
# names(aggregate_males_df) <-  gsub("Lwx.total_Lwx", "total_Lwx", names(aggregate_males_df))
# 
# 
# #### Aggregated graphs
# ### Females-diseases
# 
# 
# p_aggr_females_d_list <- list()
# index <- 1
# 
# for (ioutcome in i_outcome_d) {
#   for (d in 1:nrow(DISEASE_SHORT_NAMES)) {
#     
#     if (isex == 'male' && (DISEASE_SHORT_NAMES$disease[d] %in% c('breast cancer', 'uterine cancer'))
#         || DISEASE_SHORT_NAMES$acronym[d] == 'no_pif' || DISEASE_SHORT_NAMES$acronym[d] == 'other' || DISEASE_SHORT_NAMES$is_not_dis[d] !=0){
#     }
#     else{
#     
#       pdf(paste0(output_dir, 'graphs/aggregated/', 'total_', DISEASE_SHORT_NAMES$sname[d],'_', 'female', '_', ioutcome, '.pdf'),width=5.5,height=4)
#       
#       p_aggr_females_d_list <- ggplot(aggregate_females_df[1:84,], aes(x = aggregate_females_df[['simulation_yr']])) +
#       
#       geom_line(mapping = aes(y = aggregate_females_df[[paste('total', ioutcome, 'num_bl', DISEASE_SHORT_NAMES$sname[d], sep = '_')]], colour = paste('total', ioutcome, 'num_bl', DISEASE_SHORT_NAMES$sname[d], sep = '_'))) +
#       theme_classic() +
#       geom_hline(yintercept=0, linetype='dashed', color = 'black') +
#       geom_line(mapping = aes(y = aggregate_females_df[[paste('total', ioutcome, 'num_sc', DISEASE_SHORT_NAMES$sname[d], sep = '_')]], colour = paste('total', ioutcome, 'num_sc', DISEASE_SHORT_NAMES$sname[d], sep = '_'))) +
#       geom_line(mapping = aes(y = aggregate_females_df[[paste('total', ioutcome, 'num_diff', DISEASE_SHORT_NAMES$sname[d], sep = '_')]], colour = paste('total', ioutcome, 'num_diff', DISEASE_SHORT_NAMES$sname[d], sep = '_'))) +
#       xlab ('Simulation years') + ylab ('Cases') + labs (title = paste(DISEASE_SHORT_NAMES$disease[d], ifelse(ioutcome == 'inc', 'incidence', 'deaths'))) +
#       theme(plot.title = element_text(hjust = 0.5, size = 12)) +
#       scale_color_discrete(name = paste(''), labels = c('Baseline', 'Difference', 'Scenario')) +
#       theme(plot.title = element_text(hjust = 0.5))
#       print(p_aggr_females_d_list)
#       dev.off()
#     
#     
#     
#     
#     
#     # p_aggr_females_list <- p_aggr_list_index
#     # index <- index + 1
#     # 
#     }
#   }
# }
# 
# 
# ### Females-non_diseases
# 
# p_aggr_females_nd_list <- list()
# index <- 1
# 
# for (ioutcome in i_outcome_nd) {
#   for (d in 1:nrow(DISEASE_SHORT_NAMES)) {
#     
#     if (DISEASE_SHORT_NAMES$is_not_dis[d] != 1 || DISEASE_SHORT_NAMES$acronym[d] == 'other' || DISEASE_SHORT_NAMES$acronym[d] == 'no_pif'){
#     }
#     else {
#       
#       pdf(paste0(output_dir, 'graphs/aggregated/', 'total_', DISEASE_SHORT_NAMES$acronym[d],'_', 'female', '_', ioutcome, '.pdf'),width=5.5,height=4)
#       
#       p_aggr_females_nd_list <- ggplot(aggregate_females_df[1:84,], aes(x = aggregate_females_df[['simulation_yr']])) +
#         
#         geom_line(mapping = aes(y = aggregate_females_df[[paste('total', ioutcome, 'num_bl', DISEASE_SHORT_NAMES$acronym[d], sep = '_')]], colour = paste('total', ioutcome, 'num_bl', DISEASE_SHORT_NAMES$acronym[d], sep = '_'))) +
#         theme_classic() +
#         geom_hline(yintercept=0, linetype='dashed', color = 'black') +
#         geom_line(mapping = aes(y = aggregate_females_df[[paste('total', ioutcome, 'num_sc', DISEASE_SHORT_NAMES$acronym[d], sep = '_')]], colour = paste('total', ioutcome, 'num_sc', DISEASE_SHORT_NAMES$acronym[d], sep = '_'))) +
#         geom_line(mapping = aes(y = aggregate_females_df[[paste('total', ioutcome, 'num_diff', DISEASE_SHORT_NAMES$acronym[d], sep = '_')]], colour = paste('total', ioutcome, 'num_diff', DISEASE_SHORT_NAMES$acronym[d], sep = '_'))) +
#         xlab ('Simulation years') + ylab ('Cases') + labs (title = paste(DISEASE_SHORT_NAMES$disease[d], ifelse(ioutcome == 'inc', 'incidence', 'deaths'))) +
#         theme(plot.title = element_text(hjust = 0.5, size = 12)) +
#         scale_color_discrete(name = paste(''), labels = c('Baseline', 'Difference', 'Scenario')) +
#         theme(plot.title = element_text(hjust = 0.5))
#       print(p_aggr_females_nd_list)
#       dev.off()
#       
#       
#       
#       
#       
#       # p_aggr_females_list <- p_aggr_list_index
#       # index <- index + 1
#       # 
#     }
#   }
# }
# 
# ### Males-diseases
# 
# 
# p_aggr_males_d_list <- list()
# index <- 1
# 
# for (ioutcome in i_outcome_d) {
#   for (d in 1:nrow(DISEASE_SHORT_NAMES)) {
#     
#     if (isex == 'male' && (DISEASE_SHORT_NAMES$disease[d] %in% c('breast cancer', 'uterine cancer'))
#         || DISEASE_SHORT_NAMES$acronym[d] == 'no_pif' || DISEASE_SHORT_NAMES$acronym[d] == 'other' || DISEASE_SHORT_NAMES$is_not_dis[d] !=0){
#     }
#     else{
#       
#       pdf(paste0(output_dir, 'graphs/aggregated/', 'total_', DISEASE_SHORT_NAMES$sname[d],'_', 'female', '_', ioutcome, '.pdf'),width=5.5,height=4)
#       
#       p_aggr_males_d_list <- ggplot(aggregate_males_df[1:84,], aes(x = aggregate_males_df[['simulation_yr']])) +
#         
#         geom_line(mapping = aes(y = aggregate_males_df[[paste('total', ioutcome, 'num_bl', DISEASE_SHORT_NAMES$sname[d], sep = '_')]], colour = paste('total', ioutcome, 'num_bl', DISEASE_SHORT_NAMES$sname[d], sep = '_'))) +
#         theme_classic() +
#         geom_hline(yintercept=0, linetype='dashed', color = 'black') +
#         geom_line(mapping = aes(y = aggregate_males_df[[paste('total', ioutcome, 'num_sc', DISEASE_SHORT_NAMES$sname[d], sep = '_')]], colour = paste('total', ioutcome, 'num_sc', DISEASE_SHORT_NAMES$sname[d], sep = '_'))) +
#         geom_line(mapping = aes(y = aggregate_males_df[[paste('total', ioutcome, 'num_diff', DISEASE_SHORT_NAMES$sname[d], sep = '_')]], colour = paste('total', ioutcome, 'num_diff', DISEASE_SHORT_NAMES$sname[d], sep = '_'))) +
#         xlab ('Simulation years') + ylab ('Cases') + labs (title = paste(DISEASE_SHORT_NAMES$disease[d], ifelse(ioutcome == 'inc', 'incidence', 'deaths'))) +
#         theme(plot.title = element_text(hjust = 0.5, size = 12)) +
#         scale_color_discrete(name = paste(''), labels = c('Baseline', 'Difference', 'Scenario')) +
#         theme(plot.title = element_text(hjust = 0.5))
#       print(p_aggr_males_d_list)
#       dev.off()
#       
#       
#       
#       
#       
#       # p_aggr_males_list <- p_aggr_list_index
#       # index <- index + 1
#       # 
#     }
#   }
# }
# 
# 
# ### Males-non_diseases
# 
# p_aggr_males_nd_list <- list()
# index <- 1
# 
# for (ioutcome in i_outcome_nd) {
#   for (d in 1:nrow(DISEASE_SHORT_NAMES)) {
#     
#     if (DISEASE_SHORT_NAMES$is_not_dis[d] != 1 || DISEASE_SHORT_NAMES$acronym[d] == 'other' || DISEASE_SHORT_NAMES$acronym[d] == 'no_pif'){
#     }
#     else {
#       
#       pdf(paste0(output_dir, 'graphs/aggregated/', 'total_', DISEASE_SHORT_NAMES$acronym[d],'_', 'female', '_', ioutcome, '.pdf'),width=5.5,height=4)
#       
#       p_aggr_males_nd_list <- ggplot(aggregate_males_df[1:84,], aes(x = aggregate_males_df[['simulation_yr']])) +
#         
#         geom_line(mapping = aes(y = aggregate_males_df[[paste('total', ioutcome, 'num_bl', DISEASE_SHORT_NAMES$acronym[d], sep = '_')]], colour = paste('total', ioutcome, 'num_bl', DISEASE_SHORT_NAMES$acronym[d], sep = '_'))) +
#         theme_classic() +
#         geom_hline(yintercept=0, linetype='dashed', color = 'black') +
#         geom_line(mapping = aes(y = aggregate_males_df[[paste('total', ioutcome, 'num_sc', DISEASE_SHORT_NAMES$acronym[d], sep = '_')]], colour = paste('total', ioutcome, 'num_sc', DISEASE_SHORT_NAMES$acronym[d], sep = '_'))) +
#         geom_line(mapping = aes(y = aggregate_males_df[[paste('total', ioutcome, 'num_diff', DISEASE_SHORT_NAMES$acronym[d], sep = '_')]], colour = paste('total', ioutcome, 'num_diff', DISEASE_SHORT_NAMES$acronym[d], sep = '_'))) +
#         xlab ('Simulation years') + ylab ('Cases') + labs (title = paste(DISEASE_SHORT_NAMES$disease[d], ifelse(ioutcome == 'inc', 'incidence', 'deaths'))) +
#         theme(plot.title = element_text(hjust = 0.5, size = 12)) +
#         scale_color_discrete(name = paste(''), labels = c('Baseline', 'Difference', 'Scenario')) +
#         theme(plot.title = element_text(hjust = 0.5))
#       print(p_aggr_males_nd_list)
#       dev.off()
#       
#       
#       # p_aggr_males_list <- p_aggr_list_index
#       # index <- index + 1
#       # 
#     }
#   }
# }
# 
# 
# 
# # ---- chunk-11.2.3 Graphs aggreagated cohorts selection simulaton years----
# 
# 
# # ---- chunk-16 ----
# 
# ### aDJUST THIS CODE TO FO FEMALES, MALES, DISEASE, NOS DISEASES AND UPDATE LIST OF OUTCOMES. 
# ####This plot has to be customised to in_outcomes, here, only totals shown, but specifications are up to the user. ADD LOOP for all outcomes over time and total TABLE.
# 
# ####[] is used here to indicate the number of simulation years into the future.
# ####Disease outcomes has to be changed to the outcome of interest
# 
# #### Test code with loops for aggregated outcomes diseases burden. NOT WORKING.
# 
# ### Compare with loops for age and sex cohort outcomes.
# 
# p_aggr_list <- list()
# index <- 1
# 
# for (outcome in i_outcome_d) {
#   for (d in 1:nrow(DISEASE_SHORT_NAMES)) {
#     
#     # outcome <- i_outcome[1]
#     # disease <- i_disease[1]
#     
#     p_aggr_list_index <- ggplot(total_aggr[1:79,], aes(x = total_aggr[['sim_year']])) +
#       
#       geom_line(mapping = aes(y = total_aggr[[paste('total', outcome, 'num_bl', DISEASE_SHORT_NAMES$sname[d], sep = '_')]], colour = paste('total', outcome, 'num_bl', DISEASE_SHORT_NAMES$sname[d], sep = '_'))) +
#       theme_classic() +
#       geom_hline(yintercept=0, linetype='dashed', color = 'black') +
#       geom_line(mapping = aes(y = total_aggr[[paste('total', outcome, 'num_sc', DISEASE_SHORT_NAMES$sname[d], sep = '_')]], colour = paste('total', outcome, 'num_sc', DISEASE_SHORT_NAMES$sname[d], sep = '_'))) +
#       geom_line(mapping = aes(y = total_aggr[[paste('total', outcome, 'num_diff', DISEASE_SHORT_NAMES$sname[d], sep = '_')]], colour = paste('total', outcome, 'num_diff', DISEASE_SHORT_NAMES$sname[d], sep = '_'))) +
#       xlab ('Simulation years') + ylab ('Cases') + labs (title = paste(DISEASE_SHORT_NAMES$sname[d], outcome)) +
#       theme(plot.title = element_text(hjust = 0.5, size = 12)) +
#       scale_color_discrete(name = paste(''), labels = c('Baseline', 'Difference', 'Scenario')) +
#       theme(plot.title = element_text(hjust = 0.5))
#     p_aggr_list[[index]] <- p_aggr_list_index
#     index <- index + 1
#     
#     
#   }
# }
# 
# index <- 1
# 
# interpolation_index <- 1
# for (outcome in i_outcome_d) {
#   for (disease in i_disease) {
#     file_name = paste('output/graphs', 'Aggregated Outcomes', outcome, disease, '.jpeg', sep=' ')
#     jpeg(file_name)
#     print(p_aggr_list[[index]])
#     index <- index + 1
#     dev.off()
#   }
# }
# 
# 
# p_aggregated <- do.call(marrangeGrob, list(grobs=p_aggr_list, nrow = 2, ncol = 2))
# p_aggregated
# 
# 