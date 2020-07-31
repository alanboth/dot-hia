# ---- chunk-intro ----

# library(ggpubr)
# library(ggplot2)
# library(arsenal)
# library(janitor)
library(dplyr)
library(readr)
# library(conflicted)
# library(rlist)
# library(reshape)
# library(reshape2)
# library(zoo)
# library(stringi)
# library(tidyverse)
# library(rlist)
# library(ithimr)
# if (interactive()) {
#   library(conflicted)
# }
# conflict_prefer("filter", "dplyr")
rm (list = ls())
options(scipen=999)
source("Scripts/functions_mslt.R")
#### TO DO: 
## Udpate mslt input sheet with Australian data, need to run disbayes/dsmod for diseases
## Add Intervention duration
## Add all cause mortality pifs modifying the general life table only and compare results. 
## Add weights to matched population
## trends case fatality and incidence
## Run uncertainty


## CALCULATION ORDER only including physical activity changes
# 1) Matched population
# 2) mmets per person 
# 3) RRs per person
# 4) PIFS by age and sex


## PARAMETERS
SCEN_SHORT_NAME <- c("base", "scen1")

## UNCERTAINTY PARAMETERS

# NSAMPLES <- 1024
MMET_CYCLING <-  4.63#c(log(4.63),log(1.2))
MMET_WALKING <- 2.53 #c(log(2.53),log(1.1)) 

## TO DO: Calculate SD from CI, see Erzats
DIABETES_IHD_RR_F <<- 2.82 ## c(log(2.82),log()) CI (2.35, 3.38)
DIABETES_STROKE_RR_F <<- 2.28 ## c(log(2.28),log()) CI (1.93, 2.69)
DIABETES_IHD_RR_M <<- 2.16 ## c(log(2.16),log()) CI (1.82, 2.56)
DIABETES_STROKE_RR_M <<- 1.83 ## c(log(1.83),log()) CI (1.60, 2.08)

## GET DEMOGRAPHICS
## DATA DOSE RESPONSE RRS (FROM METAHIT)

global_path <- file.path(find.package('ithimr',lib.loc=.libPaths()), 'extdata/global/')
## for windows??
global_path <- paste0(global_path, "/")

file_name <- paste0(getwd(), "/Data/Processed/disease_outcomes_lookup.csv")
DISEASE_INVENTORY <-  read_csv(file_name)
list_of_files <- list.files(path = paste0(global_path,"dose_response/drpa/extdata/"), recursive = TRUE, pattern = "\\.csv$", full.names = TRUE)
for (i in 1:length(list_of_files)){
  assign(stringr::str_sub(basename(list_of_files[[i]]), end = -5),
         readr::read_csv(list_of_files[[i]],col_types = cols()),
         pos = 1)
}

## GET MATCHED POPULATION

synth_pop <- read_csv(paste0(getwd(), "//Data//Processed//matched_pop.csv")) %>%
  dplyr::mutate(participant_id = seq.int(nrow(synth_pop))) %>%
  dplyr::rename(age = age1)

## CALCULTE RRS PER PERSON 

### FIRST WE NEED MMETS PER PERSON  
mmets_pp <- synth_pop %>% dplyr::select(participant_id, sex, age, dem_index, starts_with("time") & contains(c("pedestrian", "bicycle")), work_ltpa_marg_met) %>%
  dplyr::mutate_all(funs(ifelse(is.na(.), 0, .))) %>%
  dplyr::mutate(base_mmet = work_ltpa_marg_met + time_base_pedestrian * MMET_WALKING + time_base_bicycle * MMET_CYCLING) %>%
  dplyr::mutate(scen1_mmet = work_ltpa_marg_met + time_scen_pedestrian * MMET_WALKING + time_scen_bicycle * MMET_CYCLING)

### SECOND WE CALCULATE RRS PER PERSON FROM MMET PER PERSON AND RRS DATA FROM ITHIMR
#### TO DO: HOW IS UNCERTAINTY IN RRS INCORPORATED (SEE FILES e.g. breast_cancer_mortality)
#### TO Do: WHICH RRS ARE USED IN FUNCTION? CHECK SOURCE FUNCTION gen_pa_rr (for example, some rrs have all and other mortatlity)  
#### TO DO: check in source formula which RRs are applied (all, mortality, incidence)
RR_PA_calculations <- ithimr::gen_pa_rr(mmets_pp)

### CALCULTE PIFS BY AGE AND SEX GROUP

# RR_PA_calculations <- RR_PA_calculations %>%
#   dplyr::mutate(dem_index = group_indices(., age_cat, sex)) ### Added here to add 

### Calculate PIFs by age and sex for air pollution and physical activity combined
#### health_burden2 was created by Rob J for metahit

#### ALAN, can you please help me with this funciton? it provisions for having air pollution RRs, bue twe do not have them yet. You can find the function 
#### in functions_mslt
pifs_pa_ap <- health_burden_2(RR_PA_calculations)


# PLACE HOLDER
pif_expanded <- read_csv(paste0(paste0(getwd(),"/Data/Processed/pif_expanded.csv")))

# PLACE HOLDER
DISEASE_SHORT_NAMES <-  read_csv(paste0(paste0(getwd(),"/Data/Processed/disease_names.csv")))

### MELB DATA

MSLT_DF <- read_csv(paste0(getwd(), "/Data/Processed/mslt.csv"))

## Parameters

year <- 2017

i_age_cohort <- c(17, 22, 27, 32, 37, 42, 47, 52, 57, 62, 67, 72, 77, 82, 87, 92, 97)

i_sex <- c('male', 'female')

trend_mort_ac <- 1 # TO DO

### Add trends diseases incidence and case fatality

### Intervention duraction
##

sc_duration <- replicate(4,1) %>% append(replicate(80, 0))


# ---- chunk-2 ----

# ---- chunk-2 ----

## Create baseline life tables

general_life_table_list_bl <- list()

index <- 1

for (iage in i_age_cohort){
  for (isex in i_sex){
    # cat('age ', age, ' and sex ', sex, '\n') #Uncomment to see index
    suppressWarnings(general_life_table_list_bl[[index]] <- RunLifeTable(in_idata = MSLT_DF,
                                                                         in_sex = isex, in_mid_age = iage))
    
    names(general_life_table_list_bl)[index] <- paste(iage, isex, sep = '_')
    index <- index + 1
  }
}


# ---- chunk-3 ----


### Change order in disease short_names to start with diabetes. This is important when calculating the scenario disease life tables as diabetes is calculated first to then 
### impact on cardiovascular disease calculations. 

dia_index <- which(DISEASE_SHORT_NAMES$sname=='dmt2')
dia_order <- c(dia_index,c(1:nrow(DISEASE_SHORT_NAMES))[-dia_index])

disease_life_table_list_bl <- list()
index <- 1

for (iage in i_age_cohort){
  for (isex in i_sex){
    for (d in c(1:nrow(DISEASE_SHORT_NAMES))[dia_order]){
      
      
      ## Exclude non-males diseases and non-chronic diseases and road injuries and disease with no pif
      if (isex == 'male' && (DISEASE_SHORT_NAMES$disease[d] %in% c('breast cancer', 'uterine cancer'))
          || DISEASE_SHORT_NAMES$is_not_dis[d] != 0 || DISEASE_SHORT_NAMES$acronym[d] == 'no_pif' || DISEASE_SHORT_NAMES$acronym[d] == 'other'){
      }
      else {
        
        # print(paste(isex, DISEASE_SHORT_NAMES$disease[d]))
        disease_life_table_list_bl[[index]] <- RunDisease(in_idata = MSLT_DF,in_mid_age = iage, in_sex = isex,  in_disease = DISEASE_SHORT_NAMES$sname[d])
        
        names(disease_life_table_list_bl)[index] <- paste(iage, isex, DISEASE_SHORT_NAMES$sname[d], sep = '_')
        
        index <- index + 1
        
      }
    }
  }
}


# ---- chunk-4 ----

## Create non_disease lists, these are by age and sex for road injuries and lwri baseline and scenario, including calculation of difference in rates. 
## Different calculation for scenario deaths and ylds for lri and injuries. 

non_disease_list <- list()
index <- 1


for (iage in i_age_cohort){
  for (isex in i_sex) {
    for (d in 1:nrow(DISEASE_SHORT_NAMES)){
      
      ## Exclude chronic disease and all-cause mortality and  pyld
      if (DISEASE_SHORT_NAMES$is_not_dis[d] != 1 || DISEASE_SHORT_NAMES$acronym[d] == 'other' || DISEASE_SHORT_NAMES$acronym[d] == 'no_pif'){
      }
      else {
        
        ## Only keep non-diseases variables in the dataframe
        td1 <- as.data.frame(filter(MSLT_DF, age >= iage & sex == isex) %>% 
                               dplyr::select(age, sex, contains(DISEASE_SHORT_NAMES$acronym[DISEASE_SHORT_NAMES$sname == DISEASE_SHORT_NAMES$sname[d]])))
        
        
        pif_non_disease <- as.data.frame(dplyr::filter(pif_expanded, age >= iage & sex == isex) %>% 
                                           dplyr::select(age, sex, contains(DISEASE_SHORT_NAMES$acronym[DISEASE_SHORT_NAMES$sname == DISEASE_SHORT_NAMES$sname[d]])))
        
        ## Modify pif by duration of intervention
        
        # pif_non_disease[,3] <- pif_non_disease[,3] * sc_duration
        
        non_disease_list[[index]] <-  RunNonDisease (td1, in_non_disease = DISEASE_SHORT_NAMES$acronym[d])
        
        non_disease_list[[index]]$non_disease <- DISEASE_SHORT_NAMES$acronym[d]
        
        ### For LRI we modify mortality rates and ylds rates by 1-PIF. For road trauma we multiple the PIF by baseline deaths/ylds rates to generate scenario
        ### ylds and mortlality rates and then take the difference
        
        if (DISEASE_SHORT_NAMES$acronym[d] == 'lri'){
          
          
          
          ## PIFs multiplied by scenario duration  
          ## RATES
          ### deaths sceanario lri
          non_disease_list[[index]][paste0('deaths_rate_sc')] <-
            non_disease_list[[index]][paste0('deaths_rate_bl')] * 
            (1 - pif_non_disease [paste0('pif_', DISEASE_SHORT_NAMES$acronym[d], '_deaths')] * sc_duration) 
          
          
          ### ylds scenario lri
          non_disease_list[[index]][paste0('ylds_rate_sc')] <-
            non_disease_list[[index]][paste0('ylds_rate_bl')] * 
            (1 - pif_non_disease [paste0('pif_', DISEASE_SHORT_NAMES$acronym[d], '_ylds')] * sc_duration) 
          
        }
        else {
          
          ## deaths sceanario injuries
          non_disease_list[[index]][paste0('deaths_rate_sc')] <-
            non_disease_list[[index]][paste0('deaths_rate_bl')] * 
            (1 - pif_non_disease [paste0('pif_', DISEASE_SHORT_NAMES$acronym[d], '_deaths')] * sc_duration) 
          
          
          ## ylds scenario injuries
          non_disease_list[[index]][paste0('ylds_rate_sc')] <-
            non_disease_list[[index]][paste0('ylds_rate_bl')] * 
            (1 - pif_non_disease [paste0('pif_', DISEASE_SHORT_NAMES$acronym[d], '_ylds')] * sc_duration) 
        }
        
        ## Difference variable
        
        ## deaths difference
        non_disease_list[[index]][paste0('diff_mort')] <- non_disease_list[[index]][paste0('deaths_rate_sc')] -
          non_disease_list[[index]][paste0('deaths_rate_bl')]
        ## ylds difference
        non_disease_list[[index]][paste0('diff_ylds')] <- non_disease_list[[index]][paste0('ylds_rate_sc')] -
          non_disease_list[[index]][paste0('ylds_rate_bl')]
        
        non_disease_list[[index]][IsNanDataFrame(non_disease_list[[index]])] <- 0
        
        names(non_disease_list)[index] <- paste(iage, isex, DISEASE_SHORT_NAMES$acronym[d], sep = '_')
        
        index <- index + 1
        
      }
    }
  }  
}
# ---- chunk-5 ----

## Create scenario life tables with new pifs,includes Diabetes loop. 

### Relative risks of diabetes for cardiovascular diseases

DIABETES_IHD_RR_F <<- 2.82 ## 2.35
DIABETES_STROKE_RR_F <<- 2.28 ## 1.93
DIABETES_IHD_RR_M <<- 2.16 ## 2.16
DIABETES_STROKE_RR_M <<- 1.83 ## 1.6


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
        pif_disease[,2] <- pif_disease[,2] * sc_duration
        
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
## Uncommnet to check scenario life tables
# View(disease_life_table_list_sc[[3]])

# ---- chunk-8 ----

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


#### NonDiseases (road injuries and lri)
mx_sc_total_non_disease <- list()
l_index <- 1
index <- 1
for (iage in i_age_cohort){
  for (isex in i_sex){
    mortality_sum <- NULL
    mortality_sum1 <- NULL
    create_new <- T
    
    ## Sum all non_diseases mortality rates
    
    for (d in 1:nrow(DISEASE_SHORT_NAMES)) {
      if (DISEASE_SHORT_NAMES$is_not_dis[d] != 1 || DISEASE_SHORT_NAMES$acronym[d] == 'no_pif' || DISEASE_SHORT_NAMES$acronym[d] == 'other'){
      }
      else {
        
        # print(paste(isex, DISEASE_SHORT_NAMES$disease[d]))
        
        if (create_new){
          mortality_sum <- non_disease_list[[index]][,age_sex_cols]
          mortality_sum$total <- 0
          create_new <- F
          mortality_sum$total <- mortality_sum$total +
            (non_disease_list[[index]]$diff_mort)
        }else{
          mortality_sum$total <- mortality_sum$total +
            (non_disease_list[[index]]$diff_mort)
        }
        
        # cat(age, ' - ', sex,' - ',  disease,' - ',  index, ' - ', l_index,  '\n')
        index <- index + 1
      }
    }
    mx_sc_total_non_disease[[l_index]] <- mortality_sum
    names(mx_sc_total_non_disease)[l_index] <- paste(iage, isex)
    
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

### Non-disease

pylds_sc_total_non_disease <- list()
l_index <- 1
index <- 1
for (iage in i_age_cohort){
  for (isex in i_sex){
    pylds_sum <- NULL
    create_new <- T
    
    for (d in 1:nrow(DISEASE_SHORT_NAMES)) {
      if (isex == 'male' && (DISEASE_SHORT_NAMES$disease[d] %in% c('breast cancer', 'uterine cancer'))
          || DISEASE_SHORT_NAMES$is_not_dis[d] != 1 || DISEASE_SHORT_NAMES$acronym[d] == 'no_pif' || DISEASE_SHORT_NAMES$acronym[d] == 'other'){
      }
      else {
        
        # print(paste(isex, DISEASE_SHORT_NAMES$disease[d]))
        
        if (create_new){
          pylds_sum <- non_disease_list[[index]][,age_sex_cols]
          pylds_sum$total <- 0
          create_new <- F
          pylds_sum$total <- pylds_sum$total +
            (non_disease_list[[index]]$diff_ylds)
        }else{
          pylds_sum$total <- pylds_sum$total +
            (non_disease_list[[index]]$diff_ylds)
        }
        
        # cat(age, ' - ', sex,' - ',  disease,' - ',  index, ' - ', l_index,  '\n')
        index <- index + 1
      }
      
    }
    pylds_sc_total_non_disease[[l_index]] <- pylds_sum
    names(pylds_sc_total_non_disease)[l_index] <- paste(iage, isex)
    l_index <- l_index + 1
  }
}

# ---- chunk-9 ----

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
      mx_sc_total_disease[[index]]$total + 
      mx_sc_total_non_disease[[index]]$total
    
    td2[td2$age >= iage & td2$sex == isex,][[paste('pyld_rate')]] <- general_life_table_list_bl[[index]]$pyld_rate + 
      pylds_sc_total_disease[[index]]$total + 
      pylds_sc_total_non_disease[[index]]$total
    
    
    # Instead of idata, feed td to run scenarios
    general_life_table_list_sc[[index]] <- RunLifeTable(in_idata = td2, in_sex = isex, in_mid_age = iage)
    #
    names(general_life_table_list_sc)[index] <- paste(iage, isex, sep = '_')
    
    
    index <- index + 1
  }
}

# ---- chunk-10 ----

## In the following list 'output_life_table', 34 data frames are nested per age and sex cohort

## Outputs are generated following the index order of disease life tables baseline and scenarios where diabates is first calculated as it impacts on cardivascular diseases. 

## In the following list 'output_life_table', 34 data frames are nested per age and sex cohort

## Outputs are generated following the index order of disease life tables baseline and scenarios where diabates is first calculated as it impacts on cardivascular diseases. 

dia_index <- which(DISEASE_SHORT_NAMES$sname=='dmt2')
dia_order <- c(dia_index,c(1:nrow(DISEASE_SHORT_NAMES))[-dia_index])


output_burden <- list()
l_index <- 1
index <- 1
index_n <- 1
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
    
    
    #### Add non_diseases (injuires and lwri)
    
    create_new <- T
    for (d in 1:nrow(DISEASE_SHORT_NAMES)) {
      if (DISEASE_SHORT_NAMES$is_not_dis[d] != 1 || DISEASE_SHORT_NAMES$acronym[d] == 'no_pif' || DISEASE_SHORT_NAMES$acronym[d] == 'other'){
      }
      else {
        if (create_new){
          
          td5 <- non_disease_list[[index_n]]
          
          
          ## Calculate numbers
          
          # ## Variable names
          #
          mx_num_bl <- paste('mx_num_bl',DISEASE_SHORT_NAMES$acronym[d], sep = '_')
          ylds_num_bl <- paste('ylds_num_bl',DISEASE_SHORT_NAMES$acronym[d], sep = '_')
          mx_num_sc <- paste('mx_num_sc',DISEASE_SHORT_NAMES$acronym[d], sep = '_')
          ylds_num_sc <- paste('ylds_num_sc',DISEASE_SHORT_NAMES$acronym[d], sep = '_')
          
          ### Baseline
          td5[[mx_num_bl]] <- non_disease_list[[index_n]]$deaths_rate_bl *
            general_life_table_list_bl[[l_index]]$Lx
          
          td5[[ylds_num_bl]]<- non_disease_list[[index_n]]$ylds_rate_bl *
            general_life_table_list_bl[[l_index]]$Lx
          
          
          ### Scenario
          td5[[mx_num_sc]] <- non_disease_list[[index_n]]$deaths_rate_sc *
            general_life_table_list_sc[[l_index]]$Lx
          
          td5[[ylds_num_sc]] <- non_disease_list[[index_n]]$ylds_rate_sc *
            general_life_table_list_bl[[l_index]]$Lx
          
          output_burden_change3 <- list()
          
          ### Difference
          
          output_burden_change3$mx_num_diff <- non_disease_list[[index_n]]$diff_mort * general_life_table_list_sc[[l_index]]$Lx
          
          output_burden_change3$ylds_num_diff <- non_disease_list[[index_n]]$diff_ylds * general_life_table_list_sc[[l_index]]$Lx
          
          
          ### Change names rates to match each non_disease
          
          names(output_burden_change3)[names(output_burden_change3) == 'diff_ylds'] <- paste('ylds_rate_diff', DISEASE_SHORT_NAMES$acronym[d], sep = '_')
          
          names(output_burden_change3)[names(output_burden_change3) == 'diff_mort'] <- paste('deaths_rate_diff', DISEASE_SHORT_NAMES$acronym[d], sep = '_')
          
          ## Change names numbers to match non_disease
          
          names(output_burden_change3)[names(output_burden_change3) == 'mx_num_diff'] <- paste('mx_num_diff', DISEASE_SHORT_NAMES$acronym[d], sep = '_')
          
          names(output_burden_change3)[names(output_burden_change3) == 'ylds_num_diff'] <- paste('ylds_num_diff', DISEASE_SHORT_NAMES$acronym[d], sep = '_')
          
          output_burden_sc <- cbind(output_burden_sc, td5)
          output_burden_sc <- cbind(output_burden_sc, output_burden_change3)
          
          create_new <- F
          
          ## Here the calculations above are repeated, here is where the F is telling to move into the next disease
          
        }else{
          
          td6 <- non_disease_list[[index_n]]
          
          
          ## Calculate numbers
          
          # ## Variable names
          #
          mx_num_bl <- paste('mx_num_bl',DISEASE_SHORT_NAMES$acronym[d], sep = '_')
          ylds_num_bl <- paste('ylds_num_bl',DISEASE_SHORT_NAMES$acronym[d], sep = '_')
          mx_num_sc <- paste('mx_num_sc',DISEASE_SHORT_NAMES$acronym[d], sep = '_')
          ylds_num_sc <- paste('ylds_num_sc',DISEASE_SHORT_NAMES$acronym[d], sep = '_')
          
          ### Baseline
          td6[[mx_num_bl]] <- non_disease_list[[index_n]]$deaths_rate_bl *
            general_life_table_list_bl[[l_index]]$Lx
          
          td6[[ylds_num_bl]]<- non_disease_list[[index_n]]$ylds_rate_bl *
            general_life_table_list_bl[[l_index]]$Lx
          
          
          ### Scenario
          td6[[mx_num_sc]] <- non_disease_list[[index_n]]$deaths_rate_sc *
            general_life_table_list_sc[[l_index]]$Lx
          
          td6[[ylds_num_sc]] <- non_disease_list[[index_n]]$ylds_rate_sc *
            general_life_table_list_bl[[l_index]]$Lx
          
          output_burden_change4 <- list()
          ### Difference
          
          output_burden_change4$mx_num_diff <- non_disease_list[[index_n]]$diff_mort * general_life_table_list_sc[[l_index]]$Lx
          
          output_burden_change4$ylds_num_diff <- non_disease_list[[index_n]]$diff_ylds * general_life_table_list_sc[[l_index]]$Lx
          
          
          ### Change names rates to match each non_disease
          
          names(output_burden_change4)[names(output_burden_change4) == 'diff_ylds'] <- paste('ylds_rate_diff', DISEASE_SHORT_NAMES$acronym[d], sep = '_')
          
          names(output_burden_change4)[names(output_burden_change4) == 'diff_mort'] <- paste('deaths_rate_diff', DISEASE_SHORT_NAMES$acronym[d], sep = '_')
          
          ## Change names numbers to match non_disease
          
          names(output_burden_change4)[names(output_burden_change4) == 'mx_num_diff'] <- paste('mx_num_diff', DISEASE_SHORT_NAMES$acronym[d], sep = '_')
          
          names(output_burden_change4)[names(output_burden_change4) == 'ylds_num_diff'] <- paste('ylds_num_diff', DISEASE_SHORT_NAMES$acronym[d], sep = '_')
          
          output_burden_sc <- cbind(output_burden_sc, td6)
          output_burden_sc <- cbind(output_burden_sc, output_burden_change4)
          
        }
        index_n <- index_n + 1
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



# ---- chunk-11.1.3 Injuries deaths and ylds ----
#### Add uncertainty intervals
#### Accumulated over the life of the cohort. For example, total life years change for females 16-19 over their life course

### Vector with diseases (may be a better way for this)

### hard coded matches, best not to
output_injuries_change <- output_df %>% 
  group_by(Gender, `Age group`) %>%
  summarise_if(is.numeric, funs(sum)) %>%
  dplyr::select(`Age group`, Gender, matches("diff_pedestrian|diff_cyclist|diff_motorcyclist|diff_motor"))



### Below is for graphs, needs more owrk

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