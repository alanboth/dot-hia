library(dplyr)
library(readr)
library(data.table)
library(srvyr)

# runs a local version of gen_pa_rr (and PA_dose_response) so ithim-r library doesn't need to be called
# ithim-r still needs to be installed so we can access the dose response folder
gen_pa_rr_wrapper <- function(mmets_pp_location,disease_inventory_location,dose_response_folder,PA_DOSE_RESPONSE_QUANTILE) {
  # mmets_pp_location=mmets_pp_MEL
  # disease_inventory_location="Data/original/ithimr/disease_outcomes_lookup.csv"
  # # location of ithmr default dose response data:
  # dose_response_folder=paste0(file.path(find.package('ithimr',lib.loc=.libPaths()), 'extdata/global'),
  #                             "/dose_response/drpa/extdata")
  # PA_DOSE_RESPONSE_QUANTILE=F
  
  ## loading copies of the ithm-r functions called
  gen_pa_rr <- function(mmets_pp) {
    dose_columns <- match(paste0(SCEN_SHORT_NAME, "_mmet"), 
                          colnames(mmets_pp))
    doses_vector <- unlist(data.frame(mmets_pp[, dose_columns]))
    for (j in c(1:nrow(DISEASE_INVENTORY))[DISEASE_INVENTORY$physical_activity == 
                                           1]) {
      pa_dn <- as.character(DISEASE_INVENTORY$pa_acronym[j])
      pa_n <- as.character(DISEASE_INVENTORY$acronym[j])
      return_vector <- PA_dose_response(cause = pa_dn, dose = doses_vector)
      for (i in 1:length(SCEN_SHORT_NAME)) {
        scen <- SCEN_SHORT_NAME[i]
        mmets_pp[[paste("RR_pa", scen, pa_n, sep = "_")]] <- return_vector$rr[(1 + (i - 1) * nrow(mmets_pp)):(i * nrow(mmets_pp))]
      }
    }
    mmets_pp
  }
  
  PA_dose_response <- function(cause, dose, confidence_intervals = PA_DOSE_RESPONSE_QUANTILE) {
    if (sum(is.na(dose)) > 0 || class(dose) != "numeric") {
      stop("Please provide dose in numeric")
    }
    if (!cause %in% c("all_cause", "breast_cancer", "cardiovascular_disease", 
                      "colon_cancer", "coronary_heart_disease", "diabetes", 
                      "endometrial_cancer", "heart_failure", "lung_cancer", 
                      "stroke", "total_cancer")) {
      stop("Unsupported cause/disease. Please select from \n\n         all_cause \n\n         breast_cancer\n\n         cardiovascular_disease \n\n         colon_cancer \n\n         coronary_heart_disease \n\n         endometrial_cancer \n\n         heart_failure \n\n         lung_cancer \n\n         stroke \n\n         total_cancer")
    }
    outcome_type <- ifelse(cause %in% c("lung_cancer", "breast_cancer", 
                                        "endometrial_cancer", "colon_cancer"), "all", "mortality") ### TO DO: add to get incidence if available
    if (cause %in% c("total_cancer", "coronary_heart_disease", 
                     "breast_cancer", "endometrial_cancer", "colon_cancer")) 
      dose[dose > 35] <- 35
    else if (cause == "lung_cancer") 
      dose[dose > 10] <- 10
    else if (cause == "stroke") 
      dose[dose > 32] <- 32
    else if (cause == "all_cause") 
      dose[dose > 16.08] <- 16.08
    fname <- paste(cause, outcome_type, sep = "_")
    lookup_table <- get(fname)
    lookup_df <- setDT(lookup_table)
    rr <- approx(x = lookup_df$dose, y = lookup_df$RR, xout = dose, 
                 yleft = 1, yright = min(lookup_df$RR))$y
    if (confidence_intervals || PA_DOSE_RESPONSE_QUANTILE == 
        T) {
      lb <- approx(x = lookup_df$dose, y = lookup_df$lb, xout = dose, 
                   yleft = 1, yright = min(lookup_df$lb))$y
      ub <- approx(x = lookup_df$dose, y = lookup_df$ub, xout = dose, 
                   yleft = 1, yright = min(lookup_df$ub))$y
    }
    if (PA_DOSE_RESPONSE_QUANTILE == T) {
      rr <- qnorm(get(paste0("PA_DOSE_RESPONSE_QUANTILE_", 
                             cause)), mean = rr, sd = (ub - lb)/1.96)
      rr[rr < 0] <- 0
    }
    if (confidence_intervals) {
      return(data.frame(rr = rr, lb = lb, ub = ub))
    }
    else {
      return(data.frame(rr = rr))
    }
  }
  
  mmets_pp <- mmets_pp_location ### Alan, I changed here, as in mslt mmets have uncertainty, so should not be read from fixed file
  DISEASE_INVENTORY <-  read.csv(disease_inventory_location,as.is=T,fileEncoding="UTF-8-BOM")
  
  # filtering down to columns with 'mmet' in their name
  SCEN_SHORT_NAME <- colnames(mmets_pp)[grep("mmet",colnames(mmets_pp))]
  # removing '_mmet' to find the base scenario and scenario names
  SCEN_SHORT_NAME <- gsub("_mmet","",SCEN_SHORT_NAME)
  
  # load every csv file in the dose response folder
  list_of_files <- list.files(path=dose_response_folder, recursive=TRUE,
                              pattern="\\.csv$", full.names=TRUE)
  for (i in 1:length(list_of_files)){
    assign(stringr::str_sub(basename(list_of_files[[i]]), end = -5),
           readr::read_csv(list_of_files[[i]],col_types = cols()),
           pos = 1)
  }
  
  RR_PA_calculations <- gen_pa_rr(mmets_pp)
  return(RR_PA_calculations)
}

health_burden_2 <- function(ind_ap_pa_location,disease_inventory_location,demographic_location,combined_AP_PA=T,calculate_AP=T){
  # ind_ap_pa_location=RR_PA_calculations_MEL
  # disease_inventory_location="Data/original/ithimr/disease_outcomes_lookup.csv"
  # demographic_location="Data/processed/DEMO.csv"
  # combined_AP_PA=F
  # calculate_AP=F

  ind_ap_pa <- ind_ap_pa_location  #read.csv(ind_ap_pa_location,as.is=T,fileEncoding="UTF-8-BOM") ## Alan I removed read as this inputs will have uncertainy
  DISEASE_INVENTORY <- read.csv(disease_inventory_location,as.is=T,fileEncoding="UTF-8-BOM")
  DEMOGRAPHIC <- read.csv(demographic_location,as.is=T,fileEncoding="UTF-8-BOM")
  
  # filtering down to columns with 'mmet' in their name
  SCEN_SHORT_NAME <- colnames(ind_ap_pa)[grep("mmet",colnames(ind_ap_pa))]
  # removing '_mmet' to find the base scenario and scenario names
  SCEN_SHORT_NAME <- gsub("_mmet","",SCEN_SHORT_NAME)
  
  pop_details <- DEMOGRAPHIC
  pif_scen <- pop_details
  pif_scen_2 <- ind_ap_pa_location %>% dplyr::select(dem_index, participant_wt, sex, age_group_2)
  # set up reference (scen1)
  reference_scenario <- SCEN_SHORT_NAME[1]
  scen_names <- SCEN_SHORT_NAME[SCEN_SHORT_NAME!=reference_scenario]
  ### iterating over all all disease outcomes
  for ( j in 1:nrow(DISEASE_INVENTORY)){
    # j=2
    # Disease acronym and full name
    ac <- as.character(DISEASE_INVENTORY$acronym[j])
    gbd_dn <- as.character(DISEASE_INVENTORY$GBD_name[j])
    # calculating health outcome, or independent pathways?
    pathways_to_calculate <- ifelse(combined_AP_PA,1,DISEASE_INVENTORY$physical_activity[j]+DISEASE_INVENTORY$air_pollution[j])
    for(path in 1:pathways_to_calculate){
      # set up column names
      if(combined_AP_PA){
        middle_bit <-
          paste0(
            ifelse(DISEASE_INVENTORY$physical_activity[j] == 1, 'pa_', ''),
            ifelse(DISEASE_INVENTORY$air_pollution[j] == 1, 'ap_', '')
          )
        middle_bit_plus <-
          paste0(
            ifelse(DISEASE_INVENTORY$physical_activity[j] == 1, 'pa_', ''),
            ifelse(DISEASE_INVENTORY$air_pollution[j] == 1, 'ap_', ''),
            ifelse(DISEASE_INVENTORY$noise[j] == 1, 'noise_', ''),
            ifelse(DISEASE_INVENTORY$nitrogen_dioxide[j] == 1, 'no2_', '')
          )
      }else{
        # if independent, choose which one
        val <- c('pa_','ap_')[which(c(DISEASE_INVENTORY$physical_activity[j],DISEASE_INVENTORY$air_pollution[j])==1)]
        if(pathways_to_calculate>1) val=val[path]
        middle_bit <- middle_bit_plus <- val
      }
      if(calculate_AP==T | middle_bit!="ap_") {
        base_var <- paste0('RR_', middle_bit, reference_scenario, '_', ac)
        scen_vars <- paste0('RR_', middle_bit, scen_names, '_', ac)
        # set up pif tables
        # dies here if you don't have air pollution
        pif_table <- setDT(ind_ap_pa[,colnames(ind_ap_pa)%in%c(base_var,'dem_index', 'participant_wt', 'age', 'sex')])
        setnames(pif_table,base_var,'outcome')
        pif_ref <- pif_table[,.(sum(outcome)),by='dem_index']
        pif_ref_2 <- pif_table[,.(outcome)] 
        ## sort pif_ref
        setorder(pif_ref,dem_index)
        for (index in 1:length(scen_vars)){
          # set up naming conventions
          scen <- scen_names[index]
          scen_var <- scen_vars[index]
          pif_name <- paste0('pif_',ac)
          # Calculate PIFs for selected scenario
          pif_table <- setDT(ind_ap_pa[,colnames(ind_ap_pa)%in%c(scen_var,'dem_index', 'participant_wt', 'age', 'sex')])
          setnames(pif_table,scen_var,'outcome')
          pif_temp <- pif_table[,.(sum(outcome)),by='dem_index']
          pif_temp_2 <- pif_table[,.(outcome)] 
          
          
          ## sort pif_temp
          setorder(pif_temp,dem_index)
          pif_scen[[pif_name]] <- (pif_ref[,V1] - pif_temp[,V1]) / pif_ref[,V1] 
          pif_scen_2[[pif_name]] <- (pif_ref_2[,outcome]  - pif_temp_2[,outcome]) / pif_ref_2[,outcome]  
        
          
        ## BZ: added code to calculate pifs by subgroups (DEMO) using participant weights. 
        ### Declare survey as weighted
        pif_wt <- pif_scen_2 %>%
        srvyr::as_survey_design(weights = participant_wt)
        ### Calculate mean pifs by age and sex
        pif_weighted <-  pif_wt  %>%
        group_by(sex, age_group_2) %>%
        dplyr::summarise_at(vars(starts_with("pif")), survey_mean)

        }
      }
    }
  }
  return(list(pif_scen, pif_weighted, pif_scen_2))
}


## Function to generate age and sex life table for baseline and scenario.

# mx:        mortality
# pyld_rate: person-years lived with a disability rate
# qx:        probability of dying
# lx:        number of survivors year 1 simulation
# dx:        number died in year 1 simulation
# Lx:        number of persons lived by cohort to age x + 1/2 (average people)
# ex:        life expectancy
# Lwx:       health adjusted life years
# ewx:       health adjusted life expectancy

RunLifeTable <- function(in_idata, in_sex, in_mid_age, death_rates=NA) {
  # in_idata=MSLT_DF
  # in_sex='male'
  # in_mid_age=17
  
  # Create a life table starting data frame from input data. 
  lf_df <- in_idata %>%
    dplyr::filter(age >= in_mid_age & sex == in_sex) %>%
    dplyr::select('sex', 'age', 'pyld_rate', 'mx')
  
  # are we using modified mortality rates?
  if(is.data.frame(death_rates)) {
    # filter to only this cohort's death rates
    cohort_death_rates <- death_rates %>%
      dplyr::filter(age_cohort == in_mid_age & sex == in_sex) %>%
      dplyr::select(age,sex,rate)
    # join to lf_df and replace mx with the new mortality data
    lf_df <- lf_df %>%
      dplyr::inner_join(cohort_death_rates, by=c('age','sex')) %>%
      dplyr::select(sex, age, pyld_rate, mx=rate)
  }
  
  
  # Create list life table variables. First as vector and then added to the data frame at the end.
  ## We model up to 100, that is the reason for the age limit in the function
  
  # probability of dying
  
  qx <-  ifelse(lf_df$age < 100, 1 - exp(-1 * lf_df$mx), 1)
  
  # number of survivors year 1 simulation
  
  num_row <- nrow(lf_df)
  lx <- rep(0,num_row)
  lx[1] <- as.numeric(in_idata$population[in_idata$age == in_mid_age & in_idata$sex == in_sex]) 
  
  # number died in year 1 simulation
  
  dx <- rep(0,num_row)
  dx[1] <- lx[1] * qx[1]
  
  # number of survivors and who die from year 2 onwards. 
  
  for (i in 2:num_row){
    lx[i] <- lx[i - 1] - dx[i - 1]
    dx[i] <- lx[i] * qx[i]
  }
  
  # number of persons lived by cohort to age x + 1/2 (average people)
  
  Lx <- rep(0,num_row)
  
  # for years up to 99
  
  for (i in 1:(num_row-1))
    Lx[i] <- (lx[i] + lx[i + 1]) / 2
  
  # for year 100, cohort dies at 100 if anyone left
  
  Lx[num_row] <- lx[num_row] / lf_df$mx[num_row]
  
  
  # create life expectancy variable
  ex <- rep(0,num_row)
  for (i in 1:num_row){
    ex[i] <- sum(Lx[i:num_row]) / lx[i]
  }
  
  # create health adjusted life years variable 
  
  Lwx <- Lx * (1 - lf_df$pyld_rate)
  
  # create health adjusted life expectancy variable
  ewx <- rep(0,num_row)
  for (i in 1:num_row){
    ewx[i] <- sum(Lwx[i:num_row]) / lx[i]
  }
  
  lf_df$qx <- qx
  lf_df$lx <- lx
  lf_df$dx <- dx
  lf_df$Lx <- Lx
  lf_df$ex <- ex
  lf_df$Lwx <- Lwx
  lf_df$ewx <- ewx
  lf_df
}

# ---- RunDisease ----

## Function to generate age and sex disease life table for baseline and scenario.
## Remission is not modelled.

RunDisease <- function(in_idata,  in_sex, in_mid_age, in_disease, incidence_trends=NA, mortality_trends=NA) {
  # in_idata=MSLT_DF
  # in_sex='male'
  # in_mid_age=17
  # in_disease='dmt2'
  
  # create disease variable for the disease life table function 
  dw_disease <- paste("dw_adj", in_disease, sep = "_")
  incidence_disease <- paste("incidence", in_disease, sep = "_")
  case_fatality_disease <- paste("case_fatality", in_disease, sep = "_")
  
  ## add generic variable names to the source data frame (in_idata)
  in_idata$dw_disease <- in_idata[[dw_disease]]
  in_idata$incidence_disease <- in_idata[[incidence_disease]]
  in_idata$case_fatality_disease <- in_idata[[case_fatality_disease]]
  
  # Select columns for lifetable calculations
  
  ##BZ: back yo using filtering, otherwise the life tables are not run by cohort (age and sex)
  dlt_df <- in_idata %>%
    dplyr::filter(age >= in_mid_age & sex == in_sex) %>% 
    dplyr::select('sex', 'age', dw_disease, incidence_disease, case_fatality_disease)
  
  ##BZ: Rob, line 264 does not filter by age and sex, each disease life table starts at firt age cohort (e.g. 17) and by gender. 
  
  # dlt_df <- in_idata[,colnames(in_idata) %in% c('sex', 'age', 'dw_disease', 'incidence_disease', 'case_fatality_disease')] # dplyr::select(sex, age, dw_disease, incidence_disease, case_fatality_disease)
  
  dlt_df$disease <- in_disease
  
  # are we using modified mortality trends?
  if(is.data.frame(mortality_trends)) {
    # filter to only this cohort's incidence trends
    cohort_mortality_trends <- mortality_trends %>%
      dplyr::filter(sex == in_sex) %>%
      dplyr::select('year',mortality_trend=in_disease) %>%
      dplyr::mutate(row_num=row_number())
    # BELEN: I'm not sure what to do with the incidence trend so I just multiplied it with case_fatality_disease
    dlt_df <- dlt_df %>%
      dplyr::mutate(row_num=row_number()) %>%
      dplyr::inner_join(cohort_mortality_trends, by=c('row_num')) %>%
      dplyr::mutate(case_fatality_disease=case_fatality_disease*mortality_trend) %>%
      dplyr::select('sex', 'age', 'dw_disease', 'incidence_disease', 'case_fatality_disease', 'disease')
  }
  
  # are we using modified incidence trends?
  if(is.data.frame(incidence_trends)) {
    # filter to only this cohort's incidence trends
    cohort_incidence_trends <- incidence_trends %>%
      dplyr::filter(sex == in_sex) %>%
      dplyr::select('year',incidence_trend=in_disease) %>%
      dplyr::mutate(row_num=row_number())
    # BELEN: I'm not sure what to do with the incidence trend so I just multiplied it with incidence_disease
    dlt_df <- dlt_df %>%
      dplyr::mutate(row_num=row_number()) %>%
      dplyr::inner_join(cohort_incidence_trends, by=c('row_num')) %>%
      dplyr::mutate(incidence_disease=incidence_disease*incidence_trend) %>%
      dplyr::select('sex', 'age', 'dw_disease', 'incidence_disease', 'case_fatality_disease', 'disease')
  }
  
  # create list of life table variables. Created as vectors and then added to dataframe. 
  # See see methods in: 1) Concept and original calculations: Barendregt, J. J., et al. (1998). "Coping with multiple morbidity in a life table." Math Popul Stud 7(1): 29-49. 
  # and 2) Latest version, variables below calculated from it: Barendregt, J. J., et al. (2003). "A generic model for the assessment of disease epidemiology: the computational basis of DisMod II." Popul Health Metr 1(1): 4-4.
  
  
  ### lx, qx, wx and vx are intermediate variables, 
  
  lx <- dlt_df$incidence_disease + dlt_df$case_fatality_disease
  qx <-  sqrt((dlt_df$incidence_disease - dlt_df$case_fatality_disease) * (dlt_df$incidence_disease - dlt_df$case_fatality_disease))
  wx <- exp(-1*(lx+qx)/2)
  vx <- exp(-1*(lx-qx)/2)
  
  ## Healthy (Sx), Disease (Cx) and Death (Dx), total (Tx) (control check, has to be 1000), total alive (Ax)
  ## persons years live at risk (PYx), prevalence rate (px), mortality rate (mx)
  ## Remission and mortality from other causes were replaced by zero in the formulas (as we assume no remission and independence of disease mortality with total mortlaity). 
  
  ### First create empty variables
  
  number_of_ages <- nrow(dlt_df)
  Sx <- Cx <- Dx <- Tx  <- Ax <- PYx <- px <- mx <- rep(0,number_of_ages)
  cfds <- dlt_df$case_fatality_disease
  ages <- dlt_df$age
  
  #### Starts with 1000 healthy (Sx) and total (Ax) people. 
  
  Sx[1] <- Ax[1] <- 1000
  
  ##### start with variables without calculation exceptions
  
  ##### variables without exceptions (else includes exception for year one of the simulation)  
  for (i in 2:(number_of_ages-1)){ ##!! this can go to "number_of_ages" now (?)
    if(qx[i-1] > 0){
      
      ### The following five variables are created to simplify Sx, Cx and Dx calculations, and do not form part of the disease life tables.
      vxmwx <- vx[i-1] - wx[i-1]
      SxpCx <- Sx[i-1]+Cx[i-1]
      dqx <- 2 * qx[i-1]
      qxmlx <- qx[i-1] - lx[i-1]
      qxplx <- qx[i-1] + lx[i-1]
      
      ### Healthy (Sx), Diseases (Cx) and Death from the Disease (Dx)
      
      Sx[i] <- Sx[i-1] * (2*vxmwx * cfds[i-1]  + (vx[i-1] * qxmlx + wx[i-1] * qxplx)) / dqx
      Cx[i] <- -1*(vxmwx*(2*(cfds[i-1]  * SxpCx - lx[i-1] * Sx[i-1]) - Cx[i-1] * lx[i-1]) - Cx[i-1] * qx[i-1] * (vx[i-1]+wx[i-1])) / dqx
      Dx[i] <- (vxmwx * (2 * cfds[i-1] * Cx[i-1] - lx[i-1]*SxpCx)- qx[i-1] * SxpCx*(vx[i-1]+wx[i-1]) + dqx * (SxpCx+Dx[i-1]) ) / dqx
    }else{
      Sx[i] <- Sx[i - 1] 
      Cx[i] <- Cx[i - 1]
      Dx[i] <- Dx[i - 1]
    }
  }
  
  
  Tx   <- Sx + Cx + Dx 
  Ax <- Sx + Cx
  
  first_indices <- 1:(number_of_ages-1)
  last_indices <- 2:number_of_ages
  PYx <- (Ax[first_indices] + Ax[last_indices])/2
  mx[first_indices] <- (Dx[last_indices] - Dx[first_indices])/PYx[first_indices]
  mx[mx<0] <- 0
  px[first_indices] <- (Cx[last_indices] + Cx[first_indices])/2/PYx[first_indices]
  
  dlt_df$Tx <- Tx
  dlt_df$mx <- mx
  dlt_df$px <- px
  dlt_df
}

# ---- GetParamters ----

### CAPITAL LETTERS ARE GLOBAL INPUTS
### lower_case are location specific (Melbourne here)

### Get stDev to calculated corrected RR
GetStDevRR <- function(RR, LB, UB){
  # RR=2.82
  # LB=2.35
  # UB=3.38
  SE=exp(((UB-LB)/3.92))
  # stDevRR=exp(sqrt(exp((2*log(RR) + 2*((UB-LB)/3.92)*(log(RR))^2)) - exp((2*log(RR) + ((UB-LB)/3.92)*(log(RR))^2))))
  return(SE) ### Belen to check calculation 
}                   

GetParamters <- function(NSAMPLES = 1,
                                    matched_population=matched_population, # from running step two Melbourne Model
                                    MMET_CYCLING = MMET_CYCLING,
                                    MMET_WALKING = MMET_WALKING,
                                    PA_DOSE_RESPONSE_QUANTILE = F,
                                    location_deaths_periodic="Victoria",
                                    location_deaths_projections="Victoria",
                                    location_population="Greater Melbourne"){
 parameters <- list()

 ### FIXED INPUTS
 
  mslt_general="Data/processed/mslt/mslt_df.csv"
  death_rate_periodic="Data/processed/mslt/deaths_periodic.csv"
  death_rates_projections="Data/processed/mslt/deaths_projections.csv"
  population_data="Data/original/abs/population_census.xlsx"
  disease_inventory_location= "Data/original/ithimr/disease_outcomes_lookup.csv"
 
 
  DISEASE_INVENTORY <- read.csv(disease_inventory_location,as.is=T,fileEncoding="UTF-8-BOM")
 
  MSLT_DF <- read.csv(mslt_general,as.is=T,fileEncoding="UTF-8-BOM")
  
  death_rate_periodic <- read.csv(death_rate_periodic,as.is=T,fileEncoding="UTF-8-BOM") %>% dplyr::filter(location == location_deaths_periodic) %>%
    dplyr::select("sex_age_cat", "mx")
  MSLT_DF <- left_join(MSLT_DF, death_rate_periodic)
  
  death_rates <- read.csv(death_rates_projections,as.is=T,fileEncoding="UTF-8-BOM") %>% dplyr::filter(location == "Victoria", assumption == "medium")

  source("Scripts/data_prep/population_prep.R")
  population <- GetPopulation(
    population_data=population_data,
    location= location_population)
  MSLT_DF <- left_join(MSLT_DF, population)
  MSLT_DF$age <- as.numeric(MSLT_DF$age)
  
  ### Add fixed inputs to parameters list
  
  parameters$MSLT_DF <- MSLT_DF ### includes death_rate_periodic
  parameters$death_projections <- death_rates ### death projection data
  parameters$persons_matched<- persons_matched
  parameters$DISEASE_INVENTORY <- DISEASE_INVENTORY
  parameters$disease_inventory_location <- disease_inventory_location  ### added this here to avoid issues with gen_pa_rr_wrapper and health_burden2 in CalculateMOdel
  parameters$PA_DOSE_RESPONSE_QUANTILE <- PA_DOSE_RESPONSE_QUANTILE
  parameters$population <- population  
  
### RANDOM INPUTS  
  ### Use random inputs if NSAMPLES is >1
  
  if (NSAMPLES > 1) {
  
  ### Variables with normal distribution
  ### MMETS
    
  ### Get Stdev diabetes
    DIABETES_IHD_RR_F <- c(2.82, GetStDevRR(2.82, 2.35, 3.38))
    DIABETES_STROKE_RR_F <- c(2.28, GetStDevRR(2.28, 1.93, 2.69))
    DIABETES_IHD_RR_M <- c(2.16, GetStDevRR(2.16, 1.82, 2.56)) 
    DIABETES_STROKE_RR_M <- c(1.83, GetStDevRR(1.83, 1.60, 2.08))
    
  normVariables <- c("MMET_CYCLING",
                     "MMET_WALKING", 
                     "DIABETES_IHD_RR_F",
                     "DIABETES_STROKE_RR_F",
                     "DIABETES_IHD_RR_M",
                     "DIABETES_STROKE_RR_M"
                     )
  for (i in 1:length(normVariables)) {
    name <- normVariables[i]
    val <- get(normVariables[i])
    if (length(val) == 1) {
      assign(name, val, envir = .GlobalEnv)
    } else {
      # Use mean and sd values in log form
      parameters[[name]] <-
        rlnorm(NSAMPLES, log(val[1]), log(val[2]))
    }
  }
  ### RR DIABATES
  
  ### To do
  
  ### Variables with quantiles distributions
  ## PA DOSE RESPONSE 
  # parameters$PA_DOSE_RESPONSE_QUANTILE <- PA_DOSE_RESPONSE_QUANTILE
  if(PA_DOSE_RESPONSE_QUANTILE == T ) {
    pa_diseases <- subset(DISEASE_INVENTORY,physical_activity==1)
    dr_pa_list <- list()
    for(disease in pa_diseases$pa_acronym)
      parameters[[paste0('PA_DOSE_RESPONSE_QUANTILE_',disease)]] <- runif(NSAMPLES,0,1)
    }
  }
  
  else {
    parameters$MMET_CYCLING <- 4.63 #c(4.63, (1.2) #lognormal  
    parameters$MMET_WALKING <- 2.53 #c(2.53, 1.1)  #lognormal 
    parameters$DIABETES_IHD_RR_F <- 2.82 ## c(2.82, CI (2.35, 3.38) get SD from CI
    parameters$DIABETES_STROKE_RR_F <- 2.28 ## c(2.28) CI (1.93, 2.69) get SD from CI
    parameters$DIABETES_IHD_RR_M <- 2.16 ## c(2.16, CI (1.82, 2.56) get SD from CI
    parameters$DIABETES_STROKE_RR_M <- 1.83 ## c(1.83) CI (1.60, 2.08) get SD from CI
  }
  
  
   parameters
}

# ---- CalculateModel ----

CalculationModel <- function(parameters, seed=1){
  
  list2env(parameters,globalenv()) ### move all elements in parameters list to global environment 
  set.seed(seed)
  
  ### Get functions
  source("Scripts/data_prep/mmet_pp.R")
  source("Scripts/ithim-r_wrappers.R")
  
  #################################################### Calculate PIFs by age and sex groups #####################################################
  # 3 calculations: mmets_pp, RR_PA_calculations and pif
  
  ### 1) Generate marginal mets for matched population, then used to derive RRs per person
  mmets_pp <- calculateMMETSperPerson(
    matched_pop_location = persons_matched,
    MMET_CYCLING = MMET_CYCLING, 
    MMET_WALKING = MMET_WALKING
  )
  #### Create age groups for easier presentation changes and convert variables to factors for summaries
  mmets_pp <- mmets_pp %>%
    dplyr::mutate(age_group = as.factor(case_when(
      age <  18             ~ "0 to 17" ,
      age >= 18 & age <= 40 ~ "18 to 40",
      age >= 41 & age <= 65 ~ "41 to 65",
      age >= 65             ~ "65 plus"))) %>%
    mutate(sex =as.factor(sex)) 
  
  ### 2) Create RRs per person to calculate PIFs
  
  #### Relative risks of physical activity
  
  RR_PA_calculations <- gen_pa_rr_wrapper(
    mmets_pp,
    disease_inventory_location="Data/original/ithimr/disease_outcomes_lookup.csv", ### Also in paramters list
    # location of ithmr default dose response data:
    dose_response_folder=paste0(file.path(find.package('ithimr',lib.loc=.libPaths()), 'extdata/global'),
                                "/dose_response/drpa/extdata"),
    PA_DOSE_RESPONSE_QUANTILE=PA_DOSE_RESPONSE_QUANTILE ### Alan, if this is true, it picks up values from the RR quantiles in the paramters list
  )
  
  ### 3) Calculate PIFs by age and sex groups
  
  pif <- health_burden_2(
    ind_ap_pa_location=RR_PA_calculations,
    disease_inventory_location="Data/original/ithimr/disease_outcomes_lookup.csv", ### Also in paramters list
    demographic_location="Data/processed/DEMO.csv",
    combined_AP_PA=F,
    calculate_AP=F
  ) 
  
  pif_age_sex <- pif[[2]] %>% dplyr::rename(age=age_group_2) %>%
    dplyr::slice(rep(1:dplyr::n(), each = 5))
  
  age <- rep(seq(16,100,1), times = 2)
  
  pif_age_sex$age <- age
  
  pif_age_sex <- pif_age_sex %>% dplyr::filter(age !=16)
  
  ############################################ Calculate MSLT ####################################################################
  
  
  ### Inputs
  pif_expanded <- pif_age_sex
  
  ### Alan, should we move this to the parameters list??? or above with age and sex??
  include <- read.csv(disease_inventory_location,as.is=T,fileEncoding="UTF-8-BOM") %>% 
    dplyr::filter(physical_activity == 1)
  
  DISEASE_SHORT_NAMES <- DISEASE_SHORT_NAMES %>%
    dplyr::filter(acronym %in% include$acronym)
  
  
  ### Steps 
  # 1) Run general life table baseline
  # 2) Run disase life tables baseline
  # 3) Run scenario life tables (where incidence is mofified by pif_expanded)
  # 4) Collect changes in mx and pylds from differences between baseline and sceanrio disease life tables
  # 5) Recalculate general life table with mx and totalpylds modified by 4
  
  # 1) Run general life table baseline
  general_life_table_list_bl <- list()
  
  # dataframe of the age and sex cohorts (crossing just does a cross product)
  age_sex_cohorts <- crossing(data.frame(age=i_age_cohort),
                              data.frame(sex=c('male', 'female'))) %>%
    dplyr::mutate(cohort=paste0(age,"_",sex))
  
  for (i in 1:nrow(age_sex_cohorts)){
    suppressWarnings(
      general_life_table_list_bl[[i]] <- RunLifeTable(
        in_idata    = MSLT_DF,
        in_sex      = age_sex_cohorts$sex[i],
        in_mid_age  = age_sex_cohorts$age[i],
        death_rates = death_projections ## Belen, add option to run static
      ))
    names(general_life_table_list_bl)[i] <- age_sex_cohorts$cohort[i]
  }
  
  # convert the list of dataframes to single dataframes
  general_life_table_bl <- bind_rows(general_life_table_list_bl, .id = "age_group") %>%
    mutate(age_group = as.numeric(gsub("_.*","",age_group)))
  
  
  
  # 2) Run disase life tables baseline
  
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
  
  # 3) Run scenario life tables (where incidence is mofified by pif_expanded)
  
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
  
  
  # 4) Collect changes in mx and pylds from differences between baseline and sceanrio disease life tables
  
  ### Sum mortality rate and pylds change scenarios
  mx_pylds_sc_total_disease_df <- disease_life_table_sc %>%
    group_by(age_group,sex,age) %>%
    summarise(mortality_sum=sum(diff_mort_disease,na.rm=T),
              pylds_sum=sum(diff_pylds_disease,na.rm=T)) %>%
    ungroup() %>%
    mutate(age_sex_cohort=paste0(age_group,'_',sex))
  
  
  # 5) Recalculate general life table with mx and totalpylds modified by 4
  
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
    td3 <- death_projections %>%
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
  
  
  # 6) Generate outputs dataframe
  
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
  
  
  ######## Dataframe with all outputs by age and sex cohort over the simulation years (years of the cohort)
  output_df <- inner_join(disease_combined,
                          general_lf,
                          by=c("age","sex","age_group"))
  
  # 7) Summary data frame by age and sex and total 
  
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
  
  
  #### Add population numbers for presentation purposes
  population <- population %>% 
    rename(cohort = sex_age_cat) %>%
    dplyr::filter(cohort %in% unique(output_df$cohort))
  
  
  ##################### Below outcomes for presentation ####################################################
  
  # Table: Life expectancy and health adjusted life expectancy 
  
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
  
  # Table: Life years and health adjusted life years ----
  
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
  
  # Table: Diseases deaths, incidence and ylds ----
  
  output_diseases_change <- output_df %>% 
    group_by(Gender, `Age group`, cohort, .add=TRUE) %>%
    summarise_if(is.numeric, funs(sum)) %>% 
    mutate_if(is.numeric, round) %>%
    left_join(population) %>%
    relocate(population, .after = Gender)%>%
    rename('Population cohort'=population) %>%
    dplyr::select(-cohort) %>%
    dplyr::select(`Age group`, Gender, matches("diff_dmt2|diff_ishd|diff_strk|diff_carc|diff_copd|diff_tbalc|diff_brsc|diff_utrc|diff_lri"))
  
  
  
  
  return(list(mmets=mmets_pp, output_df=output_df, output_df_agg_sex=output_df_agg_sex, output_df_agg_all=output_df_agg_all,
              output_life_expectancy_change=output_life_expectancy_change,output_life_years_change=output_life_years_change,
              output_diseases_change=output_diseases_change))
  
}