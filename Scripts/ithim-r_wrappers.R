library(dplyr)
library(readr)
library(data.table)

# runs a local version of gen_pa_rr (and PA_dose_response) so ithim-r library doesn't need to be called
# ithim-r still needs to be installed so we can access the dose response folder
gen_pa_rr_wrapper <- function(mmets_pp_location,disease_inventory_location,dose_response_folder,PA_DOSE_RESPONSE_QUANTILE) {
  # mmets_pp_location="Data/Processed/mets_test.csv"
  # disease_inventory_location="Data/Processed/disease_outcomes_lookup.csv"
  # dose_response_folder="/home/alan/R/x86_64-pc-linux-gnu-library/3.6/ithimr/extdata/global//dose_response/drpa/extdata"
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
        mmets_pp[[paste("RR_pa", scen, pa_n, sep = "_")]] <- return_vector$rr[(1 + 
                                                                                 (i - 1) * nrow(mmets_pp)):(i * nrow(mmets_pp))]
      }
    }
    mmets_pp
  }
  
  PA_dose_response <- function(cause, dose, confidence_intervals = F) {
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
  
  mmets_pp <- read.csv(mmets_pp_location,as.is=T,fileEncoding="UTF-8-BOM")
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
  ind_ap_pa_location="Data/Processed/RR_PA_calculations.csv"
  disease_inventory_location="Data/Processed/disease_outcomes_lookup.csv"
  demographic_location="Data/Processed/DEMO.csv"
  combined_AP_PA=F
  calculate_AP=F

  ind_ap_pa <- read.csv(ind_ap_pa_location,as.is=T,fileEncoding="UTF-8-BOM")
  DISEASE_INVENTORY <- read.csv(disease_inventory_location,as.is=T,fileEncoding="UTF-8-BOM")
  DEMOGRAPHIC <- read.csv(demographic_location,as.is=T,fileEncoding="UTF-8-BOM")
  
  # filtering down to columns with 'mmet' in their name
  SCEN_SHORT_NAME <- colnames(ind_ap_pa)[grep("mmet",colnames(ind_ap_pa))]
  # removing '_mmet' to find the base scenario and scenario names
  SCEN_SHORT_NAME <- gsub("_mmet","",SCEN_SHORT_NAME)
  
  pop_details <- DEMOGRAPHIC
  pif_scen <- pop_details
  pif_scen_2 <- pop_details
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
        pif_table <- setDT(ind_ap_pa[,colnames(ind_ap_pa)%in%c(base_var,'dem_index', 'participant_wt')])
        setnames(pif_table,base_var,'outcome')
        pif_ref <- pif_table[,.(sum(outcome)),by='dem_index']
        pif_ref_2 <- pif_table 
        ## sort pif_ref
        setorder(pif_ref,dem_index)
        for (index in 1:length(scen_vars)){
          # set up naming conventions
          scen <- scen_names[index]
          scen_var <- scen_vars[index]
          pif_name <- paste0('pif_',ac)
          # Calculate PIFs for selected scenario
          pif_table <- setDT(ind_ap_pa[,colnames(ind_ap_pa)%in%c(scen_var,'dem_index', 'participant_wt')])
          setnames(pif_table,scen_var,'outcome')
          pif_temp <- pif_table[,.(sum(outcome)),by='dem_index']
          pif_temp_2 <- pif_table 
          
          
          ## sort pif_temp
          setorder(pif_temp,dem_index)
          pif_scen[[pif_name]] <- (pif_ref[,V1] - pif_temp[,V1]) / pif_ref[,V1] 
          
        # ALAN, trying to add the same calculations as above, but using weights, not working 
        # pif_wt[[pif_name]] <- pif_temp_2 %>% mutate(pif=(pif_ref_2$outcome - .[,'outcome']) / pif_ref_2$outcome) %>%
        # srvyr::as_survey_design(weights = participant_wt)
        # pif_scen_2 <-  pif_wt  %>%
        # group_by(dem_index) %>%
        # dplyr::summarize(srvyr::survey_mean(pif))
        }
      }
    }
  }
  return(pif_scen)
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






