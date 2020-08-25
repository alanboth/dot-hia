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

  



