### Test developing montecarlo simulation
library(boot)
library(MonteCarlo)
library(pse)
library(SimDesign)

#### Paramters
NSAMPLES <- 1000
MMET_CYCLING <- c(4.63, 1.2)
MMET_WALKING <- c(2.53,1.1) 
PA_DOSE_RESPONSE_QUANTILE <- TRUE

parameters_test <- list()
synth_pop <- synth_pop %>% 
  dplyr::select(participant_id, sex, age, dem_index,
                starts_with("time") & contains(c("pedestrian", "bicycle")),
                work_ltpa_marg_met) %>%
  replace(is.na(.), 0)

normVariables <- c("MMET_CYCLING",
                   "MMET_WALKING")

for (i in 1:length(normVariables)) {
  name <- normVariables[i]
  val <- get(normVariables[i])
  # Deterministic, pick mean value, else, distribution
  if (length(val) == 1) {
    assign(name, val, envir = .GlobalEnv)
  } else {
    # Use mean and sd values in log form
    parameters_test[[name]] <-
      rlnorm(NSAMPLES, log(val[1]), log(val[2]))
  }
}

# parameters_test[["PA_DOSE_RESPONSE_QUANTILE"]] <- FALSE


PA_DOSE_RESPONSE_QUANTILE <- T

if(PA_DOSE_RESPONSE_QUANTILE == T ) {
  pa_diseases <- subset(DISEASE_INVENTORY,physical_activity==1 & pa_acronym !="all_cause" & pa_acronym !="total_cancer")
  dr_pa_list <- list()
 
  for(disease in pa_diseases$pa_acronym)
    parameters_test[[paste0('PA_DOSE_RESPONSE_QUANTILE_',disease)]] <- runif(NSAMPLES,0,1)
}


### SOme graphs
jpeg('histogramTest.jpg')
data <- parameters_test[["MMET_WALKING"]]
hist(data)
dev.off()
# 
# index <- 1
test_uncertainty <- function(synth_pop, MMET_CYCLING, MMET_WALKING) {
  
    mmets_pp <- synth_pop %>% 
      dplyr::select(participant_id, sex, age, dem_index,
                  starts_with("time") & contains(c("pedestrian", "bicycle")),
                  work_ltpa_marg_met) %>%
    replace(is.na(.), 0) 
    for (i in 1:nrow(synth_pop)) 
      ## select each row
      # mmets[[index]] <- mmets_pp[[i]] %>%
        
    dplyr::mutate(base_mmet = work_ltpa_marg_met + time_base_pedestrian * MMET_WALKING + time_base_bicycle * MMET_CYCLING) %>%
    dplyr::mutate(scen1_mmet = work_ltpa_marg_met + time_scen_pedestrian * MMET_WALKING + time_scen_bicycle * MMET_CYCLING)
      
  
  ### SECOND WE CALCULATE RRS PER PERSON FROM MMET PER PERSON AND RRS DATA FROM ITHIMR
  #### TO DO: HOW IS UNCERTAINTY IN RRS INCORPORATED (SEE FILES e.g. breast_cancer_mortality)
  #### TO Do: WHICH RRS ARE USED IN FUNCTION? CHECK SOURCE FUNCTION gen_pa_rr (for example, some rrs have all and other mortality)  
  #### TO DO: check in source formula which RRs are applied (all, mortality, incidence)
  ### Alan I added the below parameter, when we do the uncertainty analysis we change to TRUEMMET_CYCLING <- c(log(4.63),log(1.2)) # 4.63
  
  # RR_PA_calculations <- ithimr::gen_pa_rr(mmets_pp)
  
  return(mmets_pp)
}


outcomes_test <- list()
index <-  1
for (i in nrow(synth_pop)) {
  
  normVariables <- c("MMET_CYCLING",
                     "MMET_WALKING")
  
  for (i in 1:length(normVariables)) {
    name <- normVariables[i]
    val <- get(normVariables[i])
    # Deterministic, pick mean value, else, distribution
    if (length(val) == 1) {
      assign(name, val, envir = .GlobalEnv)
    } else {
      # Use mean and sd values in log form
      parameters_test[[name]] <-
        rlnorm(NSAMPLES, log(val[1]), log(val[2]))
    }
  }
  
  
  outcomes_test[[index]] <- test_uncertainty(synth_pop[[i]], MMET_CYCLING, MMET_WALKING)
  
  index <- index + 1
}

t.seed(0)
system.time(x1 <- sapply(1:NSAMPLES, test_uncertainty(synth_pop, parameters_test$MMET_CYCLING, parameters_test$MMET_WALKING)))

parameters_test$outcomes <- lapply(1:1000, test_uncertainty, parameters_test,
                                mc.cores = ifelse(Sys.info()[['sysname']] == "Windows",  1,  1000))

R   <-   5000

mu   <-   10

sdev   <-   2

N   <-   c ( 5,  30,   60 ) ### Colnames for mmets, and other inputs function

res   <-   matrix (0, R, 3) ## Find out how to trun dataframe to matrix

colnames (res)   <-   N ### Col names data frame, turn data frame to matrix

set.seed ( 77 )

for (i   in   N){             
  for (r   in   1 : R){                   
    dat   <-   rnorm ( n   = i,   mean   = mu,   sd   = sdev)                     
    res  [r,   as.character (i)]   <-   mean (dat)
  }
}

apply (res,   2, mean)

apply (res,   2, sd)
