suppressPackageStartupMessages(library(dplyr)) # for manipulating data



calculateMMETSperPerson <- function(matched_pop_location, mets, MMET_CYCLING,MMET_WALKING,MMET_MOD,MMET_VIG, TOTAL = F) {
matched_pop_location = "Data/processed/matched_pop.csv"
# mets = "Data/Physical Activity/met_values.csv" 
  
  TOTAL = F #(only include leisure PA), T = include leisure and work PA
  ## Replace with PA Compedium values  
  MMET_CYCLING = 4.63 ## ITHIMR values, how were these obtained?
  MMET_WALKING = 2.53
  
  
  # mets <- read.csv(mets,as.is=T,fileEncoding="UTF-8-BOM")
  # 
  # MMET_MOD <- dplyr::filter(mets, Category.model == "moderate leisure") 
  # MMET_MOD <- mean(MMET_MOD$METS - 1)  
  # 
  # MMET_VIG <- dplyr::filter(mets, Category.model == "vigorous leisure") 
  # MMET_VIG <- mean(MMET_VIG$METS - 1) 
  MMET_MOD <- 3.5 # As in meta analysis diabetes smith et al
  MMET_VIG <- 7

  synth_pop <- read.csv(matched_pop_location,as.is=T,fileEncoding="UTF-8-BOM") %>%
    dplyr::mutate(participant_id = row_number())
  mmets_pp <- synth_pop %>% 
    dplyr::select(participant_id, sex, age, dem_index, mod_leis_hr, mod_work_hr, age_group_scen, 
                  vig_leis_hr, mod_work_hr, walk_rc, participant_wt,
                  starts_with("time") & contains(c("walking", "bicycle"))) %>%
    replace(is.na(.), 0) %>%
    dplyr::mutate(base_mmet = ifelse(TOTAL == F, mod_leis_hr, (mod_leis_hr + mod_work_hr))  * MMET_MOD + 
                                       ifelse(TOTAL == F, vig_leis_hr, (vig_leis_hr + mod_work_hr))  * MMET_VIG +
                    walk_rc * MMET_WALKING + time_base_walking * MMET_WALKING +
                    time_base_bicycle * MMET_CYCLING) %>%
    dplyr::mutate(scen1_mmet = ifelse(TOTAL == F, mod_leis_hr, (mod_leis_hr + mod_work_hr))  * MMET_MOD + 
                    ifelse(TOTAL == F, vig_leis_hr, (vig_leis_hr + mod_work_hr))  * MMET_VIG + walk_rc *
                    MMET_WALKING + time_scen_walking * MMET_WALKING +
                    time_scen_bicycle * MMET_CYCLING)
  
  return(mmets_pp)
}
