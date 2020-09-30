suppressPackageStartupMessages(library(dplyr)) # for manipulating data



calculateMMETSperPerson <- function(matched_pop_location, MMET_CYCLING,MMET_WALKING,MMET_MOD,MMET_VIG) {
# matched_pop_location = "Data/processed/matched_pop.csv"

  MMET_MOD <- 3.5 # As in meta analysis diabetes smith et al
  MMET_VIG <- 7
 # matched_pop_location <- persons_matched
  
  synth_pop <-  matched_pop_location %>% #read.csv(matched_pop_location,as.is=T,fileEncoding="UTF-8-BOM") 
    dplyr::mutate(participant_id = row_number())
  mmets_pp <- synth_pop %>% 
    dplyr::select(participant_id, sex, age, dem_index, age_group_2, mod_leis_hr, mod_work_hr, mod_total_hr, vig_total_hr,
                  vig_leis_hr, mod_work_hr, walk_rc, participant_wt,
                  starts_with("time") & contains(c("walking", "bicycle"))) %>%
    replace(is.na(.), 0) %>%
    dplyr::mutate(base_mmet = mod_leis_hr * MMET_MOD + vig_leis_hr * MMET_VIG +
                    walk_rc * MMET_WALKING + time_base_walking * MMET_WALKING +
                    time_base_bicycle * MMET_CYCLING) %>%
    dplyr::mutate(scen1_mmet = mod_leis_hr * MMET_MOD + vig_leis_hr   * MMET_VIG + walk_rc *
                    MMET_WALKING + time_scen_walking * MMET_WALKING +
                    time_scen_bicycle * MMET_CYCLING)
  
  return(mmets_pp)
}
