suppressPackageStartupMessages(library(dplyr)) # for manipulating data

### Functions to generate mmets per person with option to include or not include work related PA.

calculateMMETSperPerson <- function(matched_pop_location, MMET_CYCLING,MMET_WALKING,MMET_MOD,MMET_VIG,TOTAL=F) {
  # matched_pop_location = "Data/processed/matched_pop.csv"
  # 
  MMET_MOD <- 3.5 # As in meta analysis diabetes smith et al
  MMET_VIG <- 7
  # MMET_CYCLING <- 4.63 
  # MMET_WALKING <- 2.53
  # matched_pop_location <- persons_matched
  
  # if matched_pop_location is a file location, read the csv. If not, then
  # use it as a dataframe.
  synth_pop <- NULL
  if(is.character(matched_pop_location)) {
    synth_pop <- read.csv(matched_pop_location, as.is=T, fileEncoding="UTF-8-BOM")
  }
  if(!is.character(matched_pop_location)) {
    synth_pop <- matched_pop_location
  }
  
  synth_pop <- synth_pop %>%
    dplyr::mutate(participant_id = row_number()) %>% 
    dplyr::select(participant_id, sex, age, dem_index, age_group_2, mod_leis_hr, mod_work_hr, mod_total_hr, vig_total_hr,
                  vig_leis_hr, mod_work_hr, walk_rc, participant_wt,
                  starts_with("time") & contains(c("walking", "bicycle"))) %>%
    replace(is.na(.), 0) 
  ### mmets= mmets transport walking + mmets transport cycling + mmets mod exercise + mmets vig exercise
  ifelse(TOTAL==F,
         mmets_pp <- synth_pop %>%
           dplyr::mutate(base_mmet = mod_leis_hr * MMET_MOD + vig_leis_hr * MMET_VIG +
                           walk_rc * MMET_WALKING + time_base_walking * MMET_WALKING +
                           time_base_bicycle * MMET_CYCLING) %>%
           dplyr::mutate(scen1_mmet = mod_leis_hr * MMET_MOD + vig_leis_hr   * MMET_VIG + walk_rc *
                           MMET_WALKING + time_scen_walking * MMET_WALKING +
                           time_scen_bicycle * MMET_CYCLING),
         mmets_pp <- synth_pop  %>%### mmets= mmets transport walking + mmets transport cycling + mmets mod exercise and work + mmets vig exercise and work
           dplyr::mutate(base_mmet = mod_total_hr * MMET_MOD + vig_total_hr * MMET_VIG +
                           walk_rc * MMET_WALKING + time_base_walking * MMET_WALKING +
                           time_base_bicycle * MMET_CYCLING) %>%
           dplyr::mutate(scen1_mmet = mod_total_hr * MMET_MOD + vig_total_hr   * MMET_VIG + walk_rc *
                           MMET_WALKING + time_scen_walking * MMET_WALKING +
                           time_scen_bicycle * MMET_CYCLING)
         
  )
  return(mmets_pp)
}

# mmets_pp_without <- calculateMMETSperPerson(
#   MMET_MOD=3.5, # As in meta analysis diabetes smith et al
#   MMET_VIG=7,
#   MMET_CYCLING=4.63,
#   MMET_WALKING=2.53,
#   matched_pop_location=persons_matched,
#   TOTAL = F)
# 
# mmets_pp_with <- calculateMMETSperPerson(
#   MMET_MOD=3.5, # As in meta analysis diabetes smith et al
#   MMET_VIG=7,
#   MMET_CYCLING=4.63,
#   MMET_WALKING=2.53,
#   matched_pop_location=persons_matched,
#   TOTAL = T)