suppressPackageStartupMessages(library(dplyr)) # for manipulating data

calculateMMETSperPerson <- function(matched_pop_location,MMET_CYCLING,MMET_WALKING,MMET_MOD,MMET_VIG) {
  # matched_pop_location = "Data/Processed/matched_pop.csv"
  # MMET_CYCLING = 4.63
  # MMET_WALKING = 2.53
  # MMET_MOD = 3.5
  # MMET_VIG = 7

  synth_pop <- read.csv(matched_pop_location,as.is=T,fileEncoding="UTF-8-BOM") %>%
    dplyr::mutate(participant_id = row_number())
  mmets_pp <- synth_pop %>% 
    dplyr::select(participant_id, sex, age, dem_index, mod_hr, vig_hr, walk_rc, participant_wt,
                  starts_with("time") & contains(c("pedestrian", "bicycle")),
                  work_ltpa_marg_met) %>%
    replace(is.na(.), 0) %>%
    dplyr::mutate(base_mmet = mod_hr * MMET_MOD + vig_hr * MMET_VIG +
                    walk_rc * MMET_WALKING + time_base_pedestrian * MMET_WALKING +
                    time_base_bicycle * MMET_CYCLING) %>%
    dplyr::mutate(scen1_mmet = mod_hr * MMET_MOD + vig_hr * MMET_VIG + walk_rc *
                    MMET_WALKING + time_scen_pedestrian * MMET_WALKING +
                    time_scen_bicycle * MMET_CYCLING)
  
  return(mmets_pp)
}
