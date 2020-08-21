suppressPackageStartupMessages(library(dplyr)) # for manipulating data

calculateMMETSperPerson <- function(matched_pop_location) {
  # matched_pop_location = "Data/Processed/matched_pop.csv"

  synth_pop <- read.csv(matched_pop_location,as.is=T,fileEncoding="UTF-8-BOM") %>%
    dplyr::mutate(participant_id = row_number())
  mmets_pp <- synth_pop %>% 
    dplyr::select(participant_id, sex, age, dem_index, mod_hr, vig_hr, walk_rc,
                  starts_with("time") & contains(c("pedestrian", "bicycle")),
                  work_ltpa_marg_met) %>%
    replace(is.na(.), 0) %>%
    dplyr::mutate(base_mmet = mod_hr * MMET_MOD + vig_hr * MMET_VIC +
                    walk_rc * MMET_WALKING + time_base_pedestrian * MMET_WALKING +
                    time_base_bicycle * MMET_CYCLING) %>%
    dplyr::mutate(scen1_mmet = mod_hr * MMET_MOD + vig_hr * MMET_VIC + walk_rc *
                    MMET_WALKING + time_scen_pedestrian * MMET_WALKING +
                    time_scen_bicycle * MMET_CYCLING)
  
  return(mmets_pp)
}


