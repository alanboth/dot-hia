#### Function to create MMETs for mslt_code

suppressPackageStartupMessages(library(dplyr)) # for manipulating data

### The functions allows to add minutes walking or cycling

### Alan I added functions with PA for Australia wide

calculateMMETSperPerson_AUS <- function(pa_location="Data/Physical activity/NHS2017-18_CSV/NHS17SPB.csv", 
                                        hh_location="Data/Physical activity/NHS2017-18_CSV/NHS17HHB.csv",
                                        MMET_CYCLING=4.62,
                                        MMET_WALKING=2.53,
                                        MMET_MOD=3.5,
                                        MMET_VIG=7,
                                        SCEN_WALK = 0, # user defined
                                        SCEN_CYCLE = 0,
                                        age_input = c("15 to 19","20 tp 24", "25 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 59", "60 to 64", 
                                                      "65 to 69", "70 to 74", "75 to 79", "80 to 84", "85 +" ),
                                        sex_input = c("male", "female")) # user defined 
  { 
  
  mmets_pp_Aus <- calculatePersonsPA(pa_location, hh_location) %>%
    
    replace(is.na(.), 0) %>%
    dplyr::mutate(time_base_pedestrian = walk_rc + walk_trans) %>%
    dplyr::mutate(time_base_bicycle = 0) %>%
    dplyr::mutate(time_scen_pedestrian = ifelse(age_group_scen %in% age_input &
                                                  sex %in% sex_input, time_base_pedestrian + SCEN_WALK/60, time_base_pedestrian)) %>%
    dplyr::mutate(time_scen_bicycle = ifelse(age_group_scen %in% age_input &
                                               sex %in% sex_input, time_base_bicycle + SCEN_CYCLE/60, time_base_bicycle))  %>%
    dplyr::mutate(base_mmet = mod_hr * MMET_MOD + vig_hr * MMET_VIG +
                    time_base_pedestrian * MMET_WALKING + time_base_bicycle * MMET_CYCLING) %>%
    dplyr::mutate(scen1_mmet = mod_hr * MMET_MOD + vig_hr * MMET_VIG +
                    time_scen_pedestrian * MMET_WALKING +
                    time_scen_bicycle * MMET_CYCLING)
  

  return(mmets_pp_Aus)
}


# test <- calculateMMETSperPerson_AUS(age_input = "15 to 19", SCEN_WALK = 60)
