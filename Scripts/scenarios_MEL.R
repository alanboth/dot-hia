
#### Function to create scenario trips to use in mslt_code

suppressPackageStartupMessages(library(dplyr)) # for manipulating data

### Example of replacing car trips of less than 5km with walking
### Inputs: Data set, speed walking, speed cycling, distance to replce walking, distance to replace cycling)



calculateScenarioMel <- function(trips_melbourne = in_data, 
                                 speed = in_speed,
                              age_input = c("0 to 17", "18 to 40", "41 to 65", "66 plus"),
                              sex_input = c("male", "female"), 
                              original_mode = "car" , # Just car trips can be replaced
                              replace_mode_walk = T,
                              replace_mode_cycle = T,
                              distance_replace_walk = "< 2km",  #c(">10km",  "6-10km", "< 2km",  "2-5km"),
                              distance_replace_cycle = "2-5km",  #c(">10km",  "6-10km", "< 2km",  "2-5km"),
                              purpose_input = c("Leisure", "Shopping", "Work related", "Pick-up or drop-off someone/something", "personal business",
                                                "Other", "accompany someone", "education","at or go home"), 
                              day = c("weekday", "weekend day")) {
                          


    
    # in_data="Data/processed/trips_melbourne.csv"
    # in_speed="Data/processed/speed_trips_melbourne.csv"
    
  
    trips_melbourne <- read.csv(in_data,as.is=T,fileEncoding="UTF-8-BOM") %>%
      dplyr::mutate(trip_mode=case_when(trip_mode=="pedestrian" ~ 'walking', 
                                                  trip_mode=="bus" ~ 'public.transport', 
                                                  trip_mode=="tram" ~ 'public.transport', 
                                                  trip_mode=="train" ~ 'public.transport',
                                                  trip_mode=="motorcycle" ~ 'other',
                                                  TRUE ~ tolower(trip_mode))) ## Add age groups to facilitate selection above and matching  
    speed <- read.csv(in_speed,as.is=T,fileEncoding="UTF-8-BOM")
    
    
    #### create column in trips_melbourne with speed data for age and sex (use median)
    walk_speed <- speed %>% dplyr::filter(activity=="walking") %>% rename(walk_mean_speed = mean) %>% dplyr::select(age_group, sex, walk_mean_speed)
    cycle_speed <- speed %>% dplyr::filter(activity=="bicycle") %>% rename(cycle_mean_speed = mean) %>% dplyr::select(age_group, sex, cycle_mean_speed)
    
    trips_melbourne <- trips_melbourne %>% inner_join(walk_speed, by=c("age_group", "sex"))
    
    trips_melbourne <- trips_melbourne %>% inner_join(cycle_speed, by=c("age_group", "sex"))
    
    
    trips_melbourne_scenarios <- trips_melbourne %>%
      dplyr::rename(trip_mode_base = trip_mode,
                    trip_duration_base = trip_duration,
                    trip_distance_base = trip_distance) %>%
      dplyr::mutate(trip_mode_scen = ifelse(trip_mode_base == original_mode 
                                               & dist_cat == distance_replace_walk 
                                               & age_group %in% age_input 
                                               & sex %in% sex_input 
                                               & trip_purpose %in% purpose_input 
                                               & day_type %in% day
                                               & replace_mode_walk == T, "walking",
                                            ifelse(trip_mode_base == original_mode 
                                            & dist_cat == distance_replace_cycle 
                                            & age_group %in% age_input 
                                            & sex %in% sex_input 
                                            & trip_purpose %in% purpose_input 
                                            & day_type %in% day
                                            & replace_mode_cycle == T, "bicycle", trip_mode_base))) %>%

      ## trip distance is the same, but time changes
      dplyr::mutate(trip_distance_scen = trip_distance_base) %>% 
      dplyr::mutate(trip_duration_scen = ifelse(trip_mode_scen == "walking" & replace_mode_walk == T,
                                                60*trip_distance_scen/walk_mean_speed,
                                                ifelse(trip_mode_scen=="bicycle" & replace_mode_cycle == T,
                                                      60*trip_distance_scen/cycle_mean_speed,### REplace with age and sex walking and cycling speed
                                                trip_duration_base)))  %>%
      dplyr::mutate(trip_duration_base_hrs = trip_duration_base * 7/60) %>% ### Alan I modified here after discussing with James.
      dplyr::mutate(trip_duration_scen_hrs = trip_duration_scen * 7/60) %>%
    mutate_if(is.character,as.factor)
    #   dplyr::mutate(trip_mode=as.factor(case_when(trip_mode_scen=="pedestrian" ~ 'walking', 
    #                                               TRUE ~ trip_mode_scen)))
    return(trips_melbourne_scenarios)

}

