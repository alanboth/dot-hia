#### Function to create scenario trips to use in mslt_code

suppressPackageStartupMessages(library(dplyr)) # for manipulating data

### Example of replacing car trips of less than 5km with walking
### Inputs: Data set, speed walking, speed cycling, distance to replce walking, distance to replace cycling)



calculateScenario <- function(trips_melbourne = in_data, 
                              age_input = (15:98) , ### replace with age grouping that matched age cohorts
                              sex_input = c("male", "female"), 
                              original_mode = "car" , # Just car trips can be replaced
                              replace_mode = "pedestrian" , #"bicycle" ## Just pedestrian or cycling
                              distance_replace = 5 ,
                              purpose_input = c("social", "buy something",  "work related", "pick-up or drop-off someone", "personal business", "unknown purpose (at start of day)",
                                             "recreational", "pick-up or deliver something", "accompany someone", "education", "other purpose", "at or go home", "change mode", "not stated"), 
                              day = c("weekday", "weekend day")) {
                          

    SPEED_WALK <- 5 # ADD AS UNCERTAIN PARAMETERS
    SPEED_CYCLE <- 20 # ADD AS UNCEARTAIN PARAMETERS
    
    
    # in_data="Data/Processed/trips_melbourne.csv"
  
    trips_melbourne <- read.csv(in_data,as.is=T,fileEncoding="UTF-8-BOM")
    
    trips_melbourne_scenarios <- trips_melbourne %>%  
      dplyr::rename(trip_mode_base = trip_mode,
                    trip_duration_base = trip_duration,
                    trip_distance_base = trip_distance) %>%
      dplyr::mutate(trip_mode_scen = ifelse(trip_mode_base == original_mode & trip_distance_base <= distance_replace & age %in% age_input &
                                            sex %in% sex_input & trip_purpose %in% purpose_input & day_type %in% day,
                                            replace_mode, trip_mode_base)) %>%
      ## trip distance is the same, but time changes
      dplyr::mutate(trip_distance_scen = trip_distance_base) %>% 
      dplyr::mutate(trip_duration_scen = ifelse(trip_mode_base != trip_mode_scen,
                                                trip_distance_scen * ifelse(replace_mode == "pedestrian", SPEED_WALK, SPEED_CYCLE),
                                                trip_duration_base))  %>%  
      dplyr::mutate(trip_duration_base_hrs = ifelse(day_type == "weekday",
                                                    trip_duration_base * 5,
                                                    trip_duration_base * 2)/60) %>%
      dplyr::mutate(trip_distance_base_hrs = ifelse(day_type == "weekday",
                                                    trip_distance_base * 5,
                                                    trip_distance_base * 2)/60) %>%  
      dplyr::mutate(trip_duration_scen_hrs = ifelse(day_type == "weekday",
                                                    trip_duration_scen * 5,
                                                    trip_duration_scen * 2)/60) %>%
      dplyr::mutate(trip_distance_scen_hrs = ifelse(day_type == "weekday",
                                                    trip_distance_scen * 5,
                                                    trip_distance_scen * 2)/60) 
    return(trips_melbourne_scenarios)

}

