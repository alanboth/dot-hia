
#### Function to create scenario trips to use in mslt_code

suppressPackageStartupMessages(library(dplyr)) # for manipulating data

### Example of replacing car trips of less than 5km with walking
### Inputs: Data set, speed walking, speed cycling, distance to replce walking, distance to replace cycling)



calculateScenarioMel <- function(trips_melbourne = in_data, 
                              age_input = c(17, 22, 27, 32, 37, 42, 47, 52, 57, 62, 67, 72, 77, 82, 87, 92, 97),
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
  
    trips_melbourne <- read.csv(in_data,as.is=T,fileEncoding="UTF-8-BOM") ## Add age groups to facilitate selection above and matching  
      #### with mslt_cohorts
      
      trips_melbourne <- trips_melbourne %>%
      dplyr::mutate(age_cat = case_when(age <   5             ~  2,
                                        age >=  5 & age <=  9 ~  7,
                                        age >= 10 & age <= 14 ~ 12,
                                        age >= 15 & age <= 19 ~ 17, 
                                        age >= 20 & age <= 24 ~ 22,
                                        age >= 25 & age <= 29 ~ 27, 
                                        age >= 30 & age <= 34 ~ 32, 
                                        age >= 35 & age <= 39 ~ 37, 
                                        age >= 40 & age <= 44 ~ 42,
                                        age >= 45 & age <= 49 ~ 47, 
                                        age >= 50 & age <= 54 ~ 52, 
                                        age >= 55 & age <= 59 ~ 57, 
                                        age >= 60 & age <= 64 ~ 62, 
                                        age >= 65 & age <= 69 ~ 67,
                                        age >= 70 & age <= 74 ~ 72, 
                                        age >= 75 & age <= 79 ~ 77,
                                        age >= 80 & age <= 84 ~ 82,
                                        age >= 85 & age <= 89 ~ 87, 
                                        age >= 90 & age <= 94 ~ 92,
                                        age >= 95 & age <= 100 ~ 97)) %>%
     
        dplyr::mutate(age_cat_2 = case_when(age_cat == 2 ~  "0 t0 4",
                                          age_cat == 7 ~  "5 to 9",
                                          age_cat ==  12 ~ "10 to 14",
                                          age_cat ==  17  ~ "15 to 19", 
                                          age_cat ==  22  ~ "20 to 24",
                                          age_cat ==  27  ~ "25 to 29",
                                          age_cat ==  32  ~ "30 to 34", 
                                          age_cat ==  37  ~ "35 to 39", 
                                          age_cat ==  42  ~ "40 to 44", 
                                          age_cat ==  47  ~ "45 to 49",
                                          age_cat ==  52  ~ "50 to 54", 
                                          age_cat ==  57  ~ "55 to 59", 
                                          age_cat ==  62  ~ "60 to 64", 
                                          age_cat ==  67  ~ "65 to 69", 
                                          age_cat ==  72  ~ "70 to 74",
                                          age_cat ==  77  ~ "75 to 79", 
                                          age_cat ==  82  ~ "80 to 84",
                                          age_cat ==  87  ~ "85 to 89",
                                          age_cat ==  92  ~ "90 to 94", 
                                          age_cat ==  97   ~ "95 to 100"))
    
    trips_melbourne_scenarios <- trips_melbourne %>%  
      dplyr::rename(trip_mode_base = trip_mode,
                    trip_duration_base = trip_duration,
                    trip_distance_base = trip_distance) %>%
      dplyr::mutate(trip_mode_scen = ifelse(trip_mode_base == original_mode & trip_distance_base <= distance_replace & age_cat %in% age_input &
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

