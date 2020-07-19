#### Here create scenarios

### Need to set a computation order

## 1) Trips Melbourne (should be fixed)
## 2) Trips Melbourne Scenarios
## 3) Matched population (baseline and scenario)
## 4) PIF PA, then PA AP and injuries. Start with PA for now. 

suppressPackageStartupMessages(library(dplyr)) # for manipulating data




### Example of replacing car trips of less than 5km with walking


calculateScenario <- function(trips_melbourne_location) {
  ### Parameters
  #### Deterministic for now, replace mean pedestrian distance and distribution parameters from VISTA
  SPEED_WALK <- 5
  
  
  # trips_melbourne_location="Data/Processed/trips_melbourne.csv"
  
  trips_melbourne <- read.csv(trips_melbourne_location,as.is=T,fileEncoding="UTF-8-BOM")
  
  trips_melbourne_scenarios <- trips_melbourne %>%  
    dplyr::rename(trip_mode_base = trip_mode,
                  trip_duration_base = trip_duration,
                  trip_distance_base = trip_distance) %>%
    dplyr::mutate(trip_mode_scen = ifelse(trip_mode_base == "car" & trip_distance_base <= 5,
                                          "pedestrian", trip_mode_base)) %>%
    ## trip distance is the same, but time changes
    dplyr::mutate(trip_distance_scen = trip_distance_base) %>% 
    dplyr::mutate(trip_duration_scen = ifelse(trip_mode_base != trip_mode_scen,
                                              trip_distance_scen * SPEED_WALK,
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



### Save and use for matching population


# write_csv(trips_melbourne_scenarios, "Data/Processed/trips_melbourne_scenarios.csv")
# write_rds(trips_melbourne_scenarios, "Data/Processed/trips_melbourne_scenarios.Rds")