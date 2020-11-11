
#### Function to create scenario trips to use in mslt_code

suppressPackageStartupMessages(library(dplyr)) # for manipulating data

### Example of replacing car trips of less than 5km with walking
### Inputs: Data set, speed walking, speed cycling, distance to replce walking, distance to replace cycling)

### BELEN TO DO: 
## Add 0 km and 1km category 
### remove 0 to 17 age category
### modify mode to replace with logic options if value present

calculateScenarioMel <- function(trips_melbourne = in_data, 
                                 speed = in_speed,
                              age_input = c( "15 to 19", "20 to 39", "40 to 64", "65 plus"), # Choose age groups: one, a few or all
                              sex_input = c("male", "female"), #Choose sex group, : one, a few or all
                              original_mode = "car" , # Just car trips can be replaced
                              # replace_mode_walk = T,
                              # replace_mode_cycle = T,
                              distance_replace_walk = "<1km", #("0 km, <1km, 1-2km, 3-5km, 6-10km, >10km") #Choose one category only
                              distance_replace_cycle = "1-2km", #("0 km, <1km, 1-2km, 3-5km, 6-10km, >10km"), #Choose one category only
                              purpose_input = c("Leisure","Shopping", "Work", "Other", "Education")) { # Choose purpose groups, : one, a few or all
                          

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
      dplyr::mutate(trip_duration_base = trip_duration_base/60) %>% 
      dplyr::mutate(trip_mode_scen = ifelse(trip_mode_base == original_mode 
                                               & dist_cat %in% distance_replace_walk 
                                               & age_group %in% age_input 
                                               & sex %in% sex_input 
                                               & trip_purpose %in% purpose_input 
                                               & distance_replace_walk  != "0 km", "walking",
                                            ifelse(trip_mode_base == original_mode 
                                            & dist_cat %in% distance_replace_cycle 
                                            & age_group %in% age_input 
                                            & sex %in% sex_input 
                                            & trip_purpose %in% purpose_input
                                            & distance_replace_cycle  != "0 km", "bicycle", trip_mode_base))) %>%

      ## trip distance is the same, but time changes
      dplyr::mutate(trip_distance_scen = trip_distance_base) %>% 
      dplyr::mutate(trip_duration_scen = ifelse(trip_mode_base == original_mode
                                                & trip_mode_scen == "walking" & distance_replace_walk  != "0 km",
                                                trip_distance_scen/walk_mean_speed,
                                                ifelse(trip_mode_base == original_mode
                                                       &trip_mode_scen=="bicycle" & distance_replace_cycle  != "0 km",
                                                      trip_distance_scen/cycle_mean_speed,### REplace with age and sex walking and cycling speed
                                                trip_duration_base)))  %>%
      dplyr::mutate(trip_duration_base_hrs = trip_duration_base * 7) %>% ### Alan I modified here after discussing with James.
      dplyr::mutate(trip_duration_scen_hrs = trip_duration_scen * 7) %>%
    mutate_if(is.character,as.factor)
    #   dplyr::mutate(trip_mode=as.factor(case_when(trip_mode_scen=="pedestrian" ~ 'walking', 
    #                                               TRUE ~ trip_mode_scen)))
    return(list(trips=trips_melbourne_scenarios, AGE=age_input, SEX=sex_input)) ### BZ: added age_input and sex_input to feed into mslt codde

}

