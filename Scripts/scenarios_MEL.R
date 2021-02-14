#### Function to create scenario trips to use in mslt_code

suppressPackageStartupMessages(library(dplyr)) # for manipulating data


calculateScenarioMel2 <- function(trips_melbourne = in_data, 
                                  walk_speed=4.8,
                                  cycle_speed=14.3,
                                  original_mode = "car" , # Just car trips can be replaced
                                  distance_replace_walk = 0,
                                  distance_replace_cycle = 0,
                                  purpose_input = "Leisure,Shopping,Work,Education,Other") {
  
  # in_data="Data/processed/trips_melbourne.csv"
  # in_speed="Data/processed/speed_trips_melbourne.csv"
  # trips_melbourne = in_data
  # speed = in_speed
  # original_mode = "car"
  # distance_replace_walk = 0
  # distance_replace_cycle = 10
  # purpose_input = "Work,Education"
  
  # it's easier to pass in a single string and then split it here
  purpose_input <- unlist(strsplit(purpose_input,","))
  
  
  trips_melbourne <- read.csv(trips_melbourne,as.is=T,fileEncoding="UTF-8-BOM") %>%
    dplyr::mutate(trip_mode=case_when(trip_mode=="pedestrian" ~ 'walking', 
                                      trip_mode=="bus" ~ 'public.transport', 
                                      trip_mode=="tram" ~ 'public.transport', 
                                      trip_mode=="train" ~ 'public.transport',
                                      trip_mode=="motorcycle" ~ 'other',
                                      TRUE ~ tolower(trip_mode))) ## Add age groups to facilitate selection above and matching  
  
  trips_melbourne_scenarios <- trips_melbourne %>%
    dplyr::rename(trip_mode_base = trip_mode,
                  trip_duration_base = trip_duration,
                  trip_distance_base = trip_distance) %>%
    dplyr::mutate(trip_duration_base = trip_duration_base/60) %>% 
    dplyr::filter(trip_distance_base!=0) %>%
    # replace with walking
    dplyr::mutate(trip_mode_scen = ifelse(trip_mode_base %in% original_mode 
                                          & trip_purpose %in% purpose_input
                                          & trip_distance_base !=0
                                          & trip_distance_base <= distance_replace_walk #=<
                                          &  distance_replace_walk >0
                                          &  distance_replace_walk != distance_replace_cycle,
                                          "walking",
                                          trip_mode_base)) %>%
    # replace with bicycle
    dplyr::mutate(trip_mode_scen = ifelse(trip_mode_base %in% original_mode 
                                          & trip_purpose %in% purpose_input
                                          & trip_distance_base !=0
                                          & trip_distance_base > distance_replace_walk 
                                          & trip_distance_base <= distance_replace_cycle #=<
                                          &  distance_replace_cycle >0,
                                          "bicycle",
                                          trip_mode_scen)) %>%
    # trip distance is the same, but time changes
    dplyr::mutate(trip_distance_scen = trip_distance_base) %>% 
    dplyr::mutate(trip_duration_scen = ifelse(trip_mode_base == original_mode
                                              & trip_mode_scen == "walking",
                                              trip_distance_scen/walk_speed,
                                              trip_duration_base)) %>%
    dplyr::mutate(trip_duration_scen = ifelse(trip_mode_base == original_mode
                                              & trip_mode_scen == "bicycle",
                                              trip_distance_scen/cycle_speed,
                                              trip_duration_scen)) %>%
    dplyr::mutate(trip_duration_base_hrs = trip_duration_base * 7) %>%
    dplyr::mutate(trip_duration_scen_hrs = trip_duration_scen * 7) %>%
    mutate_if(is.character,as.factor)
  return(trips_melbourne_scenarios)
  
}


generateMatchedPopulationScenario <- function(output_location="./scenarios/",
                                              scenario_name="default",
                                              in_data="./Data/processed/trips_melbourne.csv",
                                              # in_speed="./Data/processed/speed_trips_melbourne.csv",
                                              max_walk,
                                              max_cycle,
                                              purpose) {
  
  # in_speed="./Data/processed/speed_trips_melbourne.csv"
  # output_location="./scenarios"
  # scenario_name="all_2_10"
  # in_data="./Data/processed/trips_melbourne.csv"
  # max_walk=2
  # max_cycle=10
  # purpose="Leisure,Shopping,Work,Education,Other"
  
  # in case the directory hasn't been made yet
  dir.create(output_location, recursive=TRUE, showWarnings=FALSE)
  dir.create(paste0(output_location,"/scenarioTrips"), recursive=TRUE, showWarnings=FALSE)
  dir.create(paste0(output_location,"/personTravel"), recursive=TRUE, showWarnings=FALSE)
  
  #### 1) Generate trip set with baseline and scenario trips ####
  
  ### The following code returns persons_matched, which is an input of CalculateModel
  ### Graph: depicts change in trips by mode.
  
  ### Calculate scenarios of replacing car trips by walking and/or cycling. 
  ### Outputs: trip set with baseline and scenario trips and associated distance and time in hours per week.
  ### Inputs: baseline trips melbourne and speed file by age and sex derived from VISTA 2017-18 TRIP file.
  # max_walk=2
  # max_cycle=0
  scenario_trips <- calculateScenarioMel2(
    trips_melbourne = in_data, 
    # speed = in_speed,
    original_mode = "car", # c("car","public.transport") , # Just car trips can be replaced
    distance_replace_walk = max_walk,
    distance_replace_cycle = max_cycle,
    purpose_input = purpose
  )
  
  write.csv(scenario_trips, paste0(output_location,"/scenarioTrips/",scenario_name,".csv"), row.names=F, quote=T)
  
  
  ### 2.1) Create data set with VISTA people and allocate baseline and scenario trips to them
  persons_travel <- calculatePersonsTravelScenario(
    travel_data_location="./Data/processed/travel_data.csv", ## BZ: generated in script runInputsMelbourneExposure.R 
    scenario_location=scenario_trips ### BZ: Generated in step 1
  )
  write.csv(persons_travel, paste0(output_location,"/personTravel/",scenario_name,".csv"), row.names=F, quote=T)
  
  #### 2.2) Match NHS people to VISTA people based on age, sex, ses, work status and whether they walk for transport
  persons_matched <- calculatePersonsMatch(
    pa_location="./Data/processed/persons_pa.csv", ## BZ: generated in script runInputsMelbourneExposure.R 
    persons_travel_location=persons_travel  #"Data/processed/persons_travel.csv"
  ) %>% dplyr::mutate(scen=scenario_name)
  ##### ADD scenario names
  write.csv(persons_matched, paste0(output_location,"/",scenario_name,".csv"), row.names=F, quote=T)
}