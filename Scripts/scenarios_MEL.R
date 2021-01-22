#### Function to create scenario trips to use in mslt_code

suppressPackageStartupMessages(library(dplyr)) # for manipulating data

### Example of replacing car trips of less than 5km with walking
### Inputs: Data set, speed walking, speed cycling, distance to replce walking, distance to replace cycling)

### BELEN TO DO: 
## Add 0 km and 1km category 
### remove 0 to 17 age category
### modify mode to replace with logic options if value present

# calculateScenarioMel <- function(trips_melbourne = in_data, 
#                                  speed = in_speed,
#                                  age_input = c( "15 to 19", "20 to 39", "40 to 64", "65 plus"), # Choose age groups: one, a few or all
#                                  sex_input = c("male", "female"), #Choose sex group, : one, a few or all
#                                  original_mode = "car" , # Just car trips can be replaced
#                                  # replace_mode_walk = T,
#                                  # replace_mode_cycle = T,
#                                  distance_replace_walk = "<1km", #("0 km, <1km, 1-2km, 3-5km, 6-10km, >10km") #Choose one category only
#                                  distance_replace_cycle = "1-2km", #("0 km, <1km, 1-2km, 3-5km, 6-10km, >10km"), #Choose one category only
#                                  purpose_input = c("Leisure","Shopping", "Work", "Other", "Education")) { # Choose purpose groups, : one, a few or all
#   
#   
#   # in_data="Data/processed/trips_melbourne.csv"
#   # in_speed="Data/processed/speed_trips_melbourne.csv"
#   
#   
#   trips_melbourne <- read.csv(trips_melbourne,as.is=T,fileEncoding="UTF-8-BOM") %>%
#     dplyr::mutate(trip_mode=case_when(trip_mode=="pedestrian" ~ 'walking', 
#                                       trip_mode=="bus" ~ 'public.transport', 
#                                       trip_mode=="tram" ~ 'public.transport', 
#                                       trip_mode=="train" ~ 'public.transport',
#                                       trip_mode=="motorcycle" ~ 'other',
#                                       TRUE ~ tolower(trip_mode))) ## Add age groups to facilitate selection above and matching  
#   speed <- read.csv(speed,as.is=T,fileEncoding="UTF-8-BOM")
#   
#   
#   #### create column in trips_melbourne with speed data for age and sex (use median)
#   walk_speed <- speed %>% dplyr::filter(activity=="walking") %>% dplyr::rename(walk_mean_speed = mean) %>% dplyr::select(age_group, sex, walk_mean_speed)
#   cycle_speed <- speed %>% dplyr::filter(activity=="bicycle") %>% dplyr::rename(cycle_mean_speed = mean) %>% dplyr::select(age_group, sex, cycle_mean_speed)
#   
#   trips_melbourne <- trips_melbourne %>% inner_join(walk_speed, by=c("age_group", "sex"))
#   
#   trips_melbourne <- trips_melbourne %>% inner_join(cycle_speed, by=c("age_group", "sex"))
#   
#   
#   trips_melbourne_scenarios <- trips_melbourne %>%
#     dplyr::rename(trip_mode_base = trip_mode,
#                   trip_duration_base = trip_duration,
#                   trip_distance_base = trip_distance) %>%
#     dplyr::mutate(trip_duration_base = trip_duration_base/60) %>% 
#     dplyr::mutate(trip_mode_scen = ifelse(trip_mode_base == original_mode 
#                                           & dist_cat %in% distance_replace_walk 
#                                           & age_group %in% age_input 
#                                           & sex %in% sex_input 
#                                           & trip_purpose %in% purpose_input 
#                                           & distance_replace_walk  != "0 km", "walking",
#                                           ifelse(trip_mode_base == original_mode 
#                                                  & dist_cat %in% distance_replace_cycle 
#                                                  & age_group %in% age_input 
#                                                  & sex %in% sex_input 
#                                                  & trip_purpose %in% purpose_input
#                                                  & distance_replace_cycle  != "0 km", "bicycle", trip_mode_base))) %>%
#     
#     ## trip distance is the same, but time changes
#     dplyr::mutate(trip_distance_scen = trip_distance_base) %>% 
#     dplyr::mutate(trip_duration_scen = ifelse(trip_mode_base == original_mode
#                                               & trip_mode_scen == "walking" & distance_replace_walk  != "0 km",
#                                               trip_distance_scen/walk_mean_speed,
#                                               ifelse(trip_mode_base == original_mode
#                                                      &trip_mode_scen=="bicycle" & distance_replace_cycle  != "0 km",
#                                                      trip_distance_scen/cycle_mean_speed,### REplace with age and sex walking and cycling speed
#                                                      trip_duration_base)))  %>%
#     dplyr::mutate(trip_duration_base_hrs = trip_duration_base * 7) %>% ### Alan I modified here after discussing with James.
#     dplyr::mutate(trip_duration_scen_hrs = trip_duration_scen * 7) %>%
#     mutate_if(is.character,as.factor)
#   #   dplyr::mutate(trip_mode=as.factor(case_when(trip_mode_scen=="pedestrian" ~ 'walking', 
#   #                                               TRUE ~ trip_mode_scen)))
#   return(list(trips=trips_melbourne_scenarios, AGE=age_input, SEX=sex_input)) ### BZ: added age_input and sex_input to feed into mslt codde
#   
# }
# 


calculateScenarioMel2 <- function(trips_melbourne = in_data, 
                                  walk_speed=4.8,
                                  cycle_speed=14.3,
                                  original_mode = "car" , # Just car trips can be replaced
                                  distance_replace_walk = 0,
                                  distance_replace_cycle = 0,
                                  purpose_input = "Leisure,Shopping,Work,Education,Other") {

  # in_data="Data/processed/trips_melbourne.csv"
  # in_speed="Data/processed/speed_trips_melbourne.csv"
  trips_melbourne = in_data
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
  
  ### BZ: Use default speed
  # speed <- read.csv(speed,as.is=T,fileEncoding="UTF-8-BOM")
  # 
  # 
  # #### create column in trips_melbourne with speed data for age and sex (use median)
  # walk_speed <- speed %>% dplyr::filter(activity=="walking") %>% dplyr::rename(walk_mean_speed = mean) %>% dplyr::select(age_group, sex, walk_mean_speed)
  # cycle_speed <- speed %>% dplyr::filter(activity=="bicycle") %>% dplyr::rename(cycle_mean_speed = mean) %>% dplyr::select(age_group, sex, cycle_mean_speed)
  # 
  # trips_melbourne <- trips_melbourne %>% inner_join(walk_speed, by=c("age_group", "sex"))
  # 
  # trips_melbourne <- trips_melbourne %>% inner_join(cycle_speed, by=c("age_group", "sex"))
  
  
  trips_melbourne_scenarios <- trips_melbourne %>%
    dplyr::rename(trip_mode_base = trip_mode,
                  trip_duration_base = trip_duration,
                  trip_distance_base = trip_distance) %>%
    dplyr::mutate(trip_duration_base = trip_duration_base/60) %>% 
    # replace with walking
    dplyr::mutate(trip_mode_scen = ifelse(trip_mode_base %in% original_mode 
                                          & trip_purpose %in% purpose_input
                                          & trip_distance_base !=0
                                          & trip_distance_base <= distance_replace_walk
                                          &  distance_replace_walk !=0,
                                          "walking",
                                          trip_mode_base)) %>%
    # replace with bicycle
    dplyr::mutate(trip_mode_scen = ifelse(trip_mode_base %in% original_mode 
                                          & trip_purpose %in% purpose_input
                                          & trip_distance_base !=0
                                          & trip_distance_base > distance_replace_walk 
                                          & trip_distance_base <= distance_replace_cycle
                                          &  distance_replace_cycle !=0,
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


generateMatchedPopulationScenario <- function(output_location=paste0("./scenarios/", scenario_name, "/"),
                                              scenario_name="default",
                                              in_data="./Data/processed/trips_melbourne.csv",
                                              # in_speed="./Data/processed/speed_trips_melbourne.csv",
                                              max_walk,
                                              max_cycle,
                                              purpose) {
  
  # output_location="./scenarios"
  # scenario_name="all_2_10"
  # in_data="./Data/processed/trips_melbourne.csv"
  # in_speed="./Data/processed/speed_trips_melbourne.csv"
  # max_walk=2
  # max_cycle=10
  # purpose="Leisure,Shopping,Work,Education,Other"
  
  # in case the directory hasn't been made yet
  dir.create(output_location, recursive=TRUE, showWarnings=FALSE)
  
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
  
  
  #### Graphs
  #### Get weighted data
  
  scenario_trips_weighted <-  scenario_trips  %>%
    srvyr::as_survey_design(weights = trips_wt)
  
  #### Table with baseline and scenario proportion by mode
  scenario_trips_mode <- scenario_trips_weighted   %>% 
    group_by(trip_mode_scen,.drop = FALSE) %>%
    dplyr::summarize(prop= srvyr::survey_mean()) %>%
    dplyr::rename(mode = trip_mode_scen) %>%
    mutate(scen="scenario")
  
  baseline_trips_mode <- scenario_trips_weighted   %>% 
    group_by(trip_mode_base,.drop = FALSE) %>%
    dplyr::summarize(prop= srvyr::survey_mean()) %>%
    dplyr::rename(mode = trip_mode_base) %>%
    mutate(scen="base") 
  
  data_mode_combo <- bind_rows(scenario_trips_mode, baseline_trips_mode) %>% 
    mutate(mode = fct_reorder(mode, desc(prop)))
  
  ### Get bar chart modes distribution
  bar_chart_combo_sc <- data_mode_combo %>%
    ggplot(aes(x = mode, y = prop)) +
    geom_bar(
      aes(color = scen, fill = scen),
      stat = "identity" , position = "dodge"
    ) + 
    labs(title="Distribution trips baseline and scenario", x="", y="Proportion of all trips") +
    theme_classic() +
    geom_text(aes(label=paste0(round(prop*100,1),"%"), y=prop), size=6)  + 
    theme(plot.title = element_text(hjust = 0.5, size = 20,face="bold"),
          axis.text=element_text(size=16),
          axis.title=element_text(size=16)) +
    theme(legend.position = "right",
          legend.title = element_blank(),
          legend.text = element_text(colour = "black", size = 16),
          legend.key = element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))  +
    scale_y_continuous(labels = percent)
  
  
  bar_chart_combo_sc


  
  ggsave(paste0(output_location, "/", scenario_name,"_proportion_modes_sc.png"))
  
  
  #### 2) Generate person_matched ####
  
  ### Calculate time spents doing physical activity at baseline and scenario for individuals
  ### Outputs: persons_matched with baseline and sceanrios time in hours spents walking and cycling for transport and moderate and vigorous PA.
  ### Inputs: scenario_trips (above step), VISTA persons from VISTA 2017-18 PERSON file and moderate and vigorous excersice from NHS persons file 2017-18.
  
  ### 2.1) Create data set with VISTA people and allocate baseline and scenario trips to them
  persons_travel <- calculatePersonsTravelScenario(
    travel_data_location="Data/processed/travel_data.csv", ## BZ: generated in script runInputsMelbourneExposure.R 
    scenario_location=scenario_trips ### BZ: Generated in step 1
  )
  
  #### 2.2) Match NHS people to VISTA people based on age, sex, ses, work status and whether they walk for transport
  persons_matched <- calculatePersonsMatch(
    pa_location="Data/processed/persons_pa.csv", ## BZ: generated in script runInputsMelbourneExposure.R 
    persons_travel_location=persons_travel   #"Data/processed/persons_travel.csv"
  )

  write.csv(persons_matched, paste0(output_location,"/",scenario_name,".csv"), row.names=F, quote=T)
}