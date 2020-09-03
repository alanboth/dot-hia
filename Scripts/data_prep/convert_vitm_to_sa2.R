library(sf)
library(dplyr)
library(data.table)
library(dtplyr)

matricies_location='/home/alan/Projects/dot-hia-misc/Matrices_020920/'

sa2_tzn_mapping <- st_read("./Data/vitm/sa2_intersect_vitm_regions.sqlite",quiet=T) %>%
  mutate(sa2_main16=as.integer(as.character(sa2_main16))) %>%
  mutate(area=as.numeric(st_area(.))) %>%
  st_drop_geometry() %>%
  dplyr::select(sa2_main16,tzn,area)

# calculate weights for mapping for each sa2, the proportion each overlapping
# vitm region takes
weights_mean <- sa2_tzn_mapping %>%
  group_by(sa2_main16) %>%
  mutate(area_total=sum(area,na.rm=T)) %>%
  ungroup() %>%
  mutate(weight=area/area_total) %>%
  dplyr::select(sa2_main16,tzn,weight)

# calculate weights for mapping for each tzn, the proportion each overlapping
# sa2 region takes
weights_sum <- sa2_tzn_mapping %>%
  group_by(tzn) %>%
  mutate(area_total=sum(area,na.rm=T)) %>%
  ungroup() %>%
  mutate(weight=area/area_total) %>%
  dplyr::select(sa2_main16,tzn,weight)

vitm_to_sa2 <- function(matrix_location, weights, name) {
  # vitim_matrix_location="/home/alan/Projects/dot-hia-misc/Matrices_020920/Y2018_Base case/Car/24hr Avg car distance_Y2018_RC_car.csv"
  
  # ids
  tzn_ids <- unique(weights$tzn)%>%sort() 
  sa2_ids <- unique(weights$sa2_main16)%>%sort() 
  
  # import matrix location
  vitm_matrix <- read.csv(paste0(matricies_location,matrix_location),
                          as.is=T, fileEncoding="UTF-8-BOM")[,-1] %>%
    as.matrix()
  # rename columns and rows
  colnames(vitm_matrix) <- 1:ncol(vitm_matrix)
  rownames(vitm_matrix) <- 1:nrow(vitm_matrix)
  
  # remove dummy vitm regions
  vitm_matrix <- vitm_matrix[tzn_ids,tzn_ids]
  
  # convert from matrix to dataframe
  vitm_df <- vitm_matrix %>% 
    as.table(vitm_matrix) %>%
    as.data.frame() %>%
    mutate(from_tzn=as.integer(Var1),to_tzn=as.integer(Var2)) %>%
    select(from_tzn,to_tzn,measure=Freq) %>%
    data.table() %>%
    lazy_dt()
  
  # making the dataframe a lazy data table
  weights <- weights %>%
    lazy_dt()
  
  # replace from_tzn and to_tzn with sa2 regions proportional to the weights column
  sa2_df <- weights %>%
    # join from tzn
    inner_join(vitm_df, by=c("tzn"="from_tzn")) %>%
    mutate(measure=measure*weight) %>%
    group_by(sa2_main16,to_tzn) %>%
    summarise(measure=sum(measure)) %>%
    select(from_sa2=sa2_main16,to_tzn,measure) %>%
    # join to tzn
    inner_join(weights, by=c("to_tzn"="tzn")) %>%
    mutate(measure=measure*weight) %>%
    group_by(from_sa2,sa2_main16) %>%
    summarise(measure=sum(measure)) %>%
    select(from_sa2,to_sa2=sa2_main16,!!name:=measure) %>%
    as.data.frame()
  
  return(sa2_df)
}

aggregate_to_sa2_mean <- function(sa2_df) {
  name=colnames(sa2_df)[3]
  colnames(sa2_df)[3] <- "measure"
  measure_mean=paste0(name,"_mean")
  measure_sd=paste0(name,"_sd")
  result <- sa2_df %>%
    group_by(from_sa2) %>%
    summarise(!!measure_mean:=mean(measure,na.rm=T),!!measure_sd:=sd(measure,na.rm=T)) %>%
    rename(sa2=from_sa2)
  return(result)
}

aggregate_to_sa2_sum <- function(sa2_df) {
  name=colnames(sa2_df)[3]
  colnames(sa2_df)[3] <- "measure"
  result <- sa2_df %>%
    group_by(from_sa2) %>%
    summarise(!!name:=sum(measure,na.rm=T)) %>%
    rename(sa2=from_sa2)
  return(result)
}


# car_df
base_car_distance <- vitm_to_sa2(matrix_location='Y2018_Base case/Car/24hr Avg car distance_Y2018_RC_car.csv',
                                 weights=weights_mean,
                                 name='base_car_distance') %>%
  aggregate_to_sa2_mean()
base_car_time <- vitm_to_sa2(matrix_location='Y2018_Base case/Car/24hr Avg car Time_Y2018_RC_car.csv',
                             weights=weights_mean,
                             name='base_car_time') %>%
  aggregate_to_sa2_mean()
base_car_person_trips <- vitm_to_sa2(matrix_location='Y2018_Base case/Car/24hr car person trips_Y2018_RC_car.csv',
                                     weights=weights_sum,
                                     name='base_car_person_trips') %>%
  aggregate_to_sa2_sum()

srl_car_distance <- vitm_to_sa2(matrix_location='SRL scenario/Car/24hr Avg car distance_Y2018_SRL_car.csv',
                                weights=weights_mean,
                                name='srl_car_distance') %>%
  aggregate_to_sa2_mean()
srl_car_time <- vitm_to_sa2(matrix_location='SRL scenario/Car/24hr Avg car Time_Y2018_SRL_car.csv',
                            weights=weights_mean,
                            name='srl_car_time') %>%
  aggregate_to_sa2_mean()
srl_car_person_trips <- vitm_to_sa2(matrix_location='SRL scenario/Car/24hr car person trips_Y2018_SRL_car.csv',
                                    weights=weights_sum,
                                    name='srl_car_person_trips') %>%
  aggregate_to_sa2_sum()
car_df <- base_car_distance %>%
  full_join(srl_car_distance) %>%
  full_join(base_car_time) %>%
  full_join(srl_car_time) %>%
  full_join(base_car_person_trips) %>%
  full_join(srl_car_person_trips)
rm(base_car_distance,
   srl_car_distance,
   base_car_time,
   srl_car_time,
   base_car_person_trips,
   srl_car_person_trips)

write.csv(car_df, "Data/Processed/srl_car.csv", row.names=F, quote=T)



# # base_pt_drive_walk_distance
# 'Y2018_Base case/PT/Walk in PT drive  from home to stations/24hr Avg Egress Walk Distance_Y2018_RC_PNRPA.csv'
# 'Y2018_Base case/PT/Walk in PT drive  from home to stations/24hr Avg Transfer Walk Distance_Y2018_RC_PNRPA.csv'
# 'Y2018_Base case/PT/Walk in PT drive from stations to home/24hr Avg Access Walk Distance_Y2018_RC_PNRAP.csv'
# 'Y2018_Base case/PT/Walk in PT drive from stations to home/24hr Avg Transfer Walk Distance_Y2018_RC_PNRAP.csv'
# 
# # base_pt_drive_walk_time
# 'Y2018_Base case/PT/Walk in PT drive  from home to stations/24hr Avg Egress Walk Time_Y2018_RC_PNRPA.csv'
# 'Y2018_Base case/PT/Walk in PT drive  from home to stations/24hr Avg Trf Walk Time_Y2018_RC_PNRPA.csv'
# 'Y2018_Base case/PT/Walk in PT drive from stations to home/24hr Avg Access Walk Time_Y2018_RC_PNRAP.csv'
# 'Y2018_Base case/PT/Walk in PT drive from stations to home/24hr Avg Trf Walk Time_Y2018_RC_PNRAP.csv'
# 
# # base_pt_drive_total_demand
# 'Y2018_Base case/PT/Walk in PT drive  from home to stations/PT PNRPA_24H_Total demand_Y2018_RC_PNRPA.csv'
# 'Y2018_Base case/PT/Walk in PT drive from stations to home/PT PNRAP_24H_Total demand_Y2018_RC_PNRAP.csv'
# 
# # base_pt_walk_walk_distance
# 'Y2018_Base case/PT/PT Walk to stations_Stops and vice versa/24hr Avg Access Walk Distance_Y2018_RC_walk.csv'
# 'Y2018_Base case/PT/PT Walk to stations_Stops and vice versa/24hr Avg Egress Walk Distance_Y2018_RC_walk.csv'
# 'Y2018_Base case/PT/PT Walk to stations_Stops and vice versa/24hr Avg Transfer Walk Distance_Y2018_RC_walk.csv'
# 
# # base_pt_walk_walk_time
# 'Y2018_Base case/PT/PT Walk to stations_Stops and vice versa/24hr Avg Access Walk Time_Y2018_RC_walk.csv'
# 'Y2018_Base case/PT/PT Walk to stations_Stops and vice versa/24hr Avg Egress Walk Time_Y2018_RC_walk.csv'
# 'Y2018_Base case/PT/PT Walk to stations_Stops and vice versa/24hr Avg Trf Walk Time_Y2018_RC_walk.csv'
# 
# # base_pt_walk_total_demand
# 'Y2018_Base case/PT/PT Walk to stations_Stops and vice versa/PT Walk_24H_Total demand_Y2018_RC_walk.csv'  

################################################################################################################################################################


################################################################################
# base_pt_drive_walk_distance                                                  #
################################################################################
base_pt_drive_walk_egress_distance_hs <- vitm_to_sa2(
  matrix_location='Y2018_Base case/PT/Walk in PT drive  from home to stations/24hr Avg Egress Walk Distance_Y2018_RC_PNRPA.csv',
  weights=weights_mean,
  name='egress_distance')
base_pt_drive_walk_transfer_distance_hs <- vitm_to_sa2(
  matrix_location='Y2018_Base case/PT/Walk in PT drive  from home to stations/24hr Avg Transfer Walk Distance_Y2018_RC_PNRPA.csv',
  weights=weights_mean,
  name='transfer_distance')
base_pt_drive_walk_access_distance_sh <- vitm_to_sa2(
  matrix_location='Y2018_Base case/PT/Walk in PT drive from stations to home/24hr Avg Access Walk Distance_Y2018_RC_PNRAP.csv',
  weights=weights_mean,
  name='access_distance')
base_pt_drive_walk_transfer_distance_sh <- vitm_to_sa2(
  matrix_location='Y2018_Base case/PT/Walk in PT drive from stations to home/24hr Avg Transfer Walk Distance_Y2018_RC_PNRAP.csv',
  weights=weights_mean,
  name='transfer_distance2')
base_pt_drive_walk_distance <- base_pt_drive_walk_egress_distance_hs %>%
  full_join(base_pt_drive_walk_transfer_distance_hs) %>%
  full_join(base_pt_drive_walk_access_distance_sh) %>%
  full_join(base_pt_drive_walk_transfer_distance_sh) %>%
  mutate(base_pt_drive_walk_distance=egress_distance+transfer_distance+access_distance+transfer_distance2) %>%
  select(from_sa2,to_sa2,base_pt_drive_walk_distance) %>%
  aggregate_to_sa2_mean()
rm(base_pt_drive_walk_egress_distance_hs,
   base_pt_drive_walk_transfer_distance_hs,
   base_pt_drive_walk_access_distance_sh,
   base_pt_drive_walk_transfer_distance_sh)

################################################################################
# base_pt_drive_walk_time                                                      #
################################################################################
base_pt_drive_walk_egress_time_hs <- vitm_to_sa2(
  matrix_location='Y2018_Base case/PT/Walk in PT drive  from home to stations/24hr Avg Egress Walk Time_Y2018_RC_PNRPA.csv',
  weights=weights_mean,
  name='egress_time')
base_pt_drive_walk_transfer_time_hs <- vitm_to_sa2(
  matrix_location='Y2018_Base case/PT/Walk in PT drive  from home to stations/24hr Avg Trf Walk Time_Y2018_RC_PNRPA.csv',
  weights=weights_mean,
  name='transfer_time')
base_pt_drive_walk_access_time_sh <- vitm_to_sa2(
  matrix_location='Y2018_Base case/PT/Walk in PT drive from stations to home/24hr Avg Access Walk Time_Y2018_RC_PNRAP.csv',
  weights=weights_mean,
  name='access_time')
base_pt_drive_walk_transfer_time_sh <- vitm_to_sa2(
  matrix_location='Y2018_Base case/PT/Walk in PT drive from stations to home/24hr Avg Trf Walk Time_Y2018_RC_PNRAP.csv',
  weights=weights_mean,
  name='transfer_time2')
base_pt_drive_walk_time <- base_pt_drive_walk_egress_time_hs %>%
  full_join(base_pt_drive_walk_transfer_time_hs) %>%
  full_join(base_pt_drive_walk_access_time_sh) %>%
  full_join(base_pt_drive_walk_transfer_time_sh) %>%
  mutate(base_pt_drive_walk_time=egress_time+transfer_time+access_time+transfer_time2) %>%
  select(from_sa2,to_sa2,base_pt_drive_walk_time) %>%
  aggregate_to_sa2_mean()
rm(base_pt_drive_walk_egress_time_hs,
   base_pt_drive_walk_transfer_time_hs,
   base_pt_drive_walk_access_time_sh,
   base_pt_drive_walk_transfer_time_sh)

################################################################################
# base_pt_drive_total_demand                                                   #
################################################################################
base_pt_drive_total_demand_hs <- vitm_to_sa2(
  matrix_location='Y2018_Base case/PT/Walk in PT drive  from home to stations/PT PNRPA_24H_Total demand_Y2018_RC_PNRPA.csv',
  weights=weights_sum,
  name='demand_hs')
base_pt_drive_total_demand_sh <- vitm_to_sa2(
  matrix_location='Y2018_Base case/PT/Walk in PT drive from stations to home/PT PNRAP_24H_Total demand_Y2018_RC_PNRAP.csv',
  weights=weights_sum,
  name='demand_sh')
base_pt_drive_total_demand <- full_join(
  base_pt_drive_total_demand_hs,
  base_pt_drive_total_demand_sh) %>%
  mutate(base_pt_drive_total_demand=demand_hs+demand_sh) %>%
  select(from_sa2,to_sa2,base_pt_drive_total_demand) %>%
  aggregate_to_sa2_sum()
rm(base_pt_drive_total_demand_hs,
   base_pt_drive_total_demand_sh)

################################################################################
# base_pt_walk_walk_distance                                                   #
################################################################################
base_pt_walk_walk_access_distance <- vitm_to_sa2(
  matrix_location='Y2018_Base case/PT/PT Walk to stations_Stops and vice versa/24hr Avg Access Walk Distance_Y2018_RC_walk.csv',
  weights=weights_mean,
  name='access_distance')
base_pt_walk_walk_egress_distance <- vitm_to_sa2(
  matrix_location='Y2018_Base case/PT/PT Walk to stations_Stops and vice versa/24hr Avg Egress Walk Distance_Y2018_RC_walk.csv',
  weights=weights_mean,
  name='egress_distance')
base_pt_walk_walk_transfer_distance <- vitm_to_sa2(
  matrix_location='Y2018_Base case/PT/Walk in PT drive from stations to home/24hr Avg Access Walk Distance_Y2018_RC_PNRAP.csv',
  weights=weights_mean,
  name='transfer_distance')
base_pt_walk_walk_distance <- base_pt_walk_walk_access_distance %>%
  full_join(base_pt_walk_walk_egress_distance) %>%
  full_join(base_pt_walk_walk_transfer_distance) %>%
  mutate(base_pt_walk_walk_distance=access_distance+transfer_distance+egress_distance) %>%
  select(from_sa2,to_sa2,base_pt_walk_walk_distance) %>%
  aggregate_to_sa2_mean()
rm(base_pt_walk_walk_access_distance,
   base_pt_walk_walk_egress_distance,
   base_pt_walk_walk_transfer_distance)

################################################################################
# base_pt_walk_walk_time                                                       #
################################################################################
base_pt_walk_walk_access_time <- vitm_to_sa2(
  matrix_location='Y2018_Base case/PT/PT Walk to stations_Stops and vice versa/24hr Avg Access Walk Time_Y2018_RC_walk.csv',
  weights=weights_mean,
  name='access_time')
base_pt_walk_walk_egress_time <- vitm_to_sa2(
  matrix_location='Y2018_Base case/PT/PT Walk to stations_Stops and vice versa/24hr Avg Egress Walk Time_Y2018_RC_walk.csv',
  weights=weights_mean,
  name='egress_time')
base_pt_walk_walk_transfer_time <- vitm_to_sa2(
  matrix_location='Y2018_Base case/PT/PT Walk to stations_Stops and vice versa/24hr Avg Trf Walk Time_Y2018_RC_walk.csv',
  weights=weights_mean,
  name='transfer_time')
base_pt_walk_walk_time <- base_pt_walk_walk_access_time %>%
  full_join(base_pt_walk_walk_egress_time) %>%
  full_join(base_pt_walk_walk_transfer_time) %>%
  mutate(base_pt_walk_walk_time=access_time+transfer_time+egress_time) %>%
  select(from_sa2,to_sa2,base_pt_walk_walk_time) %>%
  aggregate_to_sa2_mean()
rm(base_pt_walk_walk_access_time,
   base_pt_walk_walk_egress_time,
   base_pt_walk_walk_transfer_time)

################################################################################
# base_pt_walk_total_demand                                                    #
################################################################################
base_pt_walk_total_demand <- vitm_to_sa2(
  matrix_location='Y2018_Base case/PT/PT Walk to stations_Stops and vice versa/PT Walk_24H_Total demand_Y2018_RC_walk.csv',
  weights=weights_sum,
  name='base_pt_walk_total_demand') %>%
  aggregate_to_sa2_sum()



################################################################################################################################################################
# SRL scenario
################################################################################################################################################################


################################################################################
# srl_pt_drive_walk_distance                                                  #
################################################################################
srl_pt_drive_walk_egress_distance_hs <- vitm_to_sa2(
  matrix_location='SRL scenario/PT/Walk in PT drive  from home to stations/24hr Avg Egress Walk Distance_Y2018_SRL_PNRPA.csv',
  weights=weights_mean,
  name='egress_distance')
srl_pt_drive_walk_transfer_distance_hs <- vitm_to_sa2(
  matrix_location='SRL scenario/PT/Walk in PT drive  from home to stations/24hr Avg Transfer Walk Distance_Y2018_SRL_PNRPA.csv',
  weights=weights_mean,
  name='transfer_distance')
srl_pt_drive_walk_access_distance_sh <- vitm_to_sa2(
  matrix_location='SRL scenario/PT/Walk in PT drive from stations to home/24hr Avg Access Walk Distance_Y2018_SRL_PNRAP.csv',
  weights=weights_mean,
  name='access_distance')
srl_pt_drive_walk_transfer_distance_sh <- vitm_to_sa2(
  matrix_location='SRL scenario/PT/Walk in PT drive from stations to home/24hr Avg Transfer Walk Distance_Y2018_SRL_PNRAP.csv',
  weights=weights_mean,
  name='transfer_distance2')
srl_pt_drive_walk_distance <- srl_pt_drive_walk_egress_distance_hs %>%
  full_join(srl_pt_drive_walk_transfer_distance_hs) %>%
  full_join(srl_pt_drive_walk_access_distance_sh) %>%
  full_join(srl_pt_drive_walk_transfer_distance_sh) %>%
  mutate(srl_pt_drive_walk_distance=egress_distance+transfer_distance+access_distance+transfer_distance2) %>%
  select(from_sa2,to_sa2,srl_pt_drive_walk_distance) %>%
  aggregate_to_sa2_mean()
rm(srl_pt_drive_walk_egress_distance_hs,
   srl_pt_drive_walk_transfer_distance_hs,
   srl_pt_drive_walk_access_distance_sh,
   srl_pt_drive_walk_transfer_distance_sh)

################################################################################
# srl_pt_drive_walk_time                                                      #
################################################################################
srl_pt_drive_walk_egress_time_hs <- vitm_to_sa2(
  matrix_location='SRL scenario/PT/Walk in PT drive  from home to stations/24hr Avg Egress Walk Time_Y2018_SRL_PNRPA.csv',
  weights=weights_mean,
  name='egress_time')
srl_pt_drive_walk_transfer_time_hs <- vitm_to_sa2(
  matrix_location='SRL scenario/PT/Walk in PT drive  from home to stations/24hr Avg Trf Walk Time_Y2018_SRL_PNRPA.csv',
  weights=weights_mean,
  name='transfer_time')
srl_pt_drive_walk_access_time_sh <- vitm_to_sa2(
  matrix_location='SRL scenario/PT/Walk in PT drive from stations to home/24hr Avg Access Walk Time_Y2018_SRL_PNRAP.csv',
  weights=weights_mean,
  name='access_time')
srl_pt_drive_walk_transfer_time_sh <- vitm_to_sa2(
  matrix_location='SRL scenario/PT/Walk in PT drive from stations to home/24hr Avg Trf Walk Time_Y2018_SRL_PNRAP.csv',
  weights=weights_mean,
  name='transfer_time2')
srl_pt_drive_walk_time <- srl_pt_drive_walk_egress_time_hs %>%
  full_join(srl_pt_drive_walk_transfer_time_hs) %>%
  full_join(srl_pt_drive_walk_access_time_sh) %>%
  full_join(srl_pt_drive_walk_transfer_time_sh) %>%
  mutate(srl_pt_drive_walk_time=egress_time+transfer_time+access_time+transfer_time2) %>%
  select(from_sa2,to_sa2,srl_pt_drive_walk_time) %>%
  aggregate_to_sa2_mean()
rm(srl_pt_drive_walk_egress_time_hs,
   srl_pt_drive_walk_transfer_time_hs,
   srl_pt_drive_walk_access_time_sh,
   srl_pt_drive_walk_transfer_time_sh)

################################################################################
# srl_pt_drive_total_demand                                                   #
################################################################################
srl_pt_drive_total_demand_hs <- vitm_to_sa2(
  matrix_location='SRL scenario/PT/Walk in PT drive  from home to stations/PT PNRPA_24H_Total demand_Y2018_SRL_PNRPA.csv',
  weights=weights_sum,
  name='demand_hs')
srl_pt_drive_total_demand_sh <- vitm_to_sa2(
  matrix_location='SRL scenario/PT/Walk in PT drive from stations to home/PT PNRAP_24H_Total demand_Y2018_SRL_PNRAP.csv',
  weights=weights_sum,
  name='demand_sh')
srl_pt_drive_total_demand <- full_join(
  srl_pt_drive_total_demand_hs,
  srl_pt_drive_total_demand_sh) %>%
  mutate(srl_pt_drive_total_demand=demand_hs+demand_sh) %>%
  select(from_sa2,to_sa2,srl_pt_drive_total_demand) %>%
  aggregate_to_sa2_sum()
rm(srl_pt_drive_total_demand_hs,
   srl_pt_drive_total_demand_sh)

################################################################################
# srl_pt_walk_walk_distance                                                   #
################################################################################
srl_pt_walk_walk_access_distance <- vitm_to_sa2(
  matrix_location='SRL scenario/PT/PT Walk to stations_Stops and vice versa/24hr Avg Access Walk Distance_Y2018_SRL_walk.csv',
  weights=weights_mean,
  name='access_distance')
srl_pt_walk_walk_egress_distance <- vitm_to_sa2(
  matrix_location='SRL scenario/PT/PT Walk to stations_Stops and vice versa/24hr Avg Egress Walk Distance_Y2018_SRL_walk.csv',
  weights=weights_mean,
  name='egress_distance')
srl_pt_walk_walk_transfer_distance <- vitm_to_sa2(
  matrix_location='SRL scenario/PT/Walk in PT drive from stations to home/24hr Avg Access Walk Distance_Y2018_SRL_PNRAP.csv',
  weights=weights_mean,
  name='transfer_distance')
srl_pt_walk_walk_distance <- srl_pt_walk_walk_access_distance %>%
  full_join(srl_pt_walk_walk_egress_distance) %>%
  full_join(srl_pt_walk_walk_transfer_distance) %>%
  mutate(srl_pt_walk_walk_distance=access_distance+transfer_distance+egress_distance) %>%
  select(from_sa2,to_sa2,srl_pt_walk_walk_distance) %>%
  aggregate_to_sa2_mean()
rm(srl_pt_walk_walk_access_distance,
   srl_pt_walk_walk_egress_distance,
   srl_pt_walk_walk_transfer_distance)

################################################################################
# srl_pt_walk_walk_time                                                       #
################################################################################
srl_pt_walk_walk_access_time <- vitm_to_sa2(
  matrix_location='SRL scenario/PT/PT Walk to stations_Stops and vice versa/24hr Avg Access Walk Time_Y2018_SRL_walk.csv',
  weights=weights_mean,
  name='access_time')
srl_pt_walk_walk_egress_time <- vitm_to_sa2(
  matrix_location='SRL scenario/PT/PT Walk to stations_Stops and vice versa/24hr Avg Egress Walk Time_Y2018_SRL_walk.csv',
  weights=weights_mean,
  name='egress_time')
srl_pt_walk_walk_transfer_time <- vitm_to_sa2(
  matrix_location='SRL scenario/PT/PT Walk to stations_Stops and vice versa/24hr Avg Trf Walk Time_Y2018_SRL_walk.csv',
  weights=weights_mean,
  name='transfer_time')
srl_pt_walk_walk_time <- srl_pt_walk_walk_access_time %>%
  full_join(srl_pt_walk_walk_egress_time) %>%
  full_join(srl_pt_walk_walk_transfer_time) %>%
  mutate(srl_pt_walk_walk_time=access_time+transfer_time+egress_time) %>%
  select(from_sa2,to_sa2,srl_pt_walk_walk_time) %>%
  aggregate_to_sa2_mean()
rm(srl_pt_walk_walk_access_time,
   srl_pt_walk_walk_egress_time,
   srl_pt_walk_walk_transfer_time)

################################################################################
# srl_pt_walk_total_demand                                                    #
################################################################################
srl_pt_walk_total_demand <- vitm_to_sa2(
  matrix_location='SRL scenario/PT/PT Walk to stations_Stops and vice versa/PT Walk_24H_Total demand_Y2018_SRL_walk.csv',
  weights=weights_sum,
  name='srl_pt_walk_total_demand') %>%
  aggregate_to_sa2_sum()

################################################################################################################################################################


pt_df <- base_pt_drive_walk_distance %>%
  full_join(srl_pt_drive_walk_distance) %>%
  full_join(base_pt_drive_walk_time) %>%
  full_join(srl_pt_drive_walk_time) %>%
  full_join(base_pt_drive_total_demand) %>%
  full_join(srl_pt_drive_total_demand) %>%
  full_join(base_pt_walk_walk_distance) %>%
  full_join(srl_pt_walk_walk_distance) %>%
  full_join(base_pt_walk_walk_time) %>%
  full_join(srl_pt_walk_walk_time) %>%
  full_join(base_pt_walk_total_demand) %>%
  full_join(srl_pt_walk_total_demand)
  

rm(base_pt_drive_walk_distance,
   srl_pt_drive_walk_distance,
   base_pt_drive_walk_time,
   srl_pt_drive_walk_time,
   base_pt_drive_total_demand,
   srl_pt_drive_total_demand,
   base_pt_walk_walk_distance,
   srl_pt_walk_walk_distance,
   base_pt_walk_walk_time,
   srl_pt_walk_walk_time,
   base_pt_walk_total_demand,
   srl_pt_walk_total_demand)

write.csv(pt_df, "Data/Processed/srl_pt.csv", row.names=F, quote=T)








sa2 <- st_read("/home/alan/Projects/dot-hia-misc/sa2.sqlite") %>%
  mutate(sa2_main16=as.integer(as.character(sa2_main16))) %>%
  select(sa2=sa2_main16)

sa2_car <- sa2 %>%
  inner_join(car_df)
st_write(sa2,"sa2_car.sqlite",delete_layer=T)

sa2_pt <- sa2 %>%
  inner_join(pt_df)
st_write(sa2,"sa2_pt.sqlite",delete_layer=T)





  # mutate(percent_distance_diff=(srl_car_distance_mean-base_car_distance_mean)/base_car_distance_mean) %>%
  # mutate(percent_time_diff=(srl_car_time_mean-base_car_time_mean)/base_car_time_mean) %>%
  # mutate(percent_person_trips_diff=(srl_car_person_trips-base_car_person_trips)/base_car_person_trips)%>%
  # mutate(percent_distance_diff=(srl_car_distance_mean-base_car_distance_mean)/base_car_distance_mean) %>%
  # mutate(percent_time_diff=(srl_car_time_mean-base_car_time_mean)/base_car_time_mean) %>%
  # mutate(percent_person_trips_diff=(srl_car_person_trips-base_car_person_trips)/base_car_person_trips)


