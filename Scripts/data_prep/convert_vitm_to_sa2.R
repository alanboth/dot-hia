library(sf)
library(dplyr)
library(data.table)
library(dtplyr)

matricies_location='/home/alan/Projects/dot-hia-misc/Matrices_020920/'
matricies_location='/home/alan/DATA/dot-hia/PT_Matrices_17_09_20/' 


# clean spatial data
sa2_intersect_vitm_regions <- st_read("/home/alan/DATA/dot-hia/sa2_intersect_vitm_regions_raw.sqlite",quiet=T) %>%
  mutate(sa2_main16=as.integer(as.character(sa2_main16))) %>%
  mutate(tzn=as.integer(as.character(tzn))) %>%
  mutate(area=as.numeric(st_area(.))) %>%
  dplyr::select(sa2_main16,tzn,area) %>%
  st_set_crs(28355)
st_write(sa2_intersect_vitm_regions,"./Data/vitm/sa2_intersect_vitm_regions.sqlite",delete_layer=T)

sa2 <- st_read("/home/alan/DATA/dot-hia/sa2.sqlite") %>%
  mutate(sa2_main16=as.integer(as.character(sa2_main16))) %>%
  select(sa2_main16,gcc_name16)
st_write(sa2,"./Data/vitm/sa2.sqlite",delete_layer=T)

vitm_regions <- st_read("/home/alan/DATA/dot-hia/vitm_regions.sqlite") %>%
  mutate(tzn=as.integer(as.character(tzn)))
st_write(vitm_regions,"./Data/vitm/vitm_regions.sqlite",delete_layer=T)

# limiting to the Greater Melbourne study region
mb <- st_read(
  "~/Datasets/ABS/2019/1270055001_ASGS_2016_vol_1_geopackage/ASGS 2016 Volume 1.gpkg",
  query="SELECT * FROM MB_2016_AUST WHERE GCCSA_NAME_2016='Greater Melbourne'") %>%
  mutate(mb_code16=as.numeric(as.character(MB_CODE_2016)),
         sa2_main16=as.numeric(as.character(SA2_MAINCODE_2016))) %>%
  dplyr::select(mb_code16,sa2_main16)
st_write(mb,"./Data/vitm/mb.sqlite",delete_layer=T)





sa2_tzn_mapping <- st_read("./Data/vitm/sa2_intersect_vitm_regions.sqlite",quiet=T) %>%
  st_drop_geometry() %>%
  dplyr::select(sa2_main16,tzn,area)

# calculate weights for mapping for each sa2, the proportion each overlapping
# vitm region takes
# weights_by_sa2
weights_mean <- sa2_tzn_mapping %>%
  group_by(sa2_main16) %>%
  mutate(area_total=sum(area,na.rm=T)) %>%
  ungroup() %>%
  mutate(weight=area/area_total) %>%
  dplyr::select(sa2_main16,tzn,weight)

# calculate weights for mapping for each tzn, the proportion each overlapping
# sa2 region takes
# weights_by_tzn
weights_sum <- sa2_tzn_mapping %>%
  group_by(tzn) %>%
  mutate(area_total=sum(area,na.rm=T)) %>%
  ungroup() %>%
  mutate(weight=area/area_total) %>%
  dplyr::select(sa2_main16,tzn,weight)

aggregate_to_sa2 <- function(matrix_location, aggregation_type, name) {
  setDTthreads(1)
  weights <- NA
  if(aggregation_type=='sum') weights <- weights_sum
  if(aggregation_type=='mean') weights <- weights_mean
  
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
    select(from_sa2,to_sa2=sa2_main16,measure) %>%
    as.data.frame()
  
  if(aggregation_type=='sum') {
    sa2_df <- sa2_df %>%
      group_by(from_sa2) %>%
      summarise(!!name:=sum(measure,na.rm=T)) %>%
      rename(sa2=from_sa2)
  }
  measure_mean=paste0(name,"_mean")
  measure_sd=paste0(name,"_sd")
  
  if(aggregation_type=='mean') {
    sa2_df <- sa2_df %>%
      group_by(from_sa2) %>%
      summarise(!!measure_mean:=mean(measure,na.rm=T),!!measure_sd:=sd(measure,na.rm=T)) %>%
      rename(sa2=from_sa2)
  }
  return(sa2_df)
}



aggregate_to_tzn <- function(matrix_location, aggregation_type, name) {
  setDTthreads(1)
  weights <- NA
  if(aggregation_type=='sum') weights <- weights_sum
  if(aggregation_type=='mean') weights <- weights_mean
  
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
  
  result <- NA
  
  if(aggregation_type=='sum') {
    results<- data.frame(tzn=rownames(vitm_matrix),
                         measure=apply(vitm_matrix, 1, FUN=sum, na.rm=T))
  }

  if(aggregation_type=='mean') {
    results<- data.frame(tzn=rownames(vitm_matrix),
                         measure=apply(vitm_matrix, 1, FUN=mean, na.rm=T))
  }
  results <- results %>%
    rename(!!name:=measure)
  return(results)
}

read_matrix <- function(matrix_location) {
  # import matrix location
  vitm_matrix <- read.csv(paste0(matricies_location,matrix_location),
                          as.is=T, fileEncoding="UTF-8-BOM")[,-1] %>%
    as.matrix()
  # rename columns and rows
  colnames(vitm_matrix) <- 1:ncol(vitm_matrix)
  rownames(vitm_matrix) <- 1:nrow(vitm_matrix)
  return(vitm_matrix)
}

sum_matricies <- function(matrix_locations) {
  matricies<-lapply(matrix_locations,read_matrix)
  return(Reduce('+', matricies))
}

# BASE PT DRIVE
base_pt_drive_distance <- sum_matricies(c(
  'Y2018_Base case/PT/Walk in PT drive  from home to stations/24hr Avg Egress Walk Distance_Y2018_RC_PNRPA.csv',
  'Y2018_Base case/PT/Walk in PT drive  from home to stations/24hr Avg Transfer Walk Distance_Y2018_RC_PNRPA.csv',
  'Y2018_Base case/PT/Walk in PT drive from stations to home/24hr Avg Access Walk Distance_Y2018_RC_PNRAP.csv',
  'Y2018_Base case/PT/Walk in PT drive from stations to home/24hr Avg Transfer Walk Distance_Y2018_RC_PNRAP.csv'
))
write.csv(base_pt_drive_distance,
          paste0(matricies_location,'Y2018_Base case/PT/base_pt_drive_distance.csv'),
          quote=F,row.names=T)

base_pt_drive_time <- sum_matricies(c(
  'Y2018_Base case/PT/Walk in PT drive  from home to stations/24hr Avg Egress Walk Time_Y2018_RC_PNRPA.csv',
  'Y2018_Base case/PT/Walk in PT drive  from home to stations/24hr Avg Trf Walk Time_Y2018_RC_PNRPA.csv',
  'Y2018_Base case/PT/Walk in PT drive from stations to home/24hr Avg Access Walk Time_Y2018_RC_PNRAP.csv',
  'Y2018_Base case/PT/Walk in PT drive from stations to home/24hr Avg Trf Walk Time_Y2018_RC_PNRAP.csv'
))
write.csv(base_pt_drive_time,
          paste0(matricies_location,'Y2018_Base case/PT/base_pt_drive_time.csv'),
          quote=F,row.names=T)

base_pt_drive_demand <- sum_matricies(c(
  'Y2018_Base case/PT/Walk in PT drive  from home to stations/PT PNRPA_24H_Total demand_Y2018_RC_PNRPA.csv',
  'Y2018_Base case/PT/Walk in PT drive from stations to home/PT PNRAP_24H_Total demand_Y2018_RC_PNRAP.csv'
))
write.csv(base_pt_drive_demand,
          paste0(matricies_location,'Y2018_Base case/PT/base_pt_drive_demand.csv'),
          quote=F,row.names=T)

# BASE PT WALK
base_pt_walk_distance <- sum_matricies(c(
  'Y2018_Base case/PT/PT Walk to stations_Stops and vice versa/24hr Avg Access Walk Distance_Y2018_RC_walk.csv',
  'Y2018_Base case/PT/PT Walk to stations_Stops and vice versa/24hr Avg Egress Walk Distance_Y2018_RC_walk.csv',
  'Y2018_Base case/PT/PT Walk to stations_Stops and vice versa/24hr Avg Transfer Walk Distance_Y2018_RC_walk.csv'
))
write.csv(base_pt_walk_distance,
          paste0(matricies_location,'Y2018_Base case/PT/base_pt_walk_distance.csv'),
          quote=F,row.names=T)

base_pt_walk_time <- sum_matricies(c(
  'Y2018_Base case/PT/PT Walk to stations_Stops and vice versa/24hr Avg Access Walk Time_Y2018_RC_walk.csv',
  'Y2018_Base case/PT/PT Walk to stations_Stops and vice versa/24hr Avg Egress Walk Distance_Y2018_RC_walk.csv',
  'Y2018_Base case/PT/PT Walk to stations_Stops and vice versa/24hr Avg Transfer Walk Distance_Y2018_RC_walk.csv'
))
write.csv(base_pt_walk_time,
          paste0(matricies_location,'Y2018_Base case/PT/base_pt_walk_time.csv'),
          quote=F,row.names=T)

# SRL PT DRIVE
srl_pt_drive_distance <- sum_matricies(c(
  'SRL scenario/PT/Walk in PT drive  from home to stations/24hr Avg Egress Walk Distance_Y2018_SRL_PNRPA.csv',
  'SRL scenario/PT/Walk in PT drive  from home to stations/24hr Avg Transfer Walk Distance_Y2018_SRL_PNRPA.csv',
  'SRL scenario/PT/Walk in PT drive from stations to home/24hr Avg Access Walk Distance_Y2018_SRL_PNRAP.csv',
  'SRL scenario/PT/Walk in PT drive from stations to home/24hr Avg Transfer Walk Distance_Y2018_SRL_PNRAP.csv'
))
write.csv(srl_pt_drive_distance,
          paste0(matricies_location,'SRL scenario/PT/srl_pt_drive_distance.csv'),
          quote=F,row.names=T)

srl_pt_drive_time <- sum_matricies(c(
  'SRL scenario/PT/Walk in PT drive  from home to stations/24hr Avg Egress Walk Time_Y2018_SRL_PNRPA.csv',
  'SRL scenario/PT/Walk in PT drive  from home to stations/24hr Avg Trf Walk Time_Y2018_SRL_PNRPA.csv',
  'SRL scenario/PT/Walk in PT drive from stations to home/24hr Avg Access Walk Time_Y2018_SRL_PNRAP.csv',
  'SRL scenario/PT/Walk in PT drive from stations to home/24hr Avg Trf Walk Time_Y2018_SRL_PNRAP.csv'
))
write.csv(srl_pt_drive_time,
          paste0(matricies_location,'SRL scenario/PT/srl_pt_drive_time.csv'),
          quote=F,row.names=T)

srl_pt_drive_demand <- sum_matricies(c(
  'SRL scenario/PT/Walk in PT drive  from home to stations/PT PNRPA_24H_Total demand_Y2018_SRL_PNRPA.csv',
  'SRL scenario/PT/Walk in PT drive from stations to home/PT PNRAP_24H_Total demand_Y2018_SRL_PNRAP.csv'
))
write.csv(srl_pt_drive_demand,
          paste0(matricies_location,'SRL scenario/PT/srl_pt_drive_demand.csv'),
          quote=F,row.names=T)

# SRL PT WALK
srl_pt_walk_distance <- sum_matricies(c(
  'SRL scenario/PT/PT Walk to stations_Stops and vice versa/24hr Avg Access Walk Distance_Y2018_SRL_walk.csv',
  'SRL scenario/PT/PT Walk to stations_Stops and vice versa/24hr Avg Egress Walk Distance_Y2018_SRL_walk.csv',
  'SRL scenario/PT/PT Walk to stations_Stops and vice versa/24hr Avg Transfer Walk Distance_Y2018_SRL_walk.csv'
))
write.csv(srl_pt_walk_distance,
          paste0(matricies_location,'SRL scenario/PT/srl_pt_walk_distance.csv'),
          quote=F,row.names=T)

srl_pt_walk_time <- sum_matricies(c(
  'SRL scenario/PT/PT Walk to stations_Stops and vice versa/24hr Avg Access Walk Time_Y2018_SRL_walk.csv',
  'SRL scenario/PT/PT Walk to stations_Stops and vice versa/24hr Avg Egress Walk Distance_Y2018_SRL_walk.csv',
  'SRL scenario/PT/PT Walk to stations_Stops and vice versa/24hr Avg Transfer Walk Distance_Y2018_SRL_walk.csv'
))
write.csv(srl_pt_walk_time,
          paste0(matricies_location,'SRL scenario/PT/srl_pt_walk_time.csv'),
          quote=F,row.names=T)














files <- tribble(
  ~scenario, ~mode    , ~measure      , ~aggregation, ~matrix_location,
  # base_car
  'base'   ,'car'     ,'distance'     ,'mean'       , 'Y2018_Base case/Car/24hr Avg car distance_Y2018_RC_car.csv',
  'base'   ,'car'     ,'time'         ,'mean'       , 'Y2018_Base case/Car/24hr Avg car Time_Y2018_RC_car.csv',
  'base'   ,'car'     ,'person_trips' ,'sum'        , 'Y2018_Base case/Car/24hr car person trips_Y2018_RC_car.csv',
       
  # srl_car       
  'srl'    ,'car'     ,'distance'     ,'mean'       , 'SRL scenario/Car/24hr Avg car distance_Y2018_SRL_car.csv',
  'srl'    ,'car'     ,'time'         ,'mean'       , 'SRL scenario/Car/24hr Avg car Time_Y2018_SRL_car.csv',
  'srl'    ,'car'     ,'person_trips' ,'sum'        , 'SRL scenario/Car/24hr car person trips_Y2018_SRL_car.csv',
       
         
  # base_pt_drive       
  'base'   ,'pt_drive','distance'     ,'mean'       , 'Y2018_Base case/PT/base_pt_drive_distance.csv',
  'base'   ,'pt_drive','time'         ,'mean'       , 'Y2018_Base case/PT/base_pt_drive_time.csv',
  'base'   ,'pt_drive','total_demand' ,'sum'        , 'Y2018_Base case/PT/base_pt_drive_demand.csv',
       
  # base_pt_walk       
  'base'   ,'pt_walk' ,'distance'     ,'mean'       , 'Y2018_Base case/PT/base_pt_walk_distance.csv',
  'base'   ,'pt_walk' ,'time'         ,'mean'       , 'Y2018_Base case/PT/base_pt_walk_time.csv',
  'base'   ,'pt_walk' ,'total_demand' ,'sum'        , 'Y2018_Base case/PT/PT Walk to stations_Stops and vice versa/PT Walk_24H_Total demand_Y2018_RC_walk.csv',
         
         
  # srl_pt_drive       
  'srl'    ,'pt_drive','distance'     ,'mean'       , 'SRL scenario/PT/srl_pt_drive_distance.csv',
  'srl'    ,'pt_drive','time'         ,'mean'       , 'SRL scenario/PT/srl_pt_drive_time.csv',
  'srl'    ,'pt_drive','total_demand' ,'sum'        , 'SRL scenario/PT/srl_pt_drive_demand.csv',
         
  # srl_pt_walk       
  'srl'    ,'pt_walk' ,'distance'     ,'mean'       , 'SRL scenario/PT/srl_pt_walk_distance.csv',
  'srl'    ,'pt_walk' ,'time'         ,'mean'       , 'SRL scenario/PT/srl_pt_walk_time.csv',
  'srl'    ,'pt_walk' ,'total_demand' ,'sum'        , 'SRL scenario/PT/PT Walk to stations_Stops and vice versa/PT Walk_24H_Total demand_Y2018_SRL_walk.csv',
) %>%
  mutate(name=paste0(scenario,"_",mode,"_",measure))

# sa2_aggregations <- mapply(aggregate_to_sa2,files$matrix_location,files$aggregation,files$name)
# 
# sa2_aggregations <- mapply(aggregate_to_sa2,files$matrix_location[1:2],files$aggregation[1:2],files$name[1:2],SIMPLIFY=F)

suppressPackageStartupMessages(library(doParallel))

setDTthreads(1)
number_cores <- max(1,floor(as.integer(detectCores())*0.5))
cl <- makeCluster(number_cores)
cat(paste0("About to start processing aggregations in parallel, using ",number_cores," cores\n"))
cat(paste0("Now processing the ",nrow(files)," matricies\n"))

registerDoParallel(cl)
start_time = Sys.time()
results <- foreach(matrix_location=files$matrix_location,
                   aggregation=files$aggregation,
                   name=files$name,
                   .combine=cbind,
                   .verbose=FALSE,
                   .packages=c("dplyr","data.table","dtplyr")
) %dopar%
  aggregate_to_sa2(matrix_location,aggregation,name)
end_time = Sys.time()
end_time - start_time
stopCluster(cl)

sa2_df <- bind_cols(data.frame(sa2=results$sa2),
                    results%>%dplyr::select(-sa2))


write.csv(sa2_df, "Data/processed/sa2_df.csv", row.names=F, quote=T)



setDTthreads(1)
number_cores <- max(1,floor(as.integer(detectCores())*0.5))
cl <- makeCluster(number_cores)
cat(paste0("About to start processing aggregations in parallel, using ",number_cores," cores\n"))
cat(paste0("Now processing the ",nrow(files)," matricies\n"))

registerDoParallel(cl)
start_time = Sys.time()
results_tzn <- foreach(matrix_location=files$matrix_location,
                       aggregation=files$aggregation,
                       name=files$name,
                       .combine=cbind,
                       .verbose=FALSE,
                       .packages=c("dplyr","data.table","dtplyr")
) %dopar%
  aggregate_to_tzn(matrix_location,aggregation,name)
end_time = Sys.time()
end_time - start_time
stopCluster(cl)

tzn_df <- bind_cols(data.frame(tzn=results_tzn$tzn),
                    results_tzn%>%dplyr::select(-tzn)) %>%
  mutate(tzn=as.integer(as.character(tzn)))
write.csv(tzn_df, "Data/processed/tzn_df.csv", row.names=F, quote=T)

tzn <- st_read("/home/alan/DATA/dot-hia/vitm_regions.sqlite") %>%
  mutate(tzn=as.integer(as.character(tzn)))


tzn_results <- tzn %>%
  inner_join(tzn_df)
st_write(tzn_results,"tzn_results.sqlite",delete_layer=T)



sa2 <- st_read("/home/alan/DATA/dot-hia/sa2.sqlite") %>%
  mutate(sa2_main16=as.integer(as.character(sa2_main16))) %>%
  select(sa2=sa2_main16)

sa2_results <- sa2 %>%
  inner_join(sa2_df)
st_write(sa2_results,"sa2_results.sqlite",delete_layer=T)

library(ggplot2)

ggplot(sa2_df, aes(x=base_pt_walk_total_demand,y=srl_pt_walk_total_demand)) +
  geom_point(alpha=0.7) +
  geom_path(stat="function",fun= function(x) {x}, color="#4daf4a", size=0.75, linetype="dashed") +
  scale_x_log10() +
  scale_y_log10()

diff_walk_total_demand <- sa2_df %>%
  mutate(diff = (srl_pt_walk_total_demand-base_pt_walk_total_demand) / base_pt_walk_total_demand)

ggplot(diff_walk_total_demand, aes(x = diff)) +
  geom_histogram()

  # mutate(percent_distance_diff=(srl_car_distance_mean-base_car_distance_mean)/base_car_distance_mean) %>%
  # mutate(percent_time_diff=(srl_car_time_mean-base_car_time_mean)/base_car_time_mean) %>%
  # mutate(percent_person_trips_diff=(srl_car_person_trips-base_car_person_trips)/base_car_person_trips)%>%
  # mutate(percent_distance_diff=(srl_car_distance_mean-base_car_distance_mean)/base_car_distance_mean) %>%
  # mutate(percent_time_diff=(srl_car_time_mean-base_car_time_mean)/base_car_time_mean) %>%
  # mutate(percent_person_trips_diff=(srl_car_person_trips-base_car_person_trips)/base_car_person_trips)


# CAR total demand SRL
vitm_count('Y2018_Base case/Car/24hr car person trips_Y2018_RC_car.csv')
17072376

# CAR total demand SRL
vitm_count('SRL scenario/Car/24hr car person trips_Y2018_SRL_car.csv')
17056116


# PT total demand base case
vitm_count('Y2018_Base case/PT/Walk in PT drive  from home to stations/PT PNRPA_24H_Total demand_Y2018_RC_PNRPA.csv')
24624002
  212104

vitm_count('Y2018_Base case/PT/Walk in PT drive from stations to home/PT PNRAP_24H_Total demand_Y2018_RC_PNRAP.csv')
       0
  201258.6

vitm_count('Y2018_Base case/PT/PT Walk to stations_Stops and vice versa/PT Walk_24H_Total demand_Y2018_RC_walk.csv')
25589669
 1031603

# PT total demand SRL
vitm_count('SRL scenario/PT/Walk in PT drive  from home to stations/PT PNRPA_24H_Total demand_Y2018_SRL_PNRPA.csv')
24816529
  219758.7

vitm_count('SRL scenario/PT/Walk in PT drive from stations to home/PT PNRAP_24H_Total demand_Y2018_SRL_PNRAP.csv')
       0
  208423.7

vitm_count('SRL scenario/PT/PT Walk to stations_Stops and vice versa/PT Walk_24H_Total demand_Y2018_SRL_walk.csv')
25123375
 1033259



vitm_count <- function(matrix_location) {
  # matrix_location='SRL scenario/PT/Walk in PT drive from stations to home/PT PNRAP_24H_Total demand_Y2018_SRL_PNRAP.csv'
  weights=weights_sum

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
  
  return(sum(vitm_matrix))
}
