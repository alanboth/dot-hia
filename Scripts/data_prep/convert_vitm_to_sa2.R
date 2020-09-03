library(sf)
library(dplyr)
library(data.table)
library(dtplyr)


vitm_to_sa2_mean <- function(vitim_matrix_location) {
  # vitim_matrix_location="/home/alan/Projects/dot-hia-misc/Matrices_020920/Y2018_Base case/Car/24hr Avg car distance_Y2018_RC_car.csv"
  
  # calculate weights for mapping for each sa2, the proportion each overlapping
  # vitm region takes
  sa2_vitm <- st_read("./Data/vitm/sa2_intersect_vitm_regions.sqlite",quiet=T) %>%
    mutate(sa2_main16=as.integer(as.character(sa2_main16))) %>%
    mutate(area=as.numeric(st_area(.))) %>%
    st_drop_geometry() %>%
    dplyr::select(sa2_main16,tzn,area) %>%
    group_by(sa2_main16) %>%
    mutate(area_total=sum(area,na.rm=T)) %>%
    ungroup() %>%
    mutate(weight=area/area_total) %>%
    dplyr::select(sa2_main16,tzn,weight)
  
  # ids
  tzn_ids <- unique(sa2_vitm$tzn)%>%sort() 
  sa2_ids <- unique(sa2_vitm$sa2_main16)%>%sort() 
  
  # import matrix location
  vitm_matrix <- read.csv(vitim_matrix_location,
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
  sa2_vitm <- sa2_vitm %>%
    lazy_dt()
  
  # replace from_tzn and to_tzn with sa2 regions proportional to the weights column
  sa2_df <- sa2_vitm %>%
    # join from tzn
    inner_join(vitm_df, by=c("tzn"="from_tzn")) %>%
    mutate(measure=measure*weight) %>%
    group_by(sa2_main16,to_tzn) %>%
    summarise(measure=sum(measure)) %>%
    select(from_sa2=sa2_main16,to_tzn,measure) %>%
    # join to tzn
    inner_join(sa2_vitm, by=c("to_tzn"="tzn")) %>%
    mutate(measure=measure*weight) %>%
    group_by(from_sa2,sa2_main16) %>%
    summarise(measure=sum(measure)) %>%
    select(from_sa2,to_sa2=sa2_main16,measure) %>%
    as.data.frame()
  
  return(sa2_df)
}

vitm_to_sa2_sum <- function(vitim_matrix_location) {
  # vitim_matrix_location="/home/alan/Projects/dot-hia-misc/Matrices_020920/Y2018_Base case/Car/24hr car person trips_Y2018_RC_car.csv"
  
  
  # calculate weights for mapping for each tzn, the proportion each overlapping
  # sa2 region takes
  vitm_sa2 <- st_read("./Data/vitm/sa2_intersect_vitm_regions.sqlite",quiet=T) %>%
    mutate(sa2_main16=as.integer(as.character(sa2_main16))) %>%
    mutate(area=as.numeric(st_area(.))) %>%
    st_drop_geometry() %>%
    dplyr::select(sa2_main16,tzn,area) %>%
    group_by(tzn) %>%
    mutate(area_total=sum(area,na.rm=T)) %>%
    ungroup() %>%
    mutate(weight=area/area_total) %>%
    dplyr::select(sa2_main16,tzn,weight)
  
  # ids
  tzn_ids <- unique(vitm_sa2$tzn)%>%sort() 
  sa2_ids <- unique(vitm_sa2$sa2_main16)%>%sort() 
  
  # import matrix location
  vitm_matrix <- read.csv(vitim_matrix_location,
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
  vitm_sa2 <- vitm_sa2 %>%
    lazy_dt()
  
  # replace from_tzn and to_tzn with sa2 regions proportional to the weights column
  sa2_df <- vitm_sa2 %>%
    # join from tzn
    inner_join(vitm_df, by=c("tzn"="from_tzn")) %>%
    mutate(measure=measure*weight) %>%
    group_by(sa2_main16,to_tzn) %>%
    summarise(measure=sum(measure)) %>%
    select(from_sa2=sa2_main16,to_tzn,measure) %>%
    # join to tzn
    inner_join(vitm_sa2, by=c("to_tzn"="tzn")) %>%
    mutate(measure=measure*weight) %>%
    group_by(from_sa2,sa2_main16) %>%
    summarise(measure=sum(measure)) %>%
    select(from_sa2,to_sa2=sa2_main16,measure) %>%
    as.data.frame()
  
  return(sa2_df)
}

aggregate_to_sa2_mean <- function(sa2_df) {
  result <- sa2_df %>%
    group_by(from_sa2) %>%
    summarise(measure_mean=mean(measure,na.rm=T),measure_sd=sd(measure,na.rm=T)) %>%
    rename(sa2=from_sa2)
  return(result)
}

aggregate_to_sa2_sum <- function(sa2_df) {
  result <- sa2_df %>%
    group_by(from_sa2) %>%
    summarise(measure=sum(measure,na.rm=T)) %>%
    rename(sa2=from_sa2)
  return(result)
}

base_car_distance <- vitm_to_sa2_mean("/home/alan/Projects/dot-hia-misc/Matrices_020920/Y2018_Base case/Car/24hr Avg car distance_Y2018_RC_car.csv") %>%
  aggregate_to_sa2_mean()
base_car_time <- vitm_to_sa2_mean("/home/alan/Projects/dot-hia-misc/Matrices_020920/Y2018_Base case/Car/24hr Avg car Time_Y2018_RC_car.csv") %>%
  aggregate_to_sa2_mean()
base_car_person_trips <- vitm_to_sa2_sum("/home/alan/Projects/dot-hia-misc/Matrices_020920/Y2018_Base case/Car/24hr car person trips_Y2018_RC_car.csv") %>%
  aggregate_to_sa2_sum()

srl_car_distance <- vitm_to_sa2_mean("/home/alan/Projects/dot-hia-misc/Matrices_020920/SRL scenario/Car/24hr Avg car distance_Y2018_SRL_car.csv") %>%
  aggregate_to_sa2_mean()
srl_car_time <- vitm_to_sa2_mean("/home/alan/Projects/dot-hia-misc/Matrices_020920/SRL scenario/Car/24hr Avg car Time_Y2018_SRL_car.csv") %>%
  aggregate_to_sa2_mean()
srl_car_person_trips <- vitm_to_sa2_sum("/home/alan/Projects/dot-hia-misc/Matrices_020920/SRL scenario/Car/24hr car person trips_Y2018_SRL_car.csv") %>%
  aggregate_to_sa2_sum()

car_df <- base_car_distance %>%
  rename(base_car_distance_mean=measure_mean,base_car_distance_sd=measure_sd) %>%
  inner_join(srl_car_distance) %>%
  rename(srl_car_distance_mean=measure_mean,srl_car_distance_sd=measure_sd) %>%
  inner_join(base_car_time) %>%
  rename(base_car_time_mean=measure_mean,base_car_time_sd=measure_sd) %>%
  inner_join(srl_car_time) %>%
  rename(srl_car_time_mean=measure_mean,srl_car_time_sd=measure_sd) %>%
  inner_join(base_car_person_trips) %>%
  rename(base_car_person_trips=measure) %>%
  inner_join(srl_car_person_trips) %>%
  rename(srl_car_person_trips=measure)







sa2 <- st_read("/home/alan/Projects/dot-hia-misc/sa2.sqlite") %>%
  mutate(sa2_main16=as.integer(as.character(sa2_main16))) %>%
  select(sa2=sa2_main16) %>%
  inner_join(car_df) %>%
  mutate(percent_distance_diff=(srl_car_distance_mean-base_car_distance_mean)/base_car_distance_mean) %>%
  mutate(percent_time_diff=(srl_car_time_mean-base_car_time_mean)/base_car_time_mean) %>%
  mutate(percent_person_trips_diff=(srl_car_person_trips-base_car_person_trips)/base_car_person_trips)

st_write(sa2,"sa2_car.sqlite",delete_layer=T)


# 
# # find out what proportion each tzn region contributes to a SA2 region (by area)
# sa2_vitm <- st_read("./Data/vitm/sa2_intersect_vitm_regions.sqlite") %>%
#   mutate(sa2_main16=as.int(as.character(sa2_main16))) %>%
#   mutate(area=as.numeric(st_area(.))) %>%
#   st_drop_geometry() %>%
#   dplyr::select(sa2_main16,tzn,area)
# 
# sa2_weight <- sa2_vitm %>%
#   group_by(sa2_main16) %>%
#   mutate(area_total=sum(area,na.rm=T)) %>%
#   ungroup() %>%
#   mutate(sa2_weight=area/area_total) %>%
#   dplyr::select(sa2_main16,tzn,sa2_weight) %>%
#   as.data.table()
# 
# 
# # # make tzn pop density pop/area
# # 
# # # multiply by pop
# # tzn_weight <- sa2_vitm %>%
# #   group_by(tzn) %>%
# #   mutate(area_total=sum(area,na.rm=T)) %>%
# #   ungroup() %>%
# #   mutate(tzn_weight=area/area_total) %>%
# #   dplyr::select(tzn,sa2_main16,tzn_weight)
# 
# 
# 
# tmp <- sa2_vitim %>%
#   group_by(sa2_main16) %>%
#   mutate(weight_total=sum(weight,na.rm=T))
# 
# 
# 
# car_distance <- read.csv("/home/alan/Projects/dot-hia-misc/Matrices_020920/Y2018_Base case/Car/24hr Avg car distance_Y2018_RC_car.csv",
#                          as.is=T, fileEncoding="UTF-8-BOM")[,-1] %>%
#   as.matrix()
# colnames(car_distance) <- 1:ncol(car_distance)
# rownames(car_distance) <- 1:nrow(car_distance)
# tmp2 
# 
# 
# 
# tzn_ids <- unique(sa2_weight$tzn)%>%sort() 
# sa2_ids <- unique(sa2_weight$sa2_main16)%>%sort() 
# 
# car_distance2 <- car_distance[tzn_ids,tzn_ids]
# 
# vitm <- st_read("/home/alan/Projects/dot-hia-misc/vitm_regions.sqlite") %>%
#   inner_join(car_mean)
# st_write(vitm,"car_mean.sqlite")
# 
# car_mean <- data.frame(tzn=tzn_ids, mean_dist=apply(car_distance2, 1, FUN=mean, na.rm=T))
# 
# 
# carTable <-  as.data.table(as.table(car_distance))[, .(from_tzn = as.integer(V1), to_tzn = as.integer(V2), measure=N)]
# 
# test_car <- car_distance %>% 
#   as.table(car_distance) %>%
#   as.data.frame() %>%
#   mutate(from_tzn=as.integer(Var1),to_tzn=as.integer(Var2)) %>%
#   select(from_tzn,to_tzn,measure=Freq) %>%
#   lazy_dt()
# 
# test_sa2 <- lazy_dt(sa2_weight)
# 
# test <- test_sa2 %>%
#   # join from tzn
#   inner_join(test_car, by=c("tzn"="from_tzn")) %>%
#   mutate(measure=measure*sa2_weight) %>%
#   group_by(sa2_main16,to_tzn) %>%
#   summarise(measure=sum(measure)) %>%
#   select(from_sa2=sa2_main16,to_tzn,measure) %>%
#   # join to tzn
#   inner_join(test_sa2, by=c("to_tzn"="tzn")) %>%
#   mutate(measure=measure*sa2_weight) %>%
#   group_by(from_sa2,sa2_main16) %>%
#   summarise(measure=sum(measure)) %>%
#   select(from_sa2,to_sa2=sa2_main16,measure) %>%
#   as.data.frame()
# 
# library(tidyr)
# test2 <- test %>%
#   pivot_wider(names_from = to_sa2, values_from = measure) %>%
#   as.matrix()
# rownames(test2) <- test2[,1]
# test2 <- test2[,-1]
# 
# sa2_car_mean <- data.frame(sa2_main16=sa2_ids, mean_dist=apply(test2, 1, FUN=mean, na.rm=T))
# 
# sa2 <- st_read("/home/alan/Projects/dot-hia-misc/sa2.sqlite") %>%
#   mutate(sa2_main16=as.numeric(as.character(sa2_main16))) %>%
#   inner_join(sa2_car_mean) %>%
#   select(sa2_main16,mean_dist)
# st_write(sa2,"sa2_car_mean.sqlite",delete_layer=T)
# 


