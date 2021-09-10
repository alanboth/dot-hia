# libraries and functions -------------------------------------------------
library(sf)
library(dplyr)
library(data.table)
library(dtplyr)



# load functions and region mapping ---------------------------------------

lga_tzn_mapping <- st_read('./Data/vitm/lga_intersect_vitm_regions.sqlite') %>%
  st_drop_geometry()
# tzn ids within Greater Melbourne. 
tzn_ids <<- unique(lga_tzn_mapping$tzn)%>%sort() 


# calculate weights for mapping for each lga, the proportion each overlapping
# vitm region takes
# weights_by_lga
weights_mean <- lga_tzn_mapping %>%
  group_by(lga_code) %>%
  mutate(area_total=sum(area,na.rm=T)) %>%
  ungroup() %>%
  mutate(weight=area/area_total) %>%
  dplyr::select(lga_code,tzn,weight)

# calculate weights for mapping for each tzn, the proportion each overlapping
# lga region takes
# weights_by_tzn
weights_sum <- lga_tzn_mapping %>%
  group_by(tzn) %>%
  mutate(area_total=sum(area,na.rm=T)) %>%
  ungroup() %>%
  mutate(weight=area/area_total) %>%
  dplyr::select(lga_code,tzn,weight)

aggregateMatrix <- function(scenario,mode,matrix_location,count_location,distance_location,time_location) {
  # scenario<-matriciesFinal[2,]$scenario
  # mode<-matriciesFinal[2,]$mode
  # count_location<-matriciesFinal[2,]$count
  # distance_location<-matriciesFinal[2,]$distance
  # time_location<-matriciesFinal[2,]$time
  
  count_matrix <- read.csv(count_location,
                           as.is=T, fileEncoding="UTF-8-BOM")[,-1] %>%
    as.matrix()
  # rename columns and rows
  colnames(count_matrix) <- 1:ncol(count_matrix)
  rownames(count_matrix) <- 1:nrow(count_matrix)
  count_matrix <- count_matrix[tzn_ids,tzn_ids]
  
  trip_count<-sum(count_matrix)
  weight_matrix <- count_matrix/trip_count
  
  # distance
  distance_matrix <- read.csv(distance_location,
                              as.is=T, fileEncoding="UTF-8-BOM")[,-1] %>%
    as.matrix()
  # rename columns and rows
  colnames(distance_matrix) <- 1:ncol(distance_matrix)
  rownames(distance_matrix) <- 1:nrow(distance_matrix)
  distance_matrix <- distance_matrix[tzn_ids,tzn_ids]
  mean_distance <- sum(weight_matrix*distance_matrix)

  # time
  time_matrix <- read.csv(time_location,
                              as.is=T, fileEncoding="UTF-8-BOM")[,-1] %>%
    as.matrix()
  # rename columns and rows
  colnames(time_matrix) <- 1:ncol(time_matrix)
  rownames(time_matrix) <- 1:nrow(time_matrix)
  time_matrix <- time_matrix[tzn_ids,tzn_ids]
  mean_time <- sum(weight_matrix*time_matrix)
  
  results<-data.frame(
    scenario=scenario,
    mode=mode,
    count=trip_count,
    distance=mean_distance,
    time=mean_time
  )
  return(results)
}

read_matrix <- function(matrix_location) {
  # import matrix location
  vitm_matrix <- read.csv(matrix_location,
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



# load matrices info ------------------------------------------------------
matriciesLocation <- "/home/alan/DATA/cloudstor/Projects/dot-hia/od-matricies"
outputMatricesLocation <- "./matriciesLGA/"

matriciesInfo <- read.csv("./Data/vitm/matriciesInfo.csv", as.is=T, fileEncoding="UTF-8-BOM") %>%
  mutate(full_location=paste0(matriciesLocation,"/",folder,"/",file))

matriciesGrouped <- matriciesInfo %>%
  group_by(scenario, measure, mode) %>%
  # we're using : to separate the files since that character isn't allowed in
  # file names
  summarise(file=paste(full_location,collapse=":")) %>%
  ungroup() %>%
  mutate(name=paste0(scenario,'_',
                     mode,'_',measure)) %>%
  mutate(location=paste0(outputMatricesLocation,name,".csv"))



# combine matrices --------------------------------------------------------
# in case the directory hasn't been made yet
dir.create(outputMatricesLocation, recursive=TRUE, showWarnings=FALSE)

# iterate through each entry in matriciesGrouped, creating the matched population
print(paste0("iterating through ",nrow(matriciesGrouped)," matricies at ",Sys.time()))
for (i in 1:nrow(matriciesGrouped)){
  files <- strsplit(matriciesGrouped[i,]$file,":") %>% unlist()
  groupedMatrix <- sum_matricies(files)
  write.csv(groupedMatrix,
            matriciesGrouped[i,]$location,
            quote=F,row.names=T)
  
  cat(paste0("\n matrix ",i,"/",nrow(matriciesGrouped)," complete at ",Sys.time(),"\n"))
}


# aggregate to Greater Melbourne ------------------------------------------
# run through each matricies grouped, calculate values.

matriciesFinal <- matriciesGrouped %>%
  dplyr::select(scenario,measure,mode,location) %>%
  pivot_wider(names_from=measure,values_from=location)


resultsFinal <- NULL


print(paste0("iterating through ",nrow(matriciesFinal)," matrix groups at ",Sys.time()))
for (i in 1:nrow(matriciesFinal)){
  resultsCurrent <- aggregateMatrix(
    scenario=matriciesFinal[i,]$scenario,
    mode=matriciesFinal[i,]$mode,
    count_location=matriciesFinal[i,]$count,
    distance_location=matriciesFinal[i,]$distance,
    time_location=matriciesFinal[i,]$time
  )
  resultsFinal <- bind_rows(resultsFinal,resultsCurrent)
  cat(paste0("\n matrix group ",i,"/",nrow(matriciesFinal)," complete at ",Sys.time(),"\n"))
}

resultsFinalProportion <- resultsFinal %>%
  group_by(scenario) %>%
  mutate(proportion=count/sum(count,na.rm=T)) %>%
  ungroup() %>%
  dplyr::select(scenario,mode,proportion,distance,time,trip_count=count)


write.csv(resultsFinalProportion, "Data/processed/DOT_df.csv", row.names=F, quote=T)


