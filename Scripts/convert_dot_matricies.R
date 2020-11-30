library(sf)
library(dplyr)
library(data.table)
library(dtplyr)


# load functions and region mapping ---------------------------------------

sa2 <- st_read("/home/alan/DATA/dot-hia/sa2.sqlite") %>%
  st_drop_geometry() %>%
  mutate(sa2_main16=as.integer(as.character(sa2_main16))) %>%
  select(sa2_main16,gcc_name16) %>%
  filter(gcc_name16=='Greater Melbourne')
  
sa2_tzn_mapping <- st_read("./Data/vitm/sa2_intersect_vitm_regions.sqlite",quiet=T) %>%
  st_drop_geometry() %>%
  filter(sa2_main16 %in% sa2$sa2_main16) %>%
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
  if(aggregation_type %in% c('sum','count')) weights <- weights_sum
  if(aggregation_type %in% c('mean','distance','time')) weights <- weights_mean
  
  # ids
  tzn_ids <- unique(weights$tzn)%>%sort() 
  sa2_ids <- unique(weights$sa2_main16)%>%sort() 
  
  # import matrix location
  vitm_matrix <- read.csv(matrix_location,
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
  
  if(aggregation_type %in% c('sum','count')) {
    sa2_df <- sa2_df %>%
      group_by(from_sa2) %>%
      summarise(!!name:=sum(measure,na.rm=T)) %>%
      rename(sa2=from_sa2)
  }
  measure_mean=paste0(name,".mean")
  measure_sd=paste0(name,".sd")
  
  if(aggregation_type %in% c('mean','distance','time')) {
    sa2_df <- sa2_df %>%
      group_by(from_sa2) %>%
      summarise(!!measure_mean:=mean(measure,na.rm=T),!!measure_sd:=sd(measure,na.rm=T)) %>%
      rename(sa2=from_sa2)
  }
  return(sa2_df)
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
outputMatricesLocation <- "./matricies/"

matriciesInfo <- read.csv("./Data/vitm/matriciesInfo.csv", as.is=T, fileEncoding="UTF-8-BOM") %>%
  mutate(full_location=paste0(matriciesLocation,"/",folder,"/",file))

matriciesGrouped <- matriciesInfo %>%
  group_by(scenario, measure, mode) %>%
  # we're using : to separate the files since that character isn't allowed in
  # file names
  summarise(file=paste(full_location,collapse=":")) %>%
  ungroup() %>%
  mutate(name=paste0(matriciesGrouped$scenario,'_',
         matriciesGrouped$mode,'_',matriciesGrouped$measure)) %>%
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



# aggregate to sa2 --------------------------------------------------------
tmp <- aggregate_to_sa2(
  matrix_location=matriciesGrouped[i,]$location,
  aggregation_type=matriciesGrouped[i,]$measure,
  name=matriciesGrouped[i,]$name
)


suppressPackageStartupMessages(library(doParallel))

setDTthreads(1)
number_cores <- max(1,floor(as.integer(detectCores())*0.5))
cl <- makeCluster(number_cores)
cat(paste0("About to start processing aggregations in parallel, using ",number_cores," cores\n"))
cat(paste0("Now processing the ",nrow(matriciesGrouped)," matricies\n"))

registerDoParallel(cl)
start_time = Sys.time()
results <- foreach(matrix_location=matriciesGrouped$location,
                   aggregation=matriciesGrouped$measure,
                   name=matriciesGrouped$name,
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


