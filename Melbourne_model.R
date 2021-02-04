#### Model for Melbourne population AUO

# suppressPackageStartupMessages(library(dplyr))
# # suppressPackageStartupMessages(library(readr))
# # suppressPackageStartupMessages(library(data.table))
# suppressPackageStartupMessages(library(tidyr))
# suppressPackageStartupMessages(library(srvyr)) 
# suppressPackageStartupMessages(library(forcats))
# suppressPackageStartupMessages(library(ggplot2))
# suppressPackageStartupMessages(library(scales))
# suppressPackageStartupMessages(library(ggeasy))
# suppressPackageStartupMessages(library(ggridges))
# suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(doParallel))

### Clean Global Environment
rm (list = ls())

### Avoid scientific notation
options(scipen=999)

#### Functions for model
source("Scripts/data_prep/mmet_pp.R")
source("Scripts/ithim-r_wrappers.R")
source("Scripts/data_prep/population_prep.R")


### Outputs location (select your local drive)
scenarioLocation <- "./scenarios"
scenarioTripsLocation <- "./scenarios/scenarioTrips"
outputLocation <- "C:/Users/e95517/OneDrive - RMIT University/HIA-DoT/dot-hia/output/melbourne-outputs-raw" #"/home/alan/DATA/dot-hia/melbourne-outputs-raw"
combinedLocation <- "C:/Users/e95517/OneDrive - RMIT University/HIA-DoT/dot-hia/output/melbourne-outputs-combined" # "/home/alan/DATA/dot-hia/melbourne-outputs-combined"
combinedLocationMMETS <- "C:/Users/e95517/OneDrive - RMIT University/HIA-DoT/dot-hia/output/melbourne-outputs-combined-mmets" #"/home/alan/DATA/dot-hia/melbourne-outputs-combined-mmets"
summarisedLocation <- "C:/Users/e95517/OneDrive - RMIT University/HIA-DoT/dot-hia/output/melbourne-outputs-summarised" #"/home/alan/DATA/dot-hia/melbourne-outputs-summarised"
finalLocation <- "./melbourne-outputs"

### Scenarios 
scenarios_Melb <- read.csv("scenarios_for_melbourne.csv",as.is=T,fileEncoding="UTF-8-BOM") %>%
  mutate(scenario_location=paste0(scenarioLocation,"/",scenario,".csv")) %>%
  mutate(trips_location=paste0(scenarioTripsLocation,"/",scenario,".csv")) %>%
  mutate(output_location=paste0(outputLocation,"/",scenario))
  

### Run model
print(paste0("iterating through ",nrow(scenarios_Melb)," scenarios at ",Sys.time()))
for (i in 1:nrow(scenarios_Melb)){

  number_cores <- max(1,floor(as.integer(detectCores())*0.8))
  cl <- makeCluster(number_cores)
  cat(paste0("About to start processing results in parallel, using ",number_cores," cores\n"))
  persons_matched=read.csv(scenarios_Melb[i,]$scenario_location,as.is=T, fileEncoding="UTF-8-BOM")
  seeds<-1:1000
  registerDoParallel(cl)
  start_time = Sys.time()
  results <- foreach::foreach(seed_current=seeds,
                              .combine=rbind,
                              .verbose=F,
                              .packages=c("dplyr","tidyr","stringr","readr","readxl","data.table","srvyr"),
                              .export=c("calculateMMETSperPerson","CalculationModel","gen_pa_rr_wrapper",
                                        "GetParamters","GetPopulation","GetStDevRR","health_burden_2",
                                        "RunDisease","RunLifeTable")
  ) %dopar%
    CalculationModel(seed=seed_current,
                     output_location=scenarios_Melb[i,]$output_location,
                     persons_matched)
  end_time = Sys.time()
  end_time - start_time
  stopCluster(cl)
  cat(paste0("\n scenario ",i,"/",nrow(scenarios_Melb)," complete at ",Sys.time(),"\n"))
}


#### Save combined outputs (for all scenarios and iterations)
##### Create directories if not created
dir.create(combinedLocation, recursive=TRUE, showWarnings=FALSE)
dir.create(combinedLocationMMETS, recursive=TRUE, showWarnings=FALSE)

print(paste0("merging ",nrow(scenarios_Melb)," scenario outputs into single file at ",Sys.time()))

# Function to combine outputs
combineOutputs <- function(inputDirectory,outputFile) {
  output_df_files<-list.files(inputDirectory,pattern="*.csv",full.names=T)
  output_df<-lapply(output_df_files,read.csv,header=T) %>%
    bind_rows(.id="run") %>%
    mutate(run=as.integer(run))
  saveRDS(output_df, file=outputFile)
}

for (i in 1:nrow(scenarios_Melb)){
  combineOutputs(paste0(scenarios_Melb[i,]$output_location,'/output_df'),
                 paste0(combinedLocation,"/",scenarios_Melb[i,]$scenario,".rds"))
  cat(paste0("\n combined scenario ",i,"/",nrow(scenarios_Melb)," complete at ",Sys.time(),"\n"))
}

for (i in 1:nrow(scenarios_Melb)){
  combineOutputs(paste0(scenarios_Melb[i,]$output_location,'/mmets'),
                 paste0(combinedLocationMMETS,"/",scenarios_Melb[i,]$scenario,".rds"))
  cat(paste0("\n combined mmets scenario ",i,"/",nrow(scenarios_Melb)," complete at ",Sys.time(),"\n"))
}

for (i in 1:nrow(scenarios_Melb)){
  combineOutputs(paste0(scenarios_Melb[i,]$output_location,'/mmets'),
                 paste0(combinedLocationMMETS,"/",scenarios_Melb[i,]$scenario,".rds"))
  cat(paste0("\n combined mmets scenario ",i,"/",nrow(scenarios_Melb)," complete at ",Sys.time(),"\n"))
}

###### ALAN, WHAT IS THIS DOING???

print(paste0("summarising ",nrow(scenarios_Melb)," scenario outputs at ",Sys.time()))
for (i in 1:nrow(scenarios_Melb)){
  output_df <- readRDS(paste0(combinedLocation,"/",scenarios_Melb[i,]$scenario,".rds"))
  summariseOutputs(scenario_location=
                     paste0(summarisedLocation,"/",scenarios_Melb[i,]$scenario),
                   output_df)
  cat(paste0("\n combined scenario ",i,"/",nrow(scenarios_Melb)," complete at ",Sys.time(),"\n"))
}

print(paste0("summarising mmets ",nrow(scenarios_Melb)," scenario outputs at ",Sys.time()))
for (i in 1:nrow(scenarios_Melb)){
  output_df <- readRDS(paste0(combinedLocationMMETS,"/",scenarios_Melb[i,]$scenario,".rds"))
  summariseMMETS(scenario_location=
                     paste0(summarisedLocation,"/",scenarios_Melb[i,]$scenario),
                   output_df)
  cat(paste0("\n combined mmets scenario ",i,"/",nrow(scenarios_Melb)," complete at ",Sys.time(),"\n"))
}

print(paste0("summarising transport modes ",nrow(scenarios_Melb)," scenario outputs at ",Sys.time()))
scenarioTrips<-NULL
for (i in 1:nrow(scenarios_Melb)){
  scenarioTripsCurrent<-summariseTransport(scenarios_Melb[i,]$trips_location,
                                           scenarios_Melb[i,]$scenario)
  scenarioTrips<-bind_rows(scenarioTrips,scenarioTripsCurrent)
  cat(paste0("\n combined mmets scenario ",i,"/",nrow(scenarios_Melb)," complete at ",Sys.time(),"\n"))
}
saveRDS(scenarioTrips,paste0(finalLocation,"/output_transport_modes.rds"))


# combine scenarios into single files

combineScenarios <- function(summarisedLocation,name) {
  scenario_names<-data.frame(scen=list.files(summarisedLocation)) %>%
    mutate(id=row_number())
  file_locations<-list.files(summarisedLocation,
                             pattern=name,
                             full.names=T,recursive=T)
  output_df<-lapply(file_locations,read.csv,header=T) %>%
    bind_rows(.id="id") %>%
    mutate(id=as.integer(id))
  output_df<-inner_join(scenario_names,output_df,by='id')%>%
    dplyr::select(-id)
  return(output_df)
}

output_df_agg_all <- combineScenarios(summarisedLocation,name="output_df_agg.csv") %>%
  rename(age=age_group_final,sex=Gender)
output_diseases_change <- combineScenarios(summarisedLocation,name="output_diseases_change.csv") %>%
  rename(age=age_group_final,sex=Gender)
output_life_expectancy_change <- combineScenarios(summarisedLocation,name="output_life_expectancy_change.csv") %>%
  rename(age=age_group_final,sex=Gender)
output_life_years_change <- combineScenarios(summarisedLocation,name="output_life_years_change.csv") %>%
  rename(age=age_group_final,sex=Gender)
output_mmets <- combineScenarios(summarisedLocation,name="output_mmets.csv")
output_mmets_graph <- combineScenarios(summarisedLocation,name="output_mmets_graph.csv")

# in case the directory hasn't been made yet
dir.create(finalLocation, recursive=TRUE, showWarnings=FALSE)
saveRDS(output_df_agg_all,paste0(finalLocation,"/output_df_agg.rds"))
saveRDS(output_diseases_change,paste0(finalLocation,"/output_diseases_change.rds"))
saveRDS(output_life_expectancy_change,paste0(finalLocation,"/output_life_expectancy_change.rds"))
saveRDS(output_life_years_change,paste0(finalLocation,"/output_life_years_change.rds"))
saveRDS(output_mmets,paste0(finalLocation,"/output_mmets.rds"))
saveRDS(output_mmets_graph,paste0(finalLocation,"/output_mmets_graph.rds"))



###### BELOW CAN BE DELETED

#### Graphs for changes in diseases over time 

### Incidence

data_f <- dplyr::filter(outputs[['output_df_agg_sex']], sex == "female") %>% dplyr::select("sex", "year", contains("diff"))
data_m <- dplyr::filter(outputs[['output_df_agg_sex']], sex == "male") %>% dplyr::select("sex", "year", contains("diff"))
data_t <- dplyr::filter(outputs[['output_df_agg_all']]) %>% dplyr::select("year", contains("diff")) %>%
  mutate(sex ="total")

data <- bind_rows(data_f, data_m, data_t)

plot <- plot <- data %>%
  ggplot(aes(x = year, y = inc_num_diff_brsc)) +
  geom_smooth(method = "loess") +
  geom_smooth(aes(y = inc_num_diff_carc, method = "loess", color="Colon cancer")) +
  geom_smooth(aes(y = inc_num_diff_dmt2, method = "loess", color="Type 2 diabetes")) +
  geom_smooth(aes(y = inc_num_diff_tbalc, method = "loess", color="Lung cancer")) +
  geom_smooth(aes(y = inc_num_diff_utrc, method = "loess", color="Uterine cancer")) +
  geom_smooth(aes(y = inc_num_diff_ishd, method = "loess", color="Ischemic heart disease")) +
  geom_smooth(aes(y = inc_num_diff_strk, method = "loess", color="Stroke")) + 
  labs(x = "Simulation year",
       title = paste("Changes in disease incidence over time"),
       y = "Numbers") +
  labs(color="") +
  theme(plot.title = element_text(hjust = 0.5, size = 12,face="bold"),
        axis.text=element_text(size=10),
        axis.title=element_text(size=10)) +
  theme_classic() +
  geom_hline(yintercept=0, linetype='dashed', color = 'black')+
  facet_wrap(. ~ sex) + 
  theme(
    strip.background = element_blank() ) +
  scale_fill_brewer(name = "Sex") +
  theme(legend.position = "bottom",
        legend.text = element_text(colour = "black", size = 8),
        legend.key = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggsave(plot, file=paste0("./output/diseases/incidence.png"), width = 14, height = 10, units = "cm")


### Deaths

plot <- plot <- data %>%
  ggplot(aes(x = year, y = mx_num_diff_brsc)) +
  geom_smooth(method = "loess") +
  geom_smooth(aes(y = mx_num_diff_carc, method = "loess", color="Colon cancer")) +
  geom_smooth(aes(y = mx_num_diff_dmt2, method = "loess", color="Type 2 diabetes")) +
  geom_smooth(aes(y = mx_num_diff_tbalc, method = "loess", color="Lung cancer")) +
  geom_smooth(aes(y = mx_num_diff_utrc, method = "loess", color="Uterine cancer")) +
  geom_smooth(aes(y = mx_num_diff_ishd, method = "loess", color="Ischemic heart disease")) +
  geom_smooth(aes(y = mx_num_diff_strk, method = "loess", color="Stroke")) + 
  labs(x = "Simulation year",
       title = paste("Changes in disease mortality over time"),
       y = "Numbers") +
  labs(color="") +
  theme(plot.title = element_text(hjust = 0.5, size = 12,face="bold"),
        axis.text=element_text(size=10),
        axis.title=element_text(size=10)) +
  theme_classic() +
  geom_hline(yintercept=0, linetype='dashed', color = 'black')+
  facet_wrap(. ~ sex) + 
  theme(
    strip.background = element_blank() ) +
  scale_fill_brewer(name = "Sex") +
  theme(legend.position = "bottom",
        legend.text = element_text(colour = "black", size = 8),
        legend.key = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggsave(plot, file=paste0("./output/diseases/deaths.png"), width = 14, height = 10, units = "cm")


#### Graph for changes in life years 

data_f <- dplyr::filter(outputs[['output_df_agg_sex']], sex == "female") %>% dplyr::select("sex", "year", "Lx_diff", "Lwx_diff")
data_m <- dplyr::filter(outputs[['output_df_agg_sex']], sex == "male") %>% dplyr::select("sex", "year", "Lx_diff", "Lwx_diff")
data_t <-  dplyr::filter(outputs[['output_df_agg_all']]) %>% dplyr::select("year", "Lx_diff", "Lwx_diff")       

plot <- data_t %>%
  ggplot(aes(x = year, y = Lx_diff)) +
  geom_smooth(method = "loess", aes(color= "Life years total")) +
  geom_smooth(data = data_t, aes(y = Lwx_diff, method = "loess",  color= "HALYs total")) +
  geom_smooth(data = data_f, aes(y = Lx_diff,  method = "loess",  color= "Life years female")) +
  geom_smooth(data = data_f, aes(y = Lwx_diff,  method = "loess",  color= "HALYs female")) +
  geom_smooth(data = data_m, aes(y = Lx_diff,  method = "loess",  color= "Life years male")) +
  geom_smooth(data = data_m, aes(y = Lwx_diff,  method = "loess",  color= "HALYs male")) +
  labs(x = "Simulation year",
       title = paste("Difference life years and health-adjusted life years"),
       y = "Numbers") +
  labs(color="") +
  theme(plot.title = element_text(hjust = 0.5, size = 12,face="bold"),
        axis.text=element_text(size=10),
        axis.title=element_text(size=10)) +
  theme(legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(colour = "black", size = 10),
        legend.key = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme_classic() +
  geom_hline(yintercept=0, linetype='dashed', color = 'black')
ggsave(plot, file=paste0("./output/LifeYears/lifeyears.png"), width = 14, height = 10, units = "cm")



