### Proportional multi-state life table model to calculate changes in: life years, health-adjusted life years, life expectancy, 
### health-adjusted life expectancy and changes in diseases' incidence and mortality numbers for a changes in transport modes.
### The model is parametrised for the Melbourne Greater Area Population. 
### Detailed explanation can be found in TechnicalDoc.


### Packages to run code
suppressPackageStartupMessages(library(dplyr))
# suppressPackageStartupMessages(library(readr))
# suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(srvyr)) 
suppressPackageStartupMessages(library(forcats))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(scales))
suppressPackageStartupMessages(library(ggeasy))
suppressPackageStartupMessages(library(ggridges))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(doParallel))

### Clean Global Environment
rm (list = ls())

### Avoid scientific notation
options(scipen=999)

### Model order: 1) Get matched population for scenarios, 2) Run HIA, 3) summarize outputs, and 4) AUO output sample


######################################## 1) Matched population ###########################################################################



# persons_matched <- read.csv("scenarios/all_2_10.csv", as.is=T, fileEncoding="UTF-8-BOM")



######################################## 2) Run HIA  ###########################################################################


source("Scripts/data_prep/mmet_pp.R")
source("Scripts/ithim-r_wrappers.R")
source("Scripts/data_prep/population_prep.R")


# ### Get age and sex  ## BZ: added based on scenarios_MEL age adn sex

scenarioLocation <- "./scenarios"
outputLocation <- "/home/alan/DATA/dot-hia/melbourne-outputs-raw"
combinedLocation <- "/home/alan/DATA/dot-hia/melbourne-outputs-combined"
summarisedLocation <- "/home/alan/DATA/dot-hia/melbourne-outputs-summarised"
finalLocation <- "/home/alan/DATA/dot-hia/melbourne-outputs"

scenarios_Melb <- read.csv("scenarios_for_melbourne.csv",as.is=T,fileEncoding="UTF-8-BOM") %>%
  mutate(scenario_location=paste0(scenarioLocation,"/",scenario,".csv")) %>%
  mutate(output_location=paste0(outputLocation,"/",scenario))
  



# iterate through each entry in scenarios_Melb, running the HAI model
print(paste0("iterating through ",nrow(scenarios_Melb)," scenarios at ",Sys.time()))
for (i in 1:nrow(scenarios_Melb)){

  number_cores <- max(1,floor(as.integer(detectCores())*0.8))
  cl <- makeCluster(number_cores)
  cat(paste0("About to start processing results in parallel, using ",number_cores," cores\n"))
  persons_matched=read.csv(scenarios_Melb[i,]$scenario_location,as.is=T, fileEncoding="UTF-8-BOM")
  seeds<-1:20
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


# in case the directory hasn't been made yet
dir.create(combinedLocation, recursive=TRUE, showWarnings=FALSE)

print(paste0("combining ",nrow(scenarios_Melb)," scenario outputs into single file at ",Sys.time()))
for (i in 1:nrow(scenarios_Melb)){
  output_df_files<-list.files(paste0(scenarios_Melb[i,]$output_location,'/output_df'),
                              pattern="*.csv",full.names=T)
  output_df<-lapply(output_df_files,read.csv,header=T) %>%
    bind_rows(.id="run") %>%
    mutate(run=as.integer(run))
  
  saveRDS(output_df, file=paste0(combinedLocation,"/",scenarios_Melb[i,]$scenario,".rds"))
  cat(paste0("\n combined scenario ",i,"/",nrow(scenarios_Melb)," complete at ",Sys.time(),"\n"))
  
}

######################################## 3) Summarise outputs  ###########################################################################

print(paste0("summarising ",nrow(scenarios_Melb)," scenario outputs at ",Sys.time()))
for (i in 1:nrow(scenarios_Melb)){
  output_df <- readRDS(paste0(combinedLocation,"/",scenarios_Melb[i,]$scenario,".rds"))
  summariseOutputs(scenario_location=
                     paste0(summarisedLocation,"/",scenarios_Melb[i,]$scenario),
                   output_df)
  cat(paste0("\n combined scenario ",i,"/",nrow(scenarios_Melb)," complete at ",Sys.time(),"\n"))
}


# combine scenarios into single files

combineScenarios <- function(summarisedLocation,name) {
  scenario_names<-data.frame(scenario=list.files(summarisedLocation)) %>%
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

output_df_agg_all <- combineScenarios(summarisedLocation,name="output_df_agg_all.csv")
output_df_agg_sex <- combineScenarios(summarisedLocation,name="output_df_agg_sex.csv")
output_diseases_change <- combineScenarios(summarisedLocation,name="output_diseases_change.csv")
output_life_expectancy_change <- combineScenarios(summarisedLocation,name="output_life_expectancy_change.csv")
output_life_years_change <- combineScenarios(summarisedLocation,name="output_life_years_change.csv")

# in case the directory hasn't been made yet
dir.create(finalLocation, recursive=TRUE, showWarnings=FALSE)
write.csv(output_df_agg_all,paste0(finalLocation,"/output_df_agg_all.csv"), row.names=F, quote=T)
write.csv(output_df_agg_sex,paste0(finalLocation,"/output_df_agg_sex.csv"), row.names=F, quote=T)
write.csv(output_diseases_change,paste0(finalLocation,"/output_diseases_change.csv"), row.names=F, quote=T)
write.csv(output_life_expectancy_change,paste0(finalLocation,"/output_life_expectancy_change.csv"), row.names=F, quote=T)
write.csv(output_life_years_change,paste0(finalLocation,"/output_life_years_change.csv"), row.names=F, quote=T)


#################################### 4) AUO output sample ###################################################################################
## Example for scenario: all trips under 2kms replaced walk and 10 replaced bike. 
## Alan, as discussed, outputs for all scenrios per output type (e.g. )

importSummarisedOutputs(scenario_location="scenarios/scenario_1")

### Plots for presentation

### Mortality diseases
tmpPlot <- output_df_agg_sex %>%
  filter(measure=="mx.num"  & scenario=="diff") %>%
  arrange(Gender,measure,disease,year)
#& Gender=="female"
ggplot(tmpPlot, aes(x=year,y=median)) +
  geom_ribbon(aes(ymin=percentile025,ymax=percentile975),fill="grey75") +
  geom_line() +
  facet_grid(disease~Gender,scales="free") +
  scale_y_continuous(
    name = waiver(),
    breaks = waiver(),
    minor_breaks = NULL,
    n.breaks = 3,
    labels = waiver()) +
  labs(x="Simulation year", y="Mortality") +
theme_bw()
ggsave("scenarios/scenario_1/mortalityError.png",width=6,height=4)

### Incidence diseases
tmpPlot <- output_df_agg_sex %>%
  filter(measure=="inc.num"  & scenario=="diff") %>%
  arrange(Gender,measure,disease,year)
#& Gender=="female"
ggplot(tmpPlot, aes(x=year,y=median)) +
  geom_ribbon(aes(ymin=percentile025,ymax=percentile975),fill="grey75") +
  geom_line() +
  facet_grid(disease~Gender,scales="free") +
  scale_y_continuous(
    name = waiver(),
    breaks = waiver(),
    minor_breaks = NULL,
    n.breaks = 3,
    labels = waiver()) +
  labs(x="Simulation year", y="Incidence") +
  theme_bw()
ggsave("scenarios/scenario_1/incidenceError.png",width=6,height=4)

### Heatlh adjusted life years
tmpPlot <- output_df_agg_sex %>%
  filter(measure=="Lwx"  & scenario=="diff") %>%
  arrange(Gender,measure,disease,year)
#& Gender=="female"
ggplot(tmpPlot, aes(x=year,y=mean)) +
  geom_ribbon(aes(ymin=percentile025,ymax=percentile975),fill="grey75") +
  geom_line() +
  facet_grid(cols=vars(Gender),scales="free") +
  scale_y_continuous(
    name = waiver(),
    breaks = waiver(),
    minor_breaks = NULL,
    # n.breaks = 3,
    labels = waiver()) +
  labs(x="Simulation year", y="Health-adjusted life years") +
  theme_bw()
ggsave("scenarios/scenario_1/HALY.png",width=6,height=4)

# # Example running just the first run of the model
# CalculationModel(seed=1,
#                  output_location="modelOutput",
#                  persons_matched)



############################################## Mode outputs ###############################################################################

### Tables to present (add to markdown) (add error bars for stochastic outcomes)


life_expectancy <- outputs[['output_life_expectancy_change']]
life_years <- outputs[['output_life_years_change']]
disease <- outputs[['output_diseases_change']]

### Graphs

#### Graph change in mmets
mmets_graphs <- outputs[['mmets']] %>%
  pivot_longer(cols = c("base_mmet", "scen1_mmet"),
               names_to = "scenario",
               values_to = "mmets")

scenario.labs <- c("Baseline", "Scenario")
names(scenario.labs) <- c("base_mmet", "scen1_mmet")

##### Graphs for mmets basline and scenario to compare with the dose response curves
mmets <- ggplot(mmets_graphs, aes(x = mmets)) +
  geom_histogram(bins = 50)  +
  labs(title="mMET-hours per week baseline and scenario", x="mMETs-hours", y="Frequency") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 12,face="bold"),
        axis.text=element_text(size=10),
        axis.title=element_text(size=10)) +
  facet_grid(. ~scenario,
             labeller = labeller(scenario = scenario.labs))+
  scale_colour_brewer(type = "seq", palette = "Spectral") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black"))+
  theme_classic()

mmets

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



