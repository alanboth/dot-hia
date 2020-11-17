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

### Model order: 1) Run intervention; 2) Run paramters (inputs CalculateModel), 3) Run HIA, and 4) summarise outputs

############################################## Run intervention Melbourne ###############################################################################
### Steps: 1) Generate trip set with baseline and scenario trips, 2) Generate persons_matched 


#### 1) Generate trip set with baseline and scenario trips ####

### The following code returns persons_mathced, which is an input of CalculateModel
### Graph: depicts change in trips by mode.

### Calculate scenarios of replacing car trips by walking and/or cycling. 
### Outputs: trip set with baseline and scenario trips and associated distance and time in hours per week.
### Inputs: baseline trips melbourne and speed file by age and sex derived from VISTA 2017-18 TRIP file.

source("Scripts/scenarios_MEL.R")
in_data="Data/processed/trips_melbourne.csv"
in_speed="Data/processed/speed_trips_melbourne.csv"
scenario <- calculateScenarioMel(trips_melbourne = in_data, 
                                       speed = in_speed,
                                       age_input = c("15 to 19","20 to 39", "40 to 64", "65 plus"), # Choose age groups
                                       sex_input = c("male", "female"), # "female"), #Choose sex group
                                       original_mode = "car" , # Just car trips can be replaced
                                       # replace_mode_walk = T,
                                       # replace_mode_cycle = T,
                                       distance_replace_walk = c("<1km", "1-2km"), #("0 km, >1km, 1-2km, 3-5km, 6-10km, >10km") #Choose one category only
                                       distance_replace_cycle = "3-5km", #("0 km, >1km, 1-2km, 3-5km, 6-10km, >10km"), #Choose one category only
                                       purpose_input = c("Leisure", "Shopping", "Work", "Other", "Education")) #, "Shopping", "Work", "Other", "Education")) # Choose purpose groups 
  
scenario_trips <- scenario[["trips"]] 
scenario_trips <-  scenario_trips %>% mutate_if(sapply(scenario_trips, is.character), as.factor) ## all character to factors for group by analysis
# write.csv(scenario_trips, "Data/processed/trips_melbourne_scenarios.csv")

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
ggsave("output/proportion_modes_sc.png")


#### 2) Generate person_matched ####

### Calculate time spents doing physical activity at baseline and scenario for individuals
### Outputs: persons_matched with baseline and sceanrios time in hours spents walking and cycling for transport and moderate and vigorous PA.
### Inputs: scenario_trips (above step), VISTA persons from VISTA 2017-18 PERSON file and moderate and vigorous excersice from NHS persons file 2017-18.
source("Scripts/data_prep/synthetic_pop.R")

### 2.1) Create data set with VISTA people and allocate baseline and scenario trips to them
persons_travel <- calculatePersonsTravelScenario(
  travel_data_location="Data/processed/travel_data.csv", ## BZ: generated in script runInputsMelbourneExposure.R 
  scenario_location=scenario_trips#"Data/processed/trips_melbourne_scenarios.csv" ### BZ: Generated in step 1
)
# write.csv(persons_travel, "Data/processed/persons_travel.csv", row.names=F, quote=T)


#### 2.2) Match NHS people to VISTA people based on age, sex, ses, work status and whether they walk for transport
persons_matched <- calculatePersonsMatch(
  pa_location="Data/processed/persons_pa.csv", ## BZ: generated in script runInputsMelbourneExposure.R 
  persons_travel_location=persons_travel   #"Data/processed/persons_travel.csv"
)
write.csv(persons_matched, "scenarios/scenario_1/matched_pop_1.csv", row.names=F, quote=T)
persons_matched <- read.csv("scenarios/scenario_1/matched_pop_1.csv", as.is=T, fileEncoding="UTF-8-BOM")

############################################## Run intervention Melbourne ###############################################################################
############################################## Run HIA ###############################################################################

### 1) Get HIA parameters
source("Scripts/ithim-r_wrappers.R")
# 
# ### To get distributions uncertain inputs change NSAMPLES and PA_DOSE_RESPONSE_QUANTILE
# parameters <- GetParamters(
#   NSAMPLES = 1, ### Alan, when this is more than one, then, those inputs with distributions are samples NSAMPLES times
#   matched_population = persons_matched,
#   MMET_CYCLING = c(4.63, 1.2), 
#   MMET_WALKING = c(2.53, 1.1),
#   PA_DOSE_RESPONSE_QUANTILE = T) ### True to run uncertainty  (creates quantiles files for RR physical activity)



### 2) Run model

### Get functions
persons_matched <- read.csv("Data/processed/matched_pop.csv", as.is=T, fileEncoding="UTF-8-BOM")
source("Scripts/data_prep/mmet_pp.R")
source("Scripts/ithim-r_wrappers.R")
source("Scripts/data_prep/population_prep.R")

### Get age and sex  ## BZ: added based on scenarios_MEL age adn sex



number_cores <- max(1,floor(as.integer(detectCores())*0.8))
cl <- makeCluster(number_cores)
cat(paste0("About to start processing results in parallel, using ",number_cores," cores\n"))

seeds<-101:1000
registerDoParallel(cl)
start_time = Sys.time()
# persons_matched <- read.csv("Data/processed/matched_pop.csv", as.is=T, fileEncoding="UTF-8-BOM")
# source("Scripts/ithim-r_wrappers.R")
results <- foreach(seed_current=seeds,
                   # output_location_current="modelOutput",
                   # persons_matched_current=persons_matched,
                   .combine=rbind,
                   .verbose=F,
                   .packages=c("dplyr","tidyr","stringr","readr","readxl","data.table","srvyr"),
                   .export=c("calculateMMETSperPerson","CalculationModel","gen_pa_rr_wrapper",
                             "GetParamters","GetPopulation","GetStDevRR","health_burden_2",
                             "RunDisease","RunLifeTable")
) %dopar%
  CalculationModel(seed=seed_current,
                   output_location="/modelOutput",
                   persons_matched)
end_time = Sys.time()
end_time - start_time
stopCluster(cl)


cat(paste0("Combining plans into single file:\n"))

output_df_files<-list.files('/modelOutput/output_df',pattern="*.csv",full.names=T)
output_df<-lapply(output_df_files,read.csv,header=T) %>%
  bind_rows(.id="run") %>%
  mutate(run=as.integer(run))
saveRDS(output_df, file = "scenarios/scenario_1/output_df.rds")

summariseOutputs(scenario_location="scenarios/scenario_1",
                 output_df)

# Belen, just run this to load the summarised outputs
importSummarisedOutputs(scenario_location="scenarios/scenario_1")

### Plots for presentation

### Mortality diseases
tmpPlot <- output_df_agg_sex %>%
  filter(measure=="mx.num"  & scenario=="diff") %>%
  arrange(Gender,measure,disease,year)
#& Gender=="female"
ggplot(tmpPlot, aes(x=year,y=mean)) +
  geom_ribbon(aes(ymin=mean-1.96*error,ymax=mean+1.96*error),fill="grey75") +
  geom_ribbon(aes(ymin=percentile05,ymax=percentile95),fill="grey75") +
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
ggplot(tmpPlot, aes(x=year,y=mean)) +
  geom_ribbon(aes(ymin=mean-1.96*error,ymax=mean+1.96*error),fill="grey75") +
  # geom_ribbon(aes(ymin=percentile05,ymax=percentile95),fill="grey75") +
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
  geom_ribbon(aes(ymin=mean-1.96*error,ymax=mean+1.96*error),fill="grey75") +
  # geom_ribbon(aes(ymin=percentile05,ymax=percentile95),fill="grey75") +
  geom_line() +
  facet_grid(disease~Gender,scales="free") +
  scale_y_continuous(
    name = waiver(),
    breaks = waiver(),
    minor_breaks = NULL,
    n.breaks = 3,
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



