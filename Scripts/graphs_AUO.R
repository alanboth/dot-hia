library(ggplot2)
library(dplyr) # for manipulating data
library(tidyr) # for pivoting data
library(srvyr) # for statistics with weightts
library(scales) # for reordering factor for graphs
library(forcats)
library(zoo) # for calculating rolling mean
library(ggeasy)
library(ggridges)

############################## Graphs function (to do age and sex graphs) ################################################


########## Change in mode of transport
GraphsMode <- function(age_val,sex_val,scen_val) {
# # 
# age_val='all'
# sex_val='all'
# scen_val='all_1_5'
  
  dataFiltered <- output_transport_modes %>% 
    dplyr::filter(age==age_val,sex==sex_val,scen==scen_val) 
  ### Get bar chart modes distribution
  bar_chart_combo_sc <- dataFiltered %>%
    # ifelse(scenariosDF$max_walk==0, dataFiltered$)
    ggplot(aes(x = mode, y = prop)) +
    geom_bar(
      aes(color = scen, fill = scenario),
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

  
}

GetMinutesText <- function(age_val,sex_val,scen_val) {
  # age_val='all'
  # sex_val='all'
  # scen_val='all_2_10'
  
  dataFiltered1 <- PAall %>%
    filter(age==age_val,sex==sex_val,scen==scen_val) 
  
  dataFiltered2 <- PAallGuide %>%
    filter(age==age_val,sex==sex_val,scen==scen_val) 
  
  text <- cat("The shift from car travel for", dataFiltered1$scen, " ", "for", dataFiltered1$sex,
        " ", "adults aged", dataFiltered1$age, " ", "for the typical resident of Melbourne", 
  "increases walking from", dataFiltered1$walk_base, "to", dataFiltered1$walk_scen,
  "and cycling", dataFiltered1$cycle_base, "to", dataFiltered1$cycle_scen, "and increases those achieving 
  physical activity guidelines from", dataFiltered2$meets_base*100,"%", "to", dataFiltered2$meets_scen*100,"%")
  
  text
  
}
 
# diseasesExample$measure cases prevented
diseasesTable <- function(age_val,sex_val,scen_val) {
  # age_val='all'
  # sex_val='all'
  # scen_val='all_2_10'
  dataFiltered <- output_diseases_change %>%
    filter(age==age_val,sex==sex_val,scen==scen_val) %>%
    mutate(measure=case_when(measure=='inc_num' ~ 'Incidence',
                             measure=='mx_num'  ~ 'Deaths')) %>%
    filter(measure%in% c("Incidence", "Deaths")) %>%
    dplyr::select(population,measure,disease,scen,mean,median,percentile025,percentile975)
}

diseasesChangeIncidence <- function(age_val,sex_val,scen_val) {
  # age_val='all'
  # sex_val='all'
  # scen_val='all_2_10'
  

  tmpPlot <- output_diseases_change %>%
    filter(age==age_val,sex==sex_val,scen==scen_val) %>%
    filter(measure=="inc_percent" & scenario== "diff") %>%
    dplyr::select(disease,median,percentile025,percentile975)
  plot <- ggplot(tmpPlot, aes(x=disease, y=median*100)) + 
    geom_bar(stat="identity", color="black", 
             position=position_dodge()) +
    geom_errorbar(aes(ymin=percentile025*100, ymax=percentile975*100), width=.2,
                  position=position_dodge(.9))  +
    labs(x="Disease", y="Percentage change diseases") +
    theme_bw()
  plot
}

diseasesChangeDeaths <- function(age_val,sex_val,scen_val) {
  # age_val='all'
  # sex_val='all'
  # scen_val='all_2_10'
  
  
  tmpPlot <- output_diseases_change %>%
    filter(age==age_val,sex==sex_val,scen==scen_val) %>%
    filter(measure=="mx_percent" & scenario== "diff") %>%
    dplyr::select(disease,median,percentile025,percentile975)
  plot <- ggplot(tmpPlot, aes(x=disease, y=median*100)) + 
    geom_bar(stat="identity", color="black", 
             position=position_dodge()) +
    geom_errorbar(aes(ymin=percentile025*100, ymax=percentile975*100), width=.2,
                  position=position_dodge(.9))  +
    labs(x="Disease", y="Percentage change deaths") +
    theme_bw()
  plot
}

incidenceDiseasesGraph <- function(age_val,sex_val,scen_val) {
  # age_val='all'
  # sex_val='all'
  # scen_val='all_2_10'
  tmpPlot <- output_df_agg_all %>%
    filter(age==age_val,sex==sex_val,scen==scen_val) %>%
    filter(measure=="inc.num" & scenario== "diff") %>%
    dplyr::select(year,disease,mean,median,percentile025,percentile975) %>%
    arrange(disease,year)
  #& Gender=="female"
  ggplot(tmpPlot, aes(x=year)) +
    geom_line(aes(y=rollmean(median, 7, na.pad=TRUE))) +
    geom_line(aes(y=rollmean(percentile025, 7, na.pad=TRUE))) +
    geom_line(aes(y=rollmean(percentile975, 7, na.pad=TRUE))) +
    facet_grid(disease~.,scales="free") +
    scale_y_continuous(
      name = waiver(),
      breaks = waiver(),
      minor_breaks = NULL,
      n.breaks = 3,
      labels = waiver()) +
    labs(x="Simulation year", y="Incidence",
         title = paste("Changes in cases of disease prevented over time")) +
    theme_bw()
}

mortalityDiseasesGraph <- function(age_val,sex_val,scen_val) {
  # age_val='all'
  # sex_val='all'
  # scen_val='all_2_10'
  tmpPlot <- output_df_agg_all %>%
    filter(age==age_val,sex==sex_val,scen==scen_val) %>%
    filter(measure=="mx.num" & scenario== "diff") %>%
    dplyr::select(year,disease,mean,median,percentile025,percentile975) %>%
    arrange(disease,year)
  #& Gender=="female"
  ggplot(tmpPlot, aes(x=year)) +
    geom_line(aes(y=rollmean(median, 7, na.pad=TRUE))) +
    geom_line(aes(y=rollmean(percentile025, 7, na.pad=TRUE))) +
    geom_line(aes(y=rollmean(percentile975, 7, na.pad=TRUE))) +
    facet_grid(disease~.,scales="free") +
    scale_y_continuous(
      name = waiver(),
      breaks = waiver(),
      minor_breaks = NULL,
      n.breaks = 3,
      labels = waiver()) +
    labs(x="Simulation year", y="Mortality",
         title = paste("Changes in cases of disease prevented over time")) +
    theme_bw()
}

halyGraph <- function(age_val,sex_val,scen_val) {
  # age_val='all'
  # sex_val='all'
  # scen_val='all_2_10'
  tmpPlot <- output_df_agg_all %>%
    filter(age==age_val,sex==sex_val,scen==scen_val) %>%
    filter(measure=="Lwx" & scenario== "diff") %>%
    dplyr::select(year,mean,median,percentile025,percentile975) %>%
    arrange(year)
  #& Gender=="female"
  ggplot(tmpPlot, aes(x=year)) +
    geom_line(aes(y=rollmean(median, 7, na.pad=TRUE))) +
    geom_line(aes(y=rollmean(percentile025, 7, na.pad=TRUE))) +
    geom_line(aes(y=rollmean(percentile975, 7, na.pad=TRUE))) +
    scale_y_continuous(
      name = waiver(),
      breaks = waiver(),
      minor_breaks = NULL,
      n.breaks = 3,
      labels = waiver()) +
    labs(x="Simulation year", y="Health-adjusted life years", 
         title = paste("Difference life years and health-adjusted life years")) +
    theme_bw()
}



######################################## Graphs for rmarkdown #################################################################
### These graphs are already generated in the file, code below to allow for manipulation for presentation

# ######################################## TRANSPORT MODES (FIGURE 1) ##########################################################
# vista_stages <- read.csv("Data/trips/trips_melbourne.csv")  %>%
#   dplyr::mutate(trip_mode=as.factor(case_when(trip_mode=="pedestrian" ~ 'walking', 
#                                               trip_mode=="bus" ~ 'public.transport', 
#                                               trip_mode=="tram" ~ 'public.transport', 
#                                               trip_mode=="train" ~ 'public.transport',
#                                               trip_mode=="motorcycle" ~ 'other',
#                                               TRUE ~ tolower(trip_mode))))
# #Weighted data ###### CHECK SHOULD BE TRIPWT, 
# vista_stages_weighted <-  vista_stages  %>%
#   srvyr::as_survey_design(weights = trips_wt)
# 
# Graphmode <- function(in_data) {
#   
#   mode_trips <- in_data  %>%
#     group_by(trip_mode,
#              .drop = TRUE) %>%
#     dplyr::summarize(prop= srvyr::survey_mean())
#   
#   mode_overall <- mode_trips %>%
#     mutate(name = fct_reorder(trip_mode, desc(prop))) %>%
#     ggplot( aes(x=name, y= prop)) +
#     geom_bar(stat="identity", fill='blue', color="darkblue", alpha=.6, width=.4) +
#     coord_flip() +
#     labs(title="Main mode of transport", x="", y="", size = 10,face="bold") +
#     theme_classic() + 
#     theme(plot.title = element_text(hjust = 0.5, size = 10)) +
#     geom_text(aes(label=paste0(round(prop*100,1),"%"), y=prop+0.05), size=4) + 
#     ylim(0, 0.8) + 
#     theme(axis.text.y = element_text(size = 12),
#           axis.text.x=element_blank())
#   
#   
#   return(mode_overall) 
# }
# 
# ##### Stages data
# 
# StagesMode <- Graphmode(vista_stages_weighted)
# 
# ggsave("SuppDocs/StagesGraphs/TripsModeBaseline.png")
# 
# ######################################## TRANSPORT MODES (FIGURE 2) ##########################################################
# vista_stages <- read.csv("Data/trips/trips_melbourne.csv")  %>%
#   dplyr::mutate(trip_mode=as.factor(case_when(trip_mode=="pedestrian" ~ 'walking', 
#                                               trip_mode=="bus" ~ 'public.transport', 
#                                               trip_mode=="tram" ~ 'public.transport', 
#                                               trip_mode=="train" ~ 'public.transport',
#                                               trip_mode=="motorcycle" ~ 'other',
#                                               TRUE ~ tolower(trip_mode))))
# #Weighted data ###### CHECK SHOULD BE TRIPWT, 
# vista_stages_weighted <-  vista_stages  %>%
#   srvyr::as_survey_design(weights = trips_wt)
# 
# dist_mode_trips <- in_data  %>%
#   group_by(dist_cat, trip_mode, 
#            .drop = TRUE) %>%
#   dplyr::summarize(prop= srvyr::survey_mean())
# 
# GraphModeDist <- function(in_data){
#   
#   dist_mode_trips <- in_data  %>%
#     group_by(dist_cat, trip_mode, 
#              .drop = TRUE) %>%
#     dplyr::summarize(prop= srvyr::survey_mean())
#   
#   dist_mode_trips_graph <- dist_mode_trips %>%
#     # mutate(dist_cat = fct_relevel(dist_cat,
#     #                               "<1km", "1-2km", "3-5km", "6-10km", ">10km")) %>%
#     # mutate(trip_mode =fct_relevel(trip_mode, "car", "public.transport", "bicycle", "walking", "other"))  %>%
#     ggplot(aes(x = trip_mode, y = prop, fill=factor(trip_mode)), stat = "identity") +
#     geom_col()  + 
#     theme_classic() + 
#     labs(title="Mode of transport by distance category", x="", y="") +
#     geom_text(aes(label=paste0(round(prop*100),"%"), y=prop + 0.10), size=5, position = position_dodge2(width = 1)) + 
#     theme(plot.title = element_text(hjust = 0.5, size = 12,face="bold"),
#           axis.text=element_text(size=16),
#           axis.title=element_text(size=16)) +
#     theme(legend.position = "bottom",
#           legend.title = element_blank(),
#           legend.text = element_text(colour = "black", size = 16),
#           legend.key = element_blank()) +
#     scale_y_continuous(labels = percent) +
#     facet_wrap(~ dist_cat) +
#     scale_fill_brewer(palette = "Set1")  +
#     theme(strip.text.x = element_text(size = 16),
#           strip.background = element_rect(
#             color="white", size=1.5, linetype="solid"
#           )) +
#     theme(axis.text.x = element_blank())
#   
#   
#   return(dist_mode_trips_graph) 
# }
# 
# 
# ##### Stage data
# 
# StageModeDist <- GraphModeDist(vista_stages_weighted)
# ggsave("SuppDocs/StagesGraphs/TripsModeDistCat.png")
# 
# ######################################### RELATIVE RISKS GRAPHS (FIGURE 4) #####################################################
# 
# 
# ## Install package if not installed, note, you need devtools
# # library(devtools)
# # install_github("ITHIM/ITHIM-R")
# # library(ithimr)
# dose_response_folder=paste0(file.path(find.package('ithimr',lib.loc=.libPaths()), 'extdata/global'), "/dose_response/drpa/extdata")
# list_of_files <- list.files(path=dose_response_folder, recursive=TRUE,
#                             pattern="\\.csv$", full.names=TRUE)
# for (i in 1:length(list_of_files)){
#   assign(stringr::str_sub(basename(list_of_files[[i]]), end = -5),
#          readr::read_csv(list_of_files[[i]],col_types = cols()),pos = 1)}
# 
# 
# graph_rr <- function(idata, name){
#   idata %>%
#     ggplot(aes(x=dose, y=RR,)) +
#     geom_line() +
#     geom_ribbon(data=idata,aes(ymin=lb,ymax=ub),alpha=0.3) +
#     labs(title=name, x="Marginal MET hours per week", y="Relative Risk") + 
#     theme(axis.text=element_text(size=10),
#           axis.title=element_text(size=10)) +
#     theme_classic() + 
#     theme(plot.title = element_text(hjust = 0.5, size = 16))
#   return(graph_rr)
#   
# }
# 
# breast_rr <- graph_rr(breast_cancer_all, "Breast cancer mortality and morbidity")
# colon_rr <- graph_rr(colon_cancer_all, "Colon cancer mortality and morbidity")
# lung_rr <- graph_rr(lung_cancer_all, "Lung cancer mortality and morbidity")
# endo_rr <- graph_rr(endometrial_cancer_all, "Uterine cancer mortality and morbidity")
# coronary_rr <- graph_rr(coronary_heart_disease_incidence, "Coronary heart disease incidence")
# stroke_rr <- graph_rr(stroke_incidence, "Stroke incidence")
# diabetes_rr <- graph_rr(diabetes_mortality, "Type 2 diabetes mortality")
