library(ggplot2)
library(dplyr) # for manipulating data
library(tidyr) # for pivoting data
# library(srvyr) # for statistics with weights
library(scales) # for reordering factor for graphs
# library(forcats)
library(zoo) # for calculating rolling mean
# library(ggeasy)
# library(ggridges)

############################## Graphs function (to do age and sex graphs) ################################################
diseaseLevels <- c("brsc","carc","dmt2","ishd","strk","tbalc","utrc")
diseaseLabels <- c("Breast cancer","Colon cancer","Diabetes type 2",
                   "Ischemic heart disease","Stroke","Lung cancer",
                   "Uterine cancer")
auo_theme <- theme_bw() +
  theme(axis.text = element_text(size=14),
        axis.title = element_text(size=16),
        legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(size=16),
        legend.key = element_blank(),
        strip.text = element_text(size = 14),
        strip.text.y = element_text(angle=0, hjust=0),
        strip.background = element_rect(fill="white",size=0))

########## Change in mode of transport
GraphsMode <- function(age_val,sex_val,scen_val) {
  # age_val='all'; sex_val='all'; scen_val='all_2_10'
  
  dataFiltered <- output_transport_modes %>% 
    dplyr::filter(age==age_val,sex==sex_val,scen==scen_val) %>%
    mutate(scenario=factor(scenario, levels=c("bl","sc"), labels=c("Baseline", "Scenario"))) %>%
    mutate(mode=factor(mode,
                       levels=c("walking","bicycle","public.transport","car","other"),
                       labels=c("Walking","Cycling","Public transport","Driving","Other")))
  
  ggplot(dataFiltered, aes(x=mode, y=prop, fill=scenario)) +
    geom_bar(stat="identity", position="dodge") + 
    # AUO teal and pink
    scale_fill_manual(values=c("#24C9AC","#EC4497")) +
    labs(y="Proportion of all trips") +
    geom_text(aes(label=paste0(round(prop*100,1),"%")),
              position=position_dodge(width=0.9), vjust=-0.25) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 5L)) +
    theme_classic() +
    theme(plot.title = element_blank(),
          axis.text=element_text(size=14),
          axis.title=element_text(size=16),
          axis.title.x=element_blank(),
          axis.text.x = element_text(angle=90, vjust=0.5, hjust=1),
          legend.position = "right",
          legend.title = element_blank(),
          legend.text = element_text(size=16),
          legend.key = element_blank())
}

GetMinutesText <- function(age_val,sex_val,scen_val) {
  # age_val='all'; sex_val='all'; scen_val='all_2_10'
  
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
  # age_val='all'; sex_val='all'; scen_val='all_2_10'
  
  dataFiltered <- output_diseases_change %>%
    filter(age==age_val,sex==sex_val,scen==scen_val) %>%
    mutate(measure=case_when(measure=='inc_num' ~ 'Incidence',
                             measure=='mx_num'  ~ 'Deaths')) %>%
    filter(measure%in% c("Incidence", "Deaths")) %>%
    mutate(across(mean:percentile975, round, digits=1)) %>%
    mutate(disease=factor(disease, levels=diseaseLevels, labels=diseaseLabels)) %>%
    dplyr::select(population,measure,disease,mean,median,percentile025,percentile975)
}

HALYsTable <- function(age_val,sex_val,scen_val) {
  # age_val='all'
  # sex_val='all'
  # scen_val='all_2_10'
  dataFiltered <- output_life_years_change %>%
    filter(age==age_val,sex==sex_val,scen==scen_val) %>%
    mutate(across(mean:percentile975, round, digits=1)) %>%
    dplyr::select(population,measure,mean,median,percentile025,percentile975)
}

diseasesChangeIncidenceTable <- function(age_val,sex_val,scen_val) {
  # age_val='all'; sex_val='all'; scen_val='all_2_10'
  tmpPlot <- output_diseases_change %>%
    filter(age==age_val,sex==sex_val,scen==scen_val) %>%
    filter(measure=="inc_percent" & scenario== "diff") %>%
    # somewhere in the scripts tablc has been mislabeled tbalct
    mutate(disease=ifelse(disease=="tbalct","tbalc",disease)) %>%
    mutate(disease=factor(disease, levels=diseaseLevels, labels=diseaseLabels)) %>%
    mutate(across(median:percentile975, round, digits=3)) %>%
    dplyr::select(disease,median,percentile025,percentile975)
}

diseasesChangeIncidence <- function(age_val,sex_val,scen_val) {
  # age_val='all'; sex_val='all'; scen_val='all_2_10'
  
  tmpPlot <- output_diseases_change %>%
    filter(age==age_val,sex==sex_val,scen==scen_val) %>%
    filter(measure=="inc_percent" & scenario== "diff") %>%
    # somewhere in the scripts tablc has been mislabeled tbalct
    mutate(disease=ifelse(disease=="tbalct","tbalc",disease)) %>%
    mutate(disease=factor(disease, levels=diseaseLevels, labels=diseaseLabels)) %>%
    dplyr::select(disease,median,percentile025,percentile975)
  
  plot <- ggplot(tmpPlot, aes(x=disease, y=median)) + 
    geom_bar(stat="identity", color=NA, fill="#24C9AC", # AUO teal
             position=position_dodge()) +
    geom_errorbar(aes(ymin=percentile025, ymax=percentile975), width=.2,
                  position=position_dodge(.9))  +
    scale_y_continuous(labels = scales::percent_format(accuracy = 5L)) +
    labs(x="Disease", y="Percentage change diseases") +
    auo_theme +
    theme(axis.title.x=element_blank(),
          axis.text.x = element_text(angle=90, vjust=0.5, hjust=1))
  plot
}

diseasesChangeDeaths <- function(age_val,sex_val,scen_val) {
  # age_val='all'; sex_val='all'; scen_val='all_2_10'
  
  tmpPlot <- output_diseases_change %>%
    filter(age==age_val,sex==sex_val,scen==scen_val) %>%
    filter(measure=="mx_percent" & scenario== "diff") %>%
    # somewhere in the scripts tablc has been mislabeled tbalct
    mutate(disease=ifelse(disease=="tbalct","tbalc",disease)) %>%
    mutate(disease=factor(disease, levels=diseaseLevels, labels=diseaseLabels)) %>%
    dplyr::select(disease,median,percentile025,percentile975)
  plot <- ggplot(tmpPlot, aes(x=disease, y=median)) + 
    geom_bar(stat="identity", color=NA, fill="#24C9AC", # AUO teal
             position=position_dodge()) +
    geom_errorbar(aes(ymin=percentile025, ymax=percentile975), width=.2,
                  position=position_dodge(.9))  +
    scale_y_continuous(labels = scales::percent_format(accuracy = 5L)) +
    labs(x="Disease", y="Percentage change deaths") +
    auo_theme +
    theme(axis.title.x=element_blank(),
          axis.text.x = element_text(angle=90, vjust=0.5, hjust=1))
  plot
}

incidenceDiseasesGraph <- function(age_val,sex_val,scen_val) {
  # age_val='all'; sex_val='all'; scen_val='all_2_10'
  tmpPlot <- output_df_agg_all %>%
    filter(age==age_val,sex==sex_val,scen==scen_val) %>%
    filter(measure=="inc.num" & scenario== "diff") %>%
    dplyr::select(year,disease,median,percentile025,percentile975) %>%
    arrange(disease,year) %>%
    group_by(disease) %>%
    # the rollmean function introduces NA values at the edges, so filling them
    # with the original values
    mutate(roll=rollmean(median,7,fill=NA),
           median=ifelse(is.na(roll),median,roll),
           roll=rollmean(percentile025,7,fill=NA),
           percentile025=ifelse(is.na(roll),percentile025,roll),
           roll=rollmean(percentile975,7,fill=NA),
           percentile975=ifelse(is.na(roll),percentile975,roll)) %>%
    ungroup() %>%
    mutate(disease=factor(disease, levels=diseaseLevels, labels=diseaseLabels))
  
  ggplot(tmpPlot, aes(x=year)) +
    geom_ribbon(aes(ymin=percentile025,ymax=percentile975),fill="#24C9AC",alpha=0.5) + # AUO teal
    geom_line(aes(y=median)) +
    facet_grid(rows=vars(disease), scales = "free_y") +
    scale_y_continuous(
      name = waiver(),
      breaks = waiver(),
      minor_breaks = NULL,
      n.breaks = 3,
      labels = waiver()) +
    scale_x_continuous(limits=c(0,85),breaks=seq(0,80,10), expand=c(0,0)) +
    labs(x="Years since scenario commenced", y="Incidence") +
    auo_theme
}

mortalityDiseasesGraph <- function(age_val,sex_val,scen_val) {
  # age_val='all'; sex_val='all'; scen_val='all_2_10'
  tmpPlot <- output_df_agg_all %>%
    filter(age==age_val,sex==sex_val,scen==scen_val) %>%
    filter(measure=="mx.num" & scenario== "diff") %>%
    dplyr::select(year,disease,median,percentile025,percentile975) %>%
    arrange(disease,year) %>%
    group_by(disease) %>%
    # the rollmean function introduces NA values at the edges, so filling them
    # with the original values
    mutate(roll=rollmean(median,7,fill=NA),
           median=ifelse(is.na(roll),median,roll),
           roll=rollmean(percentile025,7,fill=NA),
           percentile025=ifelse(is.na(roll),percentile025,roll),
           roll=rollmean(percentile975,7,fill=NA),
           percentile975=ifelse(is.na(roll),percentile975,roll)) %>%
    ungroup() %>%
    mutate(disease=factor(disease, levels=diseaseLevels, labels=diseaseLabels))
  
  ggplot(tmpPlot, aes(x=year)) +
    geom_ribbon(aes(ymin=percentile025,ymax=percentile975),fill="#24C9AC",alpha=0.5) + # AUO teal
    geom_line(aes(y=median)) +
    facet_grid(rows=vars(disease), scales = "free_y") +
    # facet_wrap(facets=vars(disease),ncol=1,scales="free_y") +
    scale_y_continuous(
      name = waiver(),
      breaks = waiver(),
      minor_breaks = NULL,
      n.breaks = 3,
      labels = waiver()) +
    scale_x_continuous(limits=c(0,85),breaks=seq(0,80,10), expand=c(0,0)) +
    labs(x="Years since scenario commenced", y="Mortality") +
    auo_theme
}

halyGraph <- function(age_val,sex_val,scen_val) {
  # age_val='all'; sex_val='all'; scen_val='all_2_10'
  tmpPlot <- output_df_agg_all %>%
    filter(age==age_val,sex==sex_val,scen==scen_val) %>%
    filter(measure=="Lwx" & scenario== "diff") %>%
    dplyr::select(year,median,percentile025,percentile975) %>%
    arrange(year) %>%
    # the rollmean function introduces NA values at the edges, so filling them
    # with the original values
    mutate(roll=rollmean(median,7,fill=NA),
           median=ifelse(is.na(roll),median,roll),
           roll=rollmean(percentile025,7,fill=NA),
           percentile025=ifelse(is.na(roll),percentile025,roll),
           roll=rollmean(percentile975,7,fill=NA),
           percentile975=ifelse(is.na(roll),percentile975,roll)) %>%
    # year 84 (the last year, has a weird uptick, removing for now)
    filter(year<=83)

  
  ggplot(tmpPlot, aes(x=year)) +
    geom_ribbon(aes(ymin=percentile025,ymax=percentile975),fill="#24C9AC",alpha=0.5) + # AUO teal
    geom_line(aes(y=median)) +
    scale_y_continuous(
      name = waiver(),
      breaks = waiver(),
      minor_breaks = NULL,
      n.breaks = 3,
      labels = waiver()) +
    scale_x_continuous(limits=c(0,85),breaks=seq(0,80,10), expand=c(0,0)) +
    labs(x="Years since scenario commenced", y="Health-adjusted life years") +
    auo_theme
}

lyGraph <- function(age_val,sex_val,scen_val) {
  # age_val='all'; sex_val='all'; scen_val='all_2_10'
  tmpPlot <- output_df_agg_all %>%
    filter(age==age_val,sex==sex_val,scen==scen_val) %>%
    filter(measure=="Lx" & scenario== "diff") %>%
    dplyr::select(year,median,percentile025,percentile975) %>%
    arrange(year) %>%
    # the rollmean function introduces NA values at the edges, so filling them
    # with the original values
    mutate(roll=rollmean(median,7,fill=NA),
           median=ifelse(is.na(roll),median,roll),
           roll=rollmean(percentile025,7,fill=NA),
           percentile025=ifelse(is.na(roll),percentile025,roll),
           roll=rollmean(percentile975,7,fill=NA),
           percentile975=ifelse(is.na(roll),percentile975,roll)) %>%
    # year 84 (the last year, has a weird uptick, removing for now)
    filter(year<=83)
  
  ggplot(tmpPlot, aes(x=year)) +
    geom_ribbon(aes(ymin=percentile025,ymax=percentile975),fill="#24C9AC",alpha=0.5) + # AUO teal
    geom_line(aes(y=median)) +
    scale_y_continuous(
      name = waiver(),
      breaks = waiver(),
      minor_breaks = NULL,
      n.breaks = 3,
      labels = waiver()) +
    scale_x_continuous(limits=c(0,85),breaks=seq(0,80,10), expand=c(0,0)) +
    labs(x="Years since scenario commenced", y="Health-adjusted life years") +
    auo_theme
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
