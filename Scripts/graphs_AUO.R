library(ggplot2)


############################## Graphs function (to do age and sex graphs) ################################################


########## Change in mode of transport
transportModeGraph <- function(age_val,sex_val,scen_val) {
  # age_val='all'
  # sex_val='all'
  # scen_val='all_2_10'
  
  dataFiltered <- output_transport_modes %>%
    filter(age==age_val,sex==sex_val,scen==scen_val) %>%
    dplyr::select(scenario,mode,mode_percent) %>%
    mutate(scenario=factor(scenario,
                           levels=c("bl","sc"),
                           labels=c("Baseline","Scenario"))) %>%
    mutate(mode=factor(mode,
                       levels=c("walking","bicycle","public.transport","car","other"),
                       labels=c("Walking","Cycling","Public transport","Driving","Other")))
  
  ggplot(dataFiltered, aes(x=mode, y=mode_percent)) +
    geom_col(position=position_dodge(), aes(fill=scenario)) + 
    scale_y_continuous(labels = scales::percent_format(accuracy = 5L)) +
    labs(title="Distribution trips baseline and scenario", x="", y="Proportion of all trips") + 
    theme(legend.title=element_blank(),
          legend.position="bottom",
          legend.margin=margin(0,0,0,0),
          legend.box.margin=margin(-5,0,5,0))
}

transportModeText <- function(age_val,sex_val,scen_val) {
  # age_val='all'
  # sex_val='all'
  # scen_val='all_2_10'
  
  dataFiltered <- output_transport_modes %>%
    filter(age==age_val,sex==sex_val,scen==scen_val) %>%
    dplyr::select(scenario,mode,mode_percent, sex, age, scen)
   
  print(paste0("This is a What-If Scenario in which you can assess the health impacts from improvements in 
               physical activity of shifting", " ",
        paste0(unique(dataFiltered$scen))))
        
  }

GraphsMode <- function(scenarios_trips) {
  
  
  dataFiltered <- output_transport_modes %>%
    filter(age==age_val,sex==sex_val,scen==scen_val) %>%
    dplyr::select(scenario,mode,mode_percent, participant_wt) 
    
  scenario_trips_weighted <-   dataFiltered  %>%
    srvyr::as_survey_design(weights = participant_wt)
  
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
}

mmetsGraph <- function(age_val,sex_val,scen_val) {
  # age_val='all'
  # sex_val='all'
  # scen_val='all_2_10'
  binwidth=250
  dataFiltered <- output_mmets_graph %>%
    filter(age==age_val,sex==sex_val,scen==scen_val)  
  scenario.labs <- c("Baseline", "Scenario")
  names(scenario.labs) <- c("base_mmet", "scen1_mmet")


  mmets_graph <- ggplot(dataFiltered , aes(x = value)) +
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
mmets_graph
}

diseasesTable <- function(age_val,sex_val,scen_val) {
  # age_val='all'
  # sex_val='all'
  # scen_val='all_2_10'
  dataFiltered <- output_diseases_change %>%
    filter(age==age_val,sex==sex_val,scen==scen_val) %>%
    mutate(measure=case_when(measure=='inc_num' ~ 'Incidence',
                             measure=='mx_num'  ~ 'Deaths')) %>%
    dplyr::select(population,measure,disease,mean,median,percentile025,percentile975)
}


#### USE moving average
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
  ggplot(tmpPlot, aes(x=year,y=median)) +
    # geom_ribbon(aes(ymin=percentile025,ymax=percentile975),fill="grey75") +
    geom_smooth(method = "loess") +
    geom_smooth(aes(y=percentile025, method = "loess")) +
    geom_smooth(aes(y=percentile975, method = "loess")) +
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
  ggplot(tmpPlot, aes(x=year,y=median)) +
    # geom_ribbon(aes(ymin=percentile025,ymax=percentile975),fill="grey75") +
    geom_smooth(method = "loess") +
    geom_smooth(aes(y=percentile025, method = "loess")) +
    geom_smooth(aes(y=percentile975, method = "loess")) +
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
  ggplot(tmpPlot, aes(x=year,y=median)) +
    # geom_ribbon(aes(ymin=percentile025,ymax=percentile975),fill="grey75") +
    geom_smooth(method = "loess") +
    geom_smooth(aes(y=percentile025, method = "loess")) +
    geom_smooth(aes(y=percentile975, method = "loess")) +
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