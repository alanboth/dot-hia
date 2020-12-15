library(ggplot2)


############################## Graphs function (to do age and sex graphs) ################################################


########## Change in mode of transport
GraphsMode <- function(scenarios_trips) {
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
}

########## Change in marginal METS

mmets_graphs <- outputs[['mmets']] %>%
  pivot_longer(cols = c("base_mmet", "scen1_mmet"),
               names_to = "scenario",
               values_to = "mmets")

scenario.labs <- c("Baseline", "Scenario")
names(scenario.labs) <- c("base_mmet", "scen1_mmet")

##### Graphs for mmets basline and scenario to compare with the dose response curves


GraphsmMETs <- function(data) {
mmets_graphs <- data %>%
  pivot_longer(cols = c("base_mmet", "scen1_mmet"),
               names_to = "scenario",
               values_to = "mmets")

scenario.labs <- c("Baseline", "Scenario")
names(scenario.labs) <- c("base_mmet", "scen1_mmet")


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

}
