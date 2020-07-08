#### Trips descriptives
### Age groups for descriptives of travel patterns

trips_melbourne  <- trips_melbourne  %>% mutate(AGEG = case_when(AGE <= 4 ~ '<5',
                                                                 AGE >= 5  & AGE <= 9 ~ '5-9',
                                                                 AGE >= 10  & AGE <= 14 ~ '10-14',
                                                                 AGE >= 15  & AGE <= 17 ~ '15-17', 
                                                                 AGE >= 18  & AGE <= 19 ~ '18-19',
                                                                 AGE >= 20  & AGE <= 24 ~ '20-24',
                                                                 AGE >= 25  & AGE <= 29 ~ '25-29', 
                                                                 AGE >= 30  & AGE <= 34 ~ '30-34', 
                                                                 AGE >= 35  & AGE <= 39 ~ '35-39', 
                                                                 AGE >= 40  & AGE <= 44 ~ '40-44', 
                                                                 AGE >= 45  & AGE <= 49 ~ '45-49', 
                                                                 AGE >= 50  & AGE <= 54 ~ '50-54', 
                                                                 AGE >= 55  & AGE <= 59 ~ '55-59', 
                                                                 AGE >= 60  & AGE <= 64 ~ '60-64', 
                                                                 AGE >= 65  & AGE <= 69 ~ '65-69', 
                                                                 AGE >= 70  & AGE <= 74 ~ '70-74', 
                                                                 AGE >= 75  & AGE <= 79 ~ '75-79',
                                                                 AGE >= 80  & AGE <= 84 ~ '80-84', 
                                                                 AGE >= 85 ~ '85>'))

##### THIS IS FOR SOME DESCRIPTIVES, MAY BE BEST TO MOVE ELSEWHERE (INCOMPLETE, WE NEED TO DECIDE WHAT DESCRIPTIVES TO PRESENT)
### Weighted survey (not sure I need this with ITHIM-R funtions, but good for running weighted descriptives)
#### Total sample and population
trips_melbourne_weighted <-  trips_melbourne  %>%
  srvyr::as_survey_design(weights = participant_wt)


#### Total trips 
ave_trips_Mel <- trips_melbourne_weighted  %>% 
  group_by(PERSID) %>%
  top_n(1, TRIPNO) %>%
  dplyr::summarize(prop= srvyr::survey_mean(),
                   Average_trips = srvyr::survey_mean(TRIPNO),
                   Median_trips = srvyr::survey_median(TRIPNO),
                   SD_Average_trips = srvyr::survey_sd(TRIPNO))



#### Total sample and population by age and sex
tot_trips_melbourne_weighted  <- trips_melbourne_weighted  %>% 
  group_by(SEX, AGEG,
           .drop = FALSE) %>%
  dplyr::summarize(prop= srvyr::survey_mean(),
                   total = srvyr::survey_total(),
                   total_unweighted = srvyr::unweighted(n()))

#### Average trips by age and sex

ave_trips_Mel <- trips_melbourne_weighted  %>% 
  group_by(PERSID) %>%
  top_n(1, TRIPNO) %>%
  group_by(SEX, AGEG,
           .drop = FALSE) %>%
  dplyr::summarize(prop= srvyr::survey_mean(),
                   Average_trips = srvyr::survey_mean(TRIPNO),
                   Median_trips = srvyr::survey_median(TRIPNO),
                   SD_Average_trips = srvyr::survey_sd(TRIPNO))


##### Average trips by mode and by mode and  age and sex

ave_stages_mode_Mel <- trips_melbourne_weighted  %>% 
  group_by(MODE, SEX,
           .drop = FALSE) %>%
  dplyr::summarize(prop= srvyr::survey_mean(),
                   total = srvyr::survey_total(),
                   total_unweighted = srvyr::unweighted(n()))

ave_trips_main_mode_Mel <- trips_melbourne_weighted  %>% 
  group_by(LINKMODE, SEX, 
           .drop = FALSE) %>%
  dplyr::summarize(prop= srvyr::survey_mean(),
                   total = srvyr::survey_total(),
                   total_unweighted = srvyr::unweighted(n()))


#### PA descriptives

## Population mean and standard deviation for prevalence of physical activity

### Means without weights
mean_modmins <- mean(pa_NHS$MODMINS, na.rm=TRUE)

mean_vigmins <- mean(pa_NHS$VIGMINS, na.rm=TRUE)

### Means with weights
pa_NHS %>%
  as_survey(weights = c(NHIFINWT)) %>%
  summarize(lr_m = survey_mean(MODMINS, na.rm = T))

mean_modmins_w <- svymean(~MODMINS, design = pa_NHS_weighted, na.rm=TRUE)

mean_vigmins_w <- svymean(~VIGMINS, design = pa_NHS_weighted, na.rm=TRUE)

mean_modmins_w <- svymean(~MODMINS, design = pa_NHS_weighted, na.rm=TRUE)

mean_vigmins_w <- svymean(~VIGMINS, design = pa_NHS_weighted, na.rm=TRUE)

### Standard deviation with weights
sd_modmins_w <- svyvar(~MODMINS, design = pa_NHS_weighted, na.rm=TRUE)

sd_vigmins_w <- svyvar(~VIGMINS, design = pa_NHS_weighted, na.rm=TRUE)

### Cumulative density function



hmodmins <- rlnorm(pa_NHS$MODMINS,mean_modmins_w,sd_modmins_w)

doR <- density(x, bw = 0.15, na.rm = TRUE)
lines(doR, col = "blue")
points(xx[i.out], rep(0.01, 10))

## Weighted observations:

## use 'counts / n' as weights:
mod_mins_density <- density(pa_NHS$MODMINS, weights = pa_NHS$NHIFINWT, na.rm = TRUE)
utils::str(dw) ## smaller n: only 126, but identical estimate:
stopifnot(all.equal(d[1:3], dw[1:3]))


#### Australiiiiian PA survey descriptives
### Make data set a survey, make grouping variables factors (Chek if we need to use replicate weights) and calculate mets
### and marginal mets

pa_NHS_weighted <-   pa_NHS  %>%
  srvyr::as_survey_design(weights = NHIFINWT)


pa_totals_by_agesex <- pa_NHS_weighted %>% 
  group_by(AGEB, SEX,
           .drop = FALSE) %>%
  dplyr::summarize(prop= srvyr::survey_mean(),
                   total = srvyr::survey_total(),
                   total_unweighted = srvyr::unweighted(n()))

### Check totals
tot_check <- c(sum(pa_totals_by_agesex$total), sum(pa_totals_by_agesex$total_unweighted))

### Check estimates againts ABS estimates for the proportion of adults (18-64) meeting the guidelines. 
### For men and women, only 15% meets recommendation of more than 150 minutes and toning/stenght twice per week. 
pa_check_all <- pa_NHS_weighted %>% 
  group_by(AGEGR,PAG18T64,
           .drop = FALSE) %>%
  dplyr::summarize(prop= srvyr::survey_mean())



### PA variables (means and quintiles)

#### Nine age groups
pa_means_by_agesex <- pa_NHS_weighted %>% 
  group_by(AGESEX) %>%
  dplyr::summarize(prop= srvyr::survey_mean(),
                   mean_modmins = srvyr::survey_mean(MODMINS),
                   mean_unweighted_mod_mins = srvyr::unweighted(mean(MODMINS)), 
                   mean_vigmins = srvyr::survey_mean(VIGMINS),
                   mean_unweighted_vig_mins = srvyr::unweighted(mean(VIGMINS)), 
                   mean_walktotmins = srvyr::survey_mean(EXWLKTME),
                   mean_unweighted_walktot_mins = srvyr::unweighted(mean(EXWLKTME)), 
                   mean_METsmin = srvyr::survey_mean(METsmin),
                   mean_unweighted_METsmin = srvyr::unweighted(mean(METsmin)),
                   mean_MMETsmin = srvyr::survey_mean(MMETsmin),
                   mean_unweighted_MMETsmin = srvyr::unweighted(mean(MMETsmin)),
                   mean_METhr = srvyr::survey_mean(METhr),
                   mean_unweighted_METhr = srvyr::unweighted(mean(METhr)),
                   mean_MMEThr = srvyr::survey_mean(MMEThr),
                   mean_unweighted_MMEThr = srvyr::unweighted(mean(MMEThr)),
                   mean_MMEThr_sc = srvyr::survey_mean(MMEThr_sc),
                   mean_unweighted_MMEThr_sc = srvyr::unweighted(mean(MMEThr_sc)),
                   sd_modmins = srvyr::survey_sd(MODMINS),
                   sd_vigmins = srvyr::survey_sd(VIGMINS),
                   sd_walktotmins = srvyr::survey_sd(EXWLKTME),
                   sd_METsmin = srvyr::survey_sd(METsmin),
                   sd_MMETsmin = srvyr::survey_sd(MMETsmin),
                   sd_METhr = srvyr::survey_sd(METhr),
                   sd_MMEThr = srvyr::survey_sd(MMEThr), 
                   sd_MMEThr_sc = srvyr::survey_sd(MMEThr_sc))


### MMETs minutes (median and quintiles)
pa_quantiles_by_agesex_MMETsmin <- pa_NHS_weighted %>% 
  group_by(AGESEX) %>%
  dplyr::summarise(MMETsmin= survey_quantile(MMETsmin, c(0.01, 0.25, 0.5, 0.75, 0.99)),
                   MMETsmin = survey_median(MMETsmin, vartype = c("ci")))


### MMET hours (median and quintiles)
pa_quantiles_by_agesex_MMEThr <- pa_NHS_weighted %>% 
  group_by(AGESEX) %>%
  dplyr::summarise(MMEThr= survey_quantile(MMEThr, c(0.01, 0.25, 0.5, 0.75, 0.99)),
                   MMEThr = survey_median(MMEThr, vartype = c("ci")))


### METs minutes (mean and quintiles)

pa_quantiles_by_agesex_METsmin <- pa_NHS_weighted %>% 
  group_by(AGESEX) %>%
  dplyr::summarise(METsmin= survey_quantile(METsmin, c(0.01, 0.25, 0.5, 0.75, 0.99)),
                   METsmin = survey_median(METsmin, vartype = c("ci")))

### MET hours (mean and quintiles)
pa_quantiles_by_agesex_METhr <- pa_NHS_weighted %>% 
  group_by(AGESEX) %>%
  dplyr::summarise(METhr= survey_quantile(METhr, c(0.01, 0.25, 0.5, 0.75, 0.99)),
                   METhr = survey_median(METhr, vartype = c("ci")))



### Draw density plots for PA distributions (METs and MMETs) 

plot(density(rlnorm(n, meanlog = 2.4, sdlog = 1.2)))


DensityFUN <- function(n, m, s) {density_out <- density(rlnorm(n, meanlog = m, sdlog = s))}


# Create variable to match whether people 18-64 and 64 plus meet PA guidelines to cross check matching (wont be exact due to weighting)
# Australia's Physical Activity & Sedentary Behaviour Guidelines for Adults (18-64 years)
# ALSO THESE VARIABLES MAY BE OF INTEREST AS AN OUTCOME. HOW MANY PEOPLE MEET GUIDELINES DU TO THE ITNERVENTION?

# NHS derives these guidelines as met if:
# Number of days did physical activity in the last week = 5-7 days
# Number of days did strength or toning activities in the last week = 2-7 days and
# Total minutes undertaken physical activity in last week = at least 150 minutes (where 'Total minutes undertaken physical activity in last week' 
#is the sum of walking for fitness + walking to and from places + moderate exercise + 2 times vigorous exercise time.)
# NHS variables
#EXNUDAYW	Number of days exercised for fitness, recreation, sport and to get to and from places in last week
#EXNUDST	Number of days did strength or toning activities in the last week
#EXWLKTME	Total minutes spent walking for exercise and transport last week
#EXLWMMIN	Total minutes undertaken moderate exercise last week
#EXLWVMIN	Total minutes undertaken vigorous exercise last week

persons_pa <- persons_pa  %>%
  dplyr::mutate(pa_guide_adults = ifelse((EXNUDAYW >=5 & EXNUDST >=2 & (EXWLKTME + EXLWMMIN + EXLWVMIN*2) >= 150), "Yes", "No"))

# Older adults
# NHS derives these guidelines as met if:
# Number of days did physical activity in the last week = 7 days and
# Number of days did physical activity for at least 30 minutes in the last week = 5-7 days
## NHS variables
#EXNUDAYW	Number of days exercised for fitness, recreation, sport and to get to and from places in last week
#EXNUDTH	Number of days exercised for at least 30 minutes in the last week

persons_pa <- persons_pa  %>%
  dplyr::mutate(pa_guide_older_adults = ifelse(EXNUDAYW >= 5 & EXNUDTH >=5,  "Yes", "No"))





persons_pa <-dplyr::select(persons_pa, age, sex, ses, state, work_status, occupation_cat, industry_cat, work_full, study_full, 
                           ltpa_marg_met, work_marg_met, work_ltpa_met, work_ltpa_marg_met, walk, walk_trans, pa_guide_adults, 
                           pa_guide_older_adults, NHIFINWT)

# Only keep adults over 18
persons_pa  <- persons_pa  %>% dplyr::filter(age>4)


# ### Calculate proportion meeting and not meeting using survey weights
## proportion tables to then compare with synthetic populations
### ABS says for 18-64: However, only 15.0% of these participants met both the physical activity and muscle strengthening aspects of the guidelines.
### we get the same using weights.

# active_adults <- dplyr::filter(persons_pa, age >= 5 & age <=14) %>%
#    srvyr::as_survey_design(weights = NHIFINWT)
# 
# meet_guide_adults <- active_adults  %>% 
#   group_by(pa_guide_adults,
#            .drop = FALSE) %>%
#   dplyr::summarize(prop_w= srvyr::survey_mean(),
#                    prop_uw = srvyr::unweighted(dplyr::n()))
# 
# strat_design_srvyr %>%
#   group_by(stype) %>%
#   summarize(n = unweighted(n()))
# 
# active_adults_older <- dplyr::filter(persons_pa, age > 14) %>%
#   srvyr::as_survey_design(weights = NHIFINWT)
# 
# meet_guide_adults_older <- active_adults_older  %>% 
#   group_by(pa_guide_older_adults,
#            .drop = FALSE) %>%
#   dplyr::summarize(prop= srvyr::survey_mean(),
#                    prop_uw = srvyr::unweighted(dplyr::n()))
