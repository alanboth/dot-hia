rm (list = ls())
library(readr)
library(dplyr)
library(tidyr)
# ---- chunk-2: Create pa data  ----  
## Get Australian National Health Survey 17-18 data 
##  One row per person
## Columns: sex, age, work_ltpa_marg_met = total leisure and work PA in a week
### MAY NOT NEED THIS, DEPENDS ON HOW WE MODEL THE PA AT THE POPULATION


pa_NHS <- read_csv(paste0(getwd(),"/Data/Physical activity/NHS2017-18_CSV/NHS17SPB.csv"))

pa_NHS  <- pa_NHS[order(pa_NHS$ABSPID),]

pa_NHS <-  dplyr::select(pa_NHS, ABSHIDB, NHIFINWT, AGEB, SEX, EXREGUIC, MODMINS,VIGMINS, EXLWMMIN, EXLWVMIN, WPAMMIN, WPAVMIN,
                         EXFSRMIN, LFSBC, OCCUP13B, EXREGUIC, PAG18T64, EXLWKTNO)



## Clean data. Replace Time in single minute unit values < 1..99996 >
#### 99997. Not applicable
#### 99998. Not stated

pa_NHS[pa_NHS==99996 | pa_NHS==99997 | pa_NHS==99998] <- NA

# pa_NHS$MODMINS - pa_NHS$EXLWMMIN - pa_NHS$WPAMMIN test that total mod/vig minutes equal leisure plus work.

### Dropping NA does not seem right, check what ABS did (4,154 values dropped)
pa_NHS <- drop_na(pa_NHS)

### Names to age variables

pa_NHS <- pa_NHS %>%  dplyr::mutate(SEX = case_when(SEX == 1 ~ 'male',
                                                    SEX == 2 ~ 'female'))

### Mid age cohort for synthetic populaton ITHIMR matching
pa_NHS <- pa_NHS %>%  dplyr::mutate(age = case_when(AGEB == 1 ~ 2,
                                                       AGEB == 2 ~ 7,
                                                       AGEB == 3 ~ 12,
                                                       AGEB == 4 ~ 17, 
                                                       AGEB == 5 ~ 17,
                                                       AGEB == 6 ~ 22,
                                                       AGEB == 7 ~ 27, 
                                                       AGEB == 8 ~ 32, 
                                                       AGEB == 9 ~ 37, 
                                                       AGEB == 10 ~ 42, 
                                                       AGEB == 11 ~ 47, 
                                                       AGEB == 12 ~ 52, 
                                                       AGEB == 13 ~ 57, 
                                                       AGEB == 14 ~ 62, 
                                                       AGEB == 15 ~ 67, 
                                                       AGEB == 16 ~ 72, 
                                                       AGEB == 17 ~ 77,
                                                       AGEB == 18 ~ 82, 
                                                       AGEB == 19 ~ 85))


### Generate marginal met hours variable. 
### NHS survey variables.   
pa_melbourne<- pa_NHS  %>%
  dplyr::mutate(ltpa_marg_met = (EXLWMMIN*4 + EXLWVMIN*6.5)/60) %>%
  dplyr::mutate(work_marg_met = (WPAMMIN*4 + WPAVMIN*6.5)/60) %>%
  dplyr::mutate(work_ltpa_met = (MODMINS*5 + VIGMINS*7.5 + EXFSRMIN*3.5)/60) %>%
  dplyr::mutate(work_ltpa_marg_met = (MODMINS*4 + VIGMINS*6.5 + EXFSRMIN*2.5)/60) %>%
  dplyr::mutate(walk_trans = EXLWKTNO)

### Rename to match ITHIMR


names(pa_melbourne)[names(pa_melbourne)=="SEX"] <- "sex"
names(pa_melbourne)[names(pa_melbourne)=="ABSHIDB"] <- "id"

# pa_melbourne <- pa_melbourne %>% dplyr::select(sex, age, agegr, work_ltpa_marg_met, ltpa_marg_met, work_marg_met, work_ltpa_met, walk_trans, id)



write_csv(pa_melbourne, (paste0(getwd(), "/Data/Processed/pa_melbourne.csv")))


