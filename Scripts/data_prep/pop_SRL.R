## Note population projections are not used in the models

# ---------------------------- Prepare working population VITM scenario -----------------------------------
suppressPackageStartupMessages(library(dplyr)) # for manipulating data
suppressPackageStartupMessages(library(tidyr)) # for pivoting data
suppressPackageStartupMessages(library(readxl)) # for reading excel files
rm (list = ls())

# Population data by age and sex group for PMSLT

## Get data VISTA and calculate proportion employed and not-employed

hh_VISTA_location="Data/Travelsurvey/VISTA12-18/H_VISTA_1218_V1.csv"
person_VISTA_location="Data/Travelsurvey/VISTA12-18/P_VISTA1218_V1.csv"
ses_index_location="Data/Travelsurvey/ABS SEIFA/ses.csv"

hh_VISTA <- read.csv(hh_VISTA_location,as.is=T,fileEncoding="UTF-8-BOM") %>%
  dplyr::select(HHID,SurveyPeriod,HomeSubRegion,HOMEPC) %>%
  filter(HHID!="") # some rows were completely blank

person_VISTA <- read.csv(person_VISTA_location,as.is=T, fileEncoding="UTF-8-BOM") %>%
  dplyr::select(PERSID, HHID, AGE, SEX, FULLTIMEWORK, ANYWORK, STUDYING, ANZSCO1,
                ANZSIC1, WDPERSWGT, WEPERSWGT)
## Add SEIFA-IRSD
ses_index <- read.csv(ses_index_location,as.is=T, fileEncoding="UTF-8-BOM") %>% 
  rename_all(~c("HOMEPC","ses")) %>%
  filter(!is.na(HOMEPC))

## Join persons and household, keep data for greater Melbourne only and create unique weights
persons_file <- left_join(person_VISTA, hh_VISTA, by = "HHID") %>% 
  filter(SurveyPeriod == "2017-18" &
           (HomeSubRegion != "Geelong" | HomeSubRegion != "Other")) %>%
  rowwise() %>% # want to sum across rows, not down columns
  mutate(participant_wt = sum(as.numeric(WDPERSWGT),as.numeric(WEPERSWGT),na.rm=T)) %>%
  dplyr::select(-WDPERSWGT,-WEPERSWGT) %>%
  as.data.frame() %>%
  inner_join(ses_index, by="HOMEPC") %>%
  ### Create age category as persons_pa (from NHS) is only available by age groups
  rename(age=AGE) %>%
  filter(age>=18) %>%
  rename(sex=SEX) %>%
  mutate(sex=case_when(sex=="M" ~ 'male', sex=="F" ~ 'female')) %>%
  mutate(dem_index = case_when(age >= 15 & age <=  19 & sex ==   "male" ~  1,
                               age >= 20 & age <=  24 & sex ==   "male" ~  3,
                               age >= 25 & age <=  29 & sex ==   "male" ~  5,
                               age >= 30 & age <=  34 & sex ==   "male" ~  7,
                               age >= 35 & age <=  39 & sex ==   "male" ~  9,
                               age >= 40 & age <=  44 & sex ==   "male" ~  11,
                               age >= 45 & age <=  49 & sex ==   "male" ~  13,
                               age >= 50 & age <=  54 & sex ==   "male" ~  15, 
                               age >= 55 & age <=  59 & sex ==   "male" ~  17,
                               age >= 60 & age <=  64 & sex ==   "male" ~ 19,
                               age >= 65 & age <=  69 & sex ==   "male" ~ 21, 
                               age >= 70 & age <=  74 & sex ==   "male" ~ 23,
                               age >= 75 & age <=  79 & sex ==   "male" ~ 25, 
                               age >= 80 & age <=  84 & sex ==   "male" ~ 27, 
                               age >= 85 & age <=  89 & sex ==   "male" ~ 29,
                               age >= 90 & age <=  94 & sex ==   "male" ~ 31,
                               age >= 95 & age <= 120 & sex ==   "male" ~ 33,
                               age >= 15 & age <=  19 & sex ==   "female" ~ 2,
                               age >= 20 & age <=  24 & sex == "female" ~ 4,
                               age >= 25 & age <=  29 & sex == "female" ~ 6,
                               age >= 30 & age <=  34 & sex == "female" ~ 8,
                               age >= 35 & age <=  39 & sex == "female" ~ 10,
                               age >= 40 & age <=  44 & sex == "female" ~ 12,
                               age >= 45 & age <=  49 & sex == "female" ~ 14,
                               age >= 50 & age <=  54 & sex == "female" ~ 16,
                               age >= 55 & age <=  59 & sex == "female" ~ 18,
                               age >= 60 & age <=  64 & sex == "female" ~ 20,
                               age >= 65 & age <=  69 & sex == "female" ~ 22,
                               age >= 70 & age <=  74 & sex == "female" ~ 24,
                               age >= 75 & age <=  79 & sex == "female" ~ 26,
                               age >= 80 & age <=  84 & sex == "female" ~ 28,
                               age >= 85 & age <=  89 & sex == "female" ~ 30,
                               age >= 90 & age <=  94 & sex == "female" ~ 32,
                               age >= 95 & age <= 120 & sex == "female" ~ 34)) %>%
  ### Employment status to match persons_pa work_status variable (LFSBC)
  mutate(work_status = ifelse(ANYWORK=="Yes",'employed','unemployed')) %>%
  dplyr::select(age, sex, ses, work_status, dem_index, participant_wt)

# Proportion of VISTA employed (use population weights)


# Weighted 
persons_file_weighted <-  persons_file  %>%
  srvyr::as_survey_design(weights = participant_wt)

# Proportion employed (loop is to get weighted proportions by dem-_index)
employed_VISTA <- list()
index <- 1
for (i in unique(persons_file$dem_index)) {
employed_VISTA[[index]] <- persons_file_weighted   %>% 
  filter(dem_index==i) %>%
  group_by(work_status,
           .drop = FALSE) %>%
  dplyr::summarize(prop= srvyr::survey_mean(),
                   total = srvyr::survey_total(),
                   total_unweighted = srvyr::unweighted(dplyr::n()))
  employed_VISTA[[index]]$dem_index <- i 
index <- index + 1
}

employed_VISTA <- do.call(rbind, employed_VISTA)

## Get population projections

DELWP_population="Data/original/delwp/DELWP Projections 2018 (Unpublished) - CONFIDENTIAL SA2 AGES_2036_final.xlsx"
population_DELWP_2016 <- readxl::read_xlsx(DELWP_population, sheet = 4, range = cell_rows(12:564)) %>%
  filter(`Area Name` == "Greater Melbourne") %>%
  dplyr::select(8:43) %>%
  pivot_longer(1:36, names_to = "dem", values_to = "2016")

population_DELWP_2021 <- readxl::read_xlsx(DELWP_population, sheet = 5, range = cell_rows(12:564)) %>%
  filter(`Area Name` == "Greater Melbourne") %>%
  dplyr::select(8:43) %>%
  pivot_longer(1:36, names_to = "dem", values_to = "2021")

population_DELWP_2026 <- readxl::read_xlsx(DELWP_population, sheet = 6, range = cell_rows(12:564)) %>%
  filter(`Area Name` == "Greater Melbourne") %>%
  dplyr::select(8:43) %>%
  pivot_longer(1:36, names_to = "dem", values_to = "2026")

population_DELWP_2031 <- readxl::read_xlsx(DELWP_population, sheet = 7, range = cell_rows(12:564)) %>%
  filter(`Area Name` == "Greater Melbourne") %>%
  dplyr::select(8:43) %>%
  pivot_longer(1:36, names_to = "dem", values_to = "2031")

population_DELWP_2036 <- readxl::read_xlsx(DELWP_population, sheet = 8, range = cell_rows(12:564)) %>%
  filter(`Area Name` == "Greater Melbourne") %>%
  dplyr::select(8:43) %>%
  pivot_longer(1:36, names_to = "dem", values_to = "2036")


population_delpw <-left_join(population_DELWP_2016, population_DELWP_2021) %>%
  left_join(population_DELWP_2026) %>%
  left_join(population_DELWP_2031) %>%
  left_join(population_DELWP_2036) 

population_delpw <- as.data.frame(apply(population_delpw,2,function(x)gsub('\\s+', ' ',x)))
population_delpw[2:6] <- lapply(population_delpw[2:6], as.numeric)

population_delpw$dem[population_delpw$dem == "Females 85 and over"] <- "Females 85"
population_delpw$dem[population_delpw$dem == "Males 85 and over"] <- "Males 85"


population_delpw <- population_delpw %>%
  separate(dem, c("sex", "age")) %>%
  mutate(age=as.numeric(age)) %>%
  mutate(age_cat = age + 2) %>%
  mutate(sex_age_cat = paste(tolower(sex), age_cat, sep="_")) %>%
  filter (age >=15)


