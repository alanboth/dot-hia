# Prepared all causes, diseases and injuries trends for: 1) Mortaity for all cause mortality, 
# 2) Incidence and case fatality for diseases and 3) Deaths and YLDs for injuries
### Check that for females all cancer rates are increasing


suppressPackageStartupMessages(library(readxl)) # for reading excel files
suppressPackageStartupMessages(library(stringr)) # for splitting strings
suppressPackageStartupMessages(library(dplyr)) # for manipulating data
suppressPackageStartupMessages(library(tidyr)) # for pivoting data
suppressPackageStartupMessages(library(forecast)) # for forecast of future trends in incidence and case fatality
### Data

incidence_trends_cancers="Data/aihw/trends/cancers_trends_incidence_aihw.xls"
mortality_trends_cancers="Data/aihw/trends/cancers_trends_mortality_aihw.xls"
trends_cvd="Data/aihw/trends/cardiovascular_disease_trends_aihw.xlsx"
grim_books="Data/aihw/trends/grim_books.csv"
trends_diabetes="Data/aihw/trends/diabetes_trends_aihw.xls"

### Methods
### 1) For data with future trends (incidence and mortality cancers): calculate annual change as ln(data(t1)/data(t0))/diff(t1,t0)
### 2) TBD, depends on data available from AIHW.
### For data without future trends (mortality cardiovascular, COPD and diabetes): calculate future trend for 10 years and derive
#### annual change as for data with trends. 
### INCIDENCE TRENDS CANCERS

### Breast
data <- readxl::read_xls(incidence_trends_cancers, sheet = "Table S5", range = "B32:L34",  
                                                     col_names = c("ASR", as.character(2011:2020)))

values <- c(log(data[[11]]/data[[2]]) /(as.numeric(colnames(data[11])) - as.numeric(as.numeric(colnames(data[2])))), 
                        (as.numeric(colnames(data[11])) - as.numeric(colnames(data[2]))))

data_2 <- data.frame(year = rep(c(0:100)), sex = rep("female", 101)) %>%
                            mutate(brsc = ifelse(year <= values[2], 
                                                 exp(values[1]* year),
                                                 exp(values[1] * values[2])))
incidence_trends_f <- data_2

### Uterine
data <- readxl::read_xls(incidence_trends_cancers, sheet = "Table S21", range = "B32:L34",
  col_names = c("ASR", as.character(2011:2020)))

values <- c(log(data[[11]]/data[[2]]) /(as.numeric(colnames(data[11])) - as.numeric(as.numeric(colnames(data[2])))), 
                                                            (as.numeric(colnames(data[11])) - as.numeric(colnames(data[2]))))
data_2 <- data.frame(year = rep(c(0:100)), sex = rep("female", 101)) %>%
  mutate(utrc = ifelse(year <= values[2], 
                       exp(values[1]* year),
                       exp(values[1] * values[2])))

incidence_trends_f <- merge(incidence_trends_f, data_2, by = c("year", "sex"))
  
### Lung 
#### Females
data <- readxl::read_xls(incidence_trends_cancers, sheet = "Table S10b", range = "B32:L34", 
                                  col_names = c("ASR", as.character(2011:2020))) 
values <- c(log(data[[11]]/data[[2]]) /(as.numeric(colnames(data[11])) - as.numeric(as.numeric(colnames(data[2])))), 
            (as.numeric(colnames(data[11])) - as.numeric(colnames(data[2]))))
data_2 <- data.frame(year = rep(c(0:100)), sex = rep("female", 101)) %>%
  mutate(tbalc = ifelse(year <= values[2], 
                       exp(values[1]* year),
                       exp(values[1] * values[2])))

incidence_trends_f <- merge(incidence_trends_f, data_2, by = c("year", "sex"))

#### Males
data<- readxl::read_xls(incidence_trends_cancers, sheet = "Table S10a", range = "B32:L34", 
                                  col_names = c("ASR", as.character(2011:2020)))

values <- c(log(data[[11]]/data[[2]]) /(as.numeric(colnames(data[11])) - as.numeric(as.numeric(colnames(data[2])))), 
              (as.numeric(colnames(data[11])) - as.numeric(colnames(data[2]))))
data_2 <- data.frame(year = rep(c(0:100)), sex = rep("male", 101)) %>%
  mutate(tbalc = ifelse(year <= values[2], 
                        exp(values[1]* year),
                        exp(values[1] * values[2])))

incidence_trends_m <- data_2

### Colorectal
#### Females
data <- readxl::read_xls(incidence_trends_cancers, sheet = "Table S3b", range = "B32:L34", 
                         col_names = c("ASR", as.character(2011:2020))) 
values <- c(log(data[[11]]/data[[2]]) /(as.numeric(colnames(data[11])) - as.numeric(as.numeric(colnames(data[2])))), 
            (as.numeric(colnames(data[11])) - as.numeric(colnames(data[2]))))
data_2 <- data.frame(year = rep(c(0:100)), sex = rep("female", 101)) %>%
  mutate(carc = ifelse(year <= values[2], 
                        exp(values[1]* year),
                        exp(values[1] * values[2])))

incidence_trends_f <- merge(incidence_trends_f, data_2, by = c("year", "sex"))

#### Males
data <- readxl::read_xls(incidence_trends_cancers, sheet = "Table S3a", range = "B32:L34", 
                                        col_names = c("ASR", as.character(2011:2020)))

values <- c(log(data[[11]]/data[[2]]) /(as.numeric(colnames(data[11])) - as.numeric(as.numeric(colnames(data[2])))), 
            (as.numeric(colnames(data[11])) - as.numeric(colnames(data[2]))))

data_2 <- data.frame(year = rep(c(0:100)), sex = rep("male", 101)) %>%
  mutate(carc = ifelse(year <= values[2], 
                       exp(values[1]* year),
                       exp(values[1] * values[2])))

incidence_trends_m <- merge(incidence_trends_m, data_2, by = c("year", "sex"))

### MORTALITY TRENDS CANCERS

data <- readxl::read_xls(mortality_trends_cancers, sheet = "Breast", range = "P6:Y19")  %>% mutate_if(is.character,as.numeric)

values <- c(log(data[[13,8]]/data[[2,8]])/(data[[13,1]]-data[[2,1]]), (data[[13,1]]-data[[2,1]]))

data_2 <- data.frame(year = rep(c(0:100)), sex =  rep("female", 101)) %>%
  mutate(brsc = ifelse(year <= values[2], 
                       exp(values[1]* year),
                       exp(values[1] * values[2])))

mortality_trends_f <- data_2

### Uterine cancer females

data <- readxl::read_xls(mortality_trends_cancers, sheet = "Uterine", range = "P6:Y19")  %>% mutate_if(is.character,as.numeric)

values <- c(log(data[[13,8]]/data[[2,8]])/(data[[13,1]]-data[[2,1]]), (data[[13,1]]-data[[2,1]]))

data_2 <- data.frame(year = rep(c(0:100)), sex =  rep("female", 101)) %>%
  mutate(utrc = ifelse(year <= values[2], 
                       exp(values[1]* year),
                       exp(values[1] * values[2])))

mortality_trends_f <- merge(mortality_trends_f, data_2, by = c("year", "sex"))

### Lung cancer females
data <- readxl::read_xls(mortality_trends_cancers, sheet = "Lung", range = "P6:Y19")  %>% mutate_if(is.character,as.numeric)

values <- c(log(data[[13,8]]/data[[2,8]])/(data[[13,1]]-data[[2,1]]), (data[[13,1]]-data[[2,1]]))

data_2 <- data.frame(year = rep(c(0:100)), sex =  rep("female", 101)) %>%
  mutate(tbalc = ifelse(year <= values[2], 
                        exp(values[1]* year),
                        exp(values[1] * values[2])))

mortality_trends_f <- merge(mortality_trends_f, data_2, by = c("year", "sex"))


### Lung cancer males

data <- readxl::read_xls(mortality_trends_cancers, sheet = "Lung", range = "P6:Y19")  %>% mutate_if(is.character,as.numeric)

values <- c(log(data[[13,3]]/data[[2,3]])/(data[[13,1]]-data[[2,1]]), (data[[13,1]]-data[[2,1]]))

data_2 <- data.frame(year = rep(c(0:100)), sex =  rep("female", 101)) %>%
  mutate(tbalc = ifelse(year <= values[2], 
                        exp(values[1]* year),
                        exp(values[1] * values[2])))

mortality_trends_m <- data_2

### Colorectal cancer

### Colorectal cancer females
data <- readxl::read_xls(mortality_trends_cancers, sheet = "Colorectal", range = "P6:Y19")  %>% mutate_if(is.character,as.numeric)

values <- c(log(data[[13,8]]/data[[2,8]])/(data[[13,1]]-data[[2,1]]), (data[[13,1]]-data[[2,1]]))

data_2 <- data.frame(year = rep(c(0:100)), sex =  rep("female", 101)) %>%
  mutate(utrc = ifelse(year <= values[2], 
                       exp(values[1]* year),
                       exp(values[1] * values[2])))

mortality_trends_f <- merge(mortality_trends_f, data_2, by = c("year", "sex"))

### Colorectal cancer males
data <- readxl::read_xls(mortality_trends_cancers, sheet = "Colorectal", range = "P6:Y19")  %>% mutate_if(is.character,as.numeric)

values <- c(log(data[[13,3]]/data[[2,3]])/(data[[13,1]]-data[[2,1]]), (data[[13,1]]-data[[2,1]]))

data_2 <- data.frame(year = rep(c(0:100)), sex =  rep("female", 101)) %>%
  mutate(tbalc = ifelse(year <= values[2], 
                        exp(values[1]* year),
                        exp(values[1] * values[2])))

mortality_trends_m <- merge(mortality_trends_m, data_2, by = c("year", "sex"))

#### Cardiovascular, COPD and diabetes
#### Get data to share with lucy

cvd_hospital <- readxl::read_xlsx(trends_cvd, sheet = "Table 2.9", range = "B8:H26", col_names = c("Year", "males_numbers", "female_numers",
                                                                                                   "persons_numbers", "male_rates", "female_rates", 
                                                                                                   "person_rates"))

write.csv(cvd_hospital, "Data/trend_data_Lucy/cvd_hospital.csv", row.names=F, quote=F)  

cvd_deaths <- readxl::read_xlsx(trends_cvd, sheet = "Table 3.6", range = "B7:E45")
  
write.csv(cvd_deaths, "Data/trend_data_Lucy/cvd_deaths.csv", row.names=F, quote=F)  

copd <- read.csv(grim_books, as.is=T) %>%
  filter(AGE_GROUP == "Total", SEX != "Persons", YEAR >= 2000,
         cause_of_death == "Chronic obstructive pulmonary disease (COPD) (ICD-10 J40–J44)") %>%
  select(YEAR, SEX, age_standardised_rate) %>%
  pivot_wider(id_cols = c(YEAR, SEX, age_standardised_rate), 
              values_from = c(age_standardised_rate), names_from = c(SEX))

write.csv(copd, "Data/trend_data_Lucy/copd.csv", row.names=F, quote=F)  

### diabetes all includes diabtes as:underlying or associated cause of death
diabetes_death_all <-  readxl::read_xls(trends_diabetes,  sheet = "Table 3.1a & Table 3.1b", range = "B8:E30")


write.csv(diabetes_death_all, "Data/trend_data_Lucy/diabetes_death_all.csv", row.names=F, quote=F)  


### diabetes under includes diabetes as underlying cause of death

diabetes_death_under <-  readxl::read_xls(trends_diabetes, sheet = "Table 3.1a & Table 3.1b", range = "B37:E75")


write.csv(diabetes_death_under, "Data/trend_data_Lucy/diabetes_death_under.csv", row.names=F, quote=F)  

### Sort out, old code but also mortlaity cancers, check with latest data from AIHW
  
    extract("Year", c("Year", "discard"), "(.+)–(.+)", remove=FALSE) %>%
  mutate(Year = as.numeric(Year))








### Injuries trends (from GBD??)