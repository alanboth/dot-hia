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
trends_copd="Data/aihw/trends/grim_books.csv"

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

incidence_trends_f <- merge(incidence_trends_f, uterine_cancer_f_expanded, by = c("year", "sex"))
  
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

incidence_trends_f <- merge(incidence_trends_f, lung_cancer_f_expanded, by = c("year", "sex"))

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



#### Cardiovascular, COPD and diabetes
### Derive trends
# Derive future trends using forecast package and auto.arima function.
# Used for stationary time series: 
# Interpretation of model: ARIMA(p,d,q) =  ARIMA(Autoregressive (AR) model of order p, number of differences: d, Moving Average (MA) model of order q)

### INCIDENCE TRENDS CARDIOVASCULAR (apply to IHD and stroke)
### Hospitalisations and as a proportion of deaths rates trend

### Females

data <- readxl::read_xlsx(trends_cvd, sheet = "Table 2.9", range = "B8:H26") %>%
  extract("Year", c("Year", "discard"), "(.+)–(.+)", remove=FALSE) %>%
  mutate(Year = as.numeric(Year))

tsData = ts(data[1:17,7], start = (2001),  frequency = 1)
plot(tsData)

arma_fit <- auto.arima(tsData)
arma_forecast <- forecast(arma_fit, h = 10, level=c(2.5, 97.5))
arma_fit_accuracy <- accuracy(arma_forecast, test)
arma_fit; arma_forecast; arma_fit_accuracy
plot(arma_forecast, ylim=c(0,100))
lines(data[1:18,7])
print(summary(arma_forecast))


values <- c(log(data[[18,7]]/data[[1,7]])/(data[[18,2]] -data[[1,2]]), data[[18,2]] -data[[1,2]])

data_2 <- data.frame(year = rep(c(0:100)), sex = rep("female", 101)) %>%
  mutate(ishd = ifelse(year <= values[2], 
                       exp(values[1]* year),
                       exp(values[1] * values[2]))) %>%
  mutate(strk = ishd)

incidence_trends_f <- merge(incidence_trends_f, data_2, by = c("year", "sex"))

### Males

data <- readxl::read_xlsx(trends_cvd, sheet = "Table 2.9", range = "B8:H26") %>%
  extract("Year", c("Year", "discard"), "(.+)–(.+)", remove=FALSE) %>%
  mutate(Year = as.numeric(Year))

values <- c(log(data[[18,6]]/data[[1,6]])/(data[[18,2]] -data[[1,2]]), data[[18,2]] -data[[1,2]])

data_2 <- data.frame(year = rep(c(0:100)), sex = rep("male", 101)) %>%
  mutate(ishd = ifelse(year <= values[2], 
                       exp(values[1]* year),
                       exp(values[1] * values[2]))) %>%
  mutate(strk = ishd)

incidence_trends_m <- merge(incidence_trends_m, data_2, by = c("year", "sex"))

### Check values of incidence as 58% of mortality trend, significantly lower
### Females

data <- readxl::read_xlsx(trends_cvd, sheet = "Table 3.6", range = "B7:E45")

values <- c((log(data[[38,3]]/data[[21,3]])/(data[[38,1]] -data[[21,1]]))*0.58, data[[38,1]] -data[[21,1]])


tsData = ts(data[28:38,3], start = (2008),  frequency = 1)
plot(tsData)

arma_fit <- auto.arima(tsData)
arma_forecast <- forecast(arma_fit, h = 5, level=c(2.5, 97.5))
arma_fit_accuracy <- accuracy(arma_forecast, test)
arma_fit; arma_forecast; arma_fit_accuracy
plot(arma_forecast, ylim=c(0,100))
lines(data[1:18,7])
print(summary(arma_forecast))

data_2 <- data.frame(year = rep(c(0:100)), sex = rep("female", 101)) %>%
  mutate(ishd_2 = ifelse(year <= values[2], 
                       exp(values[1]* year),
                       exp(values[1] * values[2]))) %>%
  mutate(strk_2 = ishd_2)

incidence_trends_f <- merge(incidence_trends_f, data_2, by = c("year", "sex"))

### Males

data <- readxl::read_xlsx(trends_cvd, sheet = "Table 3.6", range = "B7:E45")

values <- c((log(data[[38,2]]/data[[21,2]])/(data[[38,1]] -data[[21,1]]))*0.58, data[[38,1]] -data[[21,1]])

data_2 <- data.frame(year = rep(c(0:100)), sex = rep("male", 101)) %>%
  mutate(ishd_2 = ifelse(year <= values[2], 
                         exp(values[1]* year),
                         exp(values[1] * values[2]))) %>%
  mutate(strk_2 = ishd_2)

incidence_trends_m <- merge(incidence_trends_m, data_2, by = c("year", "sex"))



### INCIDENCE TRENDS COPD
### females
data <- read.csv(trends_copd, as.is=T) %>%
  filter(AGE_GROUP == "Total", SEX != "Persons", YEAR >= 2010,
         cause_of_death == "Chronic obstructive pulmonary disease (COPD) (ICD-10 J40–J44)") 
data_f <- data %>% filter(SEX == "Females")


### Exploratory

tsData = ts(data_f[1:8,8], start = (2010),  frequency = 1)
plot(tsData)
components.ts = decompose(tsData)
plot(components.ts)


#### Moving average

moving_average = forecast(ma(data_f[1:8,8], order=2), h=10, allow.multiplicative.trend = TRUE)
moving_average_accuracy = accuracy(moving_average, data_f[1:8,8])
moving_average; moving_average_accuracy
plot(moving_average, ylim=c(0,30))
lines(data_f[1:8, 8])

### Exponential

exp <- ses(data_f[1:8,8], 10, initial="simple")

### Select best model
### Gives prediction intervals, not confidence intervals. Attention how to use them if used in uncertainty.

train = data_f[1:8,8]
test = data_f[3:8,8]
arma_fit <- auto.arima(tsData)
arma_forecast <- forecast(arma_fit, h = 10, level=c(2.5, 97.5))
arma_fit_accuracy <- accuracy(arma_forecast, test)
arma_fit; arma_forecast; arma_fit_accuracy
plot(arma_forecast, ylim=c(0,30))
lines(data_f[1:8,8])
print(summary(arma_forecast))

values <- c((log(data_f[[2,8]]/data_f[[1,8]])/(data_f[[2,3]] - data_f[[1,3]])), data_f[[2,3]] - data_f[[1,3]])

data_2 <- data.frame(year = rep(c(0:100)), sex = rep("male", 101)) %>%
  mutate(copd = ifelse(year <= values[2], 
                         exp(values[1]* year),
                         exp(values[1] * values[2])))

incidence_trends_f <- merge(incidence_trends_m, data_2, by = c("year", "sex"))


### TO DO LAST (TO DO: ADD NON Cancers)

incidence_trends <- dplyr::bind_rows(incidence_trends_f, incidence_trends_m) %>%
  arrange(sex, year)


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


### Injuries trends (from GBD??)