## Trends preparation
### Trends for incidence and case fatality and all cause mortality
### Cancers: Breast cancer (females only), uterine cancer (female only), colon cancer, lung cancer
### Non-cancers: ischemic heart disease, stroke, type 2 diabetes, copd
### cHECK TRENDS IN nz bode STUDIES AND AUS STUDIES
### Not data for colon cancer

### incidence trends done: breast, uterine and lung cancers
### incidence trends to do: colon cancer, ihd, stroke, t2d and copd
### mortality trends


### TO DO: bind all columns and save
suppressPackageStartupMessages(library(readxl)) # for reading excel files
suppressPackageStartupMessages(library(stringr)) # for splitting strings
suppressPackageStartupMessages(library(dplyr)) # for manipulating data
suppressPackageStartupMessages(library(tidyr)) # for pivoting data


incidence_trends_cancers="Data/aihw/trends/cancers_trends_incidence_aihw.xls"
mortality_trends_cancers="Data/aihw/trends/cancers_trends_mortality_aihw.xls"

### CANCER INCIDENCE TRENDS
#### BREAST CANCER FEMALES

incidence_trends_breast_cancer_f <- readxl::read_xls(incidence_trends_cancers, sheet = "Table S5", range = "B6:L26") %>%
                                    mutate(incidence_trend_brsc = log(`2020`/`2011`)/(2020-2011)) %>%
                                    extract(`Age group`, c("from_age", "to_age"), "(.+)–(.+)", remove=FALSE) %>% #Expand to one yr age groups
                                    mutate(from_age = as.numeric(case_when(`Age group`== "95+"  ~  "95",
                                    TRUE  ~  from_age)),
                                    to_age = as.numeric(case_when(`Age group`=="95+"  ~  "99",
                                    TRUE  ~  to_age)),
                                    agediff = to_age - from_age + 1,  # this will equal 5 
                                    brsc = incidence_trend_brsc) %>%
                                    rename(agegroup = `Age group`)

## Now stretch the data out using an index, to create a data frame with 1 row per year of age and create a variable for year of age. 
index <- rep(1:nrow(incidence_trends_breast_cancer_f), incidence_trends_breast_cancer_f$agediff)
incidence_trends_breast_cancer_f_yr <- incidence_trends_breast_cancer_f[index,] %>%
  mutate(year = from_age + sequence(incidence_trends_breast_cancer_f$agediff) - 1) %>%
  mutate(sex = "female") %>% 
  select(sex, brsc, year)

#### UTERINE CANCERS FEMALES

#### CHECK, GENERALLY INCREASING TRENDS

incidence_trends_uterine_cancer_f <- readxl::read_xls(incidence_trends_cancers, sheet = "Table S21", range = "B6:L26") %>%
  mutate(incidence_trend_endo = log(`2020`/`2011`)/(2020-2011)) %>%
  extract(`Age group`, c("from_age", "to_age"), "(.+)–(.+)", remove=FALSE) %>% #Expand to one yr age groups
  mutate(from_age = as.numeric(case_when(`Age group`== "95+"  ~  "95",
                                         TRUE  ~  from_age)),
         to_age = as.numeric(case_when(`Age group`=="95+"  ~  "99",
                                       TRUE  ~  to_age)),
         agediff = to_age - from_age + 1,  # this will equal 5 
         utrc = incidence_trend_endo) %>%
  rename(agegroup = `Age group`)

## Now stretch the data out using an index, to create a data frame with 1 row per year of age and create a variable for year of age. 
index <- rep(1:nrow(incidence_trends_uterine_cancer_f), incidence_trends_uterine_cancer_f$agediff)
incidence_trends_uterine_cancer_f_yr <- incidence_trends_uterine_cancer_f[index,] %>%
  mutate(year = from_age + sequence(incidence_trends_uterine_cancer_f$agediff) - 1) %>%
  mutate(sex = "female") %>% 
  select(sex, utrc, year)

  
#### LUNG CANCERS FEMALES

#### CHECK, GENERALLY INCREASING TRENDS

incidence_trends_lung_cancer_f <- readxl::read_xls(incidence_trends_cancers, sheet = "Table S10b", range = "B6:L26") %>%
  mutate(incidence_trend_lung = log(`2020`/`2011`)/(2020-2011)) %>%
  extract(`Age group`, c("from_age", "to_age"), "(.+)–(.+)", remove=FALSE) %>% #Expand to one yr age groups
  mutate(from_age = as.numeric(case_when(`Age group`== "95+"  ~  "95",
                                         TRUE  ~  from_age)),
         to_age = as.numeric(case_when(`Age group`=="95+"  ~  "99",
                                       TRUE  ~  to_age)),
         agediff = to_age - from_age + 1,  # this will equal 5 
         tbalc = incidence_trend_lung) %>%
  rename(agegroup = `Age group`)

## Now stretch the data out using an index, to create a data frame with 1 row per year of age and create a variable for year of age. 
index <- rep(1:nrow(incidence_trends_lung_cancer_f), incidence_trends_lung_cancer_f$agediff)
incidence_trends_lung_cancer_f_yr <- incidence_trends_lung_cancer_f[index,] %>%
  mutate(year = from_age + sequence(incidence_trends_lung_cancer_f$agediff) - 1) %>%
  mutate(sex = "female") %>% 
  select(sex, tbalc, year)

#### LUNG CANCER MALES


incidence_trends_lung_cancer_m <- readxl::read_xls(incidence_trends_cancers, sheet = "Table S10a", range = "B6:L26") %>%
  mutate(incidence_trend_lung = log(`2020`/`2011`)/(2020-2011)) %>%
  extract(`Age group`, c("from_age", "to_age"), "(.+)–(.+)", remove=FALSE) %>% #Expand to one yr age groups
  mutate(from_age = as.numeric(case_when(`Age group`== "95+"  ~  "95",
                                         TRUE  ~  from_age)),
         to_age = as.numeric(case_when(`Age group`=="95+"  ~  "99",
                                       TRUE  ~  to_age)),
         agediff = to_age - from_age + 1,  # this will equal 5 
         tbalc = incidence_trend_lung) %>%
  rename(agegroup = `Age group`)

## Now stretch the data out using an index, to create a data frame with 1 row per year of age and create a variable for year of age. 
index <- rep(1:nrow(incidence_trends_lung_cancer_m), incidence_trends_lung_cancer_m$agediff)
incidence_trends_lung_cancer_m_yr <- incidence_trends_lung_cancer_f[index,] %>%
  mutate(year = from_age + sequence(incidence_trends_lung_cancer_m$agediff) - 1) %>%
  mutate(sex = "male") %>% 
  select(sex, tbalc, year)

#### COLON CANCERS FEMALES
#### COLON CANCR MALES

#### BIND mortality trends separetley from incidence trends
### CANCER MORTALITY TREND
### Breast cancer females

mortality_trends_breast_cancer_f <- readxl::read_xls(mortality_trends_cancers, sheet = "Breast", range = "B6:N44") 
mortality_trends_breast_cancer_f <- mortality_trends_breast_cancer_f[-c(1:20), ] %>% ### Delete males' observations
  mutate(mortality_trend_breast = log(`2025`/`2015`)/(2025-2015)) %>%
  extract(`Age group (years)`, c("from_age", "to_age"), "(.+)–(.+)", remove=FALSE) %>% #Expand to one yr age groups
  mutate(from_age = as.numeric(case_when(`Age group (years)` == "85+"  ~  "85",
                                         TRUE  ~  from_age)),
         to_age = as.numeric(case_when(`Age group (years)` =="85+"  ~  "99",
                                       TRUE  ~  to_age)),
         agediff = to_age - from_age + 1,  # this will equal 5 
         brcs = mortality_trend_breast) %>%
  rename(agegroup = `Age group (years)`)

## Now stretch the data out using an index, to create a data frame with 1 row per year of age and create a variable for year of age. 
index <- rep(1:nrow(mortality_trends_breast_cancer_f), mortality_trends_breast_cancer_f$agediff)
mortality_trends_breast_cancer_f <- mortality_trends_breast_cancer_f[index,] %>%
  mutate(year = from_age + sequence(mortality_trends_breast_cancer_f$agediff) - 1) %>%
  mutate(sex = "female") %>% 
  select(sex, brcs, year)

### Uterine cancer females
mortality_trends_uterine_cancer_f <- readxl::read_xls(mortality_trends_cancers, sheet = "Uterine", range = "B6:N44") 
mortality_trends_uterine_cancer_f <- mortality_trends_uterine_cancer_f[-c(1:20), ] %>% ### Delete males' observations
  mutate(mortality_trend_uterine = log(`2025`/`2015`)/(2025-2015)) %>%
  extract(`Age group (years)`, c("from_age", "to_age"), "(.+)–(.+)", remove=FALSE) %>% #Expand to one yr age groups
  mutate(from_age = as.numeric(case_when(`Age group (years)` == "85+"  ~  "85",
                                         TRUE  ~  from_age)),
         to_age = as.numeric(case_when(`Age group (years)` =="85+"  ~  "99",
                                       TRUE  ~  to_age)),
         agediff = to_age - from_age + 1,  # this will equal 5 
         endo = mortality_trend_uterine) %>%
  rename(agegroup = `Age group (years)`)

## Now stretch the data out using an index, to create a data frame with 1 row per year of age and create a variable for year of age. 
index <- rep(1:nrow(mortality_trends_uterine_cancer_f), mortality_trends_uterine_cancer_f$agediff)
mortality_trends_uterine_cancer_f <- mortality_trends_uterine_cancer_f[index,] %>%
  mutate(year = from_age + sequence(mortality_trends_uterine_cancer_f$agediff) - 1) %>%
  mutate(sex = "female") %>% 
  select(sex, endo, year)

### Lung cancer females
mortality_trends_lung_cancer_f <- readxl::read_xls(mortality_trends_cancers, sheet = "Lung", range = "B6:N44") 
mortality_trends_lung_cancer_f <- mortality_trends_lung_cancer_f[-c(1:20), ] %>% ### Delete males' observations
  mutate(mortality_trend_lung = log(`2025`/`2015`)/(2025-2015)) %>%
  extract(`Age group (years)`, c("from_age", "to_age"), "(.+)–(.+)", remove=FALSE) %>% #Expand to one yr age groups
  mutate(from_age = as.numeric(case_when(`Age group (years)` == "85+"  ~  "85",
                                         TRUE  ~  from_age)),
         to_age = as.numeric(case_when(`Age group (years)` =="85+"  ~  "99",
                                       TRUE  ~  to_age)),
         agediff = to_age - from_age + 1,  # this will equal 5 
         tbalc = mortality_trend_lung) %>%
  rename(agegroup = `Age group (years)`)

## Now stretch the data out using an index, to create a data frame with 1 row per year of age and create a variable for year of age. 
index <- rep(1:nrow(mortality_trends_lung_cancer_f), mortality_trends_lung_cancer_f$agediff)
mortality_trends_lung_cancer_f <- mortality_trends_lung_cancer_f[index,] %>%
  mutate(year = from_age + sequence(mortality_trends_lung_cancer_f$agediff) - 1) %>%
  mutate(sex = "female") %>% 
  select(sex, tbalc, year)

### Lung cancer males
mortality_trends_lung_cancer_m <- readxl::read_xls(mortality_trends_cancers, sheet = "Lung", range = "B6:N25") 
mortality_trends_lung_cancer_m <- mortality_trends_lung_cancer_m[-c(1), ] %>% ### Delete first row
  mutate(mortality_trend_lung = log(`2025`/`2015`)/(2025-2015)) %>%
  extract(`Age group (years)`, c("from_age", "to_age"), "(.+)–(.+)", remove=FALSE) %>% #Expand to one yr age groups
  mutate(from_age = as.numeric(case_when(`Age group (years)` == "85+"  ~  "85",
                                         TRUE  ~  from_age)),
         to_age = as.numeric(case_when(`Age group (years)` =="85+"  ~  "99",
                                       TRUE  ~  to_age)),
         agediff = to_age - from_age + 1,  # this will equal 5 
         tbalc = mortality_trend_lung) %>%
  rename(agegroup = `Age group (years)`)

## Now stretch the data out using an index, to create a data frame with 1 row per year of age and create a variable for year of age. 
index <- rep(1:nrow(mortality_trends_lung_cancer_m), mortality_trends_lung_cancer_m$agediff)
mortality_trends_lung_cancer_m <- mortality_trends_lung_cancer_m[index,] %>%
  mutate(year = from_age + sequence(mortality_trends_lung_cancer_m$agediff) - 1) %>%
  mutate(sex = "male") %>% 
  select(sex, tbalc, year)

### Colorectal cancer

### Colorectal cancer females
mortality_trends_colorectal_cancer_f <- readxl::read_xls(mortality_trends_cancers, sheet = "Colorectal", range = "B6:N44") 
mortality_trends_colorectal_cancer_f <- mortality_trends_colorectal_cancer_f[-c(1:20), ] %>% ### Delete males' observations
  mutate(mortality_trend_colorectal = log(`2025`/`2015`)/(2025-2015)) %>%
  extract(`Age group (years)`, c("from_age", "to_age"), "(.+)–(.+)", remove=FALSE) %>% #Expand to one yr age groups
  mutate(from_age = as.numeric(case_when(`Age group (years)` == "85+"  ~  "85",
                                         TRUE  ~  from_age)),
         to_age = as.numeric(case_when(`Age group (years)` =="85+"  ~  "99",
                                       TRUE  ~  to_age)),
         agediff = to_age - from_age + 1,  # this will equal 5 
         carc = mortality_trend_colorectal) %>%
  rename(agegroup = `Age group (years)`)

## Now stretch the data out using an index, to create a data frame with 1 row per year of age and create a variable for year of age. 
index <- rep(1:nrow(mortality_trends_colorectal_cancer_f), mortality_trends_colorectal_cancer_f$agediff)
mortality_trends_colorectal_cancer_f <- mortality_trends_colorectal_cancer_f[index,] %>%
  mutate(year = from_age + sequence(mortality_trends_colorectal_cancer_f$agediff) - 1) %>%
  mutate(sex = "female") %>% 
  select(sex, carc, year)

### Colorectal cancer males
mortality_trends_colorectal_cancer_m <- readxl::read_xls(mortality_trends_cancers, sheet = "Colorectal", range = "B6:N25") 
mortality_trends_colorectal_cancer_m <- mortality_trends_colorectal_cancer_m[-c(1), ] %>% ### Delete first row
  mutate(mortality_trend_colorectal = log(`2025`/`2015`)/(2025-2015)) %>%
  extract(`Age group (years)`, c("from_age", "to_age"), "(.+)–(.+)", remove=FALSE) %>% #Expand to one yr age groups
  mutate(from_age = as.numeric(case_when(`Age group (years)` == "85+"  ~  "85",
                                         TRUE  ~  from_age)),
         to_age = as.numeric(case_when(`Age group (years)` =="85+"  ~  "99",
                                       TRUE  ~  to_age)),
         agediff = to_age - from_age + 1,  # this will equal 5 
         carc = mortality_trend_colorectal) %>%
  rename(agegroup = `Age group (years)`)

## Now stretch the data out using an index, to create a data frame with 1 row per year of age and create a variable for year of age. 
index <- rep(1:nrow(mortality_trends_colorectal_cancer_m), mortality_trends_colorectal_cancer_m$agediff)
mortality_trends_colorectal_cancer_m <- mortality_trends_colorectal_cancer_m[index,] %>%
  mutate(year = from_age + sequence(mortality_trends_colorectal_cancer_m$agediff) - 1) %>%
  mutate(sex = "male") %>% 
  select(sex, carc, year)

