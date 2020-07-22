
### Generate incidence and mortality inputs from AIHW data for dismod/disbayes processing

suppressPackageStartupMessages(library(readxl)) # for reading excel files
suppressPackageStartupMessages(library(stringr)) # for splitting strings
suppressPackageStartupMessages(library(dplyr)) # for manipulating data
suppressPackageStartupMessages(library(tidyr)) # for pivoting data


incidence_AIHW="Data/aihw/cancer_incidence_AIHW_with_projections.xlsx"
mortality_AIHW="Data/aihw/cancer_mortality_AIHW_with_projections.xlsx"

### Incidence
  incidence_AIHW <- readxl::read_xlsx(incidence_AIHW, sheet = 2, range = cell_rows(6:137937)) %>%
  rename(rate = `Age-specific rate\r\n(per 100,000)`) %>%
  rename(disease = `Cancer group/site`) %>%
  mutate(Count = as.numeric(Count)) %>%
  mutate(rate = as.numeric(rate)) %>%
  mutate(rate = rate/100000) %>%
  filter(disease %in% c("Breast cancer", "Colon cancer", "Lung cancer", "Uterine cancer")) %>%
  mutate(disease = case_when(disease == "Breast cancer" ~ "brsc", 
                             disease =="Colon cancer" ~ "carc", 
                             disease =="Lung cancer" ~ "tbalc", 
                             disease =="Uterine cancer" ~ "utrc")) %>%
  filter(Year %in% c(2010:2020)) %>%
    filter(`Age group (years)` %in%  c("15–19", "20–24", "25–29", "30–34", "35–39", "40–44", "45–49",
                                       "50–54", "55–59", "60–64", "65–69", "70–74", "75–79", "80–84", "85–89","90+")) %>% 
                                        rename(age = `Age group (years)`) %>%
                                        mutate(age=replace(age, age=="90+", 90L)) %>%
                                        rowwise() %>%
                                        mutate(from_age = as.numeric(str_split(age, "–")[[1]][1])) %>%
                                        mutate(age_cat = from_age + 2)

  ### create incidence for group 97 that splits the value on 92
incidence_97 <- filter(incidence_AIHW, age_cat == 92) %>% 
  mutate(age_cat=replace(age_cat, age_cat== 92, 97L)) %>% 
  mutate(Count = Count/2) # split counts by 2 for age_cat 92

 ### cbind to incidence_AIHW, keep variables of interest and pivot wider
incidence_AIHW <- incidence_AIHW %>%
  mutate(Count = Count/2) %>%
  rbind(incidence_97) %>%
  select(Sex, Year, disease, age_cat, Count, rate, `ICD10 codes`) %>%
  rename(number = Count) %>%
  `names<-`(tolower(names(.))) %>%
  pivot_wider(id_cols = c(sex, year, disease, age_cat, number, rate, `icd10 codes`), 
              values_from = c(rate, number), names_from = c(disease),
              names_prefix = "incidence_aihw", 
              names_glue = "{names_prefix}_{.value}_{disease}") %>%
              filter(sex != "Persons") %>%
              mutate(sex = ifelse(sex=="Males", "male", "female")) %>%
              mutate(sex_age_cat = paste(tolower(sex), age_cat, sep = "_"))

### Year 2017 only for dismod and compare with GBD.
### Alan, the pivot wider generates NAs, any way to delete them?

incidence_AIHW_dismod <- filter(incidence_AIHW, year == 2017)


### Mortality 

mortality_AIHW <- readxl::read_xlsx(mortality_AIHW, sheet = 2, range = cell_rows(6:137937)) %>%
  rename(rate = `Age-specific rate\r\n(per 100,000)`) %>%
  rename(disease = `Cancer group/site`) %>%
  mutate(Count = as.numeric(Count)) %>%
  mutate(rate = as.numeric(rate)) %>%
  mutate(rate = rate/100000) %>%
  filter(disease %in% c("Breast cancer", "Colon cancer", "Lung cancer", "Uterine cancer")) %>%
  mutate(disease = case_when(disease == "Breast cancer" ~ "brsc", 
                             disease =="Colon cancer" ~ "carc", 
                             disease =="Lung cancer" ~ "tbalc", 
                             disease =="Uterine cancer" ~ "utrc")) %>%
  filter(Year %in% c(2010:2020)) %>%
  filter(`Age group (years)` %in%  c("15–19", "20–24", "25–29", "30–34", "35–39", "40–44", "45–49",
                                     "50–54", "55–59", "60–64", "65–69", "70–74", "75–79", "80–84", "85–89","90+")) %>% 
  rename(age = `Age group (years)`) %>%
  mutate(age=replace(age, age=="90+", 90L)) %>%
  rowwise() %>%
  mutate(from_age = as.numeric(str_split(age, "–")[[1]][1])) %>%
  mutate(age_cat = from_age + 2)

### create mortality for group 97 that splits the value on 92
mortality_97 <- filter(mortality_AIHW, age_cat == 92) %>% 
  mutate(age_cat=replace(age_cat, age_cat== 92, 97L)) %>% 
  mutate(Count = Count/2) # split counts by 2 for age_cat 92

### cbind to mortality_AIHW, keep variables of interest and pivot wider
mortality_AIHW <- mortality_AIHW %>%
  mutate(Count = Count/2) %>%
  rbind(mortality_97) %>%
  select(Sex, Year, disease, age_cat, Count, rate, `ICD10 codes`) %>%
  rename(number = Count) %>%
  `names<-`(tolower(names(.))) %>%
  pivot_wider(id_cols = c(sex, year, disease, age_cat, number, rate, `icd10 codes`), 
              values_from = c(rate, number), names_from = c(disease),
              names_prefix = "mortality_aihw", 
              names_glue = "{names_prefix}_{.value}_{disease}") %>%
  filter(sex != "Persons") %>%
  mutate(sex = ifelse(sex=="Males", "male", "female")) %>%
  mutate(sex_age_cat = paste(tolower(sex), age_cat, sep = "_"))

### Year 2017 only for dismod and compare with GBD.
### Alan, the pivot wider generates NAs, any way to delete them?

mortality_AIHW_dismod <- filter(mortality_AIHW, year == 2017)
