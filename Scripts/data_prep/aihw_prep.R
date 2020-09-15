
# Generate incidence and mortality inputs from AIHW data for dismod/disbayes processing
### I removed function, this is only run once to generate inputs for dismod, which is an external process

suppressPackageStartupMessages(library(readxl)) # for reading excel files
suppressPackageStartupMessages(library(stringr)) # for splitting strings
suppressPackageStartupMessages(library(dplyr)) # for manipulating data
suppressPackageStartupMessages(library(tidyr)) # for pivoting data

calculateInputsDismodAIHW <- function(incidence_AIHW, mortality_AIHW, population_deaths_location) {
# incidence_AIHW="Data/original/aihw/cancer_incidence_AIHW_with_projections.xlsx"
# mortality_AIHW="Data/original/aihw/cancer_mortality_AIHW_with_projections.xlsx"
# population_deaths_location="Data/original/abs/population_deaths.csv"


### Incidence
  incidence_AIHW <- readxl::read_xlsx(incidence_AIHW, sheet = 2, range = cell_rows(6:137937)) %>%
  dplyr::rename(rate = `Age-specific rate\r\n(per 100,000)`) %>%
  dplyr::rename(disease = `Cancer group/site`) %>%
  dplyr::mutate(Count = as.numeric(Count)) %>%
  dplyr::mutate(rate = as.numeric(rate)) %>%
  dplyr::mutate(rate = rate/100000) %>%
  dplyr::filter(disease %in% c("Breast cancer", "Colon cancer", "Lung cancer", "Uterine cancer")) %>%
  dplyr::mutate(disease = case_when(disease == "Breast cancer" ~ "brsc", 
                             disease =="Colon cancer" ~ "carc", 
                             disease =="Lung cancer" ~ "tbalc", 
                             disease =="Uterine cancer" ~ "utrc")) %>%
    dplyr::filter(Year %in% c(2010:2020)) %>%
    dplyr::filter(`Age group (years)` %in%  c("15–19", "20–24", "25–29", "30–34", "35–39", "40–44", "45–49",
                                       "50–54", "55–59", "60–64", "65–69", "70–74", "75–79", "80–84", "85–89","90+")) %>% 
    dplyr::rename(age = `Age group (years)`) %>%
    dplyr::mutate(age=replace(age, age=="90+", 90L)) %>%
                                        rowwise() %>%
    dplyr::mutate(from_age = as.numeric(str_split(age, "–")[[1]][1])) %>%
    dplyr::mutate(age_cat = from_age + 2)

  ### create incidence for group 97 that splits the value on 92
incidence_97 <- dplyr::filter(incidence_AIHW, age_cat == 92) %>% 
  dplyr::mutate(age_cat=replace(age_cat, age_cat== 92, 97L)) %>% 
  dplyr::mutate(Count = Count/2) # split counts by 2 for age_cat 92

 ### cbind to incidence_AIHW, keep variables of interest and pivot wider
incidence_AIHW <- incidence_AIHW %>%
  dplyr::mutate(Count=replace(Count, age_cat== 92, Count/2)) %>% ## divide age 92 counts by 2
  rbind(incidence_97) %>%
  dplyr::select(Sex, Year, disease, age_cat, Count, rate, `ICD10 codes`) %>%
  dplyr::rename(number = Count) %>%
  `names<-`(tolower(names(.))) %>%
  pivot_wider(id_cols = c(sex, year, disease, age_cat, number, rate, `icd10 codes`), ### ALAN: Pivot wider is doing something funny with age_cat
              values_from = c(rate, number), names_from = c(disease),
              names_prefix = "incidence_aihw", 
              names_glue = "{names_prefix}_{.value}_{disease}") %>%
              dplyr::filter(sex != "Persons") %>%
  dplyr::mutate(sex = ifelse(sex=="Males", "male", "female")) %>%
  dplyr::mutate(sex_age_cat = paste(tolower(sex), age_cat, sep = "_")) 



### Year 2017 only for dismod and compare with GBD.
### Alan, the pivot wider generates NAs, any way to delete them?

incidence_AIHW_dismod <- filter(incidence_AIHW, year == 2017) %>%
  group_by(sex_age_cat) %>%                                 ## this step is to remove all NAs
  summarise_all(funs(if(is.numeric(.)) sum(., na.rm = TRUE) else first(.)))

# write.csv(incidence_AIHW_dismod, "Data/dismod/incidence_AIHW_dismod.csv", row.names=F, quote=T)


### Mortality 

mortality_AIHW <- readxl::read_xlsx(mortality_AIHW, sheet = 2, range = cell_rows(6:137937)) %>%
  dplyr::rename(rate = `Age-specific rate\r\n(per 100,000)`) %>%
  dplyr::rename(disease = `Cancer group/site`) %>%
  dplyr::mutate(Count = as.numeric(Count)) %>%
  dplyr::mutate(rate = as.numeric(rate)) %>%
  dplyr::mutate(rate = rate/100000) %>%
  dplyr::filter(disease %in% c("Breast cancer", "Colon cancer", "Lung cancer", "Uterine cancer")) %>%
  dplyr::mutate(disease = case_when(disease == "Breast cancer" ~ "brsc", 
                             disease =="Colon cancer" ~ "carc", 
                             disease =="Lung cancer" ~ "tbalc", 
                             disease =="Uterine cancer" ~ "utrc")) %>%
  dplyr::filter(Year %in% c(2010:2020)) %>%
  dplyr::filter(`Age group (years)` %in%  c("15–19", "20–24", "25–29", "30–34", "35–39", "40–44", "45–49",
                                     "50–54", "55–59", "60–64", "65–69", "70–74", "75–79", "80–84", "85–89","90+")) %>% 
  dplyr::rename(age = `Age group (years)`) %>%
  dplyr::mutate(age=replace(age, age=="90+", 90L)) %>%
  rowwise() %>%
  dplyr::mutate(from_age = as.numeric(str_split(age, "–")[[1]][1])) %>%
  dplyr::mutate(age_cat = from_age + 2)

### create mortality for group 97 that splits the value on 92
mortality_97 <- filter(mortality_AIHW, age_cat == 92) %>% 
  dplyr::mutate(age_cat=replace(age_cat, age_cat== 92, 97L)) %>% 
  dplyr::mutate(Count = Count/2) # split counts by 2 for age_cat 92

### cbind to mortality_AIHW, keep variables of interest and pivot wider
mortality_AIHW <- mortality_AIHW %>%
  dplyr::mutate(Count=replace(Count, age_cat== 92, Count/2)) %>%
  rbind(mortality_97) %>%
  dplyr::select(Sex, Year, disease, age_cat, Count, rate, `ICD10 codes`) %>%
  dplyr::rename(number = Count) %>%
  `names<-`(tolower(names(.))) %>%
  pivot_wider(id_cols = c(sex, year, disease, age_cat, number, rate, `icd10 codes`), 
              values_from = c(rate, number), names_from = c(disease),
              names_prefix = "mortality_aihw", 
              names_glue = "{names_prefix}_{.value}_{disease}") %>%
  dplyr::filter(sex != "Persons") %>%
  dplyr::mutate(sex = ifelse(sex=="Males", "male", "female")) %>%
  dplyr::mutate(sex_age_cat = paste(tolower(sex), age_cat, sep = "_"))


mortality_AIHW_dismod <- dplyr::filter(mortality_AIHW, year == 2017) %>%
  group_by(sex_age_cat) %>%                                 ## this step is to remove all NAs
  summarise_all(funs(if(is.numeric(.)) sum(., na.rm = TRUE) else first(.)))

# write.csv(mortality_AIHW_dismod, "Data/dismod/mortality_AIHW_dismod.csv", row.names=F, quote=T)



## Population and deaths data for Australia for Dismod collection for cancers AIHW modelling

population_and_deaths <- read.csv(population_deaths_location,fileEncoding="UTF-8-BOM")

population_and_deaths <- population_and_deaths %>%
  ### Ages to numeric. Need to convert to a character first
  dplyr::mutate(Age=as.numeric(as.character(Age)))


### population numbers and deaths numbers

population_and_deaths <- population_and_deaths %>% 
  dplyr::filter(States.and.Territories == "Australia",
                Measure %in% c("Population", "Deaths"),
                Age %in% c(0:100),
                Time %in% c(2017), 
                Sex != "Persons") %>%
                pivot_wider(id_cols = c(Measure, Sex, Age, Value), 
                values_from = c(Value), names_from = c(Measure)) %>%
                mutate(rate=Deaths/Population) %>% 
                arrange(Sex, Age)

# write.csv(population_and_deaths , "Data/dismod/mortality_pop_AIHW.csv", row.names=F, quote=T)
# 
return(list(incidence_AIHW_dismod, mortality_AIHW_dismod, population_and_deaths))
}