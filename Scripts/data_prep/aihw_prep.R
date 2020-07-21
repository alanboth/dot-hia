

suppressPackageStartupMessages(library(readxl)) # for reading excel files
suppressPackageStartupMessages(library(stringr)) # for splitting strings
suppressPackageStartupMessages(library(dplyr)) # for manipulating data
suppressPackageStartupMessages(library(tidyr)) # for pivoting data
### To do: 1) inputs compare with GBD, 2) inputs for dismod II/disbayes for cancers, 3) trends case fatality and 
### incidence
### START WITH CANCERS


incidence_AIHW="Data/aihw/cancer_incidence_AIHW_with_projections.xlsx"
mortality_AIHW="Data/aihw/cancer_mortality_AIHW_with_projections.xlsx"


  incidence_AIHW <- readxl::read_xlsx(incidence_AIHW, sheet = 2, range = cell_rows(6:137937)) %>%
  rename(rates = `Age-specific rate\r\n(per 100,000)`) %>%
  rename(disease = `Cancer group/site`) %>%
  mutate(Count = as.numeric(Count)) %>%
  mutate(rates = as.numeric(rates)) %>%
  mutate(rates = rates/100000) %>%
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

 ### cbind to incidence_AIHW and keep variables of interest
incidence_AIHW <- incidence_AIHW %>%
  mutate(Count = Count/2) %>%
  rbind(incidence_97) %>%
  select(Sex, Year, disease, age_cat, Count, rates, `ICD10 codes`)

### UP TO HERE
### Pivot to wider, same format as gbd_wider

                            
  ### select age grups and ammend to match gbd
  ### rename disease to match gbd
  ### pivot wider to match gbd inputs generated are for dismod/disbayes

  

### To do: create gbd_wider like data for cancers and compare with gbd_wider

mortality_AIHW <- readxl::read_xlsx(mortality_AIHW, sheet = 2, range = cell_rows(6:137937)) %>%
  filter(`Cancer group/site` %in% c("Breast cancer", "Colon cancer", "Lung cancer", "Uterine cancer"))
### To do: create gbd_wider like data for cancers and compare with gbd_wider
