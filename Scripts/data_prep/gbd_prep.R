## Create R Markdown for data preparation

# ---- chunk-intro ----
### Packages to run this code
rm (list = ls())
library(readr)
library(dplyr)
library(stringr)

source("Scripts/functions_mslt.R")
# ---- chunk-1: Data preparation ----
### We need to prepare data for ITHIMR health outputs and MSLT

# GBD file needs to have the following columns: 
# age (=label, e.g. 15-49)
# sex (=male or female)
# measure
# cause (GBD_DATA$cause matches DISEASE_INVENTORY$GBD_name)
# metric
# burden
# min_age (=number, e.g. 15)
# max_age (=number, e.g. 49)

# ---- chunk-1.1: Data preparation-ITHIMR ----

### Update population data with Melbourne populatoin

GBD_DATA <- read_csv(paste0(getwd(),"/Data/gbd/gbd_melbourne_ithimr.csv")) 
POPULATION <- read_csv(paste0(getwd(),"/Data/Processed/population_melbourne_abs.csv"))

### Tidy GBD_DATA to match ITHIMR
GBD_DATA <- GBD_DATA %>% dplyr::filter(age != "Under 5" & age != "5 to 9" & age != "10 to 14")
GBD_DATA$age[GBD_DATA$age == "95 plus"] <- "95 to 120" 
GBD_DATA$min_age <- as.numeric(sapply(GBD_DATA$age,function(x)str_split(x,' to ')[[1]][1]))
GBD_DATA$max_age <- as.numeric(sapply(GBD_DATA$age,function(x)str_split(x,' to ')[[1]][2]))
GBD_DATA$sex <- tolower(GBD_DATA$sex)
GBD_DATA$burden <- GBD_DATA$val
GBD_DATA$cause_name <- GBD_DATA$cause
GBD_DATA$age_name <- GBD_DATA$age

### Tidy POPULATION to match ITHIMR code

POPULATION$age[POPULATION$age == "95 plus"] <- "95-120"    
POPULATION$age <- gsub("-", " to ", POPULATION$age)

### Create variable to left join population to GBD file
POPULATION$sex_age <- tolower(paste(POPULATION$sex, POPULATION$age, sep = "_"))




GBD_DATA$sex_age <- tolower(paste(GBD_DATA$sex, GBD_DATA$age, sep = "_"))
GBD_DATA <- left_join(GBD_DATA, dplyr::select(POPULATION, c('sex_age', "population")), by = "sex_age")

### order by cause, otherwise issue with function ithimrsetup
GBD_DATA <- GBD_DATA %>% dplyr::arrange(sex, age, measure, cause)


### Change format age to match ITHIMR code
POPULATION$age <- gsub(" to ", "-", POPULATION$age)



### Drop sex_age columns
POPULATION <- POPULATION %>% dplyr::select( -sex_age)
GBD_DATA <- GBD_DATA %>% dplyr::select( -sex_age)

### Save files
write_csv(GBD_DATA, paste0(getwd(), "/Data/Processed/gbd_melbourne.csv"))

write_csv(POPULATION, paste0(getwd(), "/Data/Processed/population_melbourne.csv"))

# ---- chunk-1.2: Data preparatoin MSLT ----

data_extracted <-  read_csv(paste0(getwd(),"/Data/gbd/gbd_melbourne_mslt.csv")) 

# ---- chunk-1.2: Define parameters from data ----

## Define measure (e.g. deaths) and cause parameters (e.g. all causes, breast cancer) (this is to avoid hard coding the parameters)

## Min Length is not changing anything, how can we make it characters in the first place, rather than having to ocnvert below before running RunLocDF?


### Diseases names

DISEASE_SHORT_NAMES <- data.frame(disease = tolower(as.character(unique(data_extracted$cause_name))), 
                                  sname = tolower(abbreviate(unique(data_extracted$cause_name, max = 2))),
                                  stringsAsFactors = F)

DISEASE_SHORT_NAMES <- DISEASE_SHORT_NAMES %>% mutate(is_not_dis = ifelse((str_detect(disease, "injuries") |
                                                                             str_detect(disease, "all causes") |
                                                                             str_detect(disease, "lower respiratory infections")), 
                                                                          1, 0) )

DISEASE_SHORT_NAMES[DISEASE_SHORT_NAMES$sname == "allc", "is_not_dis"] <- 2

DISEASE_SHORT_NAMES[DISEASE_SHORT_NAMES$sname == "lwri", "is_not_dis"] <- 1


### These diseases do not have prevalence or deaths. For now this, better to do something that skips them in the codes. 
### Not used yet. 
DISEASE_SHORT_NAMES[DISEASE_SHORT_NAMES$sname == "hyhd", "is_not_dis"] <- 3
DISEASE_SHORT_NAMES[DISEASE_SHORT_NAMES$sname == "mjdd", "is_not_dis"] <- 3

## Add males and females only diseases

DISEASE_SHORT_NAMES$males <- ifelse(DISEASE_SHORT_NAMES$disease %in% c("breast cancer", "uterine cancer"), 0, 1)

DISEASE_SHORT_NAMES$females <- 1

DISEASE_SHORT_NAMES$sname <- gsub("'", '', DISEASE_SHORT_NAMES$sname)

## Add column to match names diseases outcome. This is a place holder.

disease_names_execute <- read_csv(paste0(getwd(), "/Data/Processed/disease_outcomes_lookup.csv"))
disease_names_execute <- disease_names_execute[1:2]
disease_names_execute$disease <- tolower(disease_names_execute$GBD_name)

## Add column to indicate which diseases have calculated pifs

DISEASE_SHORT_NAMES <- left_join(DISEASE_SHORT_NAMES, disease_names_execute, by = "disease")
DISEASE_SHORT_NAMES$acronym[is.na(DISEASE_SHORT_NAMES$acronym)] <- "no_pif"

write_csv(DISEASE_SHORT_NAMES, paste0(getwd(),"/Data/Processed/disease_names.csv"))


disease_measures_list <- data.frame(measure = unique(data_extracted$measure_name)) %>%
  pull(measure) %>%
  as.character() %>%
  as.list()


# ---- chunk-1.3: Clean data ----

names(data_extracted) = gsub(pattern = "_name", replacement = "", x = names(data_extracted))


data_extracted$cause <- tolower(data_extracted$cause) 

# ---- chunk-1.4: Sort data ----

gbd_data_processed <- RunLocDf(data_extracted)

# ---- chunk 1.5 Get Disbayes output ----
### USE uk DATA, NEED TO GENERATE DATA FOR AUSTRALIA



place_holder_aus <- load(file = "C:\\Metahit\\mh-mslt\\data\\city regions\\Output disbayes\\uk_smoothed_res.rda")

## create column one with outcome and year
place_holder_aus <- cbind(
  mes=rownames(place_holder_aus), place_holder_aus)

### Separate avoce in outcome and year
place_holder_aus <- cbind(place_holder_aus, (str_split_fixed(place_holder_aus$
                                                                           mes, fixed('['), 2)))

place_holder_aus <- place_holder_aus[ (place_holder_aus$`1` %in% c("inc", "cf", "prev")), ]
place_holder_aus$`1` <- as.character(place_holder_aus$`1`)
place_holder_aus$`2` <- as.character(place_holder_aus$`2`)
place_holder_aus$`2` <- gsub("].*", "",place_holder_aus$`2`)


## Rename columns
names(place_holder_aus)[names(place_holder_aus) == "1"] <- "rates"
names(place_holder_aus)[names(place_holder_aus) == "2"] <- "year"

## Rename string values inc to incidence, cf to case fatality and prev to prevalence

place_holder_aus <- place_holder_aus %>%
  mutate(rates = str_replace(rates, "inc", "incidence"))  %>%
  mutate(rates = str_replace(rates, "cf", "case_fatality"))  %>%
  mutate(rates = str_replace(rates, "prev", "prevalence"))

## Move to columns for data for case_fatality, incidence and prevelence

place_holder_aus$disease_rate <- paste(place_holder_aus$rates, place_holder_aus$disease, sep = "_")
place_holder_aus2 <- place_holder_aus %>% pivot_wider(id_cols = c(area, gender, model, year), names_from = disease_rate, values_from = c(med, lower95, upper95))
names(place_holder_aus2) = gsub(pattern = "med_", replacement = "", x = names(place_holder_aus2))

disbayes_output <- place_holder_aus2 %>%
  dplyr::rename(sex = gender) %>%
  mutate_if(is.factor, as.character)
disbayes_output$year <- disbayes_output$year %>% as.numeric(disbayes_output$year)
## Change year to match mslt dataframe (0 to 100 years)
disbayes_output$year[1:101] <- 0:100

disbayes_output$sex_age_area_cat <- paste(disbayes_output$sex,disbayes_output$year, disbayes_output$area, sep = "_"  )


# ---- chunk 1.7 ---- HERE WE SHOULD USE POPULATION FOR MELBOURNE AND DEATH RATES FOR VICTORIA CALCULATES IN death_rate_prep

areas <- unique(disbayes_output$area)
i_sex <- c('male', 'female')

mslt_df <- as.data.frame(NULL)

mslt_df_list <- list()

index <- 1

for (a in areas) {
  
  ### selected data here should be gbd_data with all data, see how the code works with it 
  
  data_1 <-  dplyr::filter(gbd_data, area == a)
  data_2 <- dplyr::filter(disbayes_output, area == a)
  
  mslt_df_list[[index]] <- GenMSLTDF(data_1, data_2)
  mslt_df_list[[index]]<- replace(mslt_df_list[[index]], is.na(mslt_df_list[[index]]), 0)
  
  
  ### Change names to match with Rob's injury code
  
  names(mslt_df_list[[index]])[names(mslt_df_list[[index]]) == "deaths_rate_pdri"] <- "deaths_rate_pedestrian"
  names(mslt_df_list[[index]])[names(mslt_df_list[[index]]) == "ylds (years lived with disability)_rate_pdri"] <- "ylds_rate_pedestrian"
  
  names(mslt_df_list[[index]])[names(mslt_df_list[[index]]) == "deaths_rate_cyri"] <- "deaths_rate_cyclist"
  names(mslt_df_list[[index]])[names(mslt_df_list[[index]]) == "ylds (years lived with disability)_rate_cyri"] <- "ylds_rate_cyclist"
  
  names(mslt_df_list[[index]])[names(mslt_df_list[[index]]) == "deaths_rate_mtri"] <- "deaths_rate_motorcyclist"
  names(mslt_df_list[[index]])[names(mslt_df_list[[index]]) == "ylds (years lived with disability)_rate_mtri"] <- "ylds_rate_motorcyclist"
  
  names(mslt_df_list[[index]])[names(mslt_df_list[[index]]) == "deaths_rate_mvri"] <- "deaths_rate_motor"
  names(mslt_df_list[[index]])[names(mslt_df_list[[index]]) == "ylds (years lived with disability)_rate_mvri"] <- "ylds_rate_motor"
  
  
  names(mslt_df_list[[index]])[names(mslt_df_list[[index]]) == "deaths_rate_otri"] <- "deaths_rate_other"
  names(mslt_df_list[[index]])[names(mslt_df_list[[index]]) == "ylds (years lived with disability)_rate_otri"] <- "ylds_rate_other"
  
  names(mslt_df_list[[index]])[names(mslt_df_list[[index]]) == "deaths_rate_lwri"] <- "deaths_rate_lri"
  names(mslt_df_list[[index]])[names(mslt_df_list[[index]]) == "ylds (years lived with disability)_rate_lwri"] <- "ylds_rate_lri"
  
  mslt_df_list[[index]]$sex <- as.character(mslt_df_list[[index]]$sex)
  
  index <- index + 1
  
}
