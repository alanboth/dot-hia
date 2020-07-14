
library(readr)
library(dplyr)
library(stringr)
library(tidyr)


gbd <-  read_csv(paste0(getwd(),"/Data/gbd/gbd_melbourne_mslt.csv")) 

# ---- chunk-1.2.1: Define paramters from data ----

disease_names_execute <- read_csv(paste0(getwd(),"/Data/Processed/disease_outcomes_lookup.csv")) %>%
  dplyr::select(GBD_name, acronym) %>%
  mutate(disease = tolower(GBD_name))

DISEASE_SHORT_NAMES <- data.frame(disease = tolower(as.character(unique(gbd$cause_name))), 
                                  sname = tolower(abbreviate(unique(gbd$cause_name, max = 2))),
                                  stringsAsFactors = F) %>%
  dplyr::mutate(is_not_dis = ifelse((str_detect(disease, "injuries") |
                                       str_detect(disease, "All causes") |
                                       str_detect(disease, "Lower respiratory infections")), 
                                    1, 0) ) %>%
  dplyr::mutate(is_not_dis = case_when(sname == "allc"  ~  2,
                                       sname == "lwri"  ~  1,
                                       ## Code for major depressive disorder (no deaths) and hypertensive heart disease (no incidence)
                                       sname == "hyhd"  ~  3,
                                       sname == "mjdd"  ~  3,
                                       TRUE  ~  is_not_dis)) %>%
  left_join(disease_names_execute, by="disease") %>%
  dplyr::mutate(acronym = ifelse(str_detect(disease, "injuries"), disease, acronym),
                acronym = word(acronym, 1),
                males = ifelse(disease %in% c("uterine cancer", "breast cancer"), 0, 1),
                females = ifelse(disease %in% "prostate cancer", 0, 1),
                sname = gsub("'", '', sname),
                acronym = ifelse(is.na(acronym), "no_pif", acronym))

### Alan, what is best? save as RDS or csv? This file is needed to run MSLT code
# saveRDS(DISEASE_SHORT_NAMES, paste0(relative_path_gbd, "DISEASE_SHORT_NAMES.rds"))
write_csv(DISEASE_SHORT_NAMES, paste0(getwd(),"/Data/Processed/disease_names.csv"))

### We may not need disease_measures_list any longer

disease_measures_list <- data.frame(measure = unique(gbd$measure_name)) %>%
  pull(measure) %>%
  as.character() %>%
  as.list()


# ---- chunk-1.2.2: Clean data ----

names(gbd) <- gsub(pattern = "_name", replacement = "", x = names(gbd))
gbd <- gbd %>%
  dplyr::select(-contains("id")) %>%
  dplyr::mutate(cause = tolower(cause))

# ---- chunk-1.2.3: Generate general inputs ----


## Calculate population numbers from data

gbdnum <- gbd %>%
  dplyr::filter(metric=="Number") %>%
  dplyr::select(measure, location, sex, age, cause, Number=val)
gbdval<- gbd %>%
  dplyr::filter(metric=="Rate") %>%
  dplyr::select(measure, location, sex, age, cause, val) %>%
  dplyr::mutate(Rate = val / 100000)
gbdpop <- gbdval %>%
  left_join(gbdnum, by=c("measure","location","sex","age","cause")) %>%
  dplyr::mutate(pop = Number / Rate) %>%
  ## filter and select pop data only
  dplyr::filter(cause == "all causes", measure == "Deaths") %>%
  dplyr::select(age, sex, pop)


## Dataframe with rates per one

gbd_rate <-  dplyr::select(gbdval, measure, sex, age, cause, Rate) %>%
  dplyr::rename(rate = Rate) %>% 
  dplyr::select(measure, sex, age, cause, rate) %>%
  left_join(gbdnum) %>%
  dplyr::rename(number = Number)

## Add age interval variable for over 15, we model adults only
gbd_rate <- gbd_rate %>% dplyr::filter(age != "Under 5" & age != "5 to 9" & age != "10 to 14")
gbd_rate $age[gbd_rate $age == "95 plus"] <- "95 to 120" 
gbd_rate $from_age <- as.numeric(sapply(gbd_rate$age,function(x)str_split(x,' to ')[[1]][1]))
gbd_rate $to_age <- as.numeric(sapply(gbd_rate$age,function(x)str_split(x,' to ')[[1]][2]))
gbd_rate$age_cat <- gbd_rate$from_age  + 2

## Generate data frames for MSLT, incidence and case fatality will be replaced with disbayes inputs (or just case fatality?)

### Create wider data frame for measure and cause combinations and change all column names and string obs to lower case

#### Alan is there any way to have the names of the variables as for example, incidence_rate_copd instead of rate_incidence_copd?
gbd_wider <- gbd_rate %>% 
  dplyr::mutate(disease = tolower(abbreviate(cause))) %>%
  dplyr::mutate(age_sex = paste(age_cat, tolower(sex), sep = "_")) %>%
  pivot_wider(id_cols = c(measure, sex, age, disease, number,rate, age_cat, age_sex), 
              values_from = c(rate, number), names_from = c(measure, disease)) %>%
  left_join(gbdpop, by = c("age", "sex")) %>% 
  `names<-`(tolower(names(.)))

gbd_wider$sex <- tolower(gbd_wider$sex)

### From here we used data as inputs for disbayes and to create 1-yr frame for mslt

# ---- chunk-1.2.4: Generate mslt inputs ----

mslt_df <- data.frame(age = rep(c(0:100), 2), sex = append(rep("male", 101), 
                                                           rep("female", 101)))
## Add population numbers (Melbourne population)
### Pop data for Melbourne
pop_melb <- read_csv("Data/Processed/population_melbourne.csv")
pop_melb$from_age <- as.numeric(sapply(pop_melb$age,function(x)str_split(x,'-')[[1]][1]))
pop_melb$to_age <- as.numeric(sapply(pop_melb$age,function(x)str_split(x,'-')[[1]][2]))
pop_melb$age_cat <- pop_melb$from_age  + 2
pop_melb$sex_age_cat <- paste(tolower(pop_melb$sex), pop_melb$age_cat, sep =  "_")

pop_melb <- pop_melb %>%  dplyr::select(sex_age_cat, population)

mslt_df$sex_age_cat <- paste(mslt_df$sex,mslt_df$age, sep = "_"  )

mslt_df <- mslt_df %>% left_join(pop_melb, by = "sex_age_cat") %>%
  dplyr::mutate(age_cat = case_when(age == 2 ~ 2,
                                    age == 7  ~ 7,
                                    age == 12  ~ 12,
                                    age == 17  ~ 17, 
                                    age == 22  ~ 22,
                                    age == 27  ~ 27,
                                    age == 32  ~ 32, 
                                    age == 37  ~ 37, 
                                    age == 42  ~ 42, 
                                    age == 47  ~ 47,
                                    age == 52  ~ 52, 
                                    age == 57  ~ 57, 
                                    age == 62  ~ 62, 
                                    age == 67  ~ 67, 
                                    age == 72  ~ 72,
                                    age == 77  ~ 77, 
                                    age == 82  ~ 82,
                                    age == 87  ~ 87,
                                    age == 92 ~ 92,
                                    age == 97 ~ 97))



### Add mortality rate all cause from Victoria (best avalable data, no data for Melbourne, average over three yrs 2016-18)

deaths_melbourne <- read_csv("Data/Processed/deaths_melbourne.csv") %>%
  mutate(sex = ifelse(sex %in% "Males", "male", "female")) %>%
  mutate(sex_age_cat = paste(tolower(sex), age, sep = "_")) %>% 
  rename(mx = rate) %>%
  dplyr::select(sex_age_cat, mx)

mslt_df <- left_join(mslt_df, deaths_melbourne)

### Interpolate rates  

gbd_df <- gbd_wider

gbd_df[is.na(gbd_df)] <- 0 
#### Disability weights

all_ylds_df <- dplyr::select(gbd_df, contains("number_ylds (years lived with disability"))

### Alan, I'm dropping otherwise issue when rowSum below

all_ylds_df <- all_ylds_df[ -c(1) ]


## Adjust all cause ylds for included diseases and injuries (exclude all cause ). From here just med 

gbd_df[["allc_ylds_adj_rate_1"]] <- (gbd_df$`number_ylds (years lived with disability)_allc`  - rowSums(dplyr::select(all_ylds_df, -`number_ylds (years lived with disability)_allc`)))/ 
  gbd_df$pop

# ------------------- DWs ---------------------------#

DISEASE_SHORT_NAMES <- mutate_all(DISEASE_SHORT_NAMES, funs(tolower))

for (d in 1:nrow(DISEASE_SHORT_NAMES)){
  gbd_df[[paste0("dw_adj_", DISEASE_SHORT_NAMES$sname[d])]] <- 
    (gbd_df[[paste0("number_ylds (years lived with disability)_", DISEASE_SHORT_NAMES$sname[d])]] /
       gbd_df[[paste0("number_prevalence_", DISEASE_SHORT_NAMES$sname[d])]]) /
    ( 1 - gbd_df[["allc_ylds_adj_rate_1"]])
}

gbd_df[mapply(is.infinite, gbd_df)] <- 0
gbd_df <- replace(gbd_df, is.na(gbd_df), 0)



## Data has to be interpolated from 5-year age groups to 1-year age groups.

## Create variable names.

for (d in 1:nrow(DISEASE_SHORT_NAMES)){
  
  if (DISEASE_SHORT_NAMES$is_not_dis[d] == 0){
    
    var_name <- paste0("dw_adj_", DISEASE_SHORT_NAMES$sname[d])
    
    mslt_df[, var_name] <- 1
    
  }
}

## Interpolate dw rates 

index <- 1  

for (d in 1:nrow(DISEASE_SHORT_NAMES)){
  for(sex_index in c("male", "female")) {
    for (var in c('dw_adj')){#, 'deaths_rate', 'ylds (years lived with disability)_rate')){
      
      if (DISEASE_SHORT_NAMES$is_not_dis[d] == 0) {
        
        
        var_name <- paste0(var, '_', DISEASE_SHORT_NAMES$sname[d])
        
        data <- dplyr::filter(gbd_df, sex == sex_index) %>% dplyr::select(age, sex, age_cat, starts_with(var_name))
        
        
        x <- data$age_cat
        y <- log(data[[var_name]])
        
        InterFunc <- stats::splinefun(x, y, method = "monoH.FC", ties = mean)
        
        interpolated <- as.data.frame(InterFunc(seq(0, 100, 1)))
        age <- seq(0, 100, by = 1)
        interpolated <- cbind(interpolated, age)
        interpolated[,1] <- exp(interpolated[,1])
        ## Add column with sex to create age_sex category to then merge with input_life table
        interpolated$sex <- paste(sex_index)
        interpolated$sex_age_cat <- paste(interpolated$sex, interpolated$age, sep = "_")
        ## Change name of column death to mx and ylds to pyld_rate to then merge
        ## with input_life table
        colnames(interpolated)[1] <- var_name
        
        interpolated[IsNanDataFrame(interpolated)] <- 0
        
        interpolated[IsInfDataFrame(interpolated)] <- 0
        
        mslt_df[mslt_df$sex_age_cat == interpolated$sex_age_cat 
                & mslt_df$sex == sex_index, ][[var_name]] <- interpolated[[var_name]]
        
        index <- index + 1
      }
    }
  }
}


## Interpolate mortality and pylds (all cause mortality is from Melbourne data)

### Create variable names

for (d in 1:nrow(DISEASE_SHORT_NAMES)){
  
  var_name1 <- paste0("rate_deaths", "_", DISEASE_SHORT_NAMES$sname[d])
  
  var_name2 <- paste0("rate_ylds (years lived with disability)", "_", DISEASE_SHORT_NAMES$sname[d])
  
  mslt_df[, var_name1] <- 1
  mslt_df[, var_name2] <- 1
}

### Deaths

for (d in 1:nrow(DISEASE_SHORT_NAMES)){
  for(sex_index in c("male", "female"))  {
    for (var in c('rate_deaths')) {
      
      var_name1 <- paste0(var, '_', DISEASE_SHORT_NAMES$sname[d])
      
      data <- dplyr::filter(gbd_df, sex == sex_index) %>% dplyr::select(age, sex, age_cat, starts_with(var_name1))
      
      x <- data$age_cat
      y <- log(data[[var_name1]])
      
      InterFunc <- stats::splinefun(x, y, method = "monoH.FC", ties = mean)
      
      interpolated <- as.data.frame(InterFunc(seq(0, 100, 1)))
      age <- seq(0, 100, by = 1)
      interpolated <- cbind(interpolated, age)
      interpolated[,1] <- exp(interpolated[,1])
      ## Add column with sex to create age_sex category to then merge with input_life table
      interpolated$sex <- paste(sex_index)
      interpolated$sex_age_cat <- paste(interpolated$sex, interpolated$age, sep = "_")
      ## Change name of column death to mx and ylds to pyld_rate to then merge
      ## with input_life table
      colnames(interpolated)[1] <- var_name1
      
      interpolated[IsNanDataFrame(interpolated)] <- 0
      
      interpolated[IsInfDataFrame(interpolated)] <- 0
      
      mslt_df[mslt_df$sex_age_cat == interpolated$sex_age_cat 
              & mslt_df$sex == sex_index, ][[var_name1]] <- interpolated[[var_name1]]
      
    }
  }
}


### YLDs

for (d in 1:nrow(DISEASE_SHORT_NAMES)){
  for(sex_index in c("male", "female"))  {
    for (var in c("rate_ylds (years lived with disability)")){
      
      var_name2 <- paste0(var, '_', DISEASE_SHORT_NAMES$sname[d])
      
      data <- dplyr::filter(gbd_df, sex == sex_index) %>% dplyr::select(age, sex, age_cat, starts_with(var_name2))
      
      # browser() Data input and x and y are fine, different for all, however, interpolated values are all the same. 
      
      x <- data$age_cat
      y <- log(data[[var_name2]])
      
      interpolated <- as.data.frame(InterFunc(seq(0, 100, 1)))
      
      # browser()
      
      age <- seq(0, 100, by = 1)
      interpolated <- cbind(interpolated, age)
      interpolated[,1] <- exp(interpolated[,1])
      
      # browser()
      
      ## Add column with sex to create age_sex category to then merge with input_life table
      interpolated$sex <- paste(sex_index)
      interpolated$sex_age_cat <- paste(interpolated$sex, interpolated$age, sep = "_")
      ## Change name of column death to mx and ylds to pyld_rate to then merge
      ## with input_life table
      colnames(interpolated)[1] <- var_name2
      
      # browser()
      
      interpolated[IsNanDataFrame(interpolated)] <- 0
      
      interpolated[IsInfDataFrame(interpolated)] <- 0
      
      mslt_df[mslt_df$sex_age_cat == interpolated$sex_age_cat 
              & mslt_df$sex == sex_index, ][[var_name2]] <- interpolated[[var_name2]]
      
      
    }
  }
}


names(mslt_df)[names(mslt_df) == "rate_ylds (years lived with disability)_allc"] <- "pyld_rate"
mslt_df[is.na(mslt_df)] <- 0 
