suppressPackageStartupMessages(library(stringr)) # for splitting strings
suppressPackageStartupMessages(library(dplyr)) # for manipulating data
suppressPackageStartupMessages(library(tidyr)) # for pivoting data


calculateDiseaseNames <- function(gbd_location,disease_outcomes_location) {
  # gbd_location="Data/gbd/gbd_melbourne_mslt.csv"
  # disease_outcomes_location="Data/Processed/disease_outcomes_lookup.csv"
  
  gbd <- read.csv(gbd_location, as.is=T, fileEncoding="UTF-8-BOM") 
  
  # ---- chunk-1.2.1: Define parameters from data ----
  
  disease_names_execute <- read.csv(disease_outcomes_location,
                                    as.is=T,fileEncoding="UTF-8-BOM") %>%
    select(GBD_name, acronym) %>%
    mutate(disease = tolower(GBD_name))
  
  DISEASE_SHORT_NAMES <- data.frame(disease = tolower(as.character(unique(gbd$cause_name))), 
                                    sname = tolower(abbreviate(unique(gbd$cause_name, max = 2))),
                                    stringsAsFactors = F) %>%
    mutate(is_not_dis = ifelse(( grepl("injuries", disease) | # grepl determines if the disease string contains 'injuries'
                                   grepl("all causes", disease) |
                                   grepl("lower respiratory infections", disease)), 
                               1, 0) ) %>%
    mutate(is_not_dis = case_when(sname == "allc"  ~  2,
                                  sname == "lwri"  ~  1,
                                  ## Code for major depressive disorder (no deaths) and hypertensive heart disease (no incidence)
                                  sname == "hyhd"  ~  3,
                                  sname == "mjdd"  ~  3,
                                  TRUE  ~  is_not_dis)) %>%
    left_join(disease_names_execute, by="disease") %>%
    mutate(
      # commented out these two lines as they wouldn't allow the non-diseases to have 'no_pif' as their acronym
      # acronym = ifelse(grepl("injuries", disease), disease, acronym),
      # acronym = word(acronym, 1),
      males = ifelse(disease %in% c("uterine cancer", "breast cancer"), 0, 1),
      females = ifelse(disease %in% "prostate cancer", 0, 1),
      sname = gsub("'", '', sname),
      # this part won't work since all injuries have had their acronym set to their disease
      acronym = ifelse(is.na(acronym), sapply(strsplit(disease, " "), head, 1), acronym))  ###BZ: added column to acronyms to match mslt code
      
    
  
      
  
  return(DISEASE_SHORT_NAMES)
  ### Alan, what is best? save as RDS or csv? This file is needed to run MSLT code
  ### AB: Both are fine, but csv can be read by anything. I only use RDS for large R objects that don't easily convert into a text file.
  # saveRDS(DISEASE_SHORT_NAMES, paste0(relative_path_gbd, "DISEASE_SHORT_NAMES.rds"))
  # write.csv(DISEASE_SHORT_NAMES, "Data/Processed/disease_names.csv", row.names=F, quote=T)
}
### We may not need disease_measures_list any longer

# disease_measures_list <- data.frame(measure = unique(gbd$measure_name)) %>%
#   pull(measure) %>%
#   as.character() %>%
#   as.list()


# ---- chunk-1.2.2: Clean data ----

calculateGBDwider <- function(gbd_location) {
 # gbd_location="Data/gbd/gbd_melbourne_mslt.csv"
  
  gbd <-  read.csv(gbd_location, as.is=T, fileEncoding="UTF-8-BOM") 
  
  # remove '_name' from column names
  names(gbd) <- gsub(pattern = "_name", replacement = "", x = names(gbd))
  gbd <- gbd %>%
    select(-contains("id")) %>%
    mutate(cause = tolower(cause))
  
  # ---- chunk-1.2.3: Generate general inputs ----
  
  gbd_tmp <- gbd %>%
    select(-upper,-lower) %>%
    mutate(metric=tolower(metric)) %>%
    # this name is too long
    mutate(measure = ifelse(measure=='YLDs (Years Lived with Disability)','ylds',measure)) %>%
    mutate(age = ifelse(age=="95 plus", "95 to 120", age)) %>%
    pivot_wider(names_from="metric", values_from="val") %>%
    mutate(rate = rate / 100000) %>%
    mutate(pop = number / rate) %>%
    # only want pop for all causes
    select(measure,sex,age,cause,rate,location,number,pop_raw=pop)
  
  gbd_pop <- gbd_tmp %>%
    ## filter and select pop data only
    filter(cause == "all causes", measure == "Deaths") %>%
    select(age,sex,pop=pop_raw)
  
  ## Dataframe with rates per one
  gbd_rate <- gbd_tmp %>%
    # left_join(gbd_pop, by=c("age","sex")) %>%
    select(measure,sex,age,cause,rate,location,number) %>%
    ## Add age interval variable for over 15, we model adults only
    filter(age != "Under 5" & age != "5 to 9" & age != "10 to 14") %>%
    rowwise() %>%
    mutate(from_age = as.numeric(str_split(age,' to ')[[1]][1])) %>%
    mutate(to_age = as.numeric(str_split(age,' to ')[[1]][2])) %>%
    mutate(age_cat = from_age + 2) %>%
    # using rowwise() turns the dataframe into a tibble
    data.frame()
  
  
  
  ## Generate data frames for MSLT, incidence and case fatality will be replaced with disbayes inputs (or just case fatality?)
  
  ### Create wider data frame for measure and cause combinations and change all column names and string obs to lower case
  
  #### Alan is there any way to have the names of the variables as for example, incidence_rate_copd instead of rate_incidence_copd?
  #### AB: There is! You can use names_glue to have pretty much any format.
  gbd_wider <- gbd_rate %>% 
    mutate(disease = tolower(abbreviate(cause))) %>%
    mutate(measure = tolower(measure)) %>%
    # select(measure, sex, age, age_cat, disease, measure, rate, number) %>%
    mutate(age_sex = paste(age_cat, tolower(sex), sep = "_")) %>%
    pivot_wider(id_cols = c(measure, sex, age, disease, number,rate, age_cat, age_sex), 
                values_from = c(rate, number), names_from = c(measure, disease),
                names_glue = "{measure}_{.value}_{disease}") %>%
    left_join(gbd_pop, by = c("age", "sex")) %>% 
    mutate(sex = tolower(sex)) %>%
    arrange(sex, age_cat)
  # `names<-`(tolower(names(.)))
  ## BZ: added order data by age and sex
  # gbd_wider <- gbd_wider[order("sex", "age_cat"),]
  
  return(gbd_wider)
  
}  

calculateMSLT <- function(population_melbourne_location, deaths_melbourne_location, gbd_wider_location, dismod_output_cancers, dismod_output_non_cancers) {
  
  # population_melbourne_location="Data/Processed/population_melbourne.csv"
  # deaths_melbourne_location="Data/Processed/deaths_melbourne.csv"
  # gbd_wider_location="Data/Processed/gbd_wider.csv"
  # dismod_output_cancers="Data/Processed/dismod_output_cancers.csv"
  # dismod_output_non_cancers="Data/Processed/dismod_output_non_cancers.csv"

  ### From here we used data as inputs for disbayes and to create 1-yr frame for mslt
  
  # ---- chunk-1.2.4: Generate mslt inputs ----
  
  # male and female ages 0-100
  mslt_df <- data.frame(age = rep(c(0:100), 2), sex = append(rep("male", 101), 
                                                             rep("female", 101))) %>%
    mutate(sex_age_cat = paste(sex, age, sep="_"))
  ## Add population numbers (Melbourne population)
  ### Pop data for Melbourne
  
  pop_melb <- read.csv(population_melbourne_location, as.is=T, fileEncoding="UTF-8-BOM") %>%
    rowwise() %>%
    mutate(from_age = as.numeric(str_split(age,'-')[[1]][1])) %>%
    mutate(to_age = as.numeric(str_split(age,'-')[[1]][2])) %>%
    # using rowwise() turns the dataframe into a tibble
    data.frame() %>%
    mutate(age_cat = from_age + 2) %>%
    mutate(sex_age_cat = paste(tolower(sex), age_cat, sep="_")) %>%
    select(sex_age_cat, population)
  

  mslt_df <- mslt_df %>% left_join(pop_melb, by = "sex_age_cat") %>%
    mutate(age_cat = case_when(age == 2 ~ 2,
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
  
  
  
  ### Add mortality rate all cause from Victoria (best available data, no data for Melbourne, average over three yrs 2016-18)
  
  deaths_melbourne <- read.csv(deaths_melbourne_location, as.is=T, fileEncoding="UTF-8-BOM") %>%
    mutate(sex = ifelse(sex=="Males", "male", "female")) %>%
    mutate(sex_age_cat = paste(tolower(sex), age, sep = "_")) %>%
    rename(mx = rate) %>%
    select(sex_age_cat, mx)
  
  mslt_df <- left_join(mslt_df, deaths_melbourne)
  
  
  ### Interpolate rates  
  
  gbd_df <- read.csv(gbd_wider_location, as.is=T, fileEncoding="UTF-8-BOM")

  gbd_df[is.na(gbd_df)] <- 0 
  #### Disability weights
  
  # the row sum of all ylds_number_*disease* without ylds_number_allc
  all_ylds_count <- select(gbd_df, contains("ylds_number")) %>%
    select(-ylds_number_allc) %>%
    rowSums()

  # Adjust all cause ylds for included diseases and injuries (exclude all cause ). From here just med 
  # AB: Belen, please check that I've got this part working correctly.
  # BZ: checked, this code substracts ylds from all included causes to ylds for all cause and calculates the rate per one.
  gbd_df <- gbd_df %>%
    mutate(ylds_rate_allc_adj_1 = (ylds_number_allc - all_ylds_count)/pop)

  


  
  # AB: Ignore, this was to test representing the gbd by age_sex and disease,
  # with everything else in columns. 
  # # all values get their own column
  # gbd_longer <- gbd_df %>%
  #   pivot_longer(names_to=c("measure","rate_num","disease"),
  #                names_sep="_",
  #                cols=incidence_rate_cyri:ylds_number_copd)
  # 
  # # rows only represent age_sex and disease, everything else in columns
  # gbd_by_disease <- gbd_longer %>%
  #   pivot_wider(names_from=c("measure","rate_num"),
  #               values_from=value) %>%
  #   mutate(dw_adj=(ylds_number/prevalence_number)/(1-ylds_rate_allc_adj_1) ) %>%
  #   mutate(dw_adj=ifelse(is.nan(dw_adj),0,dw_adj))
  # 
 
  # interpolate a measure across 0-100 years
  # try the following code to test the function (interpolates dw_adj for males with allc):
  # mslt_df_by_disease%>%filter(sex=="female"&disease=="ishd")%>%pull(ylds_rate)%>%interpolateFunction()
  interpolateFunction <- function(valuesToInterpolate){
    age_group=0:100
    # only use ages where there is a value present
    age_group[is.na(valuesToInterpolate)] <- NA
    # removing the na entries
    age_group=age_group %>% .[!is.na(.)]
    valuesToInterpolate=valuesToInterpolate %>% .[!is.na(.)]
    # make the interpolation function
    # BZ: added log, rates as are cannot be interpolated (get negative values). We interpolate the log and then but with exp function for results
    InterFunc <- stats::splinefun(age_group, log(valuesToInterpolate),
                                  method="monoH.FC", ties=mean)
    # return interpolated values
    return(InterFunc(0:100))
  }
   
  # all values get their own column, expanding out to every age number
  mslt_df_longer <- mslt_df %>%
    left_join(gbd_df%>%select(-age,-age_sex)) %>%
    pivot_longer(names_to=c("measure","rate_num","disease"),
                 names_sep="_",
                 cols=incidence_rate_cyri:ylds_number_copd)
  
  # rows only represent age, sex and disease, everything else in columns.
  # Data has to be interpolated from 5-year age groups to 1-year age groups.
  mslt_df_by_disease <- mslt_df_longer %>%
    pivot_wider(names_from=c("measure","rate_num"),
                values_from=value) %>%
    mutate(dw_adj=(ylds_number/prevalence_number)/(1-ylds_rate_allc_adj_1) ) %>%
    mutate(dw_adj=ifelse(is.nan(dw_adj),0,dw_adj)) %>%
    arrange(disease,sex,age) %>%
    group_by(disease,sex) %>%
    # interpolate dw_adj
    mutate(dw_adj=exp(interpolateFunction(dw_adj))) %>%
    ## Interpolate mortality and ylds (all cause mortality is from Melbourne data)
    mutate(deaths_rate=exp(interpolateFunction(deaths_rate))) %>%
    mutate(ylds_rate=exp(interpolateFunction(ylds_rate))) %>%
    ## not sure if we were supposed to interpolate this one
    mutate(ylds_rate_allc_adj_1=exp(interpolateFunction(ylds_rate_allc_adj_1))) %>%
    ungroup()

  mslt_df_wider <- mslt_df_by_disease %>%
    select(age,sex,sex_age_cat,population,age_cat,mx,ylds_rate_allc_adj_1,
           disease,deaths_rate,ylds_rate,dw_adj)%>%
    pivot_wider(id_cols = c(age,sex,sex_age_cat,population,age_cat,mx,ylds_rate_allc_adj_1),
                names_from=disease,
                values_from=c(deaths_rate,ylds_rate,dw_adj)) %>%
    # AB: I might have this wrong
    # BZ: this is correct
    rename(pyld_rate=ylds_rate_allc_adj_1)
  
  ## Rename population to match run_life table code
  
  mslt_df_wider$population_number <- mslt_df_wider$population
  
  ### Add dismod outputs rates per one
  dismod_cancers <- read.csv(dismod_output_cancers, as.is=T, fileEncoding="UTF-8-BOM")
  dismod_non_cancers <- read.csv(dismod_output_non_cancers, as.is=T, fileEncoding="UTF-8-BOM")
  
  mslt_df_wider <- left_join(mslt_df_wider, dismod_cancers) 
  mslt_df_wider <- left_join(mslt_df_wider, dismod_non_cancers)
  
  return(mslt_df_wider)
}
