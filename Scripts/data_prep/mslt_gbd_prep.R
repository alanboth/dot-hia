suppressPackageStartupMessages(library(stringr)) # for splitting strings
suppressPackageStartupMessages(library(dplyr)) # for manipulating data
suppressPackageStartupMessages(library(tidyr)) # for pivoting data


calculateDiseaseNames <- function(gbd_location,disease_outcomes_location) {
  # gbd_location="Data/original/gbd/gbd_mslt.csv"
  # disease_outcomes_location="Data/original/ithimr/disease_outcomes_lookup.csv"
  
  gbd <- read.csv(gbd_location, as.is=T, fileEncoding="UTF-8-BOM") 
  
  disease_names_execute <- read.csv(disease_outcomes_location,
                                    as.is=T,fileEncoding="UTF-8-BOM") %>%
    dplyr::select(GBD_name, acronym) %>%
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
      males = ifelse(disease %in% c("uterine cancer", "breast cancer"), 0, 1),
      females = ifelse(disease %in% "prostate cancer", 0, 1),
      sname = gsub("'", '', sname),
      acronym = ifelse(is.na(acronym), sapply(strsplit(disease, " "), head, 1), acronym)) 
 return(DISEASE_SHORT_NAMES)

}

calculateGBDwider <- function(gbd_location) {
  # gbd_location="Data/original/gbd/gbd_mslt.csv"
  
  gbd <-  read.csv(gbd_location, as.is=T, fileEncoding="UTF-8-BOM") 
  
  # remove '_name' from column names
  names(gbd) <- gsub(pattern = "_name", replacement = "", x = names(gbd))
  gbd <- gbd %>%
    dplyr::select(-contains("id")) %>%
    mutate(cause = tolower(cause))

    gbd_tmp <- gbd %>%
    dplyr::select(-upper,-lower) %>%
    mutate(metric=tolower(metric)) %>%
    # this name is too long
    mutate(measure = ifelse(measure=='YLDs (Years Lived with Disability)','ylds',measure)) %>%
    mutate(age = ifelse(age=="95 plus", "95 to 120", age)) %>%
    pivot_wider(names_from="metric", values_from="val") %>%
    mutate(rate = rate / 100000) %>%
    mutate(pop = number / rate) %>%
    # only want pop for all causes
    dplyr::select(measure,sex,age,cause,rate,location,number,pop_raw=pop)
  
  gbd_pop <- gbd_tmp %>%
    ## filter and select pop data only
    filter(cause == "all causes", measure == "Deaths") %>%
    dplyr::select(age,sex,pop=pop_raw)
  
  ## Dataframe with rates per one
  gbd_rate <- gbd_tmp %>%
    # left_join(gbd_pop, by=c("age","sex")) %>%
    dplyr::select(measure,sex,age,cause,rate,location,number) %>%
    ## Add age interval variable for over 15, we model adults only
    filter(age != "Under 5" & age != "5 to 9" & age != "10 to 14") %>%
    rowwise() %>%
    mutate(from_age = as.numeric(str_split(age,' to ')[[1]][1])) %>%
    mutate(to_age = as.numeric(str_split(age,' to ')[[1]][2])) %>%
    mutate(age_cat = from_age + 2) %>%
    # using rowwise() turns the dataframe into a tibble
    data.frame()

  gbd_wider <- gbd_rate %>% 
    mutate(disease = tolower(abbreviate(cause))) %>%
    mutate(measure = tolower(measure)) %>%
    mutate(age_sex = paste(age_cat, tolower(sex), sep = "_")) %>%
    pivot_wider(id_cols = c(measure, sex, age, disease, number,rate, age_cat, age_sex), 
                values_from = c(rate, number), names_from = c(measure, disease),
                names_glue = "{measure}_{.value}_{disease}") %>%
    left_join(gbd_pop, by = c("age", "sex")) %>% 
    mutate(sex = tolower(sex)) %>%
    arrange(sex, age_cat)
  return(gbd_wider)
  
}  
### BZ: removed population and death rates, these are now specified in the mslt_code depending on location (Greater capital cities options and australia wide)
calculateMSLT <- function(gbd_wider_location, dismod_output_cancers, dismod_output_non_cancers) {
   # gbd_wider_location = "Data/processed/mslt/gbd_wider.csv"
   # dismod_output_cancers = "Data/processed/mslt/dismod_output_cancers.csv"
   # dismod_output_non_cancers = "Data/processed/mslt/dismod_output_non_cancers.csv"

  mslt_df <- data.frame(age = rep(c(0:100), 2), sex = append(rep("male", 101), 
                                                             rep("female", 101))) %>%
    dplyr::mutate(sex_age_cat = paste(sex, age, sep="_")) %>%
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
                               age == 97 ~ 97)) %>%
  
  dplyr::mutate(age_cat_2 = case_when(age_cat == 2 ~  "0 to 4",
                                      age_cat == 7 ~  "5 to 9",
                                      age_cat ==  12 ~ "10 to 14",
                                      age_cat ==  17  ~ "15 to 19", 
                                      age_cat ==  22  ~ "20 to 24",
                                      age_cat ==  27  ~ "25 to 29",
                                      age_cat ==  32  ~ "30 to 34", 
                                      age_cat ==  37  ~ "35 to 39", 
                                      age_cat ==  42  ~ "40 to 44", 
                                      age_cat ==  47  ~ "45 to 49",
                                      age_cat ==  52  ~ "50 to 54", 
                                      age_cat ==  57  ~ "55 to 59", 
                                      age_cat ==  62  ~ "60 to 64", 
                                      age_cat ==  67  ~ "65 to 69", 
                                      age_cat ==  72  ~ "70 to 74",
                                      age_cat ==  77  ~ "75 to 79", 
                                      age_cat ==  82  ~ "80 to 84",
                                      age_cat ==  87  ~ "85 to 89",
                                      age_cat ==  92  ~ "90 to 94", 
                                      age_cat ==  97   ~ "95 to 100"))
  
  ### Interpolate rates  
  
  gbd_df <- read.csv(gbd_wider_location, as.is=T, fileEncoding="UTF-8-BOM")
  
  gbd_df[is.na(gbd_df)] <- 0 
  
  #### Disability weights
  
  # the row sum of all ylds_number_*disease* without ylds_number_allc
  all_ylds_count <- dplyr::select(gbd_df, contains("ylds_number")) %>%
    dplyr::select(-ylds_number_allc) %>%
    rowSums()
  

  gbd_df <- gbd_df %>%
    dplyr::mutate(ylds_rate_allc_adj_1 = (ylds_number_allc - all_ylds_count)/pop)
  
  
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
    left_join(gbd_df%>%dplyr::select(-age,-age_sex)) %>%
    pivot_longer(names_to=c("measure","rate_num","disease"),
                 names_sep="_",
                 cols=incidence_rate_cyri:ylds_number_copd)
  
  # rows only represent age, sex and disease, everything else in columns.
  # Data has to be interpolated from 5-year age groups to 1-year age groups.
  mslt_df_by_disease <- mslt_df_longer %>%
    pivot_wider(names_from=c("measure","rate_num"),
                values_from=value) %>%
    dplyr::mutate(dw_adj=(ylds_number/prevalence_number)/(1-ylds_rate_allc_adj_1) ) %>%
    dplyr::mutate(dw_adj=ifelse(is.nan(dw_adj),0,dw_adj)) %>%
    arrange(disease,sex,age) %>%
    group_by(disease,sex) %>%
    # interpolate dw_adj
    dplyr::mutate(dw_adj=exp(interpolateFunction(dw_adj))) %>%
    ## Interpolate mortality and ylds (all cause mortality is from Melbourne data)
    dplyr::mutate(deaths_rate=exp(interpolateFunction(deaths_rate))) %>%
    dplyr::mutate(ylds_rate=exp(interpolateFunction(ylds_rate))) %>%
    ## not sure if we were supposed to interpolate this one
    dplyr::mutate(ylds_rate_allc_adj_1=exp(interpolateFunction(ylds_rate_allc_adj_1))) %>%
    ungroup()
  
  mslt_df_wider <- mslt_df_by_disease %>%
    dplyr::select(age,sex,sex_age_cat, age_cat, age_cat_2,ylds_rate_allc_adj_1,
           disease,deaths_rate,ylds_rate,dw_adj)%>%
    pivot_wider(id_cols = c(age,sex,sex_age_cat, age_cat,age_cat_2,ylds_rate_allc_adj_1),
                names_from=disease,
                values_from=c(deaths_rate,ylds_rate,dw_adj)) %>%
    # AB: I might have this wrong
    # BZ: this is correct
    dplyr::rename(pyld_rate=ylds_rate_allc_adj_1)
  

  
  ### Add dismod outputs rates per one
  dismod_cancers <- read.csv(dismod_output_cancers, as.is=T, fileEncoding="UTF-8-BOM")
  dismod_non_cancers <- read.csv(dismod_output_non_cancers, as.is=T, fileEncoding="UTF-8-BOM")
  
  mslt_df_wider <- left_join(mslt_df_wider, dismod_cancers) 
  mslt_df_wider <- left_join(mslt_df_wider, dismod_non_cancers)
  
  return(mslt_df_wider)
}