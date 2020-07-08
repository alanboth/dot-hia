
#---- chunk-Introduction: Disbayes  ----
  ### 1) Data inputs generation
  ### 2) Data processing with disbayes  

### Packages to run this code
# options(pkgType = "binary") 
library(rlist)
library(tidyr)
library(devtools)
# library(disbayes)

# ---- chunk-1: Disbayes input generation ----  
## The below code generates data to use as inputs in disbayes.
  

  ## To run this code, first the following data preparation (from dataprep.R scrip has to be generated)
  
  ## 1) gbd_data_processed
  ## 2) disease_measure list 
  ## 3) DISEASE_SHORT_NAMES

i_sex <- c('male', 'female')

disbayes_data <-  GenInpDisbayes(gbd_data_processed)

# ---- chunk-1.2.2: Generate num and denom using ci2num for incidence, prevalence and mortlaity ----


disbayes_data_2 <- list()
index <- 1


### for now drom ylds from disease measure list, I cannot make it work within the functions with the if statements

disease_measures_list <- disease_measures_list[-4]
  for (dm in 1:length(disease_measures_list)){
    for (d in 1:nrow(DISEASE_SHORT_NAMES)){
      in_measure <- disease_measures_list[dm] %>% as.character() %>% tolower()
    
      
      ### exclude ylds for now, we are interested in disbayes inputs but later may use ylds uncertainty parameters
      
      if (DISEASE_SHORT_NAMES$is_not_dis[d] != 0 || in_measure == "`ylds (years lived with disability)`"){
      }
      else {
        
        med <- paste0(in_measure, "_med_", DISEASE_SHORT_NAMES$sname[d])
        low <- paste0(in_measure, "_lower95_", DISEASE_SHORT_NAMES$sname[d])
        upper <- paste0(in_measure, "_upper95_", DISEASE_SHORT_NAMES$sname[d])
        
        ## These data is in 5-year age groups. 
        data <- gbd_data_processed
        
        
        disbayes_data_2[[index]] <- dplyr::select(data, population_number, location, sex_age_cat, med, low, upper)

        disbayes_data_2[[index]]$est <- disbayes_data_2[[index]][[med]]/disbayes_data_2[[index]][[1]]
        disbayes_data_2[[index]]$lower <- disbayes_data_2[[index]][[low]]/disbayes_data_2[[index]][[1]]
        disbayes_data_2[[index]]$upper <- disbayes_data_2[[index]][[upper]]/disbayes_data_2[[index]][[1]]
        disbayes_data_2[[index]]$index <- paste(in_measure, DISEASE_SHORT_NAMES$sname[d], sep = "_")
        disbayes_data_2[[index]]$indexagg <- paste(disbayes_data_2[[index]]$index, disbayes_data_2[[index]]$sex_age_cat,
                                                                      disbayes_data_2[[index]]$cityregion, sep = "_")
        
    
        ## Separate age and sex and   
        suppressWarnings(names(disbayes_data_2)[index] <- paste(in_measure, DISEASE_SHORT_NAMES$sname[d], sep = '_'))
        
        index <- index + 1
        
      
    }
  }
}


# ---- chunk-1.2.4: Generate num and denoms using Ci2NumDF ----

## trycatch is used to avoid issues with prevalence when runnind Ci2NumDF

tryCatchCi2NumDF <- function(x) tryCatch(Ci2NumDF(x), error = function(e) e)
disbayes_data_3  <- lapply(disbayes_data_2, tryCatchCi2NumDF)

# ---- chunk-1.2.5: Create a dataframe with all city regions data ----

## This step is to delete the data frames with errors from the list before appending the data frames in the list in a unique dataframe.
has_prevc <- c(grepl("prevalence",names(disbayes_data_3)) == TRUE) %>% as.character()

index <- 1
disbayes_data_3b <- list()

for (i in 1:length(disbayes_data_3)) {
  
  if(has_prevc[[index]] == "TRUE" ) {}
  else{
    
    disbayes_data_3b[[index]] <- disbayes_data_3[[i]]
  }
  index <- index + 1
}

disbayes_data_3b <-  list.clean(disbayes_data_3b, fun = is.null, recursive = TRUE)


disbayes_data_4 <- disbayes_data_3b %>% lapply(as.data.frame) %>% bind_rows() %>% group_by(indexagg)


disbayes_data_5 <- disbayes_data_4 %>%  mutate_if(is.character, RemoveAllWs)%>% 
  mutate(index = indexagg) %>% 
  separate(indexagg, c("measure", "disease", "sex", "age"))

## Add new variable with mid-age group

disbayes_data_5$agegr <- 0
disbayes_data_5$agegr [ disbayes_data_5$age =="2"] <- 0
disbayes_data_5$agegr [ disbayes_data_5$age =="7"] <- 5
disbayes_data_5$agegr [ disbayes_data_5$age =="12"] <- 10
disbayes_data_5$agegr [ disbayes_data_5$age =="17"] <- 15
disbayes_data_5$agegr [ disbayes_data_5$age =="22"] <- 20
disbayes_data_5$agegr [ disbayes_data_5$age =="27"] <- 25
disbayes_data_5$agegr [ disbayes_data_5$age =="32"] <- 30
disbayes_data_5$agegr [ disbayes_data_5$age =="37"] <- 35
disbayes_data_5$agegr [ disbayes_data_5$age =="42"] <- 40
disbayes_data_5$agegr [ disbayes_data_5$age =="47"] <- 45
disbayes_data_5$agegr [ disbayes_data_5$age =="52"] <- 50
disbayes_data_5$agegr [ disbayes_data_5$age =="57"] <- 55
disbayes_data_5$agegr [ disbayes_data_5$age =="62"] <- 60
disbayes_data_5$agegr [ disbayes_data_5$age =="67"] <- 65
disbayes_data_5$agegr [ disbayes_data_5$age =="72"] <- 70
disbayes_data_5$agegr [ disbayes_data_5$age =="77"] <- 75
disbayes_data_5$agegr [ disbayes_data_5$age =="82"] <- 80
disbayes_data_5$agegr [ disbayes_data_5$age =="87"] <- 85
disbayes_data_5$agegr [ disbayes_data_5$age =="92"] <- 90
disbayes_data_5$agegr [ disbayes_data_5$age =="97"] <- 95


disease_disbayes <- unique(disbayes_data_5$disease)
measure_disbayes <- unique(disbayes_data_5$measure)
sex_disbayes <- unique(disbayes_data_5$sex)


## To wider 


disbayes_data_6 <- disbayes_data_5 %>% 
  pivot_wider(id_cols = c(agegr, sex, population_number, measure, disease), 
              names_from = measure, values_from = c(num, denom))

## Create list
index <- 1
disbayes_data_7 <- list()


  for (d in disease_disbayes){
    for (s in sex_disbayes){
      
      
      disbayes_data_7[[index]] <- dplyr::filter(disbayes_data_6, disease == d, sex == s)
      
      # disbayes_data_7[[index]] <- disbayes_data_7[[index]][order(disbayes_input_list2[[index]]$agegr),]
      
      outage <- 0:100  # assume num and denom are the same in each year within a five-year age group
      
      # 
      ind <- findInterval(outage, disbayes_data_7[[index]]$agegr)
      disbayes_data_7[[index]] <- disbayes_data_7[[index]][ind,]
      disbayes_data_7[[index]]$age <- outage
      
      
      ### It leaves NA values (I need to have all columns filled in)
      
      
      disbayes_data_7[[index]]$index <- tolower(paste(disbayes_data_7[[index]]$sex, 
                                                           disbayes_data_7[[index]]$disease, 
                                                           disbayes_data_7[[index]]$age, sep = "_"))
      # disbayes_data_7[[index]] <- disbayes_data_7[[index]] %>% pivot_wider(id_cols = c(index), names_from = measure, values_from = c(num, denom))
      index <- index + 1
      
  }
}

# ---- chunk-1.2.6: Join dataframes with all disbayes inputs ----

## Remove disease names column and ylds column
disbayes_data <- lapply(disbayes_data, function(x) { x["disease"] <- NULL; x }) 


disbayes_data_df <- dplyr::bind_rows(disbayes_data)

disbayes_data_df$index <- paste(disbayes_data_df$sex_disease, disbayes_data_df$age_cat, sep = "_")

## Second data set with num and denom
disbayes_data_df2 <- do.call(rbind,  disbayes_data_7)
disbayes_data_df2$index <- paste(disbayes_data_df2$sex, disbayes_data_df2$disease, disbayes_data_df2$age, sep = "_")
disbayes_data_df2 <-   disbayes_data_df2[ -c(1:4, 9) ]





## Final data set to process in disbayes. Filter data by city region, disease and sex. COMPARE with saved data in rds
disbayes_inputs <- disbayes_data_df %>%
  left_join(disbayes_data_df2) %>% 
  separate(sex_disease, c("drop", "disease")) 

disbayes_inputs <- disbayes_inputs[, !(colnames(disbayes_inputs ) %in% c("drop","index"))]

## Change age_cat name to age

colnames(disbayes_inputs)[which(names(disbayes_inputs) == "age_cat")] <- "age"



# ---- chunk-2 disbayes processing ---- 

### One disease at the time. Assumtpion for age below which case fatality cases are 0
### Compare with dismod II

### Disease: 	Ischemic heart disease, 	Tracheal, bronchus, and lung cancer, 	Chronic obstructive pulmonary disease, 	Stroke
### Diabetes mellitus type 2, Colon and rectum cancer, Uterine cancer

  dat <- dplyr::filter(disbayes_inputs, disease == "copd", sex == "female") 
  
  
  resu <- disbayes:::disbayes(dat = dat,
                   inc = "inc",
                   inc_denom = "pop",
                   prev_num = "prevn",
                   prev_denom = "prevdenom",
                   mort = "mort",
                   mort_denom = "pop",
                   
                   
                   
                   ## You'll need to change this for different diseases:
                   ## the age below which all case fatalities are
                   ## assumed equal in the smoothed model 
                   eqage = 30, 
                   smooth = TRUE  # or FALSE if don't want smoothed estimates
  )
  
  ## Posterior medians and 95% credible intervals for all unknowns in the model
  summ <- summary(resu) 
  
  ## Handy tool to extract specific variables from this 
  test_output <- as.data.frame(disbayes:::summary.disbayes(resu, vars=c("cf","inc")))
  
  ## Plot smoothed and unsmoothed estimates 
  plot(resu)



## Plot just smoothed estimates
plot(summ, variable="cf")