# Gather outpust from Dismod and arrange for MSLT data frame
# Cancers are derived in Dismod using AIHW data
# Non-cancers are derived in Dismod using GBD data
# Prevalence ratios for cancers are derived from prevelence generated in Dismod with AIHW data and GBD data to adjust disability weights.

suppressPackageStartupMessages(library(stringr)) # for splitting strings
suppressPackageStartupMessages(library(dplyr)) # for manipulating data
suppressPackageStartupMessages(library(tidyr)) # for pivoting data
suppressPackageStartupMessages(library(stringi))



### Get file with diseases names
diseases="Data/Processed/disease_names.csv"

### CANCERS
#### Prepared with AIHW data
#### Identify cancers in diseases folder
cancers <- read.csv(diseases, as.is=T, fileEncoding="UTF-8-BOM") %>%
            filter(str_detect(disease, "cancer")) %>%
            pull(sname)

#### Get file names
index <- 1
files_names <- list()
for (d in cancers) {
  for (sex in c("female", "male")) {
    if (sex == 'male' && (d %in% c('brsc', 'utrc'))) {
    }
    else{
    
    files_names[[index]] <- paste0("Data/dismod/ouputs/", d, "_cancer_", sex, "_aihw", ".csv")
    index <- index + 1
    }
  }
}

### Get files
cancers_list <- lapply(files_names, read.csv)


### Clean files
names <- list()
names_2 <- list()
index <- 1
for (i in 1:length(cancers_list)){
  ## assign names to lists for later reference
  names(cancers_list)[index] <- paste0(cancers_list[[i]][1,1])
  ## extract outputs from dismod and convert to numeric
  cancers_list[[index]] <- cancers_list[[i]][,5:8] %>%
  janitor::row_to_names(3)  %>%
  mutate_if(is.character,as.numeric) 
 
  ## add sex, disease and age columns
  names[[index]] <- strsplit(files_names[[index]], ("[[:punct:][:space:]]+")) %>% unlist()
  cancers_list[[index]]$sex <- tolower(names[[index]][6]) 
  cancers_list[[index]]$disease <- tolower(names[[index]][4])
  cancers_list[[index]] <- cancers_list[[index]][-c(102:103), ]
  cancers_list[[index]]$age <- 0:100
  cancers_list[[index]]$sex_age_cat <- paste(cancers_list[[index]]$sex, cancers_list[[index]]$age, sep = "_")
  
  ## modify column names to include disase and match mslt function format
  
  name <- unique(cancers_list[[index]]$disease)
  colnames(cancers_list[[index]]) <-  c(paste0("incidence_", name),
                                         paste0("prevalence_", name),
                                         paste0("remission_", name),
                                         paste0("case_fatality_", name), "sex", "disease", "age", "sex_age_cat")
  cancers_list[[index]] <- cancers_list[[index]][ -c(5,6,7) ]
  
### Alan, how can we replace case_fatality to be zero when incidence is zero?
  
index <-  index + 1}

cancers_df <- plyr::ldply(cancers_list, rbind) %>%
  group_by(sex_age_cat) %>%                                 ## this step is to remove all NAs
  summarise_all(funs(if(is.numeric(.)) sum(., na.rm = TRUE) else first(.)))
cancers_df <- cancers_df[-c(2) ]

write.csv(cancers_df, "Data/Processed/dismod_output_cancers.csv",
           row.names=F, quote=F)

### NON-CANCERS
### Prepared with gbd data
#### Identify cancers in diseases folder
non_cancers <- read.csv(diseases, as.is=T, fileEncoding="UTF-8-BOM") %>%
  filter(is_not_dis ==0) %>%
  filter(str_detect(disease, "cancer", negate = TRUE)) %>%
  pull(sname)

#### Get file names
index <- 1
files_names <- list()
for (d in non_cancers) {
  for (sex in c("female", "male")) {
      files_names[[index]] <- paste0("Data/dismod/ouputs/", d, "_", sex, ".csv")
      index <- index + 1
  }
}

### Get files
non_cancers_list <- lapply(files_names, read.csv)


### Clean files
names <- list()
names_2 <- list()
index <- 1
for (i in 1:length(non_cancers_list)){
  ## assign names to lists for later reference
  names(non_cancers_list)[index] <- paste0(non_cancers_list[[i]][1,1])
  ## extract outputs from dismod and convert to numeric
  non_cancers_list[[index]] <- non_cancers_list[[i]][,6:9] %>%
    janitor::row_to_names(3)  %>%
    mutate_if(is.character,as.numeric)

  
  ## add sex, disease and age columns
  names[[index]] <- strsplit(files_names[[index]], ("[[:punct:][:space:]]+")) %>% unlist()
  non_cancers_list[[index]]$sex <- tolower(names[[index]][5]) 
  non_cancers_list[[index]]$disease <- tolower(names[[index]][4])
  non_cancers_list[[index]] <- non_cancers_list[[index]][-c(102:103), ]
  non_cancers_list[[index]]$age <- 0:100
  non_cancers_list[[index]]$sex_age_cat <- paste(non_cancers_list[[index]]$sex, non_cancers_list[[index]]$age, sep = "_")
  ## modify column names to include disase and match mslt function format
  
  name <- unique(non_cancers_list[[index]]$disease)
  colnames(non_cancers_list[[index]]) <-  c(paste0("incidence_", name),
                                        paste0("prevalence_", name),
                                        paste0("remission_", name),
                                        paste0("case_fatality_", name), "sex", "disease", "age", "sex_age_cat")
  non_cancers_list[[index]] <- non_cancers_list[[index]][ -c(5,6,7) ]
  
  ## prevalence and case fatality cannot have values unless incidnece has values
  ### TO DO
  index <-  index + 1}

non_cancers_df <- plyr::ldply(non_cancers_list, rbind) %>%
  group_by(sex_age_cat) %>%                                 ## this step is to remove all NAs
  summarise_all(funs(if(is.numeric(.)) sum(., na.rm = TRUE) else first(.)))
  non_cancers_df <- non_cancers_df[-c(2) ]

  
write.csv(non_cancers_df, "Data/Processed/dismod_output_non_cancers.csv",
            row.names=F, quote=F)


#### DJUSTMENT FACTOR FOR PREVALENCE USED TO CALCULATE DISABILITY WEIGHTS

#### Compare GBD and AIHW data

### CANCERS
#### Prepared with AIHW data
#### Identify cancers in diseases folder
cancers_gbd <- read.csv(diseases, as.is=T, fileEncoding="UTF-8-BOM") %>%
  filter(str_detect(disease, "cancer")) %>%
  pull(sname)

#### Get file names
index <- 1
files_names_gbd <- list()
for (d in cancers) {
  for (sex in c("female", "male")) {
    if (sex == 'male' && (d %in% c('brsc', 'utrc'))) {
    }
    else{
      
      files_names_gbd[[index]] <- paste0("Data/dismod/ouputs/", d, "_cancer_", sex, ".csv")
      index <- index + 1
    }
  }
}

### Get files
cancers_gbd_list <- lapply(files_names_gbd, read.csv)


### Clean files
names <- list()
names_2 <- list()
index <- 1
for (i in 1:length(cancers_gbd_list)){
  ## assign names to lists for later reference
  names(cancers_gbd_list)[index] <- paste0(cancers_gbd_list[[i]][1,1])
  ## extract outputs from dismod and convert to numeric
  cancers_gbd_list[[index]] <- cancers_gbd_list[[i]][,6:9] %>%
    janitor::row_to_names(3)  %>%
    mutate_if(is.character,as.numeric) 
  
  ## add sex, disease and age columns
  names[[index]] <- strsplit(files_names[[index]], ("[[:punct:][:space:]]+")) %>% unlist()
  cancers_gbd_list[[index]]$sex <- tolower(names[[index]][6]) 
  cancers_gbd_list[[index]]$disease <- tolower(names[[index]][4])
  cancers_gbd_list[[index]] <- cancers_gbd_list[[index]][-c(102:103), ]
  cancers_gbd_list[[index]]$age <- 0:100
  cancers_gbd_list[[index]]$sex_age_cat <- paste(cancers_gbd_list[[index]]$sex, cancers_gbd_list[[index]]$age, sep = "_")
  
  ## modify column names to include disase and match mslt function format
  
  name <- unique(cancers_gbd_list[[index]]$disease)
  colnames(cancers_gbd_list[[index]]) <-  c(paste0("incidence_", name),
                                        paste0("prevalence_", name),
                                        paste0("remission_", name),
                                        paste0("case_fatality_", name), "sex", "disease", "age", "sex_age_cat")
  cancers_gbd_list[[index]] <- cancers_gbd_list[[index]][ -c(5,6,7) ]
  
  ## prevalence and case fatality cannot have values unless incidnece has values
  ### TO DO
  #  
  
  index <-  index + 1}

cancers_gbd_df <- plyr::ldply(cancers_gbd_list, rbind) %>%
  group_by(sex_age_cat) %>%                                 ## this step is to remove all NAs
  summarise_all(funs(if(is.numeric(.)) sum(., na.rm = TRUE) else first(.)))
cancers_gbd_df <- cancers_df[-c(2) ]


  ### Graphs to compare estimates
  ### We expect prevalence to be higher with aihw data as we do not assume remission, these graphs are to confirm. 
  ### However, aihw has lung cancer and colon cancer, whereas gbd has colorectal and lung, tracheal and bronchus, however, less clear as 
  ### we are not comparing likes with likes
library(ggplot2)
data_aihw <- data.frame(age = rep(0:100), prevalence = cancers_list[[6]]$prevalence_carc) #, cancers_list[[1]]$incidence_tbalc)
data_gbd <- data.frame(age = rep(0:100), prevalence = cancers_gbd_list[[6]]$prevalence_carc)

p = ggplot() + 
  geom_line(data = data_aihw, aes(x = age, y = prevalence), color = "blue") +
  geom_line(data = data_gbd, aes(x = age, y = prevalence), color = "red") +
  xlab('Age') +
  ylab('prevalence')

print(p)