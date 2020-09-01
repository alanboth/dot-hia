
rm (list = ls())
library(ithimr)
PA_DOSE_RESPONSE_QUANTILE <- F 



mmets_pp="Data/Processed/mets_test.csv"
mmets_pp <- read.csv(mmets_pp, as.is=T,fileEncoding="UTF-8-BOM")

global_path <- file.path(find.package('ithimr',lib.loc=.libPaths()), 'extdata/global/')
## for windows??
global_path <- paste0(global_path, "/")
disease_outcomes_lookup_location="Data/Processed/disease_outcomes_lookup.csv"

SCEN_SHORT_NAME <- c("base", "scen1")

DISEASE_INVENTORY <-  read.csv(disease_outcomes_lookup_location,as.is=T,fileEncoding="UTF-8-BOM")
# list of ithmr default dose response data
list_of_files <- list.files(path = paste0(global_path,"dose_response/drpa/extdata"), recursive = TRUE, pattern = "\\.csv$", full.names = TRUE)
for (i in 1:length(list_of_files)){
  assign(stringr::str_sub(basename(list_of_files[[i]]), end = -5),
         readr::read_csv(list_of_files[[i]],col_types = cols()),
         pos = 1)
}

RR_PA_calculations <- ithimr::gen_pa_rr(mmets_pp)


###################### 5) PIFS by age and sex (with function health_burden_2) #####################################

# pifs_pa_ap <- health_burden_2(RR_PA_calculations)