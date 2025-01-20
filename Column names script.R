## 
## Script name: 
##
## Purpose of script: 
##
## Author: Emily Lane
##
## Date Created: 
##
## ------------------------------------------------------------------------
##
## Notes:
##   
##
## ------------------------------------------------------------------------
## load up the packages we will need: 

packages <- c("tidyverse", "versions", "stringr", "fst", "readr", "readxl")
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = TRUE, quietly = TRUE)  
rm(packages)

## ------------------------------------------------------------------------

file_names<-list.files('TRACERx/expression/transcriptomics_scripts_data_updated/20221014_transcriptomic_DATA/')
dta_type<-c()
nrows<-2000
out_dta<-data.frame(col1=matrix(nrow = nrows, ncol=100))
for(i in 45:length(file_names)){
  
  colnames(out_dta)[i]<-file_names[i]
  dta_type <- str_split(file_names[i], "\\.")[[1]][2]
  
  if(dta_type=='fst'){
    
    data <- read_fst(paste0('TRACERx/expression/transcriptomics_scripts_data_updated/20221014_transcriptomic_DATA/', file_names[i]))
    
  }else if(dta_type=='csv'){
    
    data <- read_csv(paste0('TRACERx/expression/transcriptomics_scripts_data_updated/20221014_transcriptomic_DATA/', file_names[i]))
    
  }else if(dta_type == 'rds'){
    
    data <- read_rds(paste0('TRACERx/expression/transcriptomics_scripts_data_updated/20221014_transcriptomic_DATA/', file_names[i]))
    
  }else if(dta_type == 'tsv'){
    
    data <- read_tsv(paste0('TRACERx/expression/transcriptomics_scripts_data_updated/20221014_transcriptomic_DATA/', file_names[i]))
    
  }else if(dta_type == 'RDS'){
    
    data <- readRDS(paste0('TRACERx/expression/transcriptomics_scripts_data_updated/20221014_transcriptomic_DATA/', file_names[i]))
    
  }else if(dta_type == 'xlsx'){
    
    data <- readxl::read_xlsx(paste0('TRACERx/expression/transcriptomics_scripts_data_updated/20221014_transcriptomic_DATA/', file_names[i]))
  }

  out_dta[,i]<-c(colnames(data), rep(NA, nrows-length(colnames(data))))

  
  
}


out_dta <- out_dta[,1:60]


write.csv(out_dta, 'Column names/expression folders.csv')
