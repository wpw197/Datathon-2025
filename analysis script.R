## 
## Script name: analysis script
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

packages <- c("tidyverse", "versions", "pmsampsize")
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = TRUE, quietly = TRUE)  
rm(packages)

## ------------------------------------------------------------------------
# pmsampsize(type = "b", cstatistic = 0.8, parameters = 4, prevalence = 0.197)
pmsampsize::pmsampsize(type="b",cstatistic = 0.8,parameters = 4,prevalence = 0.197)


