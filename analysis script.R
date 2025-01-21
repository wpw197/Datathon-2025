## 
## Script name: analysis script
##
## Purpose of script: 
##
## Author: 
##
## Date Created: 21.01.25
##
## ------------------------------------------------------------------------
##
## Notes:
##   
##
## ------------------------------------------------------------------------
## load up the packages we will need: 

packages <- c("tidyverse", "versions", "pmsampsize", "car")
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = TRUE, quietly = TRUE)  
rm(packages)

## ------------------------------------------------------------------------
# pmsampsize(type = "b", cstatistic = 0.8, parameters = 4, prevalence = 0.197)
pmsampsize::pmsampsize(type="b",cstatistic = 0.8,parameters = 4,prevalence = 0.197)

# Checking assumptions ----------------------------------------------------
## age gender smoking subtype 
## checking for multi collinearity
## selecting relevant columns 
collinearity_check <- TRACEx_pts_df %>% 
  select(age, sex, ethnicity, cigs_perday, years_smoking, packyears,
         smoking_status_merged, is.family.lung, histology_multi_full_genomically.confirmed)



