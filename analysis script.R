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

packages <- c("tidyverse", "versions", "pmsampsize", "car", "glm2")
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = TRUE, quietly = TRUE)  
rm(packages)

## ------------------------------------------------------------------------
# pmsampsize(type = "b", cstatistic = 0.8, parameters = 4, prevalence = 0.197)
pmsampsize::pmsampsize(type="b",cstatistic = 0.8,parameters = 4,prevalence = 0.197)


# data cleaning -----------------------------------------------------------

TRACEx_pts_df <- TRACEx_pts_df %>%
  mutate(Metastasis = ifelse( Relapse_cat_new == 'Extrathoracic'| 
                                Relapse_cat_new == 'Intra & Extra'| 
                                Relapse_cat_new == 'Intrathoracic', 1, NA)) %>% 
  mutate_at(vars(Metastasis), ~ifelse(Relapse_cat_new == 'No rec', 0, . )) %>% 
  mutate_at(vars(Metastasis), ~ifelse(Relapse_cat_new == 'Unknown Site', NA, . )) 

# running model -----------------------------------------------------------
Model1 <- glm(Metastasis~ age+ sex+smoking_status_merged+ histology_multi_full_genomically.confirmed,family=binomial(link='logit'),data=TRACEx_pts_df)
Model2 <- glm(Metastasis~ age + sex +cigs_perday+ histology_multi_full_genomically.confirmed,family=binomial(link='logit'),data=TRACEx_pts_df)
summary(Model2)

Model3 <- glm(Metastasis~ age + sex +packyears,family=binomial(link='logit'),data=TRACEx_pts_df)
summary(Model3)

Model4 <- glm(Metastasis~ age + sex+cigs_perday,family=binomial(link='logit'),data=TRACEx_pts_df)
summary(Model4)


# Checking assumptions ----------------------------------------------------
## age gender smoking sub-type 
## checking for multicollinearity

vif(Model1)



