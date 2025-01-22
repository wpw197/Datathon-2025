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

packages <- c("tidyverse", "versions", "pmsampsize", "car", "glm2", "readr",
              "caret", "vip", "rsample")
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = TRUE, quietly = TRUE)  
rm(packages)


# reading in the data -----------------------------------------------------

merged_dta <- read_csv("MSK_TRACERx_merged.csv")
MSK_NSCLC_2022_raw <- read_csv("MSK_NSCLC_2022_raw.csv")
MSK_NSCLC_2022_refined <- read_csv("MSK_NSCLC_2022_refined.csv")
TRACEx_pts_df <- read_csv("TRACEx_pts_df.csv")


# data cleaning -----------------------------------------------------------
#
tracer_dta <- TRACEx_pts_df %>% 
  select(cruk_id, age, ethnicity, sex, 
         years_smoking, cigs_perday, 
         smoking_status_merged, is.family.lung,histology_multi_full_genomically.confirmed,
         histology_lesion1, Relapse_cat_new, os_time) %>% 
  mutate(study = 'TRACERx') %>% 
    mutate( Overall_survival_months = as.numeric(os_time)/30.44)

msk_dta <- MSK_NSCLC_2022_raw %>%
  select(Patient.ID, Cancer.Type,              
         Cancer.Type.Detailed ,     NSCLC.SubType   ,          Oncotree.Code  ,Overall.Survival..Months.,
         Sex      ,                 Smoking.Status    ,        Histology      ,           
         Prior.Treatment   ,        Sample.Type    ,           Patient.Current.Age, Race.Category) %>% 
  mutate(study = 'MSK')

## age check on MSK data, looking to see for different ages recorded for the same patient
# dealing with multiples:

msk_multis_ids <- msk_dta %>% 
  group_by(Patient.ID) %>% 
  summarise(n=n()) %>% 
  filter(n>1) %>% 
  select(Patient.ID)

msk_single_ids <- msk_dta %>% 
  group_by(Patient.ID) %>% 
  summarise(n=n()) %>% 
  filter(n==1) %>% 
  select(Patient.ID)
  
msk_multis_dta <-msk_dta %>% 
  filter(Patient.ID %in% msk_multis_ids$Patient.ID)

msk_multis_dta %>% 
  filter(!is.na(Sample.Type)) %>% 
  group_by(Patient.ID, Sample.Type) %>% 
  summarise(n=n()) %>% 
  group_by(Patient.ID) %>% 
  summarise(n=n()) %>% 
  filter(n>1)

out <- data.frame()

for(i in 1:dim(msk_multis_ids)[1]){
  
  pat <- msk_multis_dta %>% 
    filter(Patient.ID==msk_multis_ids$Patient.ID[i])
  
  size <-  dim(pat %>% 
                 distinct(Cancer.Type,              
                          Cancer.Type.Detailed ,     NSCLC.SubType   ,          Oncotree.Code  ,           Overall.Survival..Months.,
                          Sex      ,                 Smoking.Status    ,        Histology      ,             
                          Prior.Treatment   ,        Sample.Type    ,           Patient.Current.Age ))[1]
  
  if(size == 1){
    
    out <- out %>% 
      bind_rows(pat %>% 
      distinct(Cancer.Type,              
               Cancer.Type.Detailed ,     NSCLC.SubType   ,          Oncotree.Code  ,           Overall.Survival..Months.,
               Sex      ,                 Smoking.Status    ,        Histology      ,           
               Prior.Treatment   ,        Sample.Type    ,           Patient.Current.Age, .keep_all = TRUE))
    
  }else{
    
    
    if('Metastasis' %in% pat$Sample.Type){
      pat$Sample.Type <- 'Metastasis'
    }
    
    pat$Patient.Current.Age<- max(pat$Patient.Current.Age)
    pat$Overall.Survival..Months. <- max(pat$Overall.Survival..Months.)
    
    out <- out %>% 
      bind_rows(pat[1,])
    
    
  }
  
}

msk_dta <- msk_dta %>% 
  filter(Patient.ID %in% msk_single_ids$Patient.ID) %>% 
  bind_rows(out)

## race/ethnicity

tracer_dta <- tracer_dta %>% 
  mutate_at(vars(ethnicity), ~ifelse(. == 'White- British', 'White', .)) %>% 
  mutate_at(vars(ethnicity), ~ifelse(. == 'White- Irish', 'White', .)) %>% 
  mutate_at(vars(ethnicity), ~ifelse(. == 'White- Other', 'White', .)) %>% 
  mutate_at(vars(ethnicity), ~ifelse(. == 'White-European', 'White', .)) %>% 
  mutate_at(vars(ethnicity), ~ifelse(. == 'Caribbean', 'Other', .)) %>% 
  mutate_at(vars(ethnicity), ~ifelse(. == 'White and Asian', 'Other', .)) %>% 
  mutate_at(vars(ethnicity), ~ifelse(. == 'White and Black', 'Other', .)) %>% 
  mutate_at(vars(ethnicity), ~ifelse(. == 'Indian', 'Asian', .)) 

msk_dta <- msk_dta %>% 
  mutate(ethnicity = ifelse(Race.Category=='ASIAN-FAR EAST/INDIAN SUBCONT', 'Asian', NA)) %>% 
  mutate_at(vars(ethnicity), ~ifelse(Race.Category == 'BLACK OR AFRICAN AMERICAN', 'Black', .)) %>% 
  mutate_at(vars(ethnicity), ~ifelse(Race.Category == 'WHITE', 'White', .)) %>%
  mutate_at(vars(ethnicity), ~ifelse(Race.Category == 'OTHER', 'Other', .)) %>%
  mutate_at(vars(ethnicity), ~ifelse(Race.Category == 'BLACK OR AFRICAN AMERICAN', 'Black', .)) %>%
  mutate_at(vars(ethnicity), ~ifelse(Race.Category == 'NATIVE AMERICAN-AM IND/ALASKA', 'Other', .)) %>%
  mutate_at(vars(ethnicity), ~ifelse(Race.Category == 'PT REFUSED TO ANSWER'|
                                       Race.Category=='NO VALUE ENTERED' |
                                       Race.Category == 'UNKNOWN', NA, .)) 

## metastasis

tracer_dta <- tracer_dta %>% 
  mutate(Metastasis = ifelse( Relapse_cat_new == 'Extrathoracic'| 
                                Relapse_cat_new == 'Intra & Extra'| 
                                Relapse_cat_new == 'Intrathoracic', "Yes", NA)) %>% 
  mutate_at(vars(Metastasis), ~ifelse(Relapse_cat_new == 'No rec', "No", . )) %>% 
  mutate_at(vars(Metastasis), ~ifelse(Relapse_cat_new == 'Unknown Site', NA, . )) 
  
msk_dta <- msk_dta %>% 
  mutate(Metastasis = ifelse( Sample.Type == 'Primary' , "Yes", NA)) %>% 
  mutate_at(vars(Metastasis), ~ifelse(Sample.Type == 'Metastasis', "No", .)) %>% 
  mutate_at(vars(Metastasis), ~ifelse(Sample.Type == 'Unknown', NA, .))

# smoking status

msk_dta <- msk_dta %>% 
  mutate(smoking_status = ifelse(Smoking.Status == TRUE, 'Smoker/Ex-Smoker', NA)) %>% 
  mutate_at(vars(smoking_status), ~ifelse(Smoking.Status == FALSE, 'Never Smoked', .))

tracer_dta <- tracer_dta %>% 
  mutate_at(vars(smoking_status_merged), ~ifelse(.=='Ex-Smoker'|.=='Smoker', 'Smoker/Ex-Smoker',.))

# histological site

msk_dta<-msk_dta %>% 
  mutate(histology=ifelse(Histology=='Other', 'Large cell carcinoma', Histology))

tracer_dta<-tracer_dta %>% 
  filter(histology_lesion1!='Carcinosarcoma'|histology_lesion1!='Collision LUAD and LUSC'|
           histology_lesion1!='combined LUAD and LCNEC'|
           histology_lesion1!='Pleomorphic carcinoma') %>% 
  mutate(histology = ifelse(histology_lesion1=='Adenosquamous carcinoma', 
                          'Squamous Cell Carcinoma',NA)) %>% 
  mutate_at(vars(histology), ~ifelse(histology_lesion1=='Invasive adenocarcinoma', 
                                     'Adenocarcinoma', .)) %>% 
  mutate_at(vars(histology), ~ifelse(histology_lesion1=='Invasive adenocarcinoma', 
                                     'Adenocarcinoma', .)) %>% 
  mutate_at(vars(histology), ~ifelse(histology_lesion1=='LCNEC', 
                                     'Large cell carcinoma', .))

msk_dta <- msk_dta %>%
  rename(Age = Patient.Current.Age, Overall_survival_months= Overall.Survival..Months.) %>% 
  select(Patient.ID, study, Sex, Age, Overall_survival_months, ethnicity,histology, smoking_status, Metastasis)

write.csv(msk_dta, 'MSK Patient Level Data.csv')

merged_dta <- msk_dta %>% 
  bind_rows(tracer_dta %>% 
              rename(Patient.ID = cruk_id, Age = age, Sex = sex, smoking_status = smoking_status_merged) %>% 
              select(Patient.ID, study, Sex, Age,  Overall_survival_months, ethnicity, 
                     histology, smoking_status, Metastasis))

write.csv(merged_dta, 'Merged data set patient level.csv')
## ------------------------------------------------------------------------
# pmsampsize(type = "b", cstatistic = 0.8, parameters = 4, prevalence = 0.197)
pmsampsize::pmsampsize(type="b",cstatistic = 0.8,parameters = 4,prevalence = 0.197)

my_dta <- read_csv("Merged data set patient level.csv")
# splitting the dataset for training and testing --------------------------
my_dta<-my_dta %>% 
  filter(!is.na(Metastasis))
set.seed(123)  # for reproducibility
churn_split <- initial_split(my_dta, prop = .7, strata = "Metastasis")
churn_train <- training(churn_split)
churn_test  <- testing(churn_split)

my_dta$Metastasis<-factor(my_dta$Metastasis, levels=c(0,1))

# running model -----------------------------------------------------------
#model1 <- glm(
#  Metastasis ~  histology ,
#  family = binomial(link='logit'), 
#  data = churn_train
#)

tidy(model1)
summary(model1)

set.seed(123)
cv_model1 <- train(
  Metastasis ~ smoking_status, 
  data = churn_train %>% 
    filter(!is.na(smoking_status)), 
  method = "glm",
  family = "binomial",
  trControl = trainControl(method = "cv", number = 10)
)
summary(cv_model1)

cv_modelhist <- train(
  Metastasis ~ histology, 
  data = churn_train %>% 
    filter(!is.na(histology)), 
  method = "glm",
  family = "binomial",
  trControl = trainControl(method = "cv", number = 10)
)
summary(cv_modelhist)

set.seed(123)
cv_model2 <- train(
  Metastasis ~ smoking_status+Age, 
  data = churn_train %>% 
    filter(!is.na(smoking_status)&!is.na(Age)), 
  method = "glm",
  family = "binomial",
  trControl = trainControl(method = "cv", number = 10)
)
summary(cv_model2)

set.seed(123)
cv_model3 <- train(
  Metastasis ~ smoking_status+Age+histology, 
  data = churn_train %>% 
    filter(!is.na(smoking_status)&!is.na(Age)&!is.na(histology)), 
  method = "glm",
  family = "binomial",
  trControl = trainControl(method = "cv", number = 10)
)
summary(cv_model3)

set.seed(123)
cv_model4 <- train(
  Metastasis ~ smoking_status+Age+histology+Sex, 
  data = churn_train %>% 
    filter(!is.na(smoking_status)&!is.na(Age)&!is.na(histology)), 
  method = "glm",
  family = "binomial",
  trControl = trainControl(method = "cv", number = 10)
)
summary(cv_model4)

# extract out of sample performance measures

summary(
  resamples(
    list(
      model1 = cv_model1, 
      model2 = cv_model2, 
      model3 = cv_model3, 
      model4=cv_model4, 
      model5=cv_modelhist
    )
  ))

pred_class <- predict(cv_model2, churn_train)

# create confusion matrix
confusionMatrix(
  data = pred_class,
  reference = churn_train$Metastasis
)

# Checking assumptions ----------------------------------------------------
## age gender smoking sub-type 
## checking for multicollinearity

vif(Model1)



