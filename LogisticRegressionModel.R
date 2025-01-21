library(glm2)
Model1 <- glm(Metastasis~ age+ sex+smoking_status_merged+ histology_multi_full_genomically.confirmed,family=binomial(link='logit'),data=TRACEx_pts_df)
summary(Model1)

table(TRACEx_pts_df$Relapse_cat_new)
TRACEx_pts_df$NoMet<-NoMet
TRACEx_pts_df[which(TRACEx_pts_df$Relapse_cat_new=="No rec"), NoMet] <- "No"

TRACEx_pts_df <- TRACEx_pts_df %>%
  mutate(Metastasis = ifelse( Relapse_cat_new == 'Extrathoracic'| 
                                Relapse_cat_new == 'Intra & Extra'| 
                                Relapse_cat_new == 'Intrathoracic', 1, NA)) %>% 
  mutate_at(vars(Metastasis), ~ifelse(Relapse_cat_new == 'No rec', 0, . )) %>% 
  mutate_at(vars(Metastasis), ~ifelse(Relapse_cat_new == 'Unknown Site', NA, . )) 

Model2 <- glm(Metastasis~ age + sex +cigs_perday+ histology_multi_full_genomically.confirmed,family=binomial(link='logit'),data=TRACEx_pts_df)
summary(Model2)

Model3 <- glm(Metastasis~ age + sex +packyears,family=binomial(link='logit'),data=TRACEx_pts_df)
summary(Model3)

Model4 <- glm(Metastasis~ age + sex+cigs_perday,family=binomial(link='logit'),data=TRACEx_pts_df)
summary(Model4)
