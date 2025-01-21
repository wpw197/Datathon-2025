library(glm2)
Model1 <- glm(Met~ age * sex *smoking_status_merged* histology_multi_full_genomically.confirmed,family=binomial(link='logit'),data=TRACEx_pts_df.csv)
summary(Model1)



Model2 <- glm(Met~ age * sex *packyears* histology_multi_full_genomically.confirmed,family=binomial(link='logit'),data=TRACEx_pts_df.csv)
summary(Model2)



