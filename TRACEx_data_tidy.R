#### Load TRACEx datasets ####
all_patients_df <- readRDS("figurecode/data/20221109_TRACERx421_all_patient_df.rds")

#### Cleaning up Supplementary_Table_14 ####
supp_t14 <- readxl::read_excel("Supp_Table_14.xlsx")
supp_t14 <- supp_t14[-c(1:4),]
colnames(supp_t14) <- supp_t14[1,]
supp_t14 <- supp_t14[-1,-c(2,4)]
names(supp_t14)[names(supp_t14) == "PublicationID"] <- "cruk_id"
cols_to_modify <- c("Surgical_bed_recurrence", "Ipsilateral_lung", "Mediastinal_lung", "Contralateral_lung", "Pleural_nodules_effusion", "Liver",
                    "Adrenal", "Bone", "Brain", "Extrathoracic_lymph_nodes", "Other", "Notes")
supp_t14 <- supp_t14 %>% rename_with(~ paste0("MET_SITE_", .), all_of(cols_to_modify))
write.csv(supp_t14, "Supp_Table_14_amended.xlsx")

#### Merge dfs together for TRACEx ####
tracex_met_pts_df <- left_join(all_patients_df, supp_t14, by = "cruk_id")
tracex_met_pts_df$Relapse_cat.x <- NULL
rows_with_na <- tracex_met_pts_df[is.na(tracex_met_pts_df$age) | 
                                    is.na(tracex_met_pts_df$sex) | 
                                    is.na(tracex_met_pts_df$ethnicity), ]

write.csv(tracex_met_pts_df, "TRACEx_pts_df.csv")
