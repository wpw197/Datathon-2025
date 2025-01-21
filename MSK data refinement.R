install.packages("dplyr")  # Only needed if not already installed
library(dplyr)


mixed_data <- read.table("combo_data.tsv",
                         sep = "\t", header = TRUE)

MSK <- mixed_data %>% filter(Study.ID == "nsclc_ctdx_msk_2022")

a <- MSK %>% distinct("Smoker.Status")


refined_MSK <- MSK %>% select( "Study.ID", "Patient.ID", "Sample.ID",
                              "Cancer.Type", "Cancer.Type.Detailed","NSCLC.SubType",
                              "Oncotree.Code", "Overall.Survival..Months.", "Sex",
                              "Smoking.Status","Histology","Metastatic.Site",
                              "Prior.Treatment")