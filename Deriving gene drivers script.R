install.packages('tidyverse')
library(tidyverse)

data %>% 
  group_by(patient_id) %>% 
  mutate(KRAS = ifelse())

n_pats <- data %>% 
  distinct(patient_id)

out<-data.frame(patient_id = n_pats$patient_id, 
                KRAS=rep(NA, dim(n_pats)[1]),
               TP53=rep(NA, dim(n_pats)[1]),
               KMT2D=rep(NA, dim(n_pats)[1]),
               STK11 =rep(NA, dim(n_pats)[1]),
               SMARCA4=rep(NA, dim(n_pats)[1]),
               KEAP1=rep(NA, dim(n_pats)[1]),
               FAT1=rep(NA, dim(n_pats)[1]),
               NF1=rep(NA, dim(n_pats)[1]),
               CDKN2A=rep(NA, dim(n_pats)[1]),
               PTPRB=rep(NA, dim(n_pats)[1]),
               RBM10=rep(NA, dim(n_pats)[1]),
               PIK3CA=rep(NA, dim(n_pats)[1]),
               ARID1A=rep(NA, dim(n_pats)[1]),
               CREBBP=rep(NA, dim(n_pats)[1]),
               COL5A2=rep(NA, dim(n_pats)[1]),
               CUXI=rep(NA, dim(n_pats)[1]),
               SETD2=rep(NA, dim(n_pats)[1]),
               FBXW7=rep(NA, dim(n_pats)[1]),
               EGFR=rep(NA, dim(n_pats)[1]),
               ARHGAP35=rep(NA, dim(n_pats)[1])
               )


for(i in 1:dim(n_pats)[1]){
  
  genes<-data %>% 
    filter(patient_id==n_pats$patient_id[i]) %>% 
    select(Hugo_Symbol)
  
  if('KRAS' %in% genes$Hugo_Symbol){
    
    out$KRAS[i] <- 1
    
  }else{
    
    out$KRAS[i] <- 0
    
  }
  
  if('TP53' %in% genes$Hugo_Symbol){
    
    out$TP53[i] <- 1
    
  }else{
    
    out$TP53[i] <- 0
    
  }
  
  if('KMT2D' %in% genes$Hugo_Symbol){
    
    out$KMT2D[i] <- 1
    
  }else{
    
    out$KMT2D[i] <- 0
  }
  
  if('STK11' %in% genes$Hugo_Symbol){
    
    out$STK11[i] <- 1
    
  }else{
    
    out$STK11[i] <- 0 
    
  }
  
  if('SMARCA4' %in% genes$Hugo_Symbol){
    
    out$SMARCA4[i] <- 1
    
  }else{
    
    out$SMARCA4[i] <- 0 
    
  }
  
  if('KEAP1' %in% genes$Hugo_Symbol){
    
    out$KEAP1[i] <- 1
    
  }else{
    
    out$KEAP1[i] <- 0
    
  }
  
  if('FAT1'  %in% genes$Hugo_Symbol){
    
    out$FAT1[i] <- 1
    
  }else{
    out$FAT1[i] <- 0 
  }
  
  if('NF1'  %in% genes$Hugo_Symbol){
    
    out$NF1[i] <- 1 
    
  }else{
    out$NF1[i] <- 0 
    
  }
  
  if('CDKN2A'  %in% genes$Hugo_Symbol){
    
    out$CDKN2A[i] <- 1 
    
  }else{
    out$CDKN2A[i] <- 0
  }
  
  
  if('PTPRB' %in% genes$Hugo_Symbol){
    
    out$PTPRB[i] <- 1
  }else{
    out$PTPRB[i] <- 0 
    
  }
  
  if('RBM10'  %in% genes$Hugo_Symbol){
    out$RBM10[i] <- 1
    
  }else{
    out$RBM10[i] <- 0 
  }
  
  if('PIK3CA' %in% genes$Hugo_Symbol){
    out$PIK3CA[i]<-1
  }
  else{
    out$PIK3CA[i]<-0
  }
  
  if('ARID1A' %in% genes$Hugo_Symbol){
    out$ARID1A[i]<-1
  }else{
    out$ARID1A[i] <-0
  }
  
  if('CREBBP' %in% genes$Hugo_Symbol){
    out$CREBBP[i] <- 1
  }else{
    out$CREBBP[i]<-0
  }
  
  if('COL5A2'  %in% genes$Hugo_Symbol){
    out$COL5A2[i] <- 1
  }else{
    out$COL5A2[i]<-0
  }
  
  if('CUXI'  %in% genes$Hugo_Symbol){
    out$CUXI[i] <- 1
  }else{
    out$CUXI[i] <- 0 
  }
  
  if('SETD2'  %in% genes$Hugo_Symbol){
    out$SETD2[i] <- 1
  }else{
    out$SETD2[i] <- 0
  }
  
  if('FBXW7' %in% genes$Hugo_Symbol){
    out$FBXW7[i]<-1
  }else{
    out$FBXW7[i]<-0
  }
  
  if('EGFR'  %in% genes$Hugo_Symbol){
    out$EGFR[i] <- 1
  }else{
    out$EGFR[i] <- 0 
  }
  
  if('ARHGAP35' %in% genes$Hugo_Symbol){
    out$ARHGAP35[i] <- 1
  }else{
    out$ARHGAP35[i] <- 0
  }
  
}

TRACERx_Gene_Drivers <- out

write.csv(TRACERx_Gene_Drivers, 'TRACERx_Gene_Drivers.csv')
