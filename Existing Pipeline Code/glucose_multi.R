# multi var LOF cleaning - prepping input for individual level CW
#AR 14/03/23

library(tidyverse)
library(doRNG)
library(doFuture)

setwd('S:/Projects/HeightProject/Original dataset/Glucose/glucose_individual_data/multiple_cleaning')
set.seed(2022)
date <- format(Sys.Date(), '%Y%m%d')

d <- readRDS("S:/Projects/HeightProject/Original dataset/Glucose/glucose_individual_data/data_cleaned/Glucose_individual_data_cleaned_wBMI.RDS")

data <- d[!is.na(d$hba1c_f)&!is.na(d$fgl_f),] ## had to add this?
df_perturb <- data
list <- which(abs(round(df_perturb$hba1c_f*10) - df_perturb$hba1c_f*10)<1e-5)
df_perturb$hba1c_f[list] <- df_perturb$hba1c_f[list] + runif(length(list), -0.05, 0.05)
list <- which(abs(round(df_perturb$fgl_f*10) - df_perturb$fgl_f*10)<1e-5)
df_perturb$fgl_f[list] <- df_perturb$fgl_f[list] + runif(length(list), -0.05, 0.05)
list <- which(abs(round(df_perturb$fgl_f*18) - df_perturb$fgl_f*18)<0.2) # FPG could also heap by 18 due to conversions from mg/dL
df_perturb$fgl_f[list] <- df_perturb$fgl_f[list] + runif(length(list), -1/36, 1/36)

clean_index <- DDoutlier::LOF(df_perturb %>% select(fgl_f, hba1c_f), k = 100)
df_perturb$marker <- clean_index
num_gone <- sum(clean_index>=2)# number of data points cleaned
print(num_gone)
table(clean_index>=2, is.na(data$self_diab))


#need to save as pdf and add number of points cleaned to pdf

pdf("LOF_cleaned_fgl_hba1c_scatter.pdf", height=10, width=12)

ggplot(df_perturb %>% subset(marker<2), aes(hba1c_f, fgl_f)) + geom_point(size=0.5, colour='grey') +
  geom_point(data = df_perturb %>% subset(marker>=2), colour='red')+
  annotate('text', x=10, y=2, label= paste0("Number of removed points = ", num_gone), size=4)

dev.off()

data$fgl_f[clean_index>=2] <- NA
data$hba1c_f[clean_index>=2] <- NA

#now need to attach the newly cleaned hba1c and fgl back to the full dataset
#print(c(sum(is.na(d$hba1c_f)),c(sum(is.na(d$hba1c_f))))) #CHECKS
d[!is.na(d$hba1c_f)&!is.na(d$fgl_f),c("fgl_f","hba1c_f")] <- data[,c("fgl_f","hba1c_f")]
#print(c(sum(is.na(d$hba1c_f)),c(sum(is.na(d$hba1c_f)))))

saveRDS(d, 'LOFcleaned_Glucose_individual_data_wBMI.RDS')
saveRDS(d, paste0('old/fully_LOFcleaned_Glucose_individual_data_wBMI_', date, '.RDS'))
