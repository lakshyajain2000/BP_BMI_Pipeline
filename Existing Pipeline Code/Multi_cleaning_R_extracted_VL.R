
# Multi-cleaning R-extracted---- 


rm(list = ls())

wd="D:/PhD_Desktop/NHANES_local/Data/R_extracted"
setwd(wd)


library(plyr)
library(tidyverse)
library(data.table)
library(reticulate)
library(ggplot2)
library(stats)
library(MASS)
library(rrcov)



date <- format(Sys.Date(),"%Y%m%d")

data<-readRDS(file = "D:/PhD_Desktop/NHANES_local/Data/R_extracted/NHANES_Cleaned_single_chol_updated.RDS")

data$non_hdl_clean<-data$tc_clean-data$hdl_clean
data$WHtR_clean<-data$waist_clean/data$height_clean

variables<-c("height_clean","weight_clean","bmi_clean","waist_clean","WHtR_clean","tc_clean","hdl_clean","non_hdl_clean",'sbp_final','dbp_final') 


log<-as.data.frame(variables)
log$values<-rep(0,length(variables))
row.names(log)<-log$variables
log$variables<-NULL
log["height_clean","values"]<-1  # all variables are log transformed except for Height and dbp 
log["dbp_final","values"]<-1



#### Applying the mahalanobis cleaning-----

RF_anthro_age_group<-c("height_clean","weight_clean","bmi_clean","waist_clean","WHtR_clean")
RF_other<-c("tc_clean","hdl_clean","non_hdl_clean",'sbp_final','dbp_final') 

pairs_to_clean=list(c("height_clean","weight_clean"),
                    c("height_clean","bmi_clean"),
                    c("height_clean","waist_clean"),
                    c("height_clean","WHtR_clean"),
                    c("weight_clean","bmi_clean"),
                    c("weight_clean","waist_clean"),
                    c("weight_clean","WHtR_clean"),
                    c("bmi_clean","waist_clean"),
                    c("bmi_clean","WHtR_clean"),
                    c("WHtR_clean","waist_clean"),
                    c("sbp_final","dbp_final"),
                    c("tc_clean","hdl_clean"),
                    #    c("tc_clean","non_hdl_clean"), we will not clean using this pair as it gives strange results
                    c("non_hdl_clean","hdl_clean"))


age5to9   <- which(data$age >= 5  & data$age < 10)
age10to14 <- which(data$age >= 10 & data$age < 15)
age20plus <- which(data$age >= 20 | !(is.na(data$age)))




mahalanobis_outliers=function(data,pairs,level_sd,age_group=age20plus){
  
  level<-(1-pnorm(level_sd))*2
  outliers<-c()
  
  for (pairs in pairs_to_clean){
    print(pairs)
    var1=pairs[1]
    var2=pairs[2]
    d<-data[age_group,c(var1,var2,"id","id_study")]
    
    
    log_1=log[var1,"values"]
    log_2=log[var2,"values"]
    if (log_1==0){d$variable1 = log(d[,var1])
    } else{d$variable1 = d[,var1]}
    
    if (log_2==0){d$variable2 = log(d[,var2])
    } else{d$variable2 = d[,var2]}
    
    a=as.data.frame(d[, c('variable1','variable2')])
    b=covMcd(a,alpha=0.95)
    m_dist <- mahalanobis(a, b$center,b$cov) 
    d$m_dist <- m_dist
    outlier_pair<-d[which(d$m_dist > qchisq(1-level, 2)),"id"]
    outliers<-c(outliers,outlier_pair)
    
    if (length(outlier_pair) == 0) {
      print(paste("No records cleaned for",pairs[1],"/",pairs[2]))
    } else {
      print(paste("N cleaned for",pairs[1],"/",pairs[2]))
      print(length(outlier_pair))
      studies=unique(data$id_study)
      for (study in studies){
        n_tot=length(which(d[which(!is.na(d[,c(var1,var2)])),]$id_study==study))
        n_final=sum(which(data$id_study==study)%in% outlier_pair)
        cleaned=paste0("percentage of outliers detected for ",study,' : ',round((n_final/n_tot)*100,4))
        print(cleaned)
        print("individual values cleaned")
        for (i in seq(1:length(outlier_pair))){
          id<-outlier_pair[i]
          val1<-data[which(data$id==id),var1]
          val2<-data[which(data$id==id),var2]
          print(paste0(var1,"=",val1," ",var2,"=",val2))
        }
        
        data[which(data$id %in% outlier_pair),var1]<-NA
        data[which(data$id %in% outlier_pair),var2]<-NA
      }
    }
    
  }
  
  print("total outlier removed")
  print(length(unique(outliers)))
  return(data[age_group,])
}

sink(paste("D:/PhD_Desktop/NHANES_local/Preprocessing/Scripts/R_extracted/log_files/Multi_Cleaning_RF_",date, ".txt", sep=""))
data_adults=mahalanobis_outliers(data,pairs_to_clean,level_sd=6)
sink()



saveRDS(data_adults, file = "NHANES_multi_cleaned_adults_chol_updated.RDS")

### Sanity checks----
# who is excluded for which pair and if there is any overlapp ---

mahalanobis_outliers_checks=function(data,pairs,level_sd,age_group=age20plus){
  
  level<-(1-pnorm(level_sd))*2
  outliers<-c()
  
  for (pairs in pairs_to_clean){
    print(pairs)
    var1=pairs[1]
    var2=pairs[2]
    d<-data[age_group,c(var1,var2,"id","id_study")]
    
    
    log_1=log[var1,"values"]
    log_2=log[var2,"values"]
    if (log_1==0){d$variable1 = log(d[,var1])
    } else{d$variable1 = d[,var1]}
    
    if (log_2==0){d$variable2 = log(d[,var2])
    } else{d$variable2 = d[,var2]}
    
    a=as.data.frame(d[, c('variable1','variable2')])
    b=covMcd(a,alpha=0.95)
    m_dist <- mahalanobis(a, b$center,b$cov) 
    d$m_dist <- m_dist
    outlier_pair<-d[which(d$m_dist > qchisq(1-level, 2)),"id"]
    outliers<-c(outliers,outlier_pair)
    
    if (length(outlier_pair) == 0) {
      print(paste("No records cleaned for",pairs[1],"/",pairs[2]))
    } else {
      print(paste("N cleaned for",pairs[1],"/",pairs[2]))
      print(length(outlier_pair))
      studies=unique(data$id_study)
      for (study in studies){
        n_tot=length(which(d[which(!is.na(d[,c(var1,var2)])),]$id_study==study))
        n_final=sum(which(data$id_study==study)%in% outlier_pair)
        cleaned=paste0("percentage of outliers detected for ",study,' : ',round((n_final/n_tot)*100,4))
        print(cleaned)
        print("individual values cleaned")
        for (i in seq(1:length(outlier_pair))){
          id<-outlier_pair[i]
          val1<-data[which(data$id==id),var1]
          val2<-data[which(data$id==id),var2]
          print(paste0(var1,"=",val1," ",var2,"=",val2))
        }
        
        data[which(data$id %in% outlier_pair),var1]<-NA
        data[which(data$id %in% outlier_pair),var2]<-NA
      }
    }
    
  }
  
  print("total outlier removed")
  print(length(unique(outliers)))
  return(unique(outliers))
}
# We want to know the number of exclusion only among those that were not alredy excluded (for the flowchart)
outliers<-mahalanobis_outliers_checks(data,pairs_to_clean,level_sd=6) #done before complete cases

data<-data[(data$age>=20)&(!(is.na(data$age))&(data$age<120)),] # keep only participants 20+

data<-data[which((!is.na(data$eGFR_clean))),]
data<-data[which((!is.na(data$height_clean))),]
data<-data[which((!is.na(data$bmi_clean))),]         
data<-data[which((!is.na(data$WHtR_clean))),]            
data<-data[which((!is.na(data$hba1c_clean))),]    
data<-data[which((!is.na(data$hdl_clean))),] 
data<-data[which((!is.na(data$non_hdl_clean))),] 
data<-data[which((!is.na(data$tc_clean))),] 
data<-data[which((!is.na(data$sbp_final))),] 
data<-data[which((!is.na(data$dbp_final))),] 

length(intersect(outliers,data$id))


