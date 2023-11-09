# Bin Zhou
# 3 Feb 2015
# Generating summary prevalence for blood pressure, including STEPS and DHS and summary data
# adult only
# with quantiles
# added new defs 11 to 16, 6 June 2015
# added correction for single PSU/stratum in the whole study (avoid spurious SEs), 6 June 2015
# added new defs 17, 18, 15 April 2016
# corrected min age for adults 25 April 2016
# added 8th measurements for INTERMAP 4 September 2016
# corrected for PYF STEPS: 1st BP measurements were dropped before providing to WHO
# moved CW metrics to optional, added 135/85 metrics for UHC, 20180507
# added new 160/100 DxTx summaries as in new summary sheets (BPDxNew)
# updated sbp/dbp_final algorithm 20220710

## Needs rewriting to be cleaner

library(survey)
library(tidyverse)
library(dtplyr)
library(data.table)
library(doParallel)
date<-format(Sys.Date(),"%Y%m%d")

data <- readRDS("/Volumes/HeightProject/Original dataset/Blood Pressure/BP_individual_data/data_cleaned/BP_individual_data_cleaned_wBMI.RDS")

wd <- "~/Documents/Pipeline/BP_BMI_Pipeline"

CW <- FALSE
DIST <- FALSE
UHC <- TRUE
UHC_test <- TRUE
UHC_allcat <- FALSE
BMI <- FALSE
mergeYFS <- FALSE

# Clean dataset ====
# remove excluded surveys
data <- subset(data,data$excluded!=1|is.na(data$excluded))

# merge YFS_urban/rural to one study for UHC works
# as id_study changed, their single-year age groups will not be taken care of, so summaries will be in usual 10-year age groups
# add as a new study  17 July 2018
if (mergeYFS) {
    yfs <- data[grep("YFS", data$survey_short), ]
    yfs$id_study <- as.character(yfs$id_study)
    yfs$id_study <- gsub("_YFS_urban", "_YFS_merge", yfs$id_study)
    yfs$id_study <- gsub("_YFS_rural", "_YFS_merge", yfs$id_study)
    yfs$survey_short <- as.character(yfs$survey_short)
    yfs$survey_short <- gsub("YFS_urban", "YFS_merge", yfs$survey_short)
    yfs$survey_short <- gsub("YFS_rural", "YFS_merge", yfs$survey_short)
    data <- rbind(data, yfs)
}

# use age_mean when age is missing for USA EPESEs
list <- which(!is.na(data$age_mean) & is.na(data$age))
print(unique(data$id_study[list]))
data$age[list]  <- data$age_mean[list]

# only keep those over 10 years old   # to be changed when moving to children/adolescents as well
data <- data[which(data$age>=18),]
data$age_min_bp_M[which(data$age_min_bp_M<18)] <- 18
data$age_min_bp_F[which(data$age_min_bp_F<18)] <- 18

# drop records cleaned during data cleaning
data <- data[which(data$dropped=="Keep"|data$dropped=="NoData"),]
# data <- data[which(data$dropped=="Keep"),]

# age are already in age groups for US EPESEs, FHS, PRHHP and CHS; and HSE 2015
list <- which(!is.na(data$age_group) & !data$id_study%in%c("GBR_2015_HSE","GBR_2016_HSE"))  # drop HSE as it can be taken care of with normal methods
print(unique(data$id_study[list]))
data$age_mean0 <- data$age_group0 <- NA
data$age_mean0[list]  <- data$age_mean[list]
data$age_group0[list] <- as.character(data$age_group[list])

# Categorize age groups ====
data$age_min  <- with(data, ifelse(sex=="male", age_min_bp_M, age_min_bp_F))
data$age_max  <- with(data, ifelse(sex=="male", age_max_bp_M, age_max_bp_F))
# generate standard age groups, regardlesss of design age ranges
data$age_mean <- with(data, ifelse(age<20, 15, ifelse(age<30, 25, ifelse(age<40, 35, ifelse(age<50, 45, ifelse(age<60, 55, ifelse(age<70, 65, ifelse(age<80, 75, 84.91))))))) )
# generate age group
data$age_group <- paste(data$age_mean-5,data$age_mean+4,sep="-")
data$age_group[which(data$age_mean==84.91)] <- "80+"
# if age in the first age group, correct mean age if needed
# current age group is [age_mean-5, age_mean+4]
# if age_mean-5<age_min, first age group should be corrected
# if age_mean+4>age_max, last age group should be corrected
# do not change the 80+ age group
x1 <- which(data$age_mean-5<data$age_min & data$age_mean!=84.91)
x2 <- which(data$age_mean+4>data$age_max & data$age_mean!=84.91)
xx1 <- x1[!x1%in%x2]; xx2 <- x2[!x2%in%x1]
xx  <- x1[x1%in%x2]
data$age_group[xx1] <- paste(data$age_min[xx1],data$age_mean[xx1]+4,sep="-")
data$age_group[xx2] <- paste(data$age_mean[xx2]-5,data$age_max[xx2],sep="-")
data$age_mean[xx1]  <- (data$age_mean[xx1] + 4 + data$age_min[xx1] +1 )/2
data$age_mean[xx2]  <- (data$age_mean[xx2] - 5 + data$age_max[xx2] +1 )/2
data$age_mean[xx]   <- (data$age_min[xx] + data$age_max[xx] + 1)/2
data$age_group[xx]  <- paste(data$age_min[xx],data$age_max[xx],sep="-")
rm(x1,x2,xx1,xx2,xx)
# change the mean age for the 80+ age group if the study is not open-ended (max age >=80 but !=200)
x <- which(data$age_max>=80&data$age_max<200 & data$age_mean==84.91)
y <- ifelse(data$age_min<80,80,data$age_min)
data$age_mean[x]  <- (y[x]+data$age_max[x]+1)/2
data$age_group[x] <- paste(y[x],data$age_max[x],sep="-")

# change the mean age when min age = max age (single age group)
x <- which(as.numeric(substr(data$age_group,1,2))==as.numeric(substr(data$age_group,4,5)))
data$age_mean[x]  <- as.numeric(substr(data$age_group,1,2))[x]

# special single age group treatment for Finland YFS, Sweden PSWG
x <- which(data$survey_short%in%c("YFS_urban","YFS_rural","PSWG","Oulu35"))
data$age_mean[x] <- data$age[x]
data$age_group[x] <- paste(data$age[x],data$age[x],sep="-")

# age are already in age groups for US EPESEs, CHS, FHS and PRHHP; and HSE 2015
data$age_mean[list]  <- data$age_mean0[list]
data$age_group[list] <- data$age_group0[list]
data$age_mean0 <- data$age_group0 <- NULL

# split 60-69 in GBR_2000/2005/2006_HSE; DX/TX was asked 65+; 20180422
list1 <- which(data$id_study%in%c("GBR_2000_HSE","GBR_2005_HSE","GBR_2006_HSE")&data$age_group=="60-69"&data$age<65)
list2 <- which(data$id_study%in%c("GBR_2000_HSE","GBR_2005_HSE","GBR_2006_HSE")&data$age_group=="60-69"&data$age>=65)
data$age_group[list1] <- "60-64"
data$age_max[list1] <- 64; data$age_mean[list1] <- (60+65)/2
data$age_group[list2] <- "65-69"
data$age_min[list2] <- 65; data$age_mean[list2] <- (65+70)/2

# split 30-39 in BRA_2013_PNS; DX/TX was asked 35+; 20191107
list1 <- which(data$id_study%in%c("BRA_2013_PNS")&data$age_group=="30-39"&data$age<35)
list2 <- which(data$id_study%in%c("BRA_2013_PNS")&data$age_group=="30-39"&data$age>=35)
data$age_group[list1] <- "30-64"
data$age_max[list1] <- 34; data$age_mean[list1] <- (30+35)/2
data$age_group[list2] <- "35-39"
data$age_min[list2] <- 35; data$age_mean[list2] <- (35+40)/2

### Calculate average BP ====
s0 = proc.time()
sbp_except_first <- rowMeans(data[,c("sbp2_f","sbp3_f","sbp4_f","sbp5_f","sbp6_f","sbp7_f","sbp8_f","sbp9_f","sbp10_f","sbp11_f","sbp12_f","sbp13_f")], na.rm=TRUE)
dbp_except_first <- rowMeans(data[,c("dbp2_f","dbp3_f","dbp4_f","dbp5_f","dbp6_f","dbp7_f","dbp8_f","dbp9_f","dbp10_f","dbp11_f","dbp12_f","dbp13_f")], na.rm=TRUE)
# take 1st measurement if nothing else is available
sbp_comb_with_first <- coalesce(sbp_except_first, data$sbp1_f)
dbp_comb_with_first <- coalesce(dbp_except_first, data$dbp1_f)
# take average measurement is nothing else is available
data$sbp_final <- coalesce(sbp_comb_with_first, data$sbp_avg_f)
data$dbp_final <- coalesce(dbp_comb_with_first, data$dbp_avg_f)
print(proc.time() - s0)

# Function version: too slow
calc_avg <- function(x){
  avg <- x[1]; x1 <- x[2]; x2 <- x[-c(1,2)]
  res <- mean(x2, na.rm=TRUE)
  if (is.na(res)) res <- x1
  if (is.na(res)) res <- avg
  return (res)
}
# old algorithm: drop first available measurement
# calc_avg0 <- function(x){
#     avg <- x[1]; x <- x[-1]
#     pos <- which(!is.na(x))
#     if (length(pos)==0) res <- avg else
#         if (length(pos)==1) res <- x[pos] else
#             if (length(pos)==2) res <- x[pos[2]] else
#                 res <- mean(x[pos[2:length(pos)]])
#     return (res)
# }
#
# s0 = proc.time()
# data$sbp_final0 <- apply(data[,c("sbp_avg_f","sbp1_f","sbp2_f","sbp3_f","sbp4_f","sbp5_f","sbp6_f","sbp7_f","sbp8_f","sbp9_f","sbp10_f","sbp11_f","sbp12_f","sbp13_f")],1,calc_avg)
# data$dbp_final0 <- apply(data[,c("dbp_avg_f","dbp1_f","dbp2_f","dbp3_f","dbp4_f","dbp5_f","dbp6_f","dbp7_f","dbp8_f","dbp9_f","dbp10_f","dbp11_f","dbp12_f","dbp13_f")],1,calc_avg)
# print(proc.time() - s0)
#
# s0 = proc.time()
# data <- data %>%
#     mutate(sbp_final2 = pmap_dbl(select(., sbp_avg_f, matches('^sbp.*_f$')), function(...) calc_avg(c(...))),
#            dbp_final2 = pmap_dbl(select(., dbp_avg_f, matches('^dbp.*_f$')), function(...) calc_avg(c(...))))
# print(proc.time() - s0)

# Special cases: didn't change to new syntax (yet)
# assign mean sbp/dbp in Ancianos Spain
# treat 6 measurement as 2 sets of 3 measurements, calculate means for 2 sets, and then average the two means
x<-which(data$id_study%in%c("ESP_2001_Ancianos", "POL_2009_PolSenior") )
sbp_avg_1 <- apply(data[x,c("sbp_avg_f","sbp1_f","sbp2_f","sbp3_f")],1,calc_avg)
sbp_avg_2 <- apply(data[x,c("sbp_avg_f","sbp4_f","sbp5_f","sbp6_f")],1,calc_avg)
dbp_avg_1 <- apply(data[x,c("dbp_avg_f","dbp1_f","dbp2_f","dbp3_f")],1,calc_avg)
dbp_avg_2 <- apply(data[x,c("dbp_avg_f","dbp4_f","dbp5_f","dbp6_f")],1,calc_avg)
data$sbp_final[x] <- apply(cbind(sbp_avg_1,sbp_avg_2),1,function(x)mean(x,na.rm=TRUE))
data$dbp_final[x] <- apply(cbind(dbp_avg_1,dbp_avg_2),1,function(x)mean(x,na.rm=TRUE))
sbp_avg_1 <- sbp_avg_2 <- dbp_avg_1 <- dbp_avg_2 <- NULL

# assign mean sbp/dbp in INTERMAP
# treat 8 measurement as 4 sets of 2 measurements, drop the first 4 and use the second 4
x<-grep("INTERMAP", data$id_study)
data$sbp_avg_f[x] <- NA
data$dbp_avg_f[x] <- NA
sbp_avg_1 <- apply(data[x,c("sbp_avg_f","sbp1_f","sbp2_f")],1,calc_avg)
sbp_avg_2 <- apply(data[x,c("sbp_avg_f","sbp3_f","sbp4_f")],1,calc_avg)
sbp_avg_3 <- apply(data[x,c("sbp_avg_f","sbp5_f","sbp6_f")],1,calc_avg)
sbp_avg_4 <- apply(data[x,c("sbp_avg_f","sbp7_f","sbp8_f")],1,calc_avg)
dbp_avg_1 <- apply(data[x,c("dbp_avg_f","dbp1_f","dbp2_f")],1,calc_avg)
dbp_avg_2 <- apply(data[x,c("dbp_avg_f","dbp3_f","dbp4_f")],1,calc_avg)
dbp_avg_3 <- apply(data[x,c("dbp_avg_f","dbp5_f","dbp6_f")],1,calc_avg)
dbp_avg_4 <- apply(data[x,c("dbp_avg_f","dbp7_f","dbp8_f")],1,calc_avg)
data$sbp_final[x] <- apply(cbind(sbp_avg_1,sbp_avg_2,sbp_avg_3,sbp_avg_4),1,function(x)mean(x,na.rm=TRUE))
data$dbp_final[x] <- apply(cbind(dbp_avg_1,dbp_avg_2,dbp_avg_3,dbp_avg_4),1,function(x)mean(x,na.rm=TRUE))
sbp_avg_1 <- sbp_avg_2 <- dbp_avg_1 <- dbp_avg_2 <- NULL
sbp_avg_3 <- sbp_avg_4 <- dbp_avg_3 <- dbp_avg_4 <- NULL

# assign mean sbp/dbp in PYF STEPS and ITA CONVERGI
# 1st measurements were dropped already: take the average of the remaining two
# no longer needed under new algorithm
# x<-which(data$id_study%in%c("PYF_2010_STEPS", "ITA_2011_CONVERGI"))
# data$sbp_final[x] <- apply(data[x,c("sbp_avg_f","sbp2_f","sbp2_f","sbp3_f")],1,calc_avg)
# data$dbp_final[x] <- apply(data[x,c("dbp_avg_f","dbp2_f","dbp2_f","dbp3_f")],1,calc_avg)

# assign mean sbp/dbp in GRC Didima and POL_2011_NATPOL
# treat 6 measurement as 2 sets of 3 measurements, drop the 1st and 4th measurements
x<-which(data$id_study%in%c("GRC_1997_Didima","POL_2011_NATPOL","POL_2002_NATPOL"))
data$sbp_final[x] <- apply(data[x,c("sbp_avg_f","sbp1_f","sbp2_f","sbp3_f","sbp5_f","sbp6_f")],1,calc_avg)
data$dbp_final[x] <- apply(data[x,c("dbp_avg_f","dbp1_f","dbp2_f","dbp3_f","dbp5_f","dbp6_f")],1,calc_avg)

saveRDS(data, file=paste0(wd,"Data_cleaned_with_average_SBP_DBP.RDS"))
# file.copy(from = paste0(wd,"Data_cleaned_with_average_SBP_DBP.RDS"), to = paste0(wd,"old/Data_cleaned_with_average_SBP_DBP_",date,".RDS"), overwrite=TRUE)

## Calculating diagnosis ====
# 00. Diagnosis: cleaned self_hyper when available; self_hyper_12mos when not
data$diagnosis <- data$self_hyper
l1 <- unique(data$id_study[!is.na(data$self_hyper)])
l2 <- unique(data$id_study[!is.na(data$self_hyper_12mos)])
list <- l2[!l2%in%l1]
data$diagnosis[data$id_study%in%list] <- data$self_hyper_12mos[data$id_study%in%list]
# 0. New_med: cleaned drug_hyper
# if new_med=1 but diagnosis=0, recode diagnosis to 1
data$new_med <- data$drug_hyper
data$diagnosis[which(data$diagnosis==0 & data$new_med==1)] <- 1  # decided on call 20180412
core_list <- c("BP_140","BP_140_90","HTN_140_90_med","HTN_160_95_med","ctrlHTN_140_90_med","HTN_160_100_med")
core_num  <- c(1:5,15)
# 1. Raised BP defind as: SBP >= 140
data$BP_140 <- 0
data$BP_140[which(data$sbp_final>=140)] <- 1
data$BP_140[which(is.na(data$sbp_final))] <- NA
# 2. Raised BP defind as: SBP >= 140 OR DBP >= 90
data$BP_140_90 <- 0
data$BP_140_90[which(data$sbp_final>=140|data$dbp_final>=90)] <- 1
data$BP_140_90[which(is.na(data$sbp_final)|is.na(data$dbp_final))] <- NA
# 3. HTN defind as: SBP >= 140 OR DBP >= 90 OR medication
data$HTN_140_90_med <- 0
data$HTN_140_90_med[which(data$sbp_final>=140|data$dbp_final>=90|data$new_med==1)] <- 1
data$HTN_140_90_med[which(is.na(data$sbp_final)|is.na(data$dbp_final)|is.na(data$new_med))] <- NA
# 4. HTN defind as: SBP >= 160 OR DBP >= 95 OR medication
data$HTN_160_95_med <- 0
data$HTN_160_95_med[which(data$sbp_final>=160|data$dbp_final>=95|data$new_med==1)] <- 1
data$HTN_160_95_med[which(is.na(data$sbp_final)|is.na(data$dbp_final)|is.na(data$new_med))] <- NA
# 5. Controlled HTN defind as: medication AND SBP < 140 AND DBP < 90
data$ctrlHTN_140_90_med <- 0
data$ctrlHTN_140_90_med[which(data$sbp_final<140&data$dbp_final<90&data$new_med==1)] <- 1
data$ctrlHTN_140_90_med[which(is.na(data$sbp_final)|is.na(data$dbp_final)|is.na(data$new_med))] <- NA
# 15. New diagnosis for CW: 160_100_med
data$HTN_160_100_med <- 0
data$HTN_160_100_med[which(data$sbp_final>=160|data$dbp_final>=100|data$new_med==1)] <- 1
data$HTN_160_100_med[which(is.na(data$sbp_final)|is.na(data$dbp_final)|is.na(data$new_med))] <- NA

CW_list <- CW_num <- c()
if (CW) {
CW_list <- c("BP_160_90","BP_160_95","HTN_130_80_med","HTN_135_85_med","BP_160","BP_150_90","BP_130_85","HTN_130_85_med","HTN_140_and_90_med","dbp_95","BP_135_85")
CW_num  <- c(6:9,11:14,16:18)
# 6. New diagnosis for CW: 160_90
data$BP_160_90 <- 0
data$BP_160_90[which(data$sbp_final>=160|data$dbp_final>=90)] <- 1
data$BP_160_90[which(is.na(data$sbp_final)|is.na(data$dbp_final))] <- NA
# 7. New diagnosis for CW: 160_95
data$BP_160_95 <- 0
data$BP_160_95[which(data$sbp_final>=160|data$dbp_final>=95)] <- 1
data$BP_160_95[which(is.na(data$sbp_final)|is.na(data$dbp_final))] <- NA
# 8. New diagnosis for CW: 130_80_med
data$HTN_130_80_med <- 0
data$HTN_130_80_med[which(data$sbp_final>=130|data$dbp_final>=80|data$new_med==1)] <- 1
data$HTN_130_80_med[which(is.na(data$sbp_final)|is.na(data$dbp_final)|is.na(data$new_med))] <- NA
# 9. New diagnosis for CW: 135_85_med
data$HTN_135_85_med <- 0
data$HTN_135_85_med[which(data$sbp_final>=135|data$dbp_final>=85|data$new_med==1)] <- 1
data$HTN_135_85_med[which(is.na(data$sbp_final)|is.na(data$dbp_final)|is.na(data$new_med))] <- NA
# 11. New diagnosis for CW: 160
data$BP_160 <- 0
data$BP_160[which(data$sbp_final>=160)] <- 1
data$BP_160[which(is.na(data$sbp_final))] <- NA
# 12. New diagnosis for CW: 150_90
data$BP_150_90 <- 0
data$BP_150_90[which(data$sbp_final>=150|data$dbp_final>=90)] <- 1
data$BP_150_90[which(is.na(data$sbp_final)|is.na(data$dbp_final))] <- NA
# 13. New diagnosis for CW: 130_85
data$BP_130_85 <- 0
data$BP_130_85[which(data$sbp_final>=130|data$dbp_final>=85)] <- 1
data$BP_130_85[which(is.na(data$sbp_final)|is.na(data$dbp_final))] <- NA
# 14. New diagnosis for CW: 130_85_med
data$HTN_130_85_med <- 0
data$HTN_130_85_med[which(data$sbp_final>=130|data$dbp_final>=85|data$new_med==1)] <- 1
data$HTN_130_85_med[which(is.na(data$sbp_final)|is.na(data$dbp_final)|is.na(data$new_med))] <- NA
# 16. HTN defind as: SBP >= 140 AND DBP >= 90 OR medication
data$HTN_140_and_90_med <- 0
data$HTN_140_and_90_med[which((data$sbp_final>=140&data$dbp_final>=90)|data$new_med==1)] <- 1
data$HTN_140_and_90_med[which(is.na(data$sbp_final)|is.na(data$dbp_final)|is.na(data$new_med))] <- NA
# 17. New diagnosis for CW: dbp_95
data$dbp_95 <- 0
data$dbp_95[which(data$dbp_final>=95)] <- 1
data$dbp_95[which(is.na(data$dbp_final))] <- NA
# 18. New diagnosis for CW: 135_85
data$BP_135_85 <- 0
data$BP_135_85[which(data$sbp_final>=135|data$dbp_final>=85)] <- 1
data$BP_135_85[which(is.na(data$sbp_final)|is.na(data$dbp_final))] <- NA

}

UHC_list <- UHC_num <- c()
if (UHC) {
UHC_list <- c("ratio_treat_over_diag","HTN_140_90_med_wdiag","HTN_160_100_med_wdiag","ratio_diag_HTN_140_90_med_wdiag","ratio_diag_HTN_160_100_med_wdiag","ratio_treat_HTN_140_90_med_wdiag","ratio_treat_HTN_160_100_med_wdiag","ratio_ctrl_HTN_140_90_med_wdiag","ratio_ctrl_HTN_160_100_med_wdiag","ratio_ctrl_130_80_HTN_140_90_med_wdiag","ratio_ctrl_135_85_HTN_140_90_med_wdiag","ratio_130_80_over_not_HTN_140_90_med_wdiag","ratio_135_85_over_not_HTN_140_90_med_wdiag","ratio_treat_DxHTN_140_90_med_wdiag","ratio_treat_DxHTN_160_100_med_wdiag","ratio_ctrl_TxHTN_140_90_med_wdiag","ratio_ctrl_TxHTN_160_100_med_wdiag",   "HTN_160_100_wmed_wdiag", "ratio_diag_HTN_160_100_wmed_wdiag", "ratio_treat_HTN_160_100_wmed_wdiag","HTN_160_100_wmed","ratio_treat_HTN_160_100_wmed","BP_160_100")
UHC_num  <- c(26,34:41,45:58)
## For treatment coverage works
# ----------------------------------
# 26. Percentage of treated over diagnosed
data$ratio_treat_over_diag <- NA
data$ratio_treat_over_diag[which(data$diagnosis==1 & !is.na(data$new_med))] <- 0
data$ratio_treat_over_diag[which(data$diagnosis==1 & data$new_med==1)] <- 1
# ----------------------------------
## new variables 20180413 ##
# 34. HTN defined as: SBP >= 140 OR DBP >= 90 OR medicaiton, AND diagnosis available
data$HTN_140_90_med_wdiag <- data$HTN_140_90_med
data$HTN_140_90_med_wdiag[is.na(data$diagnosis)] <- NA
# 35. HTN defined as: SBP >= 160 OR DBP >= 100 OR medicaiton, AND diagnosis available
data$HTN_160_100_med_wdiag <- data$HTN_160_100_med
data$HTN_160_100_med_wdiag[is.na(data$diagnosis)] <- NA
# ----------------------------------
# 36. Percentage of diagnosed over HTN_140_90_med_wdiag
data$ratio_diag_HTN_140_90_med_wdiag <- NA
data$ratio_diag_HTN_140_90_med_wdiag[which(data$HTN_140_90_med_wdiag==1)] <- 0
data$ratio_diag_HTN_140_90_med_wdiag[which(data$diagnosis==1 & (data$HTN_140_90_med_wdiag==1))] <- 1
# 37. Percentage of diagnosed over HTN_160_100_med_wdiag
data$ratio_diag_HTN_160_100_med_wdiag <- NA
data$ratio_diag_HTN_160_100_med_wdiag[which(data$HTN_160_100_med_wdiag==1)] <- 0
data$ratio_diag_HTN_160_100_med_wdiag[which(data$diagnosis==1 & (data$HTN_160_100_med_wdiag==1))] <- 1
# ----------------------------------
# 38. Percentage of treated over HTN_140_90_med_wdiag
data$ratio_treat_HTN_140_90_med_wdiag <- NA
data$ratio_treat_HTN_140_90_med_wdiag[which(data$HTN_140_90_med_wdiag==1)] <- 0
data$ratio_treat_HTN_140_90_med_wdiag[which(data$new_med==1 & (data$HTN_140_90_med_wdiag==1))] <- 1
# 39. Percentage of treated over HTN_160_100_med_wdiag
data$ratio_treat_HTN_160_100_med_wdiag <- NA
data$ratio_treat_HTN_160_100_med_wdiag[which(data$HTN_160_100_med_wdiag==1)] <- 0
data$ratio_treat_HTN_160_100_med_wdiag[which(data$new_med==1 & (data$HTN_160_100_med_wdiag==1))] <- 1
# ----------------------------------
# 40. Percentage of controlled over HTN 140_90_med_wdiag
data$ratio_ctrl_HTN_140_90_med_wdiag <- NA
data$ratio_ctrl_HTN_140_90_med_wdiag[which(data$HTN_140_90_med_wdiag==1)] <- 0
data$ratio_ctrl_HTN_140_90_med_wdiag[which(data$new_med==1 & (data$sbp_final<140 & data$dbp_final<90) & (data$HTN_140_90_med_wdiag==1))] <- 1
# 41. Percentage of controlled over HTN 160_100_med_wdiag
data$ratio_ctrl_HTN_160_100_med_wdiag <- NA
data$ratio_ctrl_HTN_160_100_med_wdiag[which(data$HTN_160_100_med_wdiag==1)] <- 0
data$ratio_ctrl_HTN_160_100_med_wdiag[which(data$new_med==1 & (data$sbp_final<160 & data$dbp_final<100) & (data$HTN_160_100_med_wdiag==1))] <- 1
## added 20180507, metrics for 130/80 cutoffs
# 45. Percentage of controlled to 130/80 over HTN 140_90_med_wdiag
data$ratio_ctrl_130_80_HTN_140_90_med_wdiag <- NA
data$ratio_ctrl_130_80_HTN_140_90_med_wdiag[which(data$HTN_140_90_med_wdiag==1)] <- 0
data$ratio_ctrl_130_80_HTN_140_90_med_wdiag[which(data$new_med==1 & (data$sbp_final<130 & data$dbp_final<80) & (data$HTN_140_90_med_wdiag==1))] <- 1
# 46. Percentage of controlled to 135/85 over HTN 140_90_med_wdiag
data$ratio_ctrl_135_85_HTN_140_90_med_wdiag <- NA
data$ratio_ctrl_135_85_HTN_140_90_med_wdiag[which(data$HTN_140_90_med_wdiag==1)] <- 0
data$ratio_ctrl_135_85_HTN_140_90_med_wdiag[which(data$new_med==1 & (data$sbp_final<135 & data$dbp_final<85) & (data$HTN_140_90_med_wdiag==1))] <- 1
# ---------------------------------- 20180510
# 47. Prevalence of 130/80 and not on medication
data$ratio_130_80_over_not_HTN_140_90_med_wdiag <- NA
data$ratio_130_80_over_not_HTN_140_90_med_wdiag[which(data$HTN_140_90_med_wdiag==0)] <- 0
data$ratio_130_80_over_not_HTN_140_90_med_wdiag[which(data$HTN_140_90_med==0 & (data$sbp_final<130 & data$dbp_final<80))] <- 1
# 48. Percentage of 135/85 over all normal BP (140/90)
data$ratio_135_85_over_not_HTN_140_90_med_wdiag <- NA
data$ratio_135_85_over_not_HTN_140_90_med_wdiag[which(data$HTN_140_90_med_wdiag==0)] <- 0
data$ratio_135_85_over_not_HTN_140_90_med_wdiag[which(data$HTN_140_90_med==0 & (data$sbp_final<135 & data$dbp_final<85))] <- 1
# ---------------------------------- 20180511
# 49. Percentage of treated over diagnosed HTN (140/90)
data$ratio_treat_DxHTN_140_90_med_wdiag <- NA
data$ratio_treat_DxHTN_140_90_med_wdiag[which(data$HTN_140_90_med_wdiag==1 & data$diagnosis==1)] <- 0
data$ratio_treat_DxHTN_140_90_med_wdiag[which(data$new_med==1 & data$HTN_140_90_med_wdiag==1 & data$diagnosis==1)] <- 1
# 50. Percentage of treated over diagnosed HTN (160/100)
data$ratio_treat_DxHTN_160_100_med_wdiag <- NA
data$ratio_treat_DxHTN_160_100_med_wdiag[which(data$HTN_160_100_med_wdiag==1 & data$diagnosis==1)] <- 0
data$ratio_treat_DxHTN_160_100_med_wdiag[which(data$new_med==1 & data$HTN_160_100_med_wdiag==1 & data$diagnosis==1)] <- 1
# ----------------------------------
# 51. Percentage of controlled over treated HTN (140/90)
data$ratio_ctrl_TxHTN_140_90_med_wdiag <- NA
data$ratio_ctrl_TxHTN_140_90_med_wdiag[which(data$HTN_140_90_med_wdiag==1 & data$new_med==1)] <- 0
data$ratio_ctrl_TxHTN_140_90_med_wdiag[which(data$new_med==1 & (data$sbp_final<140 & data$dbp_final<90) & (data$HTN_140_90_med_wdiag==1))] <- 1
# 52. Percentage of controlled over treated HTN (160/100)
data$ratio_ctrl_TxHTN_160_100_med_wdiag <- NA
data$ratio_ctrl_TxHTN_160_100_med_wdiag[which(data$HTN_160_100_med_wdiag==1 & data$new_med==1)] <- 0
data$ratio_ctrl_TxHTN_160_100_med_wdiag[which(data$new_med==1 & (data$sbp_final<160 & data$dbp_final<100) & (data$HTN_160_100_med_wdiag==1))] <- 1
# ---------------------------------- 20191021
# 53. HTN defined as: SBP >=160 OR DBP >=100, AND medication and diagnosis available
data$HTN_160_100_wmed_wdiag <- 0
data$HTN_160_100_wmed_wdiag[which(data$sbp_final>=160|data$dbp_final>=100)] <- 1
data$HTN_160_100_wmed_wdiag[which(is.na(data$sbp_final)|is.na(data$dbp_final)|is.na(data$diagnosis)|is.na(data$new_med))] <- NA
# 54. Percentage of diagnosed over HTN_160_100_wmed_wdiag
data$ratio_diag_HTN_160_100_wmed_wdiag <- NA
data$ratio_diag_HTN_160_100_wmed_wdiag[which(data$HTN_160_100_wmed_wdiag==1)] <- 0
data$ratio_diag_HTN_160_100_wmed_wdiag[which(data$diagnosis==1 & (data$HTN_160_100_wmed_wdiag==1))] <- 1
# 55. Percentage of treated over HTN_160_100_wmed_wdiag
data$ratio_treat_HTN_160_100_wmed_wdiag <- NA
data$ratio_treat_HTN_160_100_wmed_wdiag[which(data$HTN_160_100_wmed_wdiag==1)] <- 0
data$ratio_treat_HTN_160_100_wmed_wdiag[which(data$new_med==1 & (data$HTN_160_100_wmed_wdiag==1))] <- 1
# ---------------------------------- 20191025
# 56. HTN defined as: SBP >=160 OR DBP >=100, AND medication available
data$HTN_160_100_wmed <- 0
data$HTN_160_100_wmed[which(data$sbp_final>=160|data$dbp_final>=100)] <- 1
data$HTN_160_100_wmed[which(is.na(data$sbp_final)|is.na(data$dbp_final)|is.na(data$new_med))] <- NA
# 57. Percentage of treated over HTN_160_100_wmed
data$ratio_treat_HTN_160_100_wmed <- NA
data$ratio_treat_HTN_160_100_wmed[which(data$HTN_160_100_wmed==1)] <- 0
data$ratio_treat_HTN_160_100_wmed[which(data$new_med==1 & (data$HTN_160_100_wmed==1))] <- 1
# 58. 160_100
data$BP_160_100 <- 0
data$BP_160_100[which(data$sbp_final>=160|data$dbp_final>=100)] <- 1
data$BP_160_100[which(is.na(data$sbp_final)|is.na(data$dbp_final))] <- NA
}

# Prevalence of BP in all categories 20180510
UHC_allcat_list <- UHC_allcat_num <- c()
if (UHC_allcat){
UHC_allcat_list <- c("HTN_160_100_Tx_wdiag", "HTN_160_100_Dx_uTx", "HTN_160_100_uDx_wmed",
                     "HTN_140_90_160_100_Tx_wdiag", "HTN_140_90_160_100_Dx_uTx", "HTN_140_90_160_100_uDx_wmed", "ctrlHTN_140_90_med_wdiag", "L_140_90_Dx_uTx","L_140_90_uDx_wmed",
                     "HTN_135_85_140_90_Tx_wdiag", "HTN_135_85_140_90_Dx_uTx", "HTN_135_85_140_90_uDx_wmed", "ctrlHTN_135_85_med_wdiag", "L_135_85_Dx_uTx", "L_135_85_uDx_wmed",
                     "HTN_130_80_140_90_Tx_wdiag", "HTN_130_80_140_90_Dx_uTx", "HTN_130_80_140_90_uDx_wmed", "ctrlHTN_130_80_med_wdiag", "L_130_80_Dx_uTx", "L_130_80_uDx_wmed")
UHC_allcat_num  <- 101:121
# ----------------------------------
# 101. Prevalence of BP>=160/100 and treated (with diagnosis info available)
data$HTN_160_100_Tx_wdiag <- 0
data$HTN_160_100_Tx_wdiag[which((data$sbp_final>=160 | data$dbp_final>=100) & (data$new_med==1))] <- 1
data$HTN_160_100_Tx_wdiag[which(is.na(data$sbp_final) | is.na(data$dbp_final) | is.na(data$new_med) | is.na(data$diagnosis))] <- NA
# 102. Prevalence of BP>=160/100 and diagnosed but not treated
data$HTN_160_100_Dx_uTx <- 0
data$HTN_160_100_Dx_uTx[which((data$sbp_final>=160 | data$dbp_final>=100) & (data$diagnosis==1) & (data$new_med==0))] <- 1
data$HTN_160_100_Dx_uTx[which(is.na(data$sbp_final) | is.na(data$dbp_final) | is.na(data$new_med) | is.na(data$diagnosis))] <- NA
# 103. Prevalence of BP>=160/100 and undiagnosed (with treatment info available)
data$HTN_160_100_uDx_wmed <- 0
data$HTN_160_100_uDx_wmed[which((data$sbp_final>=160 | data$dbp_final>=100) & (data$diagnosis==0))] <- 1
data$HTN_160_100_uDx_wmed[which(is.na(data$sbp_final) | is.na(data$dbp_final) | is.na(data$new_med) | is.na(data$diagnosis))] <- NA
# ----------------------------------
# 104. Prevalence of BP in 140/90 to 160/100 and treated (with diagnosis info available)
data$HTN_140_90_160_100_Tx_wdiag <- 0
data$HTN_140_90_160_100_Tx_wdiag[which((data$sbp_final<160 & data$dbp_final<100) & (data$sbp_final>=140 | data$dbp_final>=90) & (data$new_med==1))] <- 1
data$HTN_140_90_160_100_Tx_wdiag[which(is.na(data$sbp_final) | is.na(data$dbp_final) | is.na(data$new_med) | is.na(data$diagnosis))] <- NA
# 105. Prevalence of BP in 140/90 to 160/100 and diagnosed but not treated
data$HTN_140_90_160_100_Dx_uTx <- 0
data$HTN_140_90_160_100_Dx_uTx[which((data$sbp_final<160 & data$dbp_final<100) & (data$sbp_final>=140 | data$dbp_final>=90) & (data$diagnosis==1) & (data$new_med==0))] <- 1
data$HTN_140_90_160_100_Dx_uTx[which(is.na(data$sbp_final) | is.na(data$dbp_final) | is.na(data$new_med) | is.na(data$diagnosis))] <- NA
# 106. Prevalence of BP in 140/90 to 160/100 and undiagnosed (with treatment info available)
data$HTN_140_90_160_100_uDx_wmed <- 0
data$HTN_140_90_160_100_uDx_wmed[which((data$sbp_final<160 & data$dbp_final<100) & (data$sbp_final>=140 | data$dbp_final>=90) & (data$diagnosis==0))] <- 1
data$HTN_140_90_160_100_uDx_wmed[which(is.na(data$sbp_final) | is.na(data$dbp_final) | is.na(data$new_med) | is.na(data$diagnosis))] <- NA
# 107. Prevalence of BP <140/90 and treated (with diagnosis info available)
data$ctrlHTN_140_90_med_wdiag <- data$ctrlHTN_140_90_med
data$ctrlHTN_140_90_med_wdiag[is.na(data$diagnosis)] <- NA
# 108. Prevalence of BP <140/90, diagnosed but not treated
data$L_140_90_Dx_uTx <- 0
data$L_140_90_Dx_uTx[which(data$sbp_final<140 & data$dbp_final<90 & data$diagnosis==1 & data$new_med==0)] <- 1
data$L_140_90_Dx_uTx[which(is.na(data$sbp_final) | is.na(data$dbp_final) | is.na(data$new_med) | is.na(data$diagnosis))] <- NA
# 109. Prevalence of BP <140/90 and undiagnosed (with treatment info available)
data$L_140_90_uDx_wmed <- 0
data$L_140_90_uDx_wmed[which(data$sbp_final<140 & data$dbp_final<90 & data$diagnosis==0)] <- 1
data$L_140_90_uDx_wmed[which(is.na(data$sbp_final) | is.na(data$dbp_final) | is.na(data$new_med) | is.na(data$diagnosis))] <- NA
# ----------------------------------
# 110. Prevalence of BP in 135/85 to 140/90 and treated (with diagnosis info available)
data$HTN_135_85_140_90_Tx_wdiag <- 0
data$HTN_135_85_140_90_Tx_wdiag[which((data$sbp_final<140 & data$dbp_final<90) & (data$sbp_final>=135 | data$dbp_final>=85) & (data$new_med==1))] <- 1
data$HTN_135_85_140_90_Tx_wdiag[which(is.na(data$sbp_final) | is.na(data$dbp_final) | is.na(data$new_med) | is.na(data$diagnosis))] <- NA
# 111. Prevalence of BP in 135/85 to 140/90 and diagnosed but not treated
data$HTN_135_85_140_90_Dx_uTx <- 0
data$HTN_135_85_140_90_Dx_uTx[which((data$sbp_final<140 & data$dbp_final<90) & (data$sbp_final>=135 | data$dbp_final>=85) & (data$diagnosis==1) & (data$new_med==0))] <- 1
data$HTN_135_85_140_90_Dx_uTx[which(is.na(data$sbp_final) | is.na(data$dbp_final) | is.na(data$new_med) | is.na(data$diagnosis))] <- NA
# 112. Prevalence of BP in 135/85 to 140/90 and undiagnosed (with treatment info available)
data$HTN_135_85_140_90_uDx_wmed <- 0
data$HTN_135_85_140_90_uDx_wmed[which((data$sbp_final<140 & data$dbp_final<90) & (data$sbp_final>=135 | data$dbp_final>=85) & (data$diagnosis==0))] <- 1
data$HTN_135_85_140_90_uDx_wmed[which(is.na(data$sbp_final) | is.na(data$dbp_final) | is.na(data$new_med) | is.na(data$diagnosis))] <- NA
# 113. Prevalence of BP <135/85 and treated (with diagnosis info available)
data$ctrlHTN_135_85_med_wdiag <- 0
data$ctrlHTN_135_85_med_wdiag[which(data$sbp_final<135 & data$dbp_final<85 & data$new_med==1)] <- 1
data$ctrlHTN_135_85_med_wdiag[which(is.na(data$sbp_final) | is.na(data$dbp_final) | is.na(data$new_med) | is.na(data$diagnosis))] <- NA
# 114. Prevalence of BP <135/85, diagnosed but not treated
data$L_135_85_Dx_uTx <- 0
data$L_135_85_Dx_uTx[which(data$sbp_final<135 & data$dbp_final<85 & data$diagnosis==1 & data$new_med==0)] <- 1
data$L_135_85_Dx_uTx[which(is.na(data$sbp_final) | is.na(data$dbp_final) | is.na(data$new_med) | is.na(data$diagnosis))] <- NA
# 115. Prevalence of BP <135/85 and undiagnosed (with treatment info available)
data$L_135_85_uDx_wmed <- 0
data$L_135_85_uDx_wmed[which(data$sbp_final<135 & data$dbp_final<85 & data$diagnosis==0)] <- 1
data$L_135_85_uDx_wmed[which(is.na(data$sbp_final) | is.na(data$dbp_final) | is.na(data$new_med) | is.na(data$diagnosis))] <- NA
# ----------------------------------
# 116. Prevalence of BP in 130/80 to 140/90 and treated (with diagnosis info available)
data$HTN_130_80_140_90_Tx_wdiag <- 0
data$HTN_130_80_140_90_Tx_wdiag[which((data$sbp_final<140 & data$dbp_final<90) & (data$sbp_final>=130 | data$dbp_final>=80) & (data$new_med==1))] <- 1
data$HTN_130_80_140_90_Tx_wdiag[which(is.na(data$sbp_final) | is.na(data$dbp_final) | is.na(data$new_med) | is.na(data$diagnosis))] <- NA
# 117. Prevalence of BP in 130/80 to 140/90 and diagnosed but not treated
data$HTN_130_80_140_90_Dx_uTx <- 0
data$HTN_130_80_140_90_Dx_uTx[which((data$sbp_final<140 & data$dbp_final<90) & (data$sbp_final>=130 | data$dbp_final>=80) & (data$diagnosis==1) & (data$new_med==0))] <- 1
data$HTN_130_80_140_90_Dx_uTx[which(is.na(data$sbp_final) | is.na(data$dbp_final) | is.na(data$new_med) | is.na(data$diagnosis))] <- NA
# 118. Prevalence of BP in 130/80 to 140/90 and undiagnosed (with treatment info available)
data$HTN_130_80_140_90_uDx_wmed <- 0
data$HTN_130_80_140_90_uDx_wmed[which((data$sbp_final<140 & data$dbp_final<90) & (data$sbp_final>=130 | data$dbp_final>=80) & (data$diagnosis==0))] <- 1
data$HTN_130_80_140_90_uDx_wmed[which(is.na(data$sbp_final) | is.na(data$dbp_final) | is.na(data$new_med) | is.na(data$diagnosis))] <- NA
# 119. Prevalence of BP <130/80 and treated (with diagnosis info available)
data$ctrlHTN_130_80_med_wdiag <- 0
data$ctrlHTN_130_80_med_wdiag[which(data$sbp_final<130 & data$dbp_final<80 & data$new_med==1)] <- 1
data$ctrlHTN_130_80_med_wdiag[which(is.na(data$sbp_final) | is.na(data$dbp_final) | is.na(data$new_med) | is.na(data$diagnosis))] <- NA
# 120. Prevalence of BP <130/80, diagnosed but not treated
data$L_130_80_Dx_uTx <- 0
data$L_130_80_Dx_uTx[which(data$sbp_final<130 & data$dbp_final<80 & data$diagnosis==1 & data$new_med==0)] <- 1
data$L_130_80_Dx_uTx[which(is.na(data$sbp_final) | is.na(data$dbp_final) | is.na(data$new_med) | is.na(data$diagnosis))] <- NA
# 121. Prevalence of BP <130/80 and undiagnosed
data$L_130_80_uDx_wmed <- 0
data$L_130_80_uDx_wmed[which(data$sbp_final<130 & data$dbp_final<80 & data$diagnosis==0)] <- 1
data$L_130_80_uDx_wmed[which(is.na(data$sbp_final) | is.na(data$dbp_final) | is.na(data$new_med) | is.na(data$diagnosis))] <- NA
}

UHC_test_list <- UHC_test_num <- c()
if (UHC_test) {
UHC_test_list <- c("ctrlHTN_160_100_med","ratio_diag_HTN_140_90_med","ratio_diag_HTN_160_100_med","ratio_treat_HTN_140_90_med","ratio_treat_HTN_160_100_med","ratio_ctrl_HTN_140_90_med","ratio_ctrl_HTN_160_100_med")#,"HTN_140_90_med_diag","HTN_160_100_med_diag","ratio_diag_HTN_140_90_med_diag","ratio_diag_HTN_160_100_med_diag","ratio_treat_HTN_140_90_med_diag","ratio_treat_HTN_160_100_med_diag","ratio_ctrl_HTN_140_90_med_diag","ratio_ctrl_HTN_160_100_med_diag","ratio_Under14090_over_Dx_Untx","ratio_Under14090_Dx_Untx_over_HTN_140_90_med_diag","ratio_Under14090_Dx_Untx_over_HTN_140_90_med_not")
UHC_test_num  <- c(19,22,23,27,28,10,31)#,20,21,24,25,29,30,32,33,42:44)
# ---------------------------------
# 19. Controlled HTN defind as: medication AND SBP < 160 AND DBP < 100
data$ctrlHTN_160_100_med <- 0
data$ctrlHTN_160_100_med[which(data$sbp_final<160&data$dbp_final<100&data$new_med==1)] <- 1
data$ctrlHTN_160_100_med[which(is.na(data$sbp_final)|is.na(data$dbp_final)|is.na(data$new_med))] <- NA
# ----------------------------------
# 22. Percentage of diagnosed over HTN 140_90_med
data$ratio_diag_HTN_140_90_med <- NA
data$ratio_diag_HTN_140_90_med[which(data$HTN_140_90_med==1 & !is.na(data$diagnosis))] <- 0
data$ratio_diag_HTN_140_90_med[which(data$diagnosis==1 & data$HTN_140_90_med==1)] <- 1
# 23. Percentage of diagnosed over HTN 160_100_med
data$ratio_diag_HTN_160_100_med <- NA
data$ratio_diag_HTN_160_100_med[which(data$HTN_160_100_med==1 & !is.na(data$diagnosis))] <- 0
data$ratio_diag_HTN_160_100_med[which(data$diagnosis==1 & data$HTN_160_100_med==1)] <- 1
# ----------------------------------
# 27. Percentage of treated over HTN_140_90_med
data$ratio_treat_HTN_140_90_med <- NA
data$ratio_treat_HTN_140_90_med[which(data$HTN_140_90_med==1)] <- 0
data$ratio_treat_HTN_140_90_med[which(data$HTN_140_90_med==1 & data$new_med==1)] <- 1
# 28. Percentage of treated over HTN_160_100_med
data$ratio_treat_HTN_160_100_med <- NA
data$ratio_treat_HTN_160_100_med[which(data$HTN_160_100_med==1)] <- 0
data$ratio_treat_HTN_160_100_med[which(data$HTN_160_100_med==1 & data$new_med==1)] <- 1
# ----------------------------------
# 10. Percentage of controlled over HTN 140_90_med
data$ratio_ctrl_HTN_140_90_med <- NA
data$ratio_ctrl_HTN_140_90_med[which(data$HTN_140_90_med==1)] <- 0
data$ratio_ctrl_HTN_140_90_med[which(data$ctrlHTN_140_90_med==1)] <- 1
# 31. Percentage of controlled over HTN 160_100_med
data$ratio_ctrl_HTN_160_100_med <- NA
data$ratio_ctrl_HTN_160_100_med[which(data$HTN_160_100_med==1)] <- 0
data$ratio_ctrl_HTN_160_100_med[which(data$ctrlHTN_160_100_med==1)] <- 1
# ----------------------------------
# 20. HTN defined as: SBP >= 140 OR DBP >= 90 OR medicaiton OR diagnosis
data$HTN_140_90_med_diag <- data$HTN_140_90_med
data$HTN_140_90_med_diag[which(data$diagnosis==1)] <- 1
data$HTN_140_90_med_diag[which(is.na(data$diagnosis) | is.na(data$HTN_140_90_med) )] <- NA
# 21. HTN defined as: SBP >= 160 OR DBP >= 100 OR medicaiton OR diagnosis
data$HTN_160_100_med_diag <- data$HTN_160_100_med
data$HTN_160_100_med_diag[which(data$diagnosis==1)] <- 1
data$HTN_160_100_med_diag[which(is.na(data$diagnosis) | is.na(data$HTN_160_100_med) )] <- NA
# ----------------------------------
# 24. Percentage of diagnosed over HTN_140_90_med_diag
data$ratio_diag_HTN_140_90_med_diag <- NA
data$ratio_diag_HTN_140_90_med_diag[which(data$HTN_140_90_med_diag==1)] <- 0
data$ratio_diag_HTN_140_90_med_diag[which(data$HTN_140_90_med_diag==1 & data$diagnosis==1)] <- 1
# 25. Percentage of diagnosed over HTN_160_100_med_diag
data$ratio_diag_HTN_160_100_med_diag <- NA
data$ratio_diag_HTN_160_100_med_diag[which(data$HTN_160_100_med_diag==1)] <- 0
data$ratio_diag_HTN_160_100_med_diag[which(data$HTN_160_100_med_diag==1 & data$diagnosis==1)] <- 1
# ----------------------------------
# 29. Percentage of treated over HTN_140_90_med_diag
data$ratio_treat_HTN_140_90_med_diag <- NA
data$ratio_treat_HTN_140_90_med_diag[which(data$HTN_140_90_med_diag==1)] <- 0
data$ratio_treat_HTN_140_90_med_diag[which(data$HTN_140_90_med_diag==1 & data$new_med==1)] <- 1
# 30. Percentage of treated over HTN_160_100_med_diag
data$ratio_treat_HTN_160_100_med_diag <- NA
data$ratio_treat_HTN_160_100_med_diag[which(data$HTN_160_100_med_diag==1)] <- 0
data$ratio_treat_HTN_160_100_med_diag[which(data$HTN_160_100_med_diag==1 & data$new_med==1)] <- 1
# ----------------------------------
# 32. Percentage of controlled over HTN_140_90_med_diag
data$ratio_ctrl_HTN_140_90_med_diag <- NA
data$ratio_ctrl_HTN_140_90_med_diag[which(data$HTN_140_90_med_diag==1)] <- 0
data$ratio_ctrl_HTN_140_90_med_diag[which(data$HTN_140_90_med_diag==1 & data$ctrlHTN_140_90_med==1)] <- 1
# 33. Percentage of controlled over HTN_160_100_med_diag
data$ratio_ctrl_HTN_160_100_med_diag <- NA
data$ratio_ctrl_HTN_160_100_med_diag[which(data$HTN_160_100_med_diag==1)] <- 0
data$ratio_ctrl_HTN_160_100_med_diag[which(data$HTN_160_100_med_diag==1 & data$ctrlHTN_160_100_med==1)] <- 1
# ----------------------------------
# 42. Percentage of dx, untx and below 140/90
data$ratio_Under14090_over_Dx_Untx <- NA
data$ratio_Under14090_over_Dx_Untx[which(data$diagnosis==1 & data$new_med==0 & !is.na(data$sbp_final) & !is.na(data$dbp_final))] <- 0
data$ratio_Under14090_over_Dx_Untx[which(data$diagnosis==1 & data$new_med==0 & (data$sbp_final<140 & data$dbp_final<90))] <- 1
# 43. Percentage of dx, untx and below 140/90, over all HTN_140_90_med_diag
data$ratio_Under14090_Dx_Untx_over_HTN_140_90_med_diag <- NA
data$ratio_Under14090_Dx_Untx_over_HTN_140_90_med_diag[which(data$HTN_140_90_med_diag==1)] <- 0
data$ratio_Under14090_Dx_Untx_over_HTN_140_90_med_diag[which(data$diagnosis==1 & data$new_med==0 & data$HTN_140_90_med_diag==1 & (data$sbp_final<140 & data$dbp_final<90))] <- 1
# 44. Percentage of dx, untx and below 140/90, over all normal BP
data$ratio_Under14090_Dx_Untx_over_HTN_140_90_med_not <- NA
data$ratio_Under14090_Dx_Untx_over_HTN_140_90_med_not[which(data$HTN_140_90_med==0)] <- 0
data$ratio_Under14090_Dx_Untx_over_HTN_140_90_med_not[which(data$diagnosis==1 & data$new_med==0 & data$HTN_140_90_med==0 & (data$sbp_final<140 & data$dbp_final<90))] <- 1
}

lists <- c(core_list, UHC_list, UHC_allcat_list, CW_list, UHC_test_list)
nums  <- c(core_num, UHC_num, UHC_allcat_num, CW_num, UHC_test_num)

### Sample design ###
countNA <- function(x)sum(!is.na(x))

# correction 6 June 2015
# if PSU or stratum only has 1 value within a survey, remove it
p1 <- tapply(data$psu,data$id_study,function(...)length(unique(...)))
p1.list <- names(p1)[which(p1==1)]
p2 <- tapply(data$stratum,data$id_study,function(...)length(unique(...)))
p2.list <- names(p2)[which(p2==1)]

# Sample design specification ====
x<-tapply(data$psu,data$id_study,countNA);             psu_list <- names(x)[which(x>0)]
x<-tapply(data$stratum,data$id_study,countNA);         strat_list <- names(x)[which(x>0)]
psu_list <- setdiff(psu_list, p1.list)
strat_list <- setdiff(strat_list, p2.list)

x<-tapply(data$samplewt_bp,data$id_study,countNA);     wt_list <- names(x)[which(x>0)]

### CHECK SAMPLE DESIGN STATUS ####
surveys <- unique(data$id_study)
all_3 <- surveys[which(surveys%in%psu_list&surveys%in%strat_list&surveys%in%wt_list)]
psu_only <- surveys[which(surveys%in%psu_list&!surveys%in%strat_list&!surveys%in%wt_list)]
strat_only <- surveys[which(surveys%in%strat_list&!surveys%in%psu_list&!surveys%in%wt_list)]
wt_only <- surveys[which(surveys%in%wt_list&!surveys%in%psu_list&!surveys%in%strat_list)]
psu_strat <- surveys[which(surveys%in%psu_list&surveys%in%strat_list&!surveys%in%wt_list)]
psu_wt <- surveys[which(surveys%in%psu_list&!surveys%in%strat_list&surveys%in%wt_list)]
strat_wt <- surveys[which(!surveys%in%psu_list&surveys%in%strat_list&surveys%in%wt_list)]
none <- surveys[which(!surveys%in%psu_list&!surveys%in%strat_list&!surveys%in%wt_list)]

# # assign 0 to missing sample weights in surveys with sample weights and in surveys with nothing
# data$samplewt_bp[ which( is.na(data$samplewt_bp) & data$id_study%in%wt_list )] <- 0

# assign 0 to missing sample weights in surveys with sample weights  ## modified 20 9 2014
#data$samplewt_bp[ which( is.na(data$samplewt_bp) & data$id_study%in%wt_list )] <- 0
# assign 1 to sample weights in surveys without sample weights
data$samplewt_bp[ which( !data$id_study%in%wt_list )] <- 1

# drop missing psu in psu_list
data <- data[which(!is.na(data$psu)|!data$id_study%in%psu_list), ]
# drop missing stratum in strat_list
data <- data[which(!is.na(data$stratum)|!data$id_study%in%strat_list), ]
# drop missing sample weights in wt_list
data <- data[which(!is.na(data$samplewt_bp)| !data$id_study%in%wt_list), ]


## Calculating summaries ====
##* NB for BP the function has age_mean; for anthro mean_age
clean_single_psu_ssa <- function(tmp){ # ssa = study-sex-age group
	psu.count 	<- ddply(tmp[!is.na(tmp$psu),],.( id_study, age_mean, sex),function(tmp)length(unique(tmp$psu)))
	single.psu <- psu.count[psu.count$V1==1 & psu.count$id_study%in%c(as.character(psu_only),as.character(psu_wt)),] # single stratum, single psu
	drop.singlepsu <- which(paste(tmp$id_study, tmp$age_mean, tmp$sex, sep="_")%in%paste(single.psu$id_study, single.psu$age_mean, single.psu$sex, sep="_"))
	print(length(drop.singlepsu))
	print(single.psu)

	if(length(drop.singlepsu)>0) {
		tmp <- tmp[-drop.singlepsu,]
	}
	return(tmp)
}

num.valid <- function(x) sum(!is.na(x)&x!=-1)
null <- t(c(NA,NA))
sumd <- function(tmp) {
  options(survey.lonely.psu = "adjust",survey.adjust.domain.lonely=TRUE)
  # print(paste(tmp$id_study[1],tmp$sex[1],tmp$age_mean[1]))
  n <- nrow(tmp)
  if (n > 1) {
    if (tmp$id_study[1]%in%all_3) {
      dsub <- svydesign(id=~psu, strata=~stratum, weights=~samplewt_bp, data=tmp, nest=TRUE)
    } else if (tmp$id_study[1]%in%psu_only) {
      dsub <- dsub_int <- svydesign(id=~psu, strata=NULL, weights=NULL, data=tmp, nest=TRUE)
    } else if (tmp$id_study[1]%in%strat_only) {
      dsub <- dsub_int <- svydesign(id=~1, strata=~stratum, weights=NULL, data=tmp, nest=TRUE)
    } else if (tmp$id_study[1]%in%wt_only) {
      dsub <- svydesign(id=~1, strata=NULL, weights=~samplewt_bp, data=tmp, nest=TRUE)
    } else if (tmp$id_study[1]%in%psu_strat) {
      dsub <- dsub_int <- svydesign(id=~psu, strata=~stratum, weights=NULL, data=tmp, nest=TRUE)
    } else if (tmp$id_study[1]%in%psu_wt) {
      dsub <- svydesign(id=~psu, strata=NULL, weights=~samplewt_bp, data=tmp, nest=TRUE)
    } else if (tmp$id_study[1]%in%strat_wt) {
      dsub <- svydesign(id=~1, strata=~stratum, weights=~samplewt_bp, data=tmp, nest=TRUE)
    } else if (tmp$id_study[1]%in%none) {
      dsub <- dsub_int <- svydesign(id=~1, strata=NULL, weights=NULL, data=tmp)
    }
  }

  n_sbp <- num.valid(tmp$sbp_final); n_dbp <- num.valid(tmp$dbp_final)
  if (n_sbp>1) {
      sbp_m  <- svymean(~sbp_final, dsub, na.rm=TRUE)
      sbp_sd <- sqrt(coef(svyvar(~sbp_final, dsub, na.rm=TRUE)))
  } else {
      sbp_m  <- null
      sbp_sd <- NA
  }
  if (n_dbp>1) {
      dbp_m <- svymean(~dbp_final, dsub, na.rm=TRUE)
      dbp_sd <- sqrt(coef(svyvar(~dbp_final, dsub, na.rm=TRUE)))
  } else {
      dbp_m  <- null
      dbp_sd <- NA
  }

  n_med <- num.valid(tmp$new_med); n_diag <- num.valid(tmp$diagnosis)
  if (n_med>1) { med_m <- svymean(~new_med, dsub, na.rm=TRUE)
  } else med_m <- null
  if (n_diag>1) { diag_m <- svymean(~diagnosis, dsub, na.rm=TRUE)
  } else diag_m <- null

  res <- data.frame(n, n_sbp, sbp_m, sbp_sd, n_dbp, dbp_m, dbp_sd, n_med, med_m, n_diag, diag_m)
  names(res) <- c("n", "n_sbp", "mean_sbp", "se_sbp", "sd_sbp", "n_dbp", "mean_dbp", "se_dbp", "sd_dbp", "n_med", "prev_med", "se_med","n_diag", "prev_diag", "se_diag")

  for (i in 1:length(lists)) {
      num <- nums[i]
      var <- lists[i]
      eval(parse(text = paste0("n",num," <- num.valid(tmp$",var,")")))   # n1 <- num.valid(tmp$variable1)

      if (get(paste0("n",num)) > 1) {
          assign(paste0("prev_", num), eval(parse(text = paste0("svymean(~",var,", dsub, na.rm=TRUE)")))) # prev_1 <- svymean(~tmp$variable1, dsub, na.rm=T)
      } else assign(paste0("prev_", num), null)                          # else prev_1 <- null
  }

  res_tmp <- eval(parse(text = paste0("data.frame(", paste(paste0(c("n","prev_"), rep(nums,each=2)), collapse=", "),")")))
  names(res_tmp) <- paste(c("n","prev","se"), rep(lists, each=3), sep="_")
  res <- data.frame(res, res_tmp)

  if (DIST) {
      if (n_sbp>2) {
          tt <- svyquantile(~sbp_final, dsub, c(0.2,0.5,0.8), se=TRUE, na.rm=TRUE, ties="rounded")
          sbp_quant <- t(c(rep(n_sbp,3),tt$quantiles,SE(tt))[c(1,4,7,2,5,8,3,6,9)])
      } else sbp_quant <- t(rep(NA,9))
      if (n_dbp>2) {
          tt <- svyquantile(~dbp_final, dsub, c(0.2,0.5,0.8), se=TRUE, na.rm=TRUE, ties="rounded")
          dbp_quant <- t(c(rep(n_dbp,3),tt$quantiles,SE(tt))[c(1,4,7,2,5,8,3,6,9)])
      } else dbp_quant <- t(rep(NA,9))

      res_dist <- data.frame(sbp_quant, dbp_quant)
      names(res_dist) <- c("n_sbp_q20", "mean_sbp_q20", "se_sbp_q20", "n_sbp_q50", "mean_sbp_q50", "se_sbp_q50", "n_sbp_q80", "mean_sbp_q80", "se_sbp_q80", "n_dbp_q20", "mean_dbp_q20", "se_dbp_q20", "n_dbp_q50", "mean_dbp_q50", "se_dbp_q50", "n_dbp_q80", "mean_dbp_q80", "se_dbp_q80")

      res <- data.frame(res, res_dist)
  }

  n_bmi_sbp <- with(tmp, sum((!is.na(bmi_clean)&bmi_clean>0)&(!is.na(sbp_final)&sbp_final>0)))
  n_bmi_dbp <- with(tmp, sum((!is.na(bmi_clean)&bmi_clean>0)&(!is.na(dbp_final)&dbp_final>0)))
  if (BMI) {
      if (n_bmi_sbp>2) {
          cor_bmi_sbp <- cov2cor(as.matrix(svyvar(~bmi_clean+sbp_final, dsub, na.rm=TRUE)))[1,2]
      } else cor_bmi_sbp <- NA
      if (n_bmi_dbp>2) {
          cor_bmi_dbp <- cov2cor(as.matrix(svyvar(~bmi_clean+dbp_final, dsub, na.rm=TRUE)))[1,2]
      } else cor_bmi_dbp <- NA

      res_bmi <- data.frame(cor_bmi_sbp, cor_bmi_dbp)
      res <- data.frame(res, res_bmi)
  }

  return(res)
}

# age_group and age_mean should be matched uniquely/interchangeablly
# use of age_mean before is to prevent matching errors in categorical data
# use of age_group thereafter is for the benefit of output # changed to age_mean 04162014

data.clean <- data[,c("id_study","is_urban", "sex","age_mean","psu","stratum","samplewt_bp","samplewt_smoke","sbp_final","dbp_final","bmi_clean","new_med","diagnosis",core_list, UHC_list, UHC_allcat_list, CW_list, UHC_test_list)]

data.clean 				<- clean_single_psu_ssa(data.clean) # HB added
data.clean$id_study 	<- droplevels(data.clean$id_study)
data.clean$stratum 		<- as.numeric(as.factor(data.clean$stratum)) # stratum here is the sample design variable not urban/rural


data.clean$split <- paste(data.clean$id_study,data.clean$sex,data.clean$age_mean)
splits <- unique(data.clean$split)

cl <- makeCluster(10, outfile = "debug.txt")
registerDoParallel(cl)
# summary0 <- foreach(i=1:length(splits), .combine=rbind, .packages="survey") %dopar% sumd(data.clean[which(data.clean$split==splits[i]),])
summary0 <- rbindlist(foreach(i=1:length(splits), .packages="survey") %dopar% sumd(data.clean[which(data.clean$split==splits[i]),]))
stopCluster(cl)


idents <- data.frame(matrix(unlist(strsplit(splits," ")), ncol=3, byrow=TRUE))
names(idents) <- c("id_study","sex","age_mean")
idents$sex <- factor(idents$sex)
idents$age_mean <- as.character(idents$age_mean)
summary <- data.frame(idents,summary0)

dt <- data.table(data)
link <- data.frame(dt[,list(age_group=unique(age_group)),by=list(id_study,sex,age_mean)])

summary <- merge(summary,link)

summary <- summary[with(summary,which(n_sbp>0|n_dbp>0)),]

## Output ====
write.csv(summary,file=paste0(wd,"old/BP_summary_ALL_wNoData_adultonly_",ifelse(mergeYFS, "mergeYFS", ""),ifelse(BMI, "_wBMI", ""),date,".csv"),row.names=FALSE)
write.csv(summary,file=paste0(wd,"BP_summary_ALL_wNoData_adultonly",ifelse(mergeYFS, "_mergeYFS", ""),ifelse(BMI, "_wBMI", ""),".csv"),row.names=FALSE)

#run urban_rural summary script
# source("S:/Projects/HeightProject/Original dataset/Urban_Rural/Metabolic risk factors/Blood pressure/bp_data individual incl dhs steps/BP Summary - adult only 20170421 distribution_parallel_urbanrural.R")

## combining summary
# source("S:/Projects/HeightProject/Original dataset/Blood Pressure/BP_summary/combining summary - adultonly.R")
