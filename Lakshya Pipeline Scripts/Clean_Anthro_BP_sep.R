source("~/Documents/Pipeline/BP_BMI_Pipeline/cleaning_functions.R")

#set up log file
sink(paste0(wd,"log_BPCleaning_",date, ".txt"))

# Clean all demographic variables--------------------------------

#age
data$age<-clean_data(data,'age')
clean_list <- which(is.na(data$age))
age.na <- table(data[clean_list,]$id_study)/table(data$id_study)*100
print("Percentage of NAs in Age (%)")
print(sort(round(age.na[which(age.na>0)],2), decreasing=TRUE))

#sex
data$sex<-clean_data(data,'sex')
data$sex <- as.factor(data$sex)
levels(data$sex) <- c("male","female")
clnList <- which(is.na(data$sex))
sex.na <- table(data[clnList,]$id_study)/table(data$id_study)*100
print("Percentage of NAs in Sex (%)")
print(sort(round(sex.na[which(sex.na>0)],2), decreasing=TRUE))

dropList <- which(is.na(data$sex)|(is.na(data$age) & is.na(data$age_mean) & is.na(data$age_group)))
data$dropped[dropList] <- paste(data$dropped[dropList],"AgeSex")

#age range

data$age <- floor(data$age)
# Mark Dropped
clnList <- with(data,
                which((sex=="male"&((is.na(age_max_bp_M)|age>age_max_bp_M)|(is.na(age_min_bp_M)|age<age_min_bp_M))) |
                        (sex=="female"&((is.na(age_max_bp_F)|age>age_max_bp_F)|(is.na(age_min_bp_F)|age<age_min_bp_F))) ) )
data$dropped[clnList] <- paste(data$dropped[clnList], "DesignAge")

# print out
age.out <- table(data[clnList,]$id_study)/table(data$id_study)*100
print("Percentage of Implausible Values (Outside Designed Range) in Age (%)")
print(sort(round(age.out[which(age.out>0)],2), decreasing=TRUE))

# Add NA columns for missing blood pressure measurements

data<-clean_missing_bp_col()

# Clean sbp 

max_sbp <- 270
min_sbp <- 70
N_sbp <- 0
N_sbp_cleaned <- 0
sbp_cleaned <- c()
sbp_error <- c(777,888,994,995,996,999) # common error codes - change here

data<-add_sbp_f()

sbp_f <- c("sbp1_f", "sbp2_f","sbp3_f", "sbp4_f", "sbp5_f", "sbp6_f", "sbp7_f", "sbp8_f", "sbp9_f", "sbp10_f", "sbp11_f", "sbp12_f", "sbp13_f", "sbp_avg_f")

for (sbp_f_i in sbp_f){
  data[[sbp_f_i]][which(data[[sbp_f_i]]<=0|data[[sbp_f_i]]%in%sbp_error)] <- NA # removing all negative values and common missing value entries
  clean_list <- which(data[[sbp_f_i]]>max_sbp | data[[sbp_f_i]]<min_sbp) # flagging all entries whose values that are outside the plausible range
  N_sbp <- N_sbp + sum(!is.na(data[[sbp_f_i]])) # number of people left are those who don't have NA for this variable - ie they didnt have NA before and their entry hasnt become NA for being implausible
  N_sbp_cleaned <- N_sbp_cleaned + length(clean_list) # number of people that have become NA because their values were outside the plausible range
  sbp_cleaned <- c(sbp_cleaned, data[[sbp_f_i]][clean_list])
  print_cleaned(sbp_f_i)
  data[[sbp_f_i]][clean_list] <- NA # Make all the implausible values NA
  data[[sbp_f_i]][which(data$unit_bp=="EXCLUDE")] <- NA
}

print("Excluded SBP data outside of the plausible range")
print(paste0(signif(N_sbp_cleaned/N_sbp, digit=3)*100, "%")) # Calculating the percentage of data points dropped because they were outside of the range 
print(table(round(sbp_cleaned))) 

# Clean dbp

max_dbp <- 150
min_dbp <- 30
N_dbp <- 0
N_dbp_cleaned <- 0
dbp_cleaned <- c()
dbp_error <- c(222,666,888,994,995,996,999,1000,9999) # common error codes - change here

data<-add_dbp_f()

dbp_f <- c("dbp1_f", "dbp2_f","dbp3_f", "dbp4_f", "dbp5_f", "dbp6_f", "dbp7_f", "dbp8_f", "dbp9_f", "dbp10_f", "dbp11_f", "dbp12_f", "dbp13_f", "dbp_avg_f")

for (dbp_f_i in dbp_f){
  data[[dbp_f_i]][which(data[[dbp_f_i]]<=0|data[[dbp_f_i]]%in%dbp_error)] <- NA # removing all negative values and common missing value entries
  clean_list <- which(data[[dbp_f_i]]>max_dbp | data[[dbp_f_i]]<min_dbp) # flagging all entries whose values that are outside the plausible range
  N_dbp <- N_dbp + sum(!is.na(data[[dbp_f_i]])) # number of people left are those who don't have NA for this variable - ie they didnt have NA before and their entry hasnt become NA for being implausible
  N_dbp_cleaned <- N_dbp_cleaned + length(clean_list) # number of people that have become NA because their values were outside the plausible range
  dbp_cleaned <- c(dbp_cleaned, data[[dbp_f_i]][clean_list])
  print_cleaned(dbp_f_i)
  data[[dbp_f_i]][clean_list] <- NA # Make all the implausible values NA
  data[[dbp_f_i]][which(data$unit_bp=="EXCLUDE")] <- NA
}

print("Excluded DBP data outside of the plausible range")
print(paste0(signif(N_dbp_cleaned/N_dbp, digit=3)*100, "%")) # Calculating the percentage of data points dropped because they were outside of the range 
print(table(round(dbp_cleaned))) 

# Mark dropped

dropList <- which(is.na(data$sbp1_f) & is.na(data$sbp2_f) & is.na(data$sbp3_f) & is.na(data$sbp4_f) & is.na(data$sbp5_f) &
                    is.na(data$sbp6_f) & is.na(data$sbp7_f) & is.na(data$sbp8_f) & is.na(data$sbp9_f) & is.na(data$sbp10_f) &
                    is.na(data$sbp11_f) & is.na(data$sbp12_f) & is.na(data$sbp13_f) & is.na(data$sbp_avg_f) &
                    is.na(data$dbp1_f) & is.na(data$dbp2_f) & is.na(data$dbp3_f) & is.na(data$dbp4_f) & is.na(data$dbp5_f) &
                    is.na(data$dbp6_f) & is.na(data$dbp7_f) & is.na(data$dbp8_f) & is.na(data$dbp9_f) & is.na(data$dbp10_f) &
                    is.na(data$dbp11_f) & is.na(data$dbp12_f) & is.na(data$dbp13_f) & is.na(data$dbp_avg_f) )
data$dropped[dropList] <- paste(data$dropped[dropList], "NoData")

calc_avg <- function(x){ # chagne to summarise one
  avg <- x[1]; x <- x[-1]
  pos <- which(!is.na(x))
  if (length(pos)==0) res <- avg else
    if (length(pos)==1) res <- x[pos] else
      if (length(pos)==2) res <- x[pos[2]] else
        res <- mean(x[pos[2:length(pos)]])
  return (res)
}
data$sbp_final <- apply(data[,c("sbp_avg_f","sbp1_f","sbp2_f","sbp3_f","sbp4_f","sbp5_f","sbp6_f","sbp7_f","sbp8_f","sbp9_f","sbp10_f","sbp11_f","sbp12_f","sbp13_f")],1,calc_avg)
data$dbp_final <- apply(data[,c("dbp_avg_f","dbp1_f","dbp2_f","dbp3_f","dbp4_f","dbp5_f","dbp6_f","dbp7_f","dbp8_f","dbp9_f","dbp10_f","dbp11_f","dbp12_f","dbp13_f")],1,calc_avg)
# calculate the average value of BP that will be used in analysis

# Clean hyper variables

data$self_hyper<-clean_data(data,'self_hyper')
data$self_hyper_12mos<-clean_data(data,'self_hyper_12mos')
data$drug_presc<-clean_data(data,'drug_presc')
data$drug_bp<-clean_data(data,'drug_bp')
data$drug_hyper<-clean_data(data,'drug_hyper')

# Clean other variables

data$samplewt_anthro[which(data$samplewt_anthro<0)] <- NA
data$samplewt_wh[which(data$samplewt_wh<0)] <- NA
data$samplewt_bp[which(data$samplewt_bp<0)] <- NA
data$psu[which(data$psu<0)] <- NA
data$stratum[which(data$stratum<0)] <- NA

### is_urban, is_pregnant, is_pregnant_exam ------------------------------
data$is_urban <- as.numeric(data$is_urban)
data$is_urban <- clean_data(data,'is_urban')

## is_pregnant

data$is_pregnant[which(data$is_pregnant<0)] <- NA
clnList <- which(data$is_pregnant!=0&data$is_pregnant!=1)
print_cleaned("is_pregnant")
data$is_pregnant[clnList] <- NA #change to function

clnList <- which(data$is_pregnant==1&(data$sex=="male"|data$age>=50|data$age<10))
print_cleaned("is_pregnant")
data$is_pregnant[clnList] <- 0
print("Cleaned pregnant males and pregnant females aged over 50 or under 10")
data$is_pregnant[which(data$sex=="male")] <- 0

data$is_pregnant_exam[which(data$is_pregnant_exam<0)] <- NA
clnList <- which(data$is_pregnant_exam!=0&data$is_pregnant_exam!=1)
print_cleaned("is_pregnant_exam")
data$is_pregnant_exam[clnList] <- NA #change to function

clnList <- which(data$is_pregnant_exam==1&(data$sex=="male"|data$age>=50|data$age<10))
print_cleaned("is_pregnant_exam")
data$is_pregnant_exam[clnList] <- 0
print("Cleaned pregnant males and pregnant females aged over 50 or under 10")
data$is_pregnant_exam[which(data$sex=="male")] <- 0

# Mark Dropped
dropList <- which(data$is_pregnant_exam==1|(is.na(data$is_pregnant_exam)&data$is_pregnant==1))
data$dropped[dropList] <- paste(data$dropped[dropList], "Preg")

table(data$sex, data$is_pregnant) # pregnant sanity check

# Clean Anthro

data$height1[data$height1<0] <- NA # remove all negative heights
data$height2[data$height2<0] <- NA
data$height3[data$height3<0] <- NA
list <- which(is.na(data$height)&(!is.na(data$height1)|!is.na(data$height2)|!is.na(data$height3))) # list all missing height entries 
data$height[list] <-  apply(data[list, c("height1","height2","height3")], 1, mean, na.rm=TRUE)

data$weight1[data$weight1<0] <- NA # remove all negative weights
data$weight2[data$weight2<0] <- NA
data$weight3[data$weight3<0] <- NA
list <- which(is.na(data$weight)&(!is.na(data$weight1)|!is.na(data$weight2)|!is.na(data$weight3)))
data$weight[list] <-  apply(data[list, c("weight1","weight2","weight3")], 1, mean, na.rm=TRUE)


data$bmi <- with(data, ifelse(is.na(height)|is.na(weight),bmi,weight/height/height*10000) ) # calculate BMI from height and weight if not given

data$bmi_clean<-ifelse(data$bmi<10|data$bmi>80, NA, data$bmi) # remove implausible BMI
data$height_clean<-ifelse(data$bmi<10|data$bmi>80, NA, data$height) # remove implausible BMI's people's height
data$weight_clean<-ifelse(data$bmi<10|data$bmi>80, NA, data$weight) # remove implausible BMI's people's weight

data$bmi_clean<-ifelse(data$height_clean<80 | data$height_clean>250, NA, data$bmi_clean) #remove implausiable height people's BMI
data$height_clean<-ifelse(data$height_clean<80 | data$height_clean>250, NA, data$height_clean) #remove implausible height people
data$weight_clean<-ifelse(data$height_clean<80 | data$height_clean>250, NA, data$weight_clean) #remove implausible height's people's weight

data$bmi_clean<-ifelse(data$weight_clean<10 | data$weight_clean>300, NA, data$bmi_clean) #remove implausible weight people's BMI
data$height_clean<-ifelse(data$weight_clean<10 | data$weight_clean>300, NA, data$height_clean) #remove implausible weight people's height 
data$weight_clean<-ifelse(data$weight_clean<10 | data$weight_clean>300, NA, data$weight_clean) #remove implausible weight people

data$height1 <- data$height2 <- data$height3 <- data$weight1 <- data$weight2 <- data$weight3 <- NULL 


## Calculate mean weight, height, wiast, hip when multiple metrics are available

data$waist1[data$waist1<0] <- NA; data$waist2[data$waist2<0] <- NA; data$waist3[data$waist3<0] <- NA
data$hip1[data$hip1<0] <- NA; data$hip2[data$hip2<0] <- NA; data$hip3[data$hip3<0] <- NA
data$waist_avg <- rowMeans(subset(data, select = c("waist1", "waist2", "waist3")), na.rm = TRUE)
data$hip_avg <- rowMeans(subset(data, select = c("hip1", "hip2", "hip3")), na.rm = TRUE)

## Replace weight, height, wiast, hip by mean values, when multiple metrics are available
data$weight <- ifelse(is.na(data$weight) & !is.na(data$weight_avg), data$weight_avg, data$weight)
data$height <- ifelse(is.na(data$height) & !is.na(data$height_avg), data$height_avg, data$height)
data$waist <- ifelse(is.na(data$waist) & !is.na(data$waist_avg), data$waist_avg, data$waist)
data$hip <- ifelse(is.na(data$hip) & !is.na(data$hip_avg), data$hip_avg, data$hip)

## Update bmi and whr variables (re-calculate them when missing, eg. bmi from measured weight and height)
check_idx <- which(!is.na(data$height) & !is.na(data$weight) & !is.na(data$bmi))
if (length(check_idx) > 0) {
  #plot calculated BMI vs reported BMI
  plot(data$weight[check_idx]/((data$height[check_idx]/100)^2), data$bmi[check_idx])
}
data$bmi_clean <- ifelse(is.na(data$bmi_clean), data$weight_clean/((data$height_clean/100)^2), data$bmi)
data$whr_clean <- ifelse(is.na(data$whr), data$waist_clean/data$hip_clean, data$whr)

my_data %>% 
  rename(
    bmi = bmi_clean,
    height = height_clean,
    weight = weight_clean,
    whr = whr_clean
  )

data <- subset(data, select = c(
  "id_study", "iso", "country", "start_year",
  "end_year", "mid_year", "survey", "survey_short",
  "survey_type", "urban_rural", "age_min_anthro_F", "age_max_anthro_F",
  "age_min_anthro_M", "age_max_anthro_M", "id", "psu",
  "age", "sex", "unit_height", "unit_weight", "waist", "unit_waist",
  "is_urban", "age_min_bp_M",
  "age_max_bp_M", "age_min_bp_F", "age_max_bp_F", "is_plasma",
  "device_bp", "is_multi_cuff", "is_multi_bp",
  "drug_hyper_definition", "hip", "unit_hip", "is_pregnant",
  "sbp1", "sbp2", "dbp1",
  "dbp2", "self_hyper", "drug_hyper", "birth_y", "birth_m", "stratum",
  "samplewt_anthro", "samplewt_wh",
  "samplewt_smoke", "is_fasting",
  "smoker",
  "smoke_num_curr", "smoke_ever", "sbp_avg", "dbp_avg",
  "sbp3", "dbp3",
  "averaged_properly", "samplewt_bp", "samplewt_kidney",
  "samplewt_blood", "ethnicity", "income", "waist1",
  "waist2", "waist3", "hip1", "hip2",
  "hip3", "fasting_time", "self_hyper_12mos", "d_hypt",
  "sbp4", "sbp5", "dbp4", "dbp5",
  "sbp6", "dbp6", "sbp7", "dbp7",
  "sbp8", "dbp8", "sbp9", "dbp9",
  "self_hyper_preg", "drug_bp", "whr",
  "year",
  "drug_hyper_12mos", "drug_hyper_type", "sbp10", "sbp11",
  "sbp12", "sbp13", "dbp10", "dbp11",
  "dbp12", "dbp13",
  "whtr", "age_group_original", "age_mean_orginal",
  "drug_hyper2", "pulse5",
  "drug_hyper_class", "meas_bp", "is_urban_all", "unit_bp",
  "self_hyper_new12mos", "meas_bp_12mos", "freq",
  "drug_presc", "is_pregnant_exam", "excluded", "dropped", "sbp1_f", "sbp2_f",
  "sbp3_f", "sbp4_f", "sbp5_f", "sbp6_f",
  "sbp7_f", "sbp8_f", "sbp9_f", "sbp10_f",
  "sbp11_f", "sbp12_f", "sbp13_f", "sbp_avg_f",
  "dbp1_f", "dbp2_f", "dbp3_f", "dbp4_f",
  "dbp5_f", "dbp6_f", "dbp7_f", "dbp8_f",
  "dbp9_f", "dbp10_f", "dbp11_f", "dbp12_f",
  "dbp13_f", "dbp_avg_f", "sbp_final", "dbp_final",
  "bmi", "height", "weight"
))

sink()
saveRDS(data,file=paste0(wd,"BP_Anthro_Indiv_Cleaned.RDS"))
