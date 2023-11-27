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

# Clean hyper variables

data$self_hyper<-clean_data(data, 'self_hyper') # Get rid of negative values
clean_list <- which(data$self_hyper!=0&data$self_hyper!=1) # flag the datapoints that dont have the variable as either 0 or 1
print_cleaned("self_hyper") # Print all the studies and percentanges that were dropped that had this variable
data$self_hyper[clean_list] <- NA # Make all the wrong values NA


# Clean anthro variables

source("~/Documents/Pipeline/BP_BMI_Pipeline/Anthropometrics_cleaning_functions.R")

## Calculate mean weight, height, wiast, hip when multiple metrics are available
data$height1[data$height1<0] <- NA; data$height2[data$height2<0] <- NA; data$height3[data$height3<0] <- NA
data$weight1[data$weight1<0] <- NA; data$weight2[data$weight2<0] <- NA; data$weight3[data$weight3<0] <- NA
#data$waist1[data$waist1<0] <- NA; data$waist2[data$waist2<0] <- NA; data$waist3[data$waist3<0] <- NA
#data$hip1[data$hip1<0] <- NA; data$hip2[data$hip2<0] <- NA; data$hip3[data$hip3<0] <- NA

data$weight_avg <- rowMeans(subset(data, select = c("weight1", "weight2", "weight3")), na.rm = TRUE)
data$height_avg <- rowMeans(subset(data, select = c("height1", "height2", "height3")), na.rm = TRUE)
#data$waist_avg <- rowMeans(subset(data, select = c("waist1", "waist2", "waist3")), na.rm = TRUE)
#data$hip_avg <- rowMeans(subset(data, select = c("hip1", "hip2", "hip3")), na.rm = TRUE)

## Replace weight, height, wiast, hip by mean values, when multiple metrics are available
data$weight <- ifelse(is.na(data$weight) & !is.na(data$weight_avg), data$weight_avg, data$weight)
data$height <- ifelse(is.na(data$height) & !is.na(data$height_avg), data$height_avg, data$height)
#data$waist <- ifelse(is.na(data$waist) & !is.na(data$waist_avg), data$waist_avg, data$waist)
#data$hip <- ifelse(is.na(data$hip) & !is.na(data$hip_avg), data$hip_avg, data$hip)

## Update bmi and whr variables (re-calculate them when missing, eg. bmi from measured weight and height)
check_idx <- which(!is.na(data$height) & !is.na(data$weight) & !is.na(data$bmi))
if (length(check_idx) > 0) {
  #plot calculated BMI vs reported BMI
  plot(data$weight[check_idx]/((data$height[check_idx]/100)^2), data$bmi[check_idx])
}
data$bmi <- ifelse(is.na(data$bmi), data$weight/((data$height/100)^2), data$bmi) ## UPDATED ON 06/02/20!! THIS SHOULDNT BE WITHIN THE NEXT IF STATEMENT!!!!!
#data$whr <- ifelse(is.na(data$whr), data$waist/data$hip, data$whr)

check <- ddply(data, .(id_study), function(tmp) any(!is.na(tmp$bmi)) & (all(is.na(tmp$height))|all(is.na(tmp$weight))))
bmi_only_surveyids <- as.character(check[check$V1==TRUE, 1])
for (var in c("weight", "height", "waist", "hip", "whr", "bmi")) {
  data <- clean_anthro(data, var, bmi_only_surveyids = bmi_only_surveyids)
}

# names_select <- setdiff(names(data), grep("(height|weight|bmi|waist|hip|whr|sbp|dbp)(?=[1-9]|_avg|$)", names(data), value=TRUE, perl = TRUE))
# data <- data[, names_select]

