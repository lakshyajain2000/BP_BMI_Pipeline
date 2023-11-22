source("~/Documents/Pipeline/BP_BMI_Pipeline/cleaning_functions.R")

#set up log file
sink(paste0(wd,"log_BPCleaning_",date, ".txt"))

# Clean all metadata variables--------------------------------

#age
data$age<-clean_data(data,'age')

clnList <- which(is.na(data$age))
age.na <- table(data[clnList,]$id_study)/table(data$id_study)*100
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

bp_missing<-clean_missing_bp_col()



