# Bin Zhou
# 13 June 2014
# Cleaning BP data
# added 8 measurements
# added self_hyper_12mos 8 Feb 2018

library(plyr)

date <- format(Sys.Date(),"%Y%m%d") # Set the date to a consistent format

wd <- "~/Documents/Pipeline/BP_BMI_Pipeline"  # Set working directory to folder where the output RDS file will be saved

indir <- "/Volumes/HeightProject/Original dataset/Data/Surveys/Extracted survey/Merged/" # Call the folder with the merged dataset in 
dhs   <- readRDS("/Volumes/HeightProject/Original dataset/Data/Surveys/DHS/DHS-formatted_latest.RDS")  # DHS dataframe
steps <- readRDS("/Volumes/HeightProject/Original dataset/Data/Surveys/STEPS/STEPSdata_GLU_BP_chol_formatted_latest.RDS")  # STEPS dataframe
data  <- readRDS(paste0(indir,"BP_individual_wBMI_data.RDS"))


data_list <- readRDS(paste(indir,"survey_data_availability.RDS",sep="")) #List of metadata for each survey and what variables are available

# Removes columns from DHS and STEPS that are not in the other individual data
dhs   <- dhs[,names(dhs)%in%c(names(data),"self_hyper_12mos")]
steps <- steps[,names(steps)%in%c(names(data),"self_hyper_12mos")]

# change sex in DHS to 1/2
dhs$sex <- as.numeric(dhs$sex)

# temporary correction 20170112
levels(data$unit_bp) <- c("mmHg", "EXCLUDE")

# assign scope and coverage to Uzb DHS, Portugal PPSOC
dhs$survey_type[dhs$id_study=="UZB_2002_DHS"] <- "National"
dhs$urban_rural[dhs$id_study=="UZB_2002_DHS"] <- "both"
data$survey_type[data$id_study=="PRT_2009_PPSOC"] <- "Community"
data$urban_rural[data$id_study=="PRT_2009_PPSOC"] <- "urban"
# change scope for IRN IRCAP: email from Meryem 19/10/2020
data$survey_type[data$id_study=="IRN_2017_IRCAP"] <- "Subnational"

# assign scope to COL STEPS 2010 (only for glucose and cholesterol)
#data$urban_rural[data$id_study=="COL_2010_STEPS"] <- "urban"
# change scope for Canadian CAN_1996_CaMos: was one site only; 13 May 2019
data$survey_type[data$id_study=="CAN_1996_CaMos"] <- "Community"

# China CLHLS 2008/12/24/18 are national (subnational for blood measures; older rounds are only national)
data$survey_type[data$id_study %in% c("CHN_2008_CLHLS", "CHN_2012_CLHLS", "CHN_2014_CLHLS", "CHN_2018_CLHLS")] <- "National"

## remove surveys without any bp data ##
bp_list <- data_list$id_study[which(data_list$has_bp=="YES")]
data  <- subset(data,data$id_study%in%bp_list)
steps <- subset(steps,steps$id_study%in%bp_list)
dhs   <- subset(dhs,dhs$id_study%in%bp_list)

data <- rbind.fill(data,dhs,steps) # combine all STEPS, DHS and individual data together 
rm(dhs,steps)
data$id_study <- as.factor(data$id_study)

#set up log file
sink(paste0(wd,"log_BPCleaning_",date, ".txt"))

data$dropped <- ""
data$excluded <- 0

print_cleaned <- function(var) {
  print(paste("Percentage Cleaned (No. of cleaned/No. of non-NAs):",var,"(%)"))
  if (length(clnList)==0) {
    print("No records cleaned")
  } else {
    cln.table <- table(data[clnList,]$id_study)/table(data[!is.na(data[var]),]$id_study)*100
    print(sort(round(cln.table[which(cln.table!=Inf&cln.table>0)],2), decreasing=TRUE))
  }
} # Create a function that will output success rate for each variable per study in the log file


# 1. Clean age for 0-120, set the rest to missing  ==============================================
# 2. Clean sex for 1/2, set the rest to missing. Label the variable 1=male, 2=female
data$age[which(data$age>120|data$age<0)] <- NA
data$sex[which(data$sex!=1&data$sex!=2&!is.na(data$sex))] <- NA
# set sex as factor
data$sex <- as.factor(data$sex)
levels(data$sex) <- c("male","female")

# Mark Dropped
dropList <- which(is.na(data$sex)|(is.na(data$age) & is.na(data$age_mean) & is.na(data$age_group)))
data$dropped[dropList] <- paste(data$dropped[dropList],"AgeSex")

# 3. Table the missing percentages for sex and age by study =====================================
clnList <- which(is.na(data$age))
age.na <- table(data[clnList,]$id_study)/table(data$id_study)*100
print("Percentage of NAs in Age (%)")
print(sort(round(age.na[which(age.na>0)],2), decreasing=TRUE))

clnList <- which(is.na(data$sex))
sex.na <- table(data[clnList,]$id_study)/table(data$id_study)*100
print("Percentage of NAs in Sex (%)")
print(sort(round(sex.na[which(sex.na>0)],2), decreasing=TRUE))

# Clean for ages outside the design range -------------------------------------------------------
# floor age
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

# 4. Clean data in bp ======================================================================
##   set negatives to missing #
##   exclude if unit == "EXCLUDE" #

if (!"sbp4"%in%names(data))   data$sbp4 <- data$dbp4 <- NA  # for when only a subset is loaded for cleaning and sbp4 onwards do not exist
if (!"sbp5"%in%names(data))   data$sbp5 <- data$dbp5 <- NA
if (!"sbp6"%in%names(data))   data$sbp6 <- data$dbp6 <- NA
if (!"sbp7"%in%names(data))   data$sbp7 <- data$dbp7 <- NA
if (!"sbp8"%in%names(data))   data$sbp8 <- data$dbp8 <- NA
if (!"sbp9"%in%names(data))   data$sbp9 <- data$dbp9 <- NA
if (!"sbp10"%in%names(data))  data$sbp10 <- data$dbp10 <- NA
if (!"sbp11"%in%names(data))  data$sbp11 <- data$dbp11 <- NA
if (!"sbp12"%in%names(data))  data$sbp12 <- data$dbp12 <- NA
if (!"sbp13"%in%names(data))  data$sbp13 <- data$dbp13 <- NA
if (!"sbp_avg"%in%names(data))  data$sbp_avg <- data$dbp_avg <- NA

### a. sbp ------------------------------------------------------------------
max_sbp <- 270
min_sbp <- 70

N_sbp <- 0
N_sbp_cleaned <- 0
sbp_cleaned <- c()

data$sbp1_f <- data$sbp1
data$sbp1_f[which(data$sbp1_f<=0|data$sbp1_f%in%c(777,888,994,995,996,999))] <- NA # removing all negative values and common missing value entries
clnList <- which(data$sbp1_f>max_sbp | data$sbp1_f<min_sbp) # flagging all entries whose values that are outside the plausible range
N_sbp <- N_sbp + sum(!is.na(data$sbp1_f)) # number of people left are those who don't have NA for this variable - ie they didnt have NA before and their entry hasnt become NA for being implausible
N_sbp_cleaned <- N_sbp_cleaned + length(clnList) # number of people that have become NA because their values were outside the plausible range
sbp_cleaned <- c(sbp_cleaned, data$sbp1_f[clnList]) 
print_cleaned("sbp1_f")
data$sbp1_f[clnList] <- NA # Make all the implausible values NA
data$sbp1_f[which(data$unit_bp=="EXCLUDE")] <- NA # Make the values which have already be decided to be excluded to be NA

data$sbp2_f <- data$sbp2
data$sbp2_f[which(data$sbp2_f<=0|data$sbp2_f%in%c(777,888,994,995,996,999))] <- NA
clnList <- which(data$sbp2_f>max_sbp | data$sbp2_f<min_sbp)
N_sbp <- N_sbp + sum(!is.na(data$sbp2_f))
N_sbp_cleaned <- N_sbp_cleaned + length(clnList)
sbp_cleaned <- c(sbp_cleaned, data$sbp2_f[clnList])
print_cleaned("sbp2_f")
data$sbp2_f[clnList] <- NA
data$sbp2_f[which(data$unit_bp=="EXCLUDE")] <- NA

data$sbp3_f <- data$sbp3
data$sbp3_f[which(data$sbp3_f<=0|data$sbp3_f%in%c(777,888,994,995,996,999))] <- NA
clnList <- which(data$sbp3_f>max_sbp | data$sbp3_f<min_sbp)
N_sbp <- N_sbp + sum(!is.na(data$sbp3_f))
N_sbp_cleaned <- N_sbp_cleaned + length(clnList)
sbp_cleaned <- c(sbp_cleaned, data$sbp3_f[clnList])
print_cleaned("sbp3_f")
data$sbp3_f[clnList] <- NA
data$sbp3_f[which(data$unit_bp=="EXCLUDE")] <- NA

data$sbp4_f <- data$sbp4
data$sbp4_f[which(data$sbp4_f<=0|data$sbp4_f%in%c(777,888,994,995,996,999))] <- NA
clnList <- which(data$sbp4_f>max_sbp | data$sbp4_f<min_sbp)
N_sbp <- N_sbp + sum(!is.na(data$sbp4_f))
N_sbp_cleaned <- N_sbp_cleaned + length(clnList)
sbp_cleaned <- c(sbp_cleaned, data$sbp4_f[clnList])
print_cleaned("sbp4_f")
data$sbp4_f[clnList] <- NA
data$sbp4_f[which(data$unit_bp=="EXCLUDE")] <- NA

data$sbp5_f <- data$sbp5
data$sbp5_f[which(data$sbp5_f<=0|data$sbp5_f%in%c(777,888,994,995,996,999))] <- NA
clnList <- which(data$sbp5_f>max_sbp | data$sbp5_f<min_sbp)
N_sbp <- N_sbp + sum(!is.na(data$sbp5_f))
N_sbp_cleaned <- N_sbp_cleaned + length(clnList)
sbp_cleaned <- c(sbp_cleaned, data$sbp5_f[clnList])
print_cleaned("sbp5_f")
data$sbp5_f[clnList] <- NA
data$sbp5_f[which(data$unit_bp=="EXCLUDE")] <- NA

data$sbp6_f <- data$sbp6
data$sbp6_f[which(data$sbp6_f<=0|data$sbp6_f%in%c(777,888,994,995,996,999))] <- NA
clnList <- which(data$sbp6_f>max_sbp | data$sbp6_f<min_sbp)
N_sbp <- N_sbp + sum(!is.na(data$sbp6_f))
N_sbp_cleaned <- N_sbp_cleaned + length(clnList)
sbp_cleaned <- c(sbp_cleaned, data$sbp6_f[clnList])
print_cleaned("sbp6_f")
data$sbp6_f[clnList] <- NA
data$sbp6_f[which(data$unit_bp=="EXCLUDE")] <- NA

data$sbp7_f <- data$sbp7
data$sbp7_f[which(data$sbp7_f<=0|data$sbp7_f%in%c(777,888,994,995,996,999))] <- NA
clnList <- which(data$sbp7_f>max_sbp | data$sbp7_f<min_sbp)
N_sbp <- N_sbp + sum(!is.na(data$sbp7_f))
N_sbp_cleaned <- N_sbp_cleaned + length(clnList)
sbp_cleaned <- c(sbp_cleaned, data$sbp7_f[clnList])
print_cleaned("sbp7_f")
data$sbp7_f[clnList] <- NA
data$sbp7_f[which(data$unit_bp=="EXCLUDE")] <- NA

data$sbp8_f <- data$sbp8
data$sbp8_f[which(data$sbp8_f<=0|data$sbp8_f%in%c(777,888,994,995,996,999))] <- NA
clnList <- which(data$sbp8_f>max_sbp | data$sbp8_f<min_sbp)
N_sbp <- N_sbp + sum(!is.na(data$sbp8_f))
N_sbp_cleaned <- N_sbp_cleaned + length(clnList)
sbp_cleaned <- c(sbp_cleaned, data$sbp8_f[clnList])
print_cleaned("sbp8_f")
data$sbp8_f[clnList] <- NA
data$sbp8_f[which(data$unit_bp=="EXCLUDE")] <- NA

data$sbp9_f <- data$sbp9
data$sbp9_f[which(data$sbp9_f<=0|data$sbp9_f%in%c(777,888,994,995,996,999))] <- NA
clnList <- which(data$sbp9_f>max_sbp | data$sbp9_f<min_sbp)
N_sbp <- N_sbp + sum(!is.na(data$sbp9_f))
N_sbp_cleaned <- N_sbp_cleaned + length(clnList)
sbp_cleaned <- c(sbp_cleaned, data$sbp9_f[clnList])
print_cleaned("sbp9_f")
data$sbp9_f[clnList] <- NA
data$sbp9_f[which(data$unit_bp=="EXCLUDE")] <- NA

data$sbp10_f <- data$sbp10
data$sbp10_f[which(data$sbp10_f<=0|data$sbp10_f%in%c(777,888,994,995,996,999))] <- NA
clnList <- which(data$sbp10_f>max_sbp | data$sbp10_f<min_sbp)
N_sbp <- N_sbp + sum(!is.na(data$sbp10_f))
N_sbp_cleaned <- N_sbp_cleaned + length(clnList)
sbp_cleaned <- c(sbp_cleaned, data$sbp10_f[clnList])
print_cleaned("sbp10_f")
data$sbp10_f[clnList] <- NA
data$sbp10_f[which(data$unit_bp=="EXCLUDE")] <- NA

data$sbp11_f <- data$sbp11
data$sbp11_f[which(data$sbp11_f<=0|data$sbp11_f%in%c(777,888,994,995,996,999))] <- NA # common code for missing data - need to be variable specific
clnList <- which(data$sbp11_f>max_sbp | data$sbp11_f<min_sbp)
N_sbp <- N_sbp + sum(!is.na(data$sbp11_f))
N_sbp_cleaned <- N_sbp_cleaned + length(clnList)
sbp_cleaned <- c(sbp_cleaned, data$sbp11_f[clnList])
print_cleaned("sbp11_f")
data$sbp11_f[clnList] <- NA
data$sbp11_f[which(data$unit_bp=="EXCLUDE")] <- NA

data$sbp12_f <- data$sbp12
data$sbp12_f[which(data$sbp12_f<=0|data$sbp12_f%in%c(777,888,994,995,996,999))] <- NA
clnList <- which(data$sbp12_f>max_sbp | data$sbp12_f<min_sbp)
N_sbp <- N_sbp + sum(!is.na(data$sbp12_f))
N_sbp_cleaned <- N_sbp_cleaned + length(clnList)
sbp_cleaned <- c(sbp_cleaned, data$sbp12_f[clnList])
print_cleaned("sbp12_f")
data$sbp12_f[clnList] <- NA
data$sbp12_f[which(data$unit_bp=="EXCLUDE")] <- NA

data$sbp13_f <- data$sbp13
data$sbp13_f[which(data$sbp13_f<=0|data$sbp13_f%in%c(777,888,994,995,996,999))] <- NA
clnList <- which(data$sbp13_f>max_sbp | data$sbp13_f<min_sbp)
N_sbp <- N_sbp + sum(!is.na(data$sbp13_f))
N_sbp_cleaned <- N_sbp_cleaned + length(clnList)
sbp_cleaned <- c(sbp_cleaned, data$sbp13_f[clnList])
print_cleaned("sbp13_f")
data$sbp13_f[clnList] <- NA
data$sbp13_f[which(data$unit_bp=="EXCLUDE")] <- NA

data$sbp_avg_f <- data$sbp_avg
data$sbp_avg_f[which(data$sbp_avg_f<=0|data$sbp_avg_f%in%c(777,888,994,995,996,999))] <- NA
clnList <- which(data$sbp_avg_f>max_sbp | data$sbp_avg_f<min_sbp)
N_sbp <- N_sbp + sum(!is.na(data$sbp_avg_f))
N_sbp_cleaned <- N_sbp_cleaned + length(clnList)
sbp_cleaned <- c(sbp_cleaned, data$sbp_avg_f[clnList])
print_cleaned("sbp_avg_f")
data$sbp_avg_f[clnList] <- NA
data$sbp_avg_f[which(data$unit_bp=="EXCLUDE")] <- NA

print("Excluded SBP data outside of the plausible range")
print(paste0(signif(N_sbp_cleaned/N_sbp, digit=3)*100, "%")) # Calculating the percentage of data points dropped because they were outside of the range 
print(table(round(sbp_cleaned))) 

### b.dbp ------------------------------------------------------------------
max_dbp <- 150
min_dbp <- 30

N_dbp <- 0
N_dbp_cleaned <- 0
dbp_cleaned <- c()

data$dbp1_f <- data$dbp1
data$dbp1_f[which(data$dbp1_f<=0|data$dbp1_f%in%c(222,666,888,994,995,996,999,1000,9999))] <- NA
clnList <- which(data$dbp1_f>max_dbp | data$dbp1_f<min_dbp)
N_dbp <- N_dbp + sum(!is.na(data$dbp1_f))
N_dbp_cleaned <- N_dbp_cleaned + length(clnList)
dbp_cleaned <- c(dbp_cleaned, data$dbp1_f[clnList])
print_cleaned("dbp1_f")
data$dbp1_f[clnList] <- NA
data$dbp1_f[which(data$unit_bp=="EXCLUDE")] <- NA

data$dbp2_f <- data$dbp2
data$dbp2_f[which(data$dbp2_f<=0|data$dbp2_f%in%c(222,666,888,994,995,996,999,1000,9999))] <- NA
clnList <- which(data$dbp2_f>max_dbp | data$dbp2_f<min_dbp)
N_dbp <- N_dbp + sum(!is.na(data$dbp2_f))
N_dbp_cleaned <- N_dbp_cleaned + length(clnList)
dbp_cleaned <- c(dbp_cleaned, data$dbp2_f[clnList])
print_cleaned("dbp2_f")
data$dbp2_f[clnList] <- NA
data$dbp2_f[which(data$unit_bp=="EXCLUDE")] <- NA

data$dbp3_f <- data$dbp3
data$dbp3_f[which(data$dbp3_f<=0|data$dbp3_f%in%c(222,666,888,994,995,996,999,1000,9999))] <- NA
clnList <- which(data$dbp3_f>max_dbp | data$dbp3_f<min_dbp)
N_dbp <- N_dbp + sum(!is.na(data$dbp3_f))
N_dbp_cleaned <- N_dbp_cleaned + length(clnList)
dbp_cleaned <- c(dbp_cleaned, data$dbp3_f[clnList])
print_cleaned("dbp3_f")
data$dbp3_f[clnList] <- NA
data$dbp3_f[which(data$unit_bp=="EXCLUDE")] <- NA

data$dbp4_f <- data$dbp4
data$dbp4_f[which(data$dbp4_f<=0|data$dbp4_f%in%c(222,666,888,994,995,996,999,1000,9999))] <- NA
clnList <- which(data$dbp4_f>max_dbp | data$dbp4_f<min_dbp)
N_dbp <- N_dbp + sum(!is.na(data$dbp4_f))
N_dbp_cleaned <- N_dbp_cleaned + length(clnList)
dbp_cleaned <- c(dbp_cleaned, data$dbp4_f[clnList])
print_cleaned("dbp4_f")
data$dbp4_f[clnList] <- NA
data$dbp4_f[which(data$unit_bp=="EXCLUDE")] <- NA

data$dbp5_f <- data$dbp5
data$dbp5_f[which(data$dbp5_f<=0|data$dbp5_f%in%c(222,666,888,994,995,996,999,1000,9999))] <- NA
clnList <- which(data$dbp5_f>max_dbp | data$dbp5_f<min_dbp)
N_dbp <- N_dbp + sum(!is.na(data$dbp5_f))
N_dbp_cleaned <- N_dbp_cleaned + length(clnList)
dbp_cleaned <- c(dbp_cleaned, data$dbp5_f[clnList])
print_cleaned("dbp5_f")
data$dbp5_f[clnList] <- NA
data$dbp5_f[which(data$unit_bp=="EXCLUDE")] <- NA

data$dbp6_f <- data$dbp6
data$dbp6_f[which(data$dbp6_f<=0|data$dbp6_f%in%c(222,666,888,994,995,996,999,1000,9999))] <- NA
clnList <- which(data$dbp6_f>max_dbp | data$dbp6_f<min_dbp)
N_dbp <- N_dbp + sum(!is.na(data$dbp6_f))
N_dbp_cleaned <- N_dbp_cleaned + length(clnList)
dbp_cleaned <- c(dbp_cleaned, data$dbp6_f[clnList])
print_cleaned("dbp6_f")
data$dbp6_f[clnList] <- NA
data$dbp6_f[which(data$unit_bp=="EXCLUDE")] <- NA

data$dbp7_f <- data$dbp7
data$dbp7_f[which(data$dbp7_f<=0|data$dbp7_f%in%c(222,666,888,994,995,996,999,1000,9999))] <- NA
clnList <- which(data$dbp7_f>max_dbp | data$dbp7_f<min_dbp)
N_dbp <- N_dbp + sum(!is.na(data$dbp7_f))
N_dbp_cleaned <- N_dbp_cleaned + length(clnList)
dbp_cleaned <- c(dbp_cleaned, data$dbp7_f[clnList])
print_cleaned("dbp7_f")
data$dbp7_f[clnList] <- NA
data$dbp7_f[which(data$unit_bp=="EXCLUDE")] <- NA

data$dbp8_f <- data$dbp8
data$dbp8_f[which(data$dbp8_f<=0|data$dbp8_f%in%c(222,666,888,994,995,996,999,1000,9999))] <- NA
clnList <- which(data$dbp8_f>max_dbp | data$dbp8_f<min_dbp)
N_dbp <- N_dbp + sum(!is.na(data$dbp8_f))
N_dbp_cleaned <- N_dbp_cleaned + length(clnList)
dbp_cleaned <- c(dbp_cleaned, data$dbp8_f[clnList])
print_cleaned("dbp8_f")
data$dbp8_f[clnList] <- NA
data$dbp8_f[which(data$unit_bp=="EXCLUDE")] <- NA

data$dbp9_f <- data$dbp9
data$dbp9_f[which(data$dbp9_f<=0|data$dbp9_f%in%c(222,666,888,994,995,996,999,1000,9999))] <- NA
clnList <- which(data$dbp9_f>max_dbp | data$dbp9_f<min_dbp)
N_dbp <- N_dbp + sum(!is.na(data$dbp9_f))
N_dbp_cleaned <- N_dbp_cleaned + length(clnList)
dbp_cleaned <- c(dbp_cleaned, data$dbp9_f[clnList])
print_cleaned("dbp9_f")
data$dbp9_f[clnList] <- NA
data$dbp9_f[which(data$unit_bp=="EXCLUDE")] <- NA

data$dbp10_f <- data$dbp10
data$dbp10_f[which(data$dbp10_f<=0|data$dbp10_f%in%c(222,666,888,994,995,996,999,1000,9999))] <- NA
clnList <- which(data$dbp10_f>max_dbp | data$dbp10_f<min_dbp)
N_dbp <- N_dbp + sum(!is.na(data$dbp10_f))
N_dbp_cleaned <- N_dbp_cleaned + length(clnList)
dbp_cleaned <- c(dbp_cleaned, data$dbp10_f[clnList])
print_cleaned("dbp10_f")
data$dbp10_f[clnList] <- NA
data$dbp10_f[which(data$unit_bp=="EXCLUDE")] <- NA

data$dbp11_f <- data$dbp11
data$dbp11_f[which(data$dbp11_f<=0|data$dbp11_f%in%c(222,666,888,994,995,996,999,1000,9999))] <- NA
clnList <- which(data$dbp11_f>max_dbp | data$dbp11_f<min_dbp)
N_dbp <- N_dbp + sum(!is.na(data$dbp11_f))
N_dbp_cleaned <- N_dbp_cleaned + length(clnList)
dbp_cleaned <- c(dbp_cleaned, data$dbp11_f[clnList])
print_cleaned("dbp11_f")
data$dbp11_f[clnList] <- NA
data$dbp11_f[which(data$unit_bp=="EXCLUDE")] <- NA

data$dbp12_f <- data$dbp12
data$dbp12_f[which(data$dbp12_f<=0|data$dbp12_f%in%c(222,666,888,994,995,996,999,1000,9999))] <- NA
clnList <- which(data$dbp12_f>max_dbp | data$dbp12_f<min_dbp)
N_dbp <- N_dbp + sum(!is.na(data$dbp12_f))
N_dbp_cleaned <- N_dbp_cleaned + length(clnList)
dbp_cleaned <- c(dbp_cleaned, data$dbp12_f[clnList])
print_cleaned("dbp12_f")
data$dbp12_f[clnList] <- NA
data$dbp12_f[which(data$unit_bp=="EXCLUDE")] <- NA

data$dbp13_f <- data$dbp13
data$dbp13_f[which(data$dbp13_f<=0|data$dbp13_f%in%c(222,666,888,994,995,996,999,1000,9999))] <- NA
clnList <- which(data$dbp13_f>max_dbp | data$dbp13_f<min_dbp)
N_dbp <- N_dbp + sum(!is.na(data$dbp13_f))
N_dbp_cleaned <- N_dbp_cleaned + length(clnList)
dbp_cleaned <- c(dbp_cleaned, data$dbp13_f[clnList])
print_cleaned("dbp13_f")
data$dbp13_f[clnList] <- NA
data$dbp13_f[which(data$unit_bp=="EXCLUDE")] <- NA

data$dbp_avg_f <- data$dbp_avg
data$dbp_avg_f[which(data$dbp_avg_f<=0|data$dbp_avg_f%in%c(222,666,888,994,995,996,999,1000,9999))] <- NA
clnList <- which(data$dbp_avg_f>max_dbp | data$dbp_avg_f<min_dbp)
N_dbp <- N_dbp + sum(!is.na(data$dbp_avg_f))
N_dbp_cleaned <- N_dbp_cleaned + length(clnList)
dbp_cleaned <- c(dbp_cleaned, data$dbp_avg_f[clnList])
print_cleaned("dbp_avg_f")
data$dbp_avg_f[clnList] <- NA
data$dbp_avg_f[which(data$unit_bp=="EXCLUDE")] <- NA

print("Excluded DBP data outside of the plausible range")
print(paste0(signif(N_dbp_cleaned/N_dbp, digit=3)*100,"%"))
print(table(round(dbp_cleaned)))

# Mark Dropped
dropList <- which(is.na(data$sbp1_f) & is.na(data$sbp2_f) & is.na(data$sbp3_f) & is.na(data$sbp4_f) & is.na(data$sbp5_f) &
                  is.na(data$sbp6_f) & is.na(data$sbp7_f) & is.na(data$sbp8_f) & is.na(data$sbp9_f) & is.na(data$sbp10_f) &
                  is.na(data$sbp11_f) & is.na(data$sbp12_f) & is.na(data$sbp13_f) & is.na(data$sbp_avg_f) &
                  is.na(data$dbp1_f) & is.na(data$dbp2_f) & is.na(data$dbp3_f) & is.na(data$dbp4_f) & is.na(data$dbp5_f) &
                  is.na(data$dbp6_f) & is.na(data$dbp7_f) & is.na(data$dbp8_f) & is.na(data$dbp9_f) & is.na(data$dbp10_f) &
                  is.na(data$dbp11_f) & is.na(data$dbp12_f) & is.na(data$dbp13_f) & is.na(data$dbp_avg_f) )
data$dropped[dropList] <- paste(data$dropped[dropList], "NoData")

### g-j. antihypertensive drug use ------------------------------------

# The variable should be either 0 or 1
data$self_hyper[which(data$self_hyper<0)] <- NA # Get rid of negative values
clnList <- which(data$self_hyper!=0&data$self_hyper!=1) # flag the datapoints that dont have the variable as either 0 or 1
print_cleaned("self_hyper") # Print all the studies and percentanges that were dropped that had this variable
data$self_hyper[clnList] <- NA # Make all the wrong values NA

data$self_hyper_12mos[which(data$self_hyper_12mos<0)] <- NA
clnList <- which(data$self_hyper_12mos!=0&data$self_hyper_12mos!=1)
print_cleaned("self_hyper_12mos")
data$self_hyper_12mos[clnList] <- NA

data$drug_presc[which(data$drug_presc<0)] <- NA
clnList <- which(data$drug_presc!=0&data$drug_presc!=1)
print_cleaned("drug_presc")
data$drug_presc[clnList] <- NA

data$drug_bp[which(data$drug_bp<0)] <- NA
clnList <- which(data$drug_bp!=0&data$drug_bp!=1)
print_cleaned("drug_bp")
data$drug_bp[clnList] <- NA

data$drug_hyper[which(data$drug_hyper<0)] <- NA
clnList <- which(data$drug_hyper!=0&data$drug_hyper!=1)
print_cleaned("drug_hyper")
data$drug_hyper[clnList] <- NA

# All of these variables should either be 0 or 1 and this does that 

### m.q. samplewt_bp, psu, stratum -----------------------------------------
data$samplewt_bp[which(data$samplewt_bp<0)] <- NA # Make negative values 0
data$psu[which(data$psu<0)] <- NA # Make negative values 0
data$stratum[which(data$stratum<0)] <- NA # Make negative values 0

## clean samplewt_smoke
data$samplewt_smoke[which(data$samplewt_smoke<0)] <- NA # Make negative values 0
# use samplewt_bp where samplewt_smoke is unavailable
list_bp  <- unique(data$id_study[!is.na(data$samplewt_bp)])
list_int <- unique(data$id_study[!is.na(data$samplewt_smoke)])
list <- setdiff(list_bp, list_int)
data$samplewt_smoke[data$id_study%in%list] <- data$samplewt_bp[data$id_study%in%list]


### p. is_urban, is_pregnant, is_pregnant_exam ------------------------------
data$is_urban[which(data$is_urban<0)] <- NA # All of these variables should either be 0 or 1 and this does that 
clnList <- which(data$is_urban!=0&data$is_urban!=1)
print_cleaned("is_urban")
data$is_urban[clnList] <- NA

## is_pregnant # All of these variables should either be 0 or 1 and this does that 
data$is_pregnant[which(data$is_pregnant<0)] <- NA
clnList <- which(data$is_pregnant!=0&data$is_pregnant!=1)
print_cleaned("is_pregnant")
data$is_pregnant[clnList] <- NA
# clean for age/sex 
clnList <- which(data$is_pregnant==1&(data$sex=="male"|data$age>=50|data$age<10)) # remove all people who are yes for pregnant despite being male, under 10 or 50 and over (so exclude very young and old pregnancies)
print_cleaned("is_pregnant")
data$is_pregnant[clnList] <- 0
print("Cleaned pregnant males and pregnant females aged over 50 or under 10")
data$is_pregnant[which(data$sex=="male")] <- 0 # change those values to no

## is_pregnant_exam
data$is_pregnant_exam[which(data$is_pregnant_exam<0)] <- NA # Remove negative values
clnList <- which(data$is_pregnant_exam!=0&data$is_pregnant_exam!=1) # Flag values that are not 0 or 1
print_cleaned("is_pregnant_exam")
data$is_pregnant_exam[clnList] <- NA # Make those wrong values NA
# clean for age/sex
clnList <- which(data$is_pregnant_exam==1&(data$sex=="male"|data$age>=50|data$age<10))
print_cleaned("is_pregnant_exam")
data$is_pregnant_exam[clnList] <- 0
print("Cleaned pregnant males and pregnant females aged over 50 or under 10")
data$is_pregnant_exam[which(data$sex=="male")] <- 0

# Mark Dropped
dropList <- which(data$is_pregnant_exam==1|(is.na(data$is_pregnant_exam)&data$is_pregnant==1))
data$dropped[dropList] <- paste(data$dropped[dropList], "Preg")

# 6. Generate drug use value (0) for non-hypertensive individuals ---------------
## modified 20140912
library(data.table)
DT <- data.table(data)
count <- as.data.frame(DT[,list(N_drug=sum(!is.na(drug_hyper)),N_all=sum(!is.na(sex))),by=list(id_study,age)])
count0 <- as.data.frame(DT[,list(N_drug_all=sum(!is.na(drug_hyper))),by=list(id_study)])
count <- merge(count,count0)
data<-merge(data,count)
## 20140912
## only for survey/age where drug_hyper is not entirely missing
## and for survey/age with small sample size (cutoff=30), which caused it entirely missing, for surveys with drug available
# data$drug_hyper[which(data$self_hyper==0&is.na(data$drug_hyper)&(data$N_drug>0|(data$N_drug==0&data$N_all<30&data$N_drug_all>0)))] <- 0
## 20141026
## only for survey where drug_hyper is not entirely missing
## age pattern is dealt with case by case
# data$drug_hyper[which(data$self_hyper==0&is.na(data$drug_hyper)&(data$N_drug_all>0))] <- 0   # pending discussion 20180411
## origional
## regardless of drug_hyper availability by surveys/age
# data$drug_hyper[which(data$self_hyper==0&is.na(data$drug_hyper))] <- 0

## assign missing drug_hyper to 0 in NHANES; temporary 19 Sept 2014
# <19 for 2000/2006/2010/2012
data$drug_hyper[which(data$id_study%in%c("USA_2000_NHANES","USA_2006_NHANES","USA_2010_NHANES","USA_2012_NHANES")&data$age<19&is.na(data$drug_hyper))] <- 0

## assign missing drug_hyper to 0 in CHNS; temporary 19 Sept 2014
# <16 for 1991
data$drug_hyper[which(data$id_study%in%c("CHN_1991_CHNS")&data$age<16&is.na(data$drug_hyper))] <- 0
# <14 for 1997/2000
data$drug_hyper[which(data$id_study%in%c("CHN_1997_CHNS","CHN_2000_CHNS")&data$age<14&is.na(data$drug_hyper))] <- 0
# <12 for 2004/2006/2009/2011
data$drug_hyper[which(data$id_study%in%c("CHN_2004_CHNS","CHN_2006_CHNS","CHN_2009_CHNS","CHN_2011_CHNS")&data$age<12&is.na(data$drug_hyper))] <- 0

## assign missing drug_hyper to 0 in STEPS; treatment before Melanie corrects STEPS dataset
# data$drug_hyper[which(substr(as.character(data$id_study),10,14)=="STEPS"&(data$id_study!="MOZ_2005_STEPS")&is.na(data$drug_hyper))] <- 0  # MOZ STEPS was not from WHO 20160808
# removed 20180314: STEPS has been dealt with in the latest dataset from Stefan


# assign drug_hyper when drug_presc or drug_bp exists instead where drug_hyper does not exist
list <- which(is.na(data$drug_hyper) & !is.na(data$drug_presc) & data$N_drug_all==0)
data$drug_hyper[list] <- data$drug_presc[list]
## drug_bp currently adding no new info 14 March 2018
## if use, recode drug use to 0 if self_hyper is 0
# list <- which(is.na(data$drug_hyper) & is.na(data$drug_presc) & !is.na(data$drug_bp) & data$N_drug_all==0)
# data$drug_hyper[list] <- data$drug_bp[list]

# Clean empty string in Dropped
data$dropped[which(data$dropped=="")] <- " Keep"
data$dropped <- factor(data$dropped)
levels(data$dropped) <- substr(levels(data$dropped), 2, nchar(levels(data$dropped)) )

#Clean urban/rural variables 'urban_rural' and 'is_urban'
data$urban_rural[data$urban_rural=='mixed'] <- 'both'
table(data$is_urban, data$urban_rural, exclude=NULL)

unique(data$id_study[which(data$urban_rural=='rural' & data$is_urban == 1)] )# rural studies with participants coded as urban
unique(data$id_study[which(data$urban_rural=='urban' & data$is_urban == 0)] )# urban studies with participants coded as rural
# check if these studies should be 'both'
# in some cases only urban / only rural studies have been coded as having specifically urban/rural participants in 'is_urban'
# 'is_urban' should be NA for only urban / only rural studies

rural_ids <- unique(data $id_study[data $is_urban==0])
unique(data$urban_rural[data$id_study%in%rural_ids])
urban_ids <- unique(data $id_study[data $is_urban==1])
unique(data$urban_rural[data$id_study%in%urban_ids])

setdiff(urban_ids, rural_ids)# mostly because they are urban studies.
unique(data$id_study[data$id_study %in% setdiff(urban_ids, rural_ids)&data$urban_rural == 'both'])  #"USA_1999_CARDIAC5" - need to check
setdiff( rural_ids,urban_ids)# mostly becasue they are urban studies.
unique(data$id_study[data$id_study %in% setdiff(rural_ids,urban_ids)&data$urban_rural == 'both'])

# recode is_urban to 'urban'/'rural'/'NA'
data$is_urban[data$urban_rural %in% c('rural','urban')] <- NA
table(data$is_urban, data$urban_rural, exclude=NULL)

data$is_urban <- ifelse(data$is_urban==0, 'rural', ifelse(data$is_urban==1, 'urban', 'NA'))
# print out surveys where urban/rural assignment has not been finalised (place of residence variable extracted under is_urban_all)
recode_is_urban <- ddply(data[!is.na(data$is_urban_all),],.(id_study, is_urban_all), function(tmp) nrow(tmp))
write.csv(recode_is_urban , file='S:/Projects/HeightProject/Original dataset/Urban_Rural/Metabolic risk factors/Blood pressure/bp_data individual incl dhs steps/recode is_urban_all in data extraction.csv')




# Clean BMI ===================================
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

sink()
saveRDS(data,file=paste0(wd,"BP_individual_data_cleaned_wBMI.RDS"))
