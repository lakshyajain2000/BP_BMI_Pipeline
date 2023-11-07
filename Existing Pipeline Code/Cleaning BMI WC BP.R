# Bin Zhou
# 25 March 2020
# Cleaning BMI WC and BP for Aidan

library(plyr)
library(dplyr)

date <- format(Sys.Date(),"%Y%m%d")

wd <- "S:/Projects/HeightProject/Original dataset/Data/Surveys/Extracted survey/Merged/Outgoings/For Aidan/"

indir <- "S:/Projects/HeightProject/Original dataset/Data/Surveys/Extracted survey/Merged/"
# dhs   <- readRDS("S:/Projects/HeightProject/Original dataset/Data/Surveys/DHS/archived/DHS-formatted_20211109.RDS")  # DHS
steps <- readRDS("S:/Projects/HeightProject/Original dataset/Data/Surveys/STEPS/old/STEPSdata_GLU_BP_chol_formatted_20211109.RDS")  # STEPS
data  <- readRDS(paste0(indir,"old/individual_extracted_2021 11 08.RDS"))
dhs <- data[c(),]

data <- data %>% select(id_study, id, sex, age, age_mean, age_group,
                psu, stratum, samplewt_anthro, samplewt_bp, samplewt_wh, samplewt_smoke,
                mid_year, start_year, end_year,
                iso, country, survey, survey_short,
                survey_type, urban_rural, is_urban,
                age_min_anthro_F, age_min_anthro_M, age_max_anthro_F, age_max_anthro_M,
                age_min_bp_F, age_min_bp_M, age_max_bp_F, age_max_bp_M, averaged_properly, is_multi_bp, is_multi_cuff, unit_bp,
                matches('bmi|height|weight|waist|hip|whr|sbp|dbp|is_pregnant'),
                self_hyper, drug_hyper, drug_bp, drug_presc)

# data_list <- readRDS(paste(indir,"survey_data_availability.RDS",sep=""))
# bp_anthro_list <- data_list$id_study[which(data_list$has_bp=="YES" & data_list$has_anthro=="YES")]

dhs   <- dhs[,names(dhs)%in%c(names(data),"self_hyper_12mos")]
steps <- steps[,names(steps)%in%c(names(data),"self_hyper_12mos")]
gc()

# change sex in DHS to 1/2
# dhs$sex <- as.numeric(dhs$sex)

# assign scope and coverage to Uzb DHS, Portugal PPSOC
dhs$survey_type[dhs$id_study=="UZB_2002_DHS"] <- "National"
dhs$urban_rural[dhs$id_study=="UZB_2002_DHS"] <- "both"
data$survey_type[data$id_study=="PRT_2009_PPSOC"] <- "Community"
data$urban_rural[data$id_study=="PRT_2009_PPSOC"] <- "urban"

# assign scope to COL STEPS 2010 (only for glucose and cholesterol)
#data$urban_rural[data$id_study=="COL_2010_STEPS"] <- "urban"
# change scope for Canadian CAN_1996_CaMos: was one site only; 13 May 2019
data$survey_type[data$id_study=="CAN_1996_CaMos"] <- "Community"

## remove surveys without any bp data ##
# data  <- subset(data,data$id_study%in%bp_list)
# steps <- subset(steps,steps$id_study%in%bp_list)
# dhs   <- subset(dhs,dhs$id_study%in%bp_list)

data <- rbind.fill(data,dhs,steps)
rm(dhs,steps)
data$id_study <- as.factor(data$id_study)
gc()

#set up log file
sink(paste0(wd,"log_AllCleaning_",date, ".txt"))

data$dropped <- ""

print_cleaned <- function(var) {
  print(paste("Percentage Cleaned (No. of cleaned/No. of non-NAs):",var,"(%)"))
  if (length(clnList)==0) {
    print("No records cleaned")
  } else {
    cln.table <- table(data[clnList,]$id_study)/table(data[!is.na(data[var]),]$id_study)*100
    print(sort(round(cln.table[which(cln.table!=Inf&cln.table>0)],2), decreasing=TRUE))
  }
}


# 1. Clean age for 0-120, set the rest to missing  ==============================================
# 2. Clean sex for 1/2, set the rest to missing. Label the variable 1=male, 2=female
data$age[which(data$age>120|data$age<0)] <- NA
data$sex[which(data$sex!=1&data$sex!=2&!is.na(data$sex))] <- NA
# # set sex as factor
# data$sex <- as.factor(data$sex)
# levels(data$sex) <- c("male","female")

# Mark Dropped
dropList <- which(is.na(data$sex)|is.na(data$age))
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
                which((sex==1&((is.na(age_max_bp_M)|age>age_max_bp_M)|(is.na(age_min_bp_M)|age<age_min_bp_M))) |
                        (sex==2&((is.na(age_max_bp_F)|age>age_max_bp_F)|(is.na(age_min_bp_F)|age<age_min_bp_F))) ) )
data$dropped[clnList] <- paste(data$dropped[clnList], "DesignAgeBP")
# Mark Dropped
clnList <- with(data,
                which((sex==1&((is.na(age_max_anthro_M)|age>age_max_anthro_M)|(is.na(age_min_anthro_M)|age<age_min_anthro_M))) |
                        (sex==2&((is.na(age_max_anthro_F)|age>age_max_anthro_F)|(is.na(age_min_anthro_F)|age<age_min_anthro_F))) ) )
data$dropped[clnList] <- paste(data$dropped[clnList], "DesignAgeAnthro")

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

### a. sbp ------------------------------------------------------------------
max_sbp <- 270
min_sbp <- 70

N_sbp <- 0
N_sbp_cleaned <- 0
sbp_cleaned <- c()

data$sbp1_f <- data$sbp1
data$sbp1_f[which(data$sbp1_f<=0|data$sbp1_f%in%c(777,888,994,995,996,999))] <- NA
clnList <- which(data$sbp1_f>max_sbp | data$sbp1_f<min_sbp)
N_sbp <- N_sbp + sum(!is.na(data$sbp1_f))
N_sbp_cleaned <- N_sbp_cleaned + length(clnList)
sbp_cleaned <- c(sbp_cleaned, data$sbp1_f[clnList])
print_cleaned("sbp1_f")
data$sbp1_f[clnList] <- NA
data$sbp1_f[which(data$unit_bp=="EXCLUDE")] <- NA

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
data$sbp11_f[which(data$sbp11_f<=0|data$sbp11_f%in%c(777,888,994,995,996,999))] <- NA
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
print(paste0(signif(N_sbp_cleaned/N_sbp, digit=3)*100, "%"))
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
data$self_hyper[which(data$self_hyper<0)] <- NA
clnList <- which(data$self_hyper!=0&data$self_hyper!=1)
print_cleaned("self_hyper")
data$self_hyper[clnList] <- NA

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


### m.q. samplewt_bp, psu, stratum -----------------------------------------
data$samplewt_anthro[which(data$samplewt_anthro<0)] <- NA
data$samplewt_wh[which(data$samplewt_wh<0)] <- NA
data$samplewt_bp[which(data$samplewt_bp<0)] <- NA
data$psu[which(data$psu<0)] <- NA
data$stratum[which(data$stratum<0)] <- NA

## clean samplewt_smoke
data$samplewt_smoke[which(data$samplewt_smoke<0)] <- NA
# use samplewt_bp where samplewt_smoke is unavailable
list_bp  <- unique(data$id_study[!is.na(data$samplewt_bp)])
list_int <- unique(data$id_study[!is.na(data$samplewt_smoke)])
list <- setdiff(list_bp, list_int)
data$samplewt_smoke[data$id_study%in%list] <- data$samplewt_bp[data$id_study%in%list]


### p. is_urban, is_pregnant, is_pregnant_exam ------------------------------
data$is_urban[which(data$is_urban<0)] <- NA
clnList <- which(data$is_urban!=0&data$is_urban!=1)
print_cleaned("is_urban")
data$is_urban[clnList] <- NA

## is_pregnant
data$is_pregnant[which(data$is_pregnant<0)] <- NA
clnList <- which(data$is_pregnant!=0&data$is_pregnant!=1)
print_cleaned("is_pregnant")
data$is_pregnant[clnList] <- NA
# clean for age/sex
clnList <- which(data$is_pregnant==1&(data$sex==1|data$age>=50|data$age<10))
print_cleaned("is_pregnant")
data$is_pregnant[clnList] <- 0
print("Cleaned pregnant males and pregnant females aged over 50 or under 10")
data$is_pregnant[which(data$sex==1)] <- 0

## is_pregnant_exam
data$is_pregnant_exam[which(data$is_pregnant_exam<0)] <- NA
clnList <- which(data$is_pregnant_exam!=0&data$is_pregnant_exam!=1)
print_cleaned("is_pregnant_exam")
data$is_pregnant_exam[clnList] <- NA
# clean for age/sex
clnList <- which(data$is_pregnant_exam==1&(data$sex==1|data$age>=50|data$age<10))
print_cleaned("is_pregnant_exam")
data$is_pregnant_exam[clnList] <- 0
print("Cleaned pregnant males and pregnant females aged over 50 or under 10")
data$is_pregnant_exam[which(data$sex==1)] <- 0

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
rm(DT); gc()
# data<-merge(data,count)   # consumes too much memory

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

# assign drug_hyper when drug_presc or drug_bp exists instead where drug_hyper does not exist
list <- which(is.na(data$drug_hyper) & !is.na(data$drug_presc) & data$id_study%in%count$id_study[which(count$N_drug_all==0)])
data$drug_hyper[list] <- data$drug_presc[list]
## drug_bp currently adding no new info 14 March 2018
## if use, recode drug use to 0 if self_hyper is 0
# list <- which(is.na(data$drug_hyper) & is.na(data$drug_presc) & !is.na(data$drug_bp) & data$N_drug_all==0)
# data$drug_hyper[list] <- data$drug_bp[list]

# Clean empty string in Dropped
data$dropped[which(data$dropped=="")] <- " Keep"
data$dropped <- factor(data$dropped)
levels(data$dropped) <- substr(levels(data$dropped), 2, nchar(levels(data$dropped)) )


### Calculate average BP ====
calc_avg <- function(x){
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
x<-which(data$id_study%in%c("PYF_2010_STEPS", "ITA_2011_CONVERGI"))
data$sbp_final[x] <- apply(data[x,c("sbp_avg_f","sbp2_f","sbp2_f","sbp3_f")],1,calc_avg)
data$dbp_final[x] <- apply(data[x,c("dbp_avg_f","dbp2_f","dbp2_f","dbp3_f")],1,calc_avg)

# assign mean sbp/dbp in GRC Didima and POL_2011_NATPOL
# treat 6 measurement as 2 sets of 3 measurements, drop the 1st and 4th measurements
x<-which(data$id_study%in%c("GRC_1997_Didima","POL_2011_NATPOL","POL_2002_NATPOL"))
data$sbp_final[x] <- apply(data[x,c("sbp_avg_f","sbp1_f","sbp2_f","sbp3_f","sbp5_f","sbp6_f")],1,calc_avg)
data$dbp_final[x] <- apply(data[x,c("dbp_avg_f","dbp1_f","dbp2_f","dbp3_f","dbp5_f","dbp6_f")],1,calc_avg)


## Defining diagnosis and medication ======================================================
# Diagnosis: cleaned self_hyper when available; self_hyper_12mos when not
data$diagnosis <- data$self_hyper
l1 <- unique(data$id_study[!is.na(data$self_hyper)])
l2 <- unique(data$id_study[!is.na(data$self_hyper_12mos)])
list <- l2[!l2%in%l1]
data$diagnosis[data$id_study%in%list] <- data$self_hyper_12mos[data$id_study%in%list]
# New_med: cleaned drug_hyper
# if new_med=1 but diagnosis=0, recode diagnosis to 1
data$new_med <- data$drug_hyper
data$diagnosis[which(data$diagnosis==0 & data$new_med==1)] <- 1  # decided on call 20180412


## Define raised BP and hypertension using 140/90 and 160/100 cut-off ===================================
# Raised BP defind as: SBP >= 140 OR DBP >= 90
# data$BP_140_90 <- 0
# data$BP_140_90[which(data$sbp_final>=140 | data$dbp_final>=90)] <- 1
# data$BP_140_90[which(is.na(data$sbp_final) | is.na(data$dbp_final))] <- NA
# # Raised BP defind as: SBP >= 140 OR DBP >= 90, AND medication data available (ie not missing)
# data$BP_140_90_wmed <- data$BP_140_90
# data$BP_140_90_wmed[is.na(data$new_med)] <- NA
# HTN defind as: SBP >= 140 OR DBP >= 90 OR medication
data$HTN_140_90_med <- 0
data$HTN_140_90_med[which(data$sbp_final>=140 | data$dbp_final>=90 | data$new_med==1)] <- 1
data$HTN_140_90_med[which(is.na(data$sbp_final) | is.na(data$dbp_final) | is.na(data$new_med))] <- NA
# # Raised BP (stage 2) defined as: SBP >= 160_OR DBP >= 100
# data$BP_160_100 <- 0
# data$BP_160_100[which(data$sbp_final>=160 | data$dbp_final>=100)] <- 1
# data$BP_160_100[which(is.na(data$sbp_final) | is.na(data$dbp_final))] <- NA
# # Raised BP (stage 2) defined as: SBP >=160 OR DBP >=100, AND medication data available (ie not missing)
# data$BP_160_100_wmed <- data$BP_160_100
# data$BP_160_100_wmed[is.na(data$new_med)] <- NA
# # HTN (stage 2) defind as: SBP >= 140 OR DBP >= 90 OR medication
# data$HTN_160_100_med <- 0
# data$HTN_160_100_med[which(data$sbp_final>=160 | data$dbp_final>=100 | data$new_med==1)] <- 1
# data$HTN_160_100_med[which(is.na(data$sbp_final) | is.na(data$dbp_final) | is.na(data$new_med))] <- NA
# # Controlled HTN defind as: medication AND SBP < 140 AND DBP < 90
# data$ctrlHTN_140_90_med <- 0
# data$ctrlHTN_140_90_med[which(data$sbp_final<140 & data$dbp_final<90 & data$new_med==1)] <- 1
# data$ctrlHTN_140_90_med[which(is.na(data$sbp_final) | is.na(data$dbp_final) | is.na(data$new_med))] <- NA


#Clean urban/rural variables 'urban_rural' and 'is_urban'
data$urban_rural[data$urban_rural=='mixed'] <- 'both'
table(data$is_urban, data$urban_rural, exclude=NULL)

unique(data$id_study[which(data$urban_rural=='rural' & data$is_urban == 1)] )# rural studies with participants coded as urban
unique(data$id_study[which(data$urban_rural=='urban' & data$is_urban == 0)] )# urban studies with participants coded as rural
# check if these studies should be 'both'
# in some cases only urban / only rural studies have been coded as having specifically urban/rural participants in 'is_urban'
# 'is_urban' should be NA for only urban / only rural studies

# recode is_urban to 'urban'/'rural'/'NA'
data$is_urban[data$urban_rural %in% c('rural','urban')] <- NA
table(data$is_urban, data$urban_rural, exclude=NULL)
data$is_urban <- ifelse(data$is_urban==0, 'rural', ifelse(data$is_urban==1, 'urban', 'NA'))

# Clean BMI ===================================
source("S:/Projects/HeightProject/Original dataset/Anthropometrics/Individual_data/Anthropometrics_cleaning_functions.R")

## Calculate mean weight, height, wiast, hip when multiple metrics are available
data$height1[data$height1<0] <- NA; data$height2[data$height2<0] <- NA; data$height3[data$height3<0] <- NA
data$weight1[data$weight1<0] <- NA; data$weight2[data$weight2<0] <- NA; data$weight3[data$weight3<0] <- NA
data$waist1[data$waist1<0] <- NA; data$waist2[data$waist2<0] <- NA; data$waist3[data$waist3<0] <- NA
data$hip1[data$hip1<0] <- NA; data$hip2[data$hip2<0] <- NA; data$hip3[data$hip3<0] <- NA

data$weight_avg <- rowMeans(subset(data, select = c("weight1", "weight2", "weight3")), na.rm = TRUE)
data$height_avg <- rowMeans(subset(data, select = c("height1", "height2", "height3")), na.rm = TRUE)
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
data$bmi <- ifelse(is.na(data$bmi), data$weight/((data$height/100)^2), data$bmi) ## UPDATED ON 06/02/20!! THIS SHOULDNT BE WITHIN THE NEXT IF STATEMENT!!!!!
data$whr <- ifelse(is.na(data$whr), data$waist/data$hip, data$whr)

check <- ddply(data, .(id_study), function(tmp) any(!is.na(tmp$bmi)) & (all(is.na(tmp$height))|all(is.na(tmp$weight))))
bmi_only_surveyids <- as.character(check[check$V1==TRUE, 1])
for (var in c("weight", "height", "waist", "hip", "whr", "bmi")) {
    data <- clean_anthro(data, var, bmi_only_surveyids = bmi_only_surveyids)
}

# names_select <- setdiff(names(data), grep("(height|weight|bmi|waist|hip|whr|sbp|dbp)(?=[1-9]|_avg|$)", names(data), value=TRUE, perl = TRUE))
# data <- data[, names_select]


# Output ========================================================================================
saveRDS(data,file=paste0(wd,"Anthro_WH_BMI_cleaned_replicate_submission_version_allcols.RDS"))
sink()
