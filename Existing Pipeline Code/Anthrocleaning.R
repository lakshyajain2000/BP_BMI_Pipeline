################################################################################
########################## PRELIMINARIES #######################################
################################################################################


## Load required packages
library(survey)
library(plyr)
library(reshape)
#library(doParallel) # currently not used
#library(data.table) # currently not used
#library(dtplyr)
library(dplyr)
library(stringi)


## Initial settings
rm(list=ls(all=TRUE))
date <- format(Sys.Date(),"%Y %m %d") # set date
study <- "OTHER"

## Set working directory and output directories ##
outdir_meta <-"S:/Projects/HeightProject/Original dataset/Anthropometrics/Individual_data/metadata"
outdir_bmi <-"S:/Projects/HeightProject/Original dataset/Anthropometrics/Individual_data/bmi"
outdir_height <-"S:/Projects/HeightProject/Original dataset/Anthropometrics/Individual_data/height"
outdir_meta_ur <-"S:/Projects/HeightProject/Original dataset/Anthropometrics/Individual_data/metadata_ur"
outdir_bmi_ur <-"S:/Projects/HeightProject/Original dataset/Anthropometrics/Individual_data/bmi_ur"
outdir_height_ur <- "S:/Projects/HeightProject/Original dataset/Anthropometrics/Individual_data/height_ur"

################################################################################
############################# DATA #############################################
################################################################################

sink(paste0("S:/Projects/HeightProject/Original dataset/Anthropometrics/Individual_data/cleaning_log1_",date,".txt"))


## Load latest merged dataset
data <- readRDS("S:/Projects/HeightProject/Original dataset/Data/Surveys/Extracted survey/Merged/individual_extracted_latest.RDS")



# temporary
data$age[data$id_study=='GRL_2018_PHSG'] <- data$age_mean[data$id_study=='GRL_2018_PHSG']

only_new_data <- FALSE ## If TRUE, The script only cleans and summarizes the new data sources (compared to the date of the anthro_cleanest_latest.RData file)
## If you want to clean and summarize all data sources, then set to FALSE
if (only_new_data) {
  ## Check new data sources ##
  latest_anthro <- file.info("S:/Projects/HeightProject/Original dataset/Anthropometrics/Individual_data/anthro_cleaned_latest.RDS")$mtime
  data$mod_time_num <- as.numeric(data$mod_time)
  data <- filter(data, mod_time_num >= as.numeric(latest_anthro))
  if (nrow(data) == 0) {
    message("STOP: Nothing to be updated!!")
  }
}

## Load functions
source("S:/Projects/HeightProject/Original dataset/Anthropometrics/Individual_data/Anthropometrics_cleaning_functions.R")


################################################################################
########################## DATA CLEANING #######################################
################################################################################
data_list <- readRDS("S:/Projects/HeightProject/Original dataset/Data/Surveys/Extracted survey/Merged/survey_data_availability.RDS")


## remove surveys without any anthro data ##
data_list <- subset(data_list, !(data_list$id_study =="GTM_2015_DHS" & data_list$has_anthro ==""))

anthro_list <- data_list$id_study[which(data_list$has_anthro=="YES")]

#data  <- data %>% filter(data$id_study%in%anthro_list)
data <- subset(data, data$id_study %in% anthro_list)

##Drop columns which aren't used
anthro_var <- c('id_study', 'id', 'sex', 'age','age_mean', 'age_group',   # Bin added 'id' 20220107
                'psu','stratum', 'samplewt_anthro',
                'mid_year', 'start_year', 'end_year',
                'iso', 'country', 'survey', 'survey_short',
                'survey_type', 'urban_rural', 'is_urban', "is_urban_all",
                'age_range_F', 'age_range_M',
                'age_min_anthro_F', 'age_min_anthro_M', 'age_max_anthro_F', 'age_max_anthro_M',
                'is_pregnant', grep("height|waist|hip|weight|bmi|whr", colnames(data), value = T))
data <- data[, intersect(anthro_var, names(data))]

data$id_study <- as.factor(data$id_study) #will this cause issues later?

## GENERAL CLEANING ##

## Sex check ##
table(data$sex, exclude = NULL) # possible values for sex variable

## Recode age <5 to NA
data$age[data$age <5] <- NA
data$age <- trunc(data$age) # keeps only integers

## Clean age and gender variables
data <- clean_anthro(data, var = "age")
data <- clean_anthro(data, var = "sex")

## Keep data for which no missing values for age, sex (clean) and only population >5 years
data <- data %>% filter(data$age >= 5 & !is.na(data$age) & !is.na(data$sex))

## Remove studies where we use different subsets for different RF eg. PHL NNS
tmp_exc <- read.csv("S:/Projects/HeightProject/Original dataset/Anthropometrics/Individual_data/temp_exclusions.csv", stringsAsFactors = FALSE)
for(i in 2:nrow(tmp_exc)){ # ARM: 26/12/21 - start in (2) - second row
  ind <- unlist(strsplit(tmp_exc[i,"ind_exclude"],";"))
  ind <- stringi::stri_c(ind,collapse="|")
  data[data$id_study %in% tmp_exc[i,"id_study"], grep(pattern = ind,names(data),value = T)] <- NA
}


## Move DOWNSTREAM
## Remove GSHS surveys with enrollment rate below 70 pc
gshs_exc <- read.csv("S:/Projects/HeightProject/Original dataset/Anthropometrics/Individual_data/GSHS_exclusions.csv")
gshs_exc_m <-gshs_exc %>%filter ( sex == 1)
gshs_exc_f <- gshs_exc %>% filter( sex == 2)
ind_m <- which(data$id_study %in% gshs_exc_m$id_study & data$sex == 1)
ind_f <- which(data$id_study %in% gshs_exc_f$id_study & data$sex == 2)
all_ind <- c(ind_m, ind_f)
if(length(all_ind) > 0) {
  data <- data[-all_ind, ]
}

#for study sex groups excluded above update age min/max anthro so not flagged downstream for age range but no data
data$age_min_anthro_F[data$id_study %in% gshs_exc_f$id_study] <- NA
data$age_max_anthro_F[data$id_study %in% gshs_exc_f$id_study] <- NA
data$age_min_anthro_M[data$id_study %in% gshs_exc_m$id_study] <- NA
data$age_max_anthro_M[data$id_study %in% gshs_exc_m$id_study] <- NA

## Change urban_rural and/or survey_type for studies with different coverage/scope for different risk factors   # Bin 20210906
unique(data$id_study[data$survey_type %in% c("mixed", "Mixed")])

# IRN_2017_IRCAP: National for anthro (others are subnational)
data$survey_type[which(data$survey_type  %in% c("mixed", "Mixed") & data$id_study=='IRN_2017_IRCAP')] <- "National"
# CHN_2008_CLHLS CHN_2012_CLHLS CHN_2014_CLHLS CHN_2018_CLHLS : National for anthro
data$survey_type[which(data$survey_type  %in% c("mixed", "Mixed") & data$id_study %in% c("CHN_2008_CLHLS", "CHN_2012_CLHLS", "CHN_2014_CLHLS", "CHN_2018_CLHLS"))] <- "National"
# CAN_1996_CaMos: #Although the study was conducted in 9 cities blood samples were only takin in one (thus becomes community for blood-based RFs) - Subantionl anthro
data$survey_type[which(data$survey_type  %in% c("mixed", "Mixed") & data$id_study=='CAN_1996_CaMos')] <- "Subnational"

# COL_2010_STEPS: both for anthro (glucose and cholesterol urban only)
data$urban_rural[which(data$urban_rural == "mixed" & data$id_study=='COL_2010_STEPS')] <- "both"

##Temp correct Dec 22
data$survey_type[which(data$id_study=="IRN_2018_easterniran")] <- "Community"

###clean multiple measurements **only up tp 3 measures **
data <- clean_anthro(data, var = "multiple height")
data <- clean_anthro(data, var = "multiple weight")
data <- clean_anthro(data, var = "multiple waist")
data <- clean_anthro(data, var = "multiple hip")

## Calculate mean weight, height, waist, hip when multiple metrics are available
data$weight_avg <- rowMeans(subset(data, select = c("weight1", "weight2", "weight3")), na.rm = TRUE)
data$height_avg <- rowMeans(subset(data, select = c("height1", "height2", "height3")), na.rm = TRUE)
data$waist_avg <- rowMeans(subset(data, select = c("waist1", "waist2", "waist3")), na.rm = TRUE)
data$hip_avg <- rowMeans(subset(data, select = c("hip1", "hip2", "hip3")), na.rm = TRUE)

list_to_change <- which(is.na(data$weight) & !is.na(data$weight_avg))
data$weight[list_to_change] <- data$weight_avg[list_to_change]
list_to_change <- which(is.na(data$height) & !is.na(data$height_avg))
data$height[list_to_change] <- data$height_avg[list_to_change]
list_to_change <- which(is.na(data$hip) & !is.na(data$hip_avg))
data$hip[list_to_change] <- data$hip_avg[list_to_change]
list_to_change <- which(is.na(data$waist) & !is.na(data$waist))
data$waist[list_to_change] <- data$waist[list_to_change]


## Drop columns that are not needed ##
data[colnames(data) %in% c("weight_avg", "height_avg", "waist_avg", "hip_avg",  "waist1", "waist2", "waist3", "hip1", "hip2", "hip3", "height1", "height2","height3", "weight1","weight2","weight3")] <- NULL

## Update bmi and whr variables (re-calculate them when missing, eg. bmi from measured weight and height)
# Check calculated vs reported BMI
summary(data$bmi[!is.na(data$bmi)])
summary(data$weight[!is.na(data$bmi)]/((data$height[!is.na(data$bmi)]/100)^2))

check_idx <- which(!is.na(data$height) & !is.na(data$weight) & !is.na(data$bmi))
if (length(check_idx) > 0) {
  #plot calculated BMI vs reported BMI
  plot(data$weight[check_idx]/((data$height[check_idx]/100)^2), data$bmi[check_idx])
}

list_to_change <- which(is.na(data$bmi))
data$bmi[list_to_change] <- data$weight[list_to_change] / ((data$height[list_to_change]/100)^2)

list_to_change <- which(is.na(data$whr))
data$whr[list_to_change] <- data$waist[list_to_change]/data$hip[list_to_change]

## Keep data for which anthro data (before cleaning - for metadata)
data <- data %>% filter(!is.na(data$bmi)| !is.na(data$weight) | !is.na(data$height) |
                          !is.na(data$hip) | !is.na(data$waist) | !is.na(data$whr))

## Clean pregnant, BMI, height, weight, waist, hip, whr according to age group (PRs updated by Nia)
check <- ddply(data,.(id_study), function(tmp)any(!is.na(tmp$bmi)) & (all(is.na(tmp$height))|all(is.na(tmp$weight))))
bmi_only_surveyids <- as.character(check[check$V1==TRUE, 1])
## bmi_only_surveyids is the list of surveys which report just bmi, but not height and weight: 2019 06 28 = "TUR_1995_TARF", "USA_1991_ARIC"

overall_clean_list <- c()
for (var in c("pregnant", "weight", "height", "waist", "hip", "whr","bmi")) { # make sure pregnant goes first, and BMI last!
  data <- clean_anthro(data, var, bmi_only_surveyids = bmi_only_surveyids)
}

pregnant_list      <- which(data$is_pregnant == 1)
N_pregnant         <- length(pregnant_list)
overall_clean_list <- setdiff(overall_clean_list, pregnant_list)
N_clean            <- length(overall_clean_list)

N_bmi_clean        <- sum(!is.na(data$bmi_clean))

print(paste0("Proportion pregnant (all studies): ", 100 *N_pregnant/(N_pregnant + N_clean + N_bmi_clean)))
print(paste0("Proportion cleaned (all studies): ", 100 * N_clean/(N_pregnant + N_clean + N_bmi_clean)))

#subset to surveys from post 1980 [[and national 1977-1979]]
subset_1980 <- data
subset_1980$mid_year[which(subset_1980$survey_type == "National" & subset_1980$mid_year %in% c(1977,1978,1979))] <- 1980
subset_1980 <- subset_1980[which(subset_1980$mid_year >= 1980),]
overall_clean_list <- c()
for (var in c("pregnant", "weight", "height", "waist", "hip", "whr","bmi")) { # make sure pregnant goes first, and BMI last!
  subset_1980 <- clean_anthro(subset_1980, var, bmi_only_surveyids = bmi_only_surveyids)
}
pregnant_list      <- which(subset_1980$is_pregnant == 1)
N_pregnant         <- length(pregnant_list)
overall_clean_list <- setdiff(overall_clean_list, pregnant_list)
N_clean            <- length(overall_clean_list)

N_bmi_clean        <- sum(!is.na(subset_1980$bmi_clean))

print(paste0("Proportion pregnant (studies post 1980): ", 100 *N_pregnant/(N_pregnant + N_clean + N_bmi_clean)))
print(paste0("Proportion cleaned (studies post 1980): ", 100 * N_clean/(N_pregnant + N_clean + N_bmi_clean)))



## Check if a complete survey was dropped just for cleaning
setdiff(data$id_study[!is.na(data$bmi)] ,data$id_study[!is.na(data$bmi_clean)])
setdiff(data$id_study[!is.na(data$height)] ,data$id_study[!is.na(data$height_clean)])
setdiff(data$id_study[!is.na(data$weight)] ,data$id_study[!is.na(data$weight_clean)])



## Create study age range groups & age groups

## Add age design variables
data$age_design_max[(data$sex ==1)] <- data$age_max_anthro_M[(data$sex ==1)]
data$age_design_min[(data$sex ==1)] <- data$age_min_anthro_M[(data$sex ==1)]
data$age_design_max[(data$sex ==2)] <- data$age_max_anthro_F[(data$sex ==2)]
data$age_design_min[(data$sex ==2)] <- data$age_min_anthro_F[(data$sex ==2)]

# Age are already in age groups for some studies
list <- which(!is.na(data$age_group))
print(unique(data$id_study[list]))
data$age_mean0 <- data$age_group0 <- NA
data$age_mean0[list]  <- data$age_mean[list]
data$age_group0[list] <- as.character(data$age_group[list])

data <- age_range_groups(data)
age_groups <- make_age_groups(age=data$age, age_design_min=data$age_design_min,
                              age_design_max=data$age_design_max, anthro = TRUE) # ARM changed 26/12/21
data$age_mean <- age_groups$age_mean
data$age_group <- age_groups$age_group

data$age_mean[list]  <- data$age_mean0[list]
data$age_group[list] <- data$age_group0[list]
data$age_mean0 <- data$age_group0 <- NULL

## Clean manually special case of single age group #YFS and The Jerusalem Longitudinal Cohort Study PSWG
special_case <- c("YFS_rural","YFS_urban","JLCS","PSWG")
data$age_group 	<- ifelse(data$survey_short %in% special_case, data$age, data$age_group)
data$age_mean 	<- ifelse(data$survey_short %in% special_case, data$age, data$age_mean)

## Survey design
# Check whether there is a mix of subjects with reported and unreported (NA) (Just informative)
# of survey design variables within a study (problematic studies)
# December 2022 - Rosie updated to also count participants with samplewts =0
data$id_study <- as.character(data$id_study)
rownames(data) <- stringi::stri_c("S", 1:nrow(data), sep = "")
data$names <- rownames(data)
a_c <- ddply(data,.(id_study), function(tmp) svy_check(tmp, var = "samplewt_anthro"))[, c("id_study", "N_NA", "%_NA",  "min_sw", "max_sw")] # Will print an error if there are not NAs
print(a_c)
# BLZ_2005_CAMDI 33pc, HND_2004_CAMDI 30 pc, GTM_2002_CAMDI 26pc, SLV_2004_CAMDI 34pc.
p_c <- ddply(data,.(id_study), function(tmp) svy_check(tmp, var = "psu"))[, c("id_study", "N_NA", "%_NA")]
print(p_c)
# MEX_2012_ENSANUT
s_c <- ddply(data,.(id_study), function(tmp) svy_check(tmp, var = "stratum"))[, c("id_study", "N_NA", "%_NA")]
print(s_c)

## Remove subjects with NA values in problematic studies (see function information)
# December 2022 - Rosie updated to drop participants with samplewts =0
data <- clean_svydesign(data) # Do not forget to add rownames to data (see above)

## Set to NA psu of survey-sex-age groups with single PSU
# If the study has only one psu, it should be set to NA. Else the function svydesign gives an error
psu_ans <- ddply(data,.(id_study), function(tmp) length(unique(tmp$psu)))
psu_one <- as.character(filter(psu_ans, V1 == 1)$id_study)
if(length(psu_one) > 0) {
  data$psu[data$id_study %in% psu_one] <- NA
}
psu_ans_stratified <- ddply(data,.(id_study, sex, age_mean), function(tmp) length(unique(tmp$psu)) == 1 &
                              !all(is.na(tmp$psu)) & nrow(tmp) > 1)
filter(psu_ans_stratified, V1 == TRUE) ## psu will be set to NA within the get_summary function

psu_ans_stratified_ur <- ddply(data,.(id_study, sex, age_mean, is_urban), function(tmp) length(unique(tmp$psu)) == 1 &
                                 !all(is.na(tmp$psu)) & nrow(tmp) > 1)
filter(psu_ans_stratified_ur, V1 == TRUE) ## psu will be set to NA within the get_summary function

## Check summaries ##
summary(data$bmi); summary(data$bmi_clean)
summary(data$height); summary(data$height_clean)
summary(data$weight); summary(data$weight_clean)
summary(data$hip); summary(data$hip_clean)
summary(data$waist); summary(data$waist_clean)


################################################################################
################################################################################

## URBAN_RURAL CLEANING ##

## Clean urban_rural variable #
data$urban_rural <- as.character(data$urban_rural)
data$is_urban <- as.character(data$is_urban)
if(any(!data$urban_rural %in% c('both','urban','rural'))) {
  stop ('Recode variable urban_rural: there are values other than urban/rural/both')
}
if(any(!(data$is_urban%in%c(0,1, NA)))) {
  stop ('Recode variable is_urban: there are values other than 0/1/NA')
}

##Temporary correction correct downstream # note JOR_2004_BRFSS relabelled rural as there is only 1 urban individual with measurements
data$is_urban[data$id_study == "JOR_2004_BRFSS"] <- 0

# ## Check consistency between urban_rural and is_urban variables
num_Cat <- ddply(data, .(id_study), function(tmp) data.frame(n = length(unique(tmp$is_urban))))
data <- merge(data, num_Cat, by =('id_study')) ; rm(num_Cat)
table(data$urban_rural, data$is_urban,data$n,  exclude = NULL)
# #check extraction of following studies
with(data[data$id_study%in%unique(data$id_study[which(data$urban_rural%in%c('urban','rural') &  data$n>1)]),], table(id_study, is_urban, urban_rural,exclude=NULL))


## Save data ##
anthro_var <- c('id_study', 'id', 'sex', 'age','age_mean', 'age_group',   # Bin added 'id' 20220107
                'psu','stratum', 'samplewt_anthro',
                'mid_year', 'start_year', 'end_year',
                'iso', 'country', 'survey', 'survey_short',
                'survey_type', 'urban_rural', 'is_urban',
                'age_range_F', 'age_range_M',
                'age_min_anthro_F', 'age_min_anthro_M', 'age_max_anthro_F', 'age_max_anthro_M',
                'bmi_clean', 'height_clean', 'weight_clean', 'waist_clean', 'hip_clean', 'whr_clean',
                'bmi', 'height', 'weight', 'waist', 'hip', 'whr', 'is_pregnant')
data <- data[order(as.character(data$id_study)), anthro_var]
if (only_new_data) {
  data_new <- data
  rm(data)
  data <- readRDS("S:/Projects/HeightProject/Original dataset/Anthropometrics/Individual_data/anthro_cleaned_latest.RDS")
  common_cols <- intersect(names(data), names(data_new))
  if(length(intersect(data$id_study, data_new$id_study)) > 0) {
    message("Some studies from data_new are included in anthro_cleaned_latest. Replacing studies in data by data_new")
    data <- rbind(subset(data, !id_study %in% data_new$id_study)[, common_cols], data_new[, common_cols])
  } else {
    data <- rbind(data[, common_cols], data_new[, common_cols])
  }
  saveRDS(data, file ="S:/Projects/HeightProject/Original dataset/Anthropometrics/Individual_data/anthro_cleaned_latest.RDS")
  
  file.copy(from = "S:/Projects/HeightProject/Original dataset/Anthropometrics/Individual_data/anthro_cleaned_latest.RDS",
            to = stringi::stri_c("S:/Projects/HeightProject/Original dataset/Anthropometrics/Individual_data/anthro_old/anthro_cleaned_latest",
                                 date, ".RDS", sep = ""))
  
  rm(data)
  data <- data_new
  rm(data_new)
} else {
  saveRDS(data, file ="S:/Projects/HeightProject/Original dataset/Anthropometrics/Individual_data/anthro_cleaned_latest.RDS")
  
  file.copy(from = "S:/Projects/HeightProject/Original dataset/Anthropometrics/Individual_data/anthro_cleaned_latest.RDS",
            to = stringi::stri_c("S:/Projects/HeightProject/Original dataset/Anthropometrics/Individual_data/anthro_old/anthro_cleaned_latest",
                                 date, ".RDS", sep = ""))
  
}
rm(list=ls(all=TRUE)); gc()


sink()

# q()
