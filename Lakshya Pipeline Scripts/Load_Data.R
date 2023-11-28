# Data loading file: Taken from BP Cleaning 

library(plyr)

date <- format(Sys.Date(),"%Y%m%d")


# Load all data
# Set the date to a consistent format

wd <- "~/Documents/Pipeline/BP_BMI_Pipeline"  # Set working directory to folder where the output RDS file will be saved

indir <- "/Volumes/HeightProject/Original dataset/Data/Surveys/Extracted survey/Merged/" # Call the folder with the merged dataset in 
dhs   <- readRDS("/Volumes/HeightProject/Original dataset/Data/Surveys/DHS/DHS-formatted_latest.RDS")  # DHS dataframe
steps <- readRDS("/Volumes/HeightProject/Original dataset/Data/Surveys/STEPS/STEPSdata_GLU_BP_chol_formatted_latest.RDS")  # STEPS dataframe
data  <- readRDS("/Volumes/HeightProject/Original dataset/Data/Surveys/Extracted survey/Merged/testing subsets/1percent_subset_individual_extracted_20231019.RDS")


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

data$dropped <- ""
data$excluded <- 0