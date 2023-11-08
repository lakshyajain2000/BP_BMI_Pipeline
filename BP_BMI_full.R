#Full data cleaning and summarising for BP by BMI and hypertension treatment by BMI
# Output will be RDS files grouped by BMI (each entry assigned a BMI category)

library(plyr)
library(dplyr)

date <- format(Sys.Date(),"%Y%m%d")

#Set working directory to Anthro_BP folder
wd <- "S:/Projects/HeightProject/Original dataset/Data/Mulitple_risk_factors/Anthro_BP/"

# Individual studies have already been merged at this point
indir <- "S:/Projects/HeightProject/Original dataset/Data/Surveys/Extracted survey/Merged"
dhs   <- readRDS("S:/Projects/HeightProject/Original dataset/Data/Surveys/DHS/archived/DHS-formatted_20211109.RDS")  # DHS
steps <- readRDS("S:/Projects/HeightProject/Original dataset/Data/Surveys/STEPS/old/STEPSdata_GLU_BP_chol_formatted_20211109.RDS")  # STEPS
data  <- readRDS(paste0(indir,"old/BP_individual_wBMI_data.RDS"))

# Remove all irrelevant studies

