# Full BP by BMI Pipeline (sourcing from other files)

source("~/Documents/Pipeline/BP_BMI_Pipeline/Lakshya Pipeline Scripts/Load_Data.R") # Load data

source("~/Documents/Pipeline/BP_BMI_Pipeline/Lakshya Pipeline Scripts/Clean_Anthro_BP_sep.R") # clean data individually

source("~/Documents/Pipeline/BP_BMI_Pipeline/Lakshya Pipeline Scripts/Multicleaning.R") # clean data together

# At this point there will be a cleaned dataset ready to summarise