
# Multivariate cleaning script

# This script will clean all variables pairwise to return a completely clean dataframe ready for summarising

source("~/Documents/Pipeline/BP_BMI_Pipeline/Existing Pipeline Code/Mahalanobis_detection.R")

data <- readRDS("~/Documents/Pipeline/BP_BMI_PipelineBP_Anthro_Indiv_Cleaned.RDS")

#W

pdf("Mahalanobis_plots.pdf",   # The directory you want to save the file in
    width = 12, # The width of the plot in inches
    height = 10)


# Multivariate cleaning script

# This script will clean all variables pairwise to return a completely clean dataframe ready for summarising

# Clean SBP and DBP together

bp_clean <- maha_clean(data$sbp_final, data$dbp_final)
bp_clean$scatter
length(bp_clean$outliers)
if (length(bp_clean$outliers)>0) data_clean <- data[-bp_clean$outliers, ]

# Clean Height and weight together

hw_clean <- maha_clean(data$height, data$weight)
hw_clean$scatter
length(hw_clean$outliers)
if (length(hw_clean$outliers)>0) data_clean1 <- data[-hw_clean$outliers, ]

# Clean Height and BMI together

height_to_drop <- c()
weight
bmi
waist circum
whr waist-to-hip-ratio

hBMI_clean <- maha_clean(data$height, data$bmi)
hBMI_clean$scatter
length(hBMI_clean$outliers)
if (length(hBMI_clean$outliers)>0) data_clean2 <- data[-hBMI_clean$outliers, ]
# add to height/weight/bmi lists


# Clean Weight and BMI together

wBMI_clean <- maha_clean(data$weight, data$bmi)
wBMI_clean$scatter
length(wBMI_clean$outliers)
if (length(wBMI_clean$outliers)>0) data_clean3 <- data[-wBMI_clean$outliers, ]
# add to height/weight/bmi lists

# Clean Height and WC together

hWC_clean <- maha_clean(data$waist, data$height)
hWC_clean$scatter
print(length(hWC_clean$outliers))
if (length(hWC_clean$outliers)>0) data_clean4 <- data[-hWC_clean$outliers, ]
# add to height/bmi/wc/whtr lists

# Clean Weight and WC together

wWC_clean <- maha_clean(data$waist, data$weight)
wWC_clean$scatter
print(length(wWC_clean$outliers))
if (length(wWC_clean$outliers)>0) data_clean5 <- data[-wWC_clean$outliers, ]
# add to weight/bmi/wc/whtr lists

# Clean Height and WHtR together

hWtHR_clean <- maha_clean(data$wth, data$height)
hWtHR_clean$scatter
print(length(hWtHR_clean$outliers))
if (length(hWtHR_clean$outliers)>0) data_clean6 <- data[-hWtHR_clean$outliers, ]
# add to height/bmi/wc/whtr lists

# Clean Weight and WtHR together

wWtHR_clean <- maha_clean(data$wth, data$height)
wWtHR_clean$scatter
print(length(wWtHR_clean$outliers))
if (length(wWtHR_clean$outliers)>0) data_clean7 <- data[-wWtHR_clean$outliers, ]
# add to weight/bmi/wc/whtr lists

# Clean WC and BMI together

WCBMI_clean <- maha_clean(data$waist, data$bmi)
WCBMI_clean$scatter
print(length(WCBMI_clean$outliers))
if (length(WCBMI_clean$outliers)>0) data_clean8 <- data[-WCBMI_clean$outliers, ]
# add to weight/bmi/wc/whtr/height lists

# Clean WC and WtHR together

WCWtHR_clean <- maha_clean(data$waist, data$wth)
WCWtHR_clean$scatter
print(length(WCWtHR_clean$outliers))
if (length(WCWtHR_clean$outliers)>0) data_clean9 <- data[-WCWtHR_clean$outliers, ]
# add to wc/whtr lists

# Clean WtHR and BMI together

BMIWtHR_clean <- maha_clean(data$bmi, data$wth)
BMIWtHR_clean$scatter
print(length(BMIWtHR_clean$outliers))
if (length(BMIWtHR_clean$outliers)>0) data_clean10 <- data[-BMIWtHR_clean$outliers, ]
# add to weight/bmi/wc/whtr/height lists


data<-rbind(data_clean,data_clean1,data_clean2,data_clean3, data_clean4, data_clean5, data_clean6,
            data_clean7, data_clean8, data_clean9, clean_data10)

data %>% distinct()

dev.off()
