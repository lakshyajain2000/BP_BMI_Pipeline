library(plyr)
library(tidyverse)

date <- format(Sys.Date(),"%Y%m%d")


# Load all data without steps
# Set the date to a consistent format

wd <- "S:/Projects/HeightProject/Original dataset/Mulitple_risk_factors/Anthro_BP"  # Set working directory to folder where the output RDS file will be saved

indir <- "S:/Projects/HeightProject/Original dataset/Data/Surveys/Extracted survey/Merged/" # Call the folder with the merged dataset in 
dhs   <- readRDS("S:/Projects/HeightProject/Original dataset/Data/Surveys/DHS/DHS-formatted_latest.RDS")  # DHS dataframe
data  <- readRDS("S:/Projects/HeightProject/Original dataset/Data/Surveys/Extracted survey/Merged/individual_extracted_latest.RDS")

data_list <- readRDS(paste(indir,"survey_data_availability.RDS",sep="")) #List of metadata for each survey and what variables are available

# Removes columns from DHS and STEPS that are not in the other individual data
dhs   <- dhs[,names(dhs)%in%c(names(data),"self_hyper_12mos")]

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
dhs   <- subset(dhs,dhs$id_study%in%bp_list)

data <- rbind.fill(data,dhs) # combine all STEPS, DHS and individual data together 
rm(dhs)
data$id_study <- as.factor(data$id_study)

data$dropped <- ""
data$excluded <- 0

# Subset dataframe to desired variables

data_small<-subset(data, select = c(bmi, height, waist, weight, id_study, unit_height, unit_weight, unit_waist, age))
data_small$height<- clean_data(data_small, 'height')
data_small$weight <- clean_data(data_small, 'weight')
data_small$bmi <- ifelse(is.na(data_small$bmi), data_small$weight/((data_small$height/100)^2), data_small$bmi) # add bmi by calculation
data_small$bmi <- clean_data(data_small, 'bmi')
data_small$waist[which(data_small$waist<=0|data_small$waist>200)] <- NA # Make negative values 0
data_id<-sort(as.character(unique(data_small$id_study)))

# Calculate percentage of measurements below 50

waist_percentage<-as.data.frame(cbind(data_id, rep(NA, length(data_id))))
for (i in 1:length(data_id)){
  study_i <- data_small %>% filter(id_study==data_id[i])
  waist_percentage[i, 2] <- (sum(study_i$waist < 50, na.rm = TRUE)/nrow(study_i))*100
}


waist_percentage$V2<-as.numeric(waist_percentage$V2)
waist_percentage <- waist_percentage[order(waist_percentage[, 2], decreasing=TRUE), ]
studies_investigate <- waist_percentage[1:200, ]

pdf("waist v bmi no_STEPS.pdf",   # The directory you want to save the file in
    width = 16, # The width of the plot in inches
    height = 10)

plot_list <- list()
for (i in 1:nrow(studies_investigate)){
  study_i <- data_small %>% filter(id_study==studies_investigate[i, 1])
  p<-ggplot(study_i, aes(x=waist, y=bmi))+
    geom_point()+
    xlab("Waist Circumference (cm)")+
    ylab("BMI (kg/m^2)")+
    ggtitle(studies_investigate[i, 1], "\n")+
    theme(plot.title = element_text(size = 20), 
          axis.text = element_text(size = 12))
  print(p)
  plot_list[[i]]<-p
}
dev.off()
