# Looking at the issues in the full dataset

data  <- readRDS("/Volumes/HeightProject/Original dataset/Mulitple_risk_factors/Anthro_BPBP_Anthro_Indiv_Cleaned.RDS")

# Line in height-waist

library(tidyverse)

data_equal<- data %>% filter(height==waist)
data_id<-as.character(unique(data_equal$id_study))
data_equal_full <- subset(data, id_study %in% data_id)
data_equal_full <- data_equal_full %>% mutate(diff = height-waist)


pdf("equal waist-height no ylim.pdf",   # The directory you want to save the file in
    width = 16, # The width of the plot in inches
    height = 10)


plot_list <- list()
for (i in 1:183){
  study_i <- data %>% filter(id_study==data_id[i])
  diff<-as.data.frame(study_i$height-study_i$waist)
  diff<-diff %>%
    filter(!is.na(study_i$height-study_i$waist))
  p<-ggplot(diff, aes(x=diff[,1], fill = diff[,1]==0))+
    geom_histogram(bins=400, show.legend = FALSE)+
    xlab("Height-Waist")+
    ggtitle(data_id[i])
  print(p)
  plot_list[[i]]<-p
}
dev.off()

problem_surveys<-c("MEX_2006_ENSANUT", "MMR_2014_STEPS", "PRY_2011_enctfactriesgo",
                   "MEX_2010_SAGE", "SEN_2015_STEPS", "ARE_2018_STEPS","BFA_2013_STEPS",
                   "BFA_2021_STEPS", "CIV_2005_STEPS_urban", "COM_2011_STEPS", "DZA_2017_STEPS",
                   "EGY_2017_STEPS", "FSM_2009_STEPS_yap", "LKA_2021_STEPS", "MDA_2013_STEPS",
                   "MDG_2005_STEPS", "TJK_2016_STEPS", "TON_2011_STEPS",
                   "TZA_2012_STEPS", "UGA_2014_STEPS","TCD_2008_STEPS")

problem_surveys<-sort(problem_surveys)
problem_surveys<-c(problem_surveys,"TUN_1997_AHP")


pdf("Equal Height-Waist spike at 0.pdf",   # The directory you want to save the file in
    width = 16, # The width of the plot in inches
    height = 10)


plot_list <- list()
for (i in 1:22){
  study_i <- data %>% filter(id_study==problem_surveys[i])
  diff<-as.data.frame(study_i$height-study_i$waist)
  diff<-diff %>%
    filter(!is.na(study_i$height-study_i$waist))
  p<-ggplot(diff, aes(x=diff[,1], fill = diff[,1]==0))+
    geom_histogram(binwidth=0.25, show.legend = FALSE)+
    xlab("Height-Waist")+
    labs(title = paste(problem_surveys[i], "Percentage of 0s:", (nrow(subset(diff, diff[, 1]==0))/nrow(diff))*100))
  print(p)
  plot_list[[i]]<-p
}
dev.off()


subset(diff, diff[, 1]==0)

diff[,1]
