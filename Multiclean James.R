#height_to_drop <- c()
library(RColorBrewer)
library(gridExtra)

#to extract plot limits from ggplot object
get_plot_limits <- function(plot) {
  gb = ggplot_build(plot)
  xmin = gb$layout$panel_params[[1]]$x.range[1]
  xmax = gb$layout$panel_params[[1]]$x.range[2]
  ymin = gb$layout$panel_params[[1]]$y.range[1]
  ymax = gb$layout$panel_params[[1]]$y.range[2]
  list(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)
}



JBfolder <- "S:\\Projects\\HeightProject\\Original dataset\\Waist_hip\\WHtR vs hypertension\\check multivariate cleaning/"


source(paste0(JBfolder,"Mahalanobis_detection1 JB.R"))
data <- readRDS("~/Documents/Pipeline/BP_BMI_PipelineBP_Anthro_Indiv_Cleaned.RDS")

#add id variable so that can join all the exclusions together
data$id <-  1:nrow(data)


t0 <- Sys.time()

# Functionalise this
# Needs lists of rows to get rid of

All_pairs <- data.frame(var1="SBP (log)", var2="DBP",subsetted="Age 20-59",var_pair=1)
bp_clean6 <- maha_clean(data$id[which(data$age>=20 & data$age<60)], log(data$sbp_final[which(data$age>=20 & data$age<60)]), data$dbp_final[which(data$age>=20 & data$age<60)], SD=6)
bp_clean7 <- maha_clean(data$id[which(data$age>=20 & data$age<60)], log(data$sbp_final[which(data$age>=20 & data$age<60)]), data$dbp_final[which(data$age>=20 & data$age<60)], SD=7)
bp_clean8 <- maha_clean(data$id[which(data$age>=20 & data$age<60)], log(data$sbp_final[which(data$age>=20 & data$age<60)]), data$dbp_final[which(data$age>=20 & data$age<60)], SD=8)
cleaned_rows6 <- which(bp_clean6$scatter$data$outlier=="detected")
cleaned_rows7 <- which(bp_clean7$scatter$data$outlier=="detected")
cleaned_rows8 <- which(bp_clean8$scatter$data$outlier=="detected")

d26 <- bp_clean6$scatter$data[cleaned_rows6,]
d26$SD <- rep(6, nrow(d26))
d27 <- bp_clean7$scatter$data[cleaned_rows7,]
d27$SD <- rep(7, nrow(d27))
d28 <- bp_clean8$scatter$data[cleaned_rows8,]
d28$SD <- rep(8, nrow(d28))
d2 <- rbind(d26, d27, d28)
d2$SD<-as.factor(d2$SD)

d2$var_pair <- 1
all_d2 <- d2[,c("id","SD","var_pair")]

SD_number <-  c(6,7,8)
SD_count<- c(nrow(d26), nrow(d27), nrow(d28))
SD_info<-as.data.frame(cbind(SD_number, SD_count))

scat1 <- bp_clean8$scatter$data

g1 <- ggplot(scat1,aes(x=exp(var1),y=var2)) +
  xlab("SBP (mmHg)")+
  ylab("DBP (mmHg)")+
  labs(title = paste("SBP (log) vs DBP ; Age 20-59 (sample size:", nrow(data), "; \nOutliers detected: SD=6:", nrow(d26),", SD=7:", nrow(d27),", SD=8:", nrow(d28),")"))+
  scale_fill_gradientn(colours=c("grey10","grey10","grey10",colorRampPalette( rev(c("red","orange","gold","cornflowerblue", "blue1")) )(100)),trans="log10", limits=c(1,15000) ) + 
  theme_bw() +
  geom_abline(slope = 1, intercept = 0, color = "purple") 

#----------------------------

All_pairs <- rbind(All_pairs, data.frame(var1="SBP (log)", var2="DBP",subsetted="Age 60+",var_pair=2))
bp_clean6_old <- maha_clean(data$id[which(data$age>=60)], log(data$sbp_final[which(data$age>=60)]), data$dbp_final[which(data$age>=60)], SD=6)
bp_clean7_old <- maha_clean(data$id[which(data$age>=60)], log(data$sbp_final[which(data$age>=60)]), data$dbp_final[which(data$age>=60)], SD=7)
bp_clean8_old <- maha_clean(data$id[which(data$age>=60)], log(data$sbp_final[which(data$age>=60)]), data$dbp_final[which(data$age>=60)], SD=8)
cleaned_rows6_old <- which(bp_clean6_old$scatter$data$outlier=="detected")
cleaned_rows7_old <- which(bp_clean7_old$scatter$data$outlier=="detected")
cleaned_rows8_old <- which(bp_clean8_old$scatter$data$outlier=="detected")

d26 <- bp_clean6_old$scatter$data[cleaned_rows6_old,]
d26$SD <- rep(6, nrow(d26))
d27 <- bp_clean7_old$scatter$data[cleaned_rows7_old,]
d27$SD <- rep(7, nrow(d27))
d28 <- bp_clean8_old$scatter$data[cleaned_rows8_old,]
d28$SD <- rep(8, nrow(d28))
d2 <- rbind(d26, d27, d28)
d2$SD<-as.factor(d2$SD)

d2$var_pair <- 2
all_d2 <- rbind(all_d2, d2[,c("id","SD","var_pair")])
scat2 <- bp_clean6_old$scatter$data

SD_number <- c(6,7,8)
SD_count<- c(nrow(d26), nrow(d27), nrow(d28))
SD_info<-as.data.frame(cbind(SD_number, SD_count))

g2 <- ggplot(scat2,aes(x=exp(var1),y=var2)) +
  xlab("SBP (mmHg)")+
  ylab("DBP (mmHG)")+
  labs(title = paste("SBP (log) vs DBP  Age 60+ (sample size:", nrow(data), "; \nOutliers detected: SD=6:", nrow(d26),", SD=7:", nrow(d27),", SD=8:", nrow(d28),")"))+
  scale_fill_gradientn(colours=c("grey10","grey10","grey10",colorRampPalette( rev(c("red","orange","gold","cornflowerblue", "blue1")) )(100)),trans="log10", limits=c(1,15000) ) +
  theme_bw()+
  geom_abline(slope = 1, intercept = 0, color = "purple")

#----------------------------

All_pairs <- rbind(All_pairs, data.frame(var1="Height", var2="Weight (log)",subsetted="Age 20+",var_pair=3))
hw_clean6 <- maha_clean(data$id[which(data$age>=20)], data$height[which(data$age>=20)], log(data$weight[which(data$age>=20)]), SD=6)
hw_clean7 <- maha_clean(data$id[which(data$age>=20)], data$height[which(data$age>=20)], log(data$weight[which(data$age>=20)]), SD=7)
hw_clean8 <- maha_clean(data$id[which(data$age>=20)], data$height[which(data$age>=20)], log(data$weight[which(data$age>=20)]), SD=8)
cleaned_rows6 <- which(hw_clean6$scatter$data$outlier=="detected")
cleaned_rows7 <- which(hw_clean7$scatter$data$outlier=="detected")
cleaned_rows8 <- which(hw_clean8$scatter$data$outlier=="detected")

d26 <- hw_clean6$scatter$data[cleaned_rows6,]
d26$SD <- rep(6, nrow(d26))
d27 <- hw_clean7$scatter$data[cleaned_rows7,]
d27$SD <- rep(7, nrow(d27))
d28 <- hw_clean8$scatter$data[cleaned_rows8,]
d28$SD <- rep(8, nrow(d28))
d2 <- rbind(d26, d27, d28)
d2$SD<-as.factor(d2$SD)

d2$var_pair <- 3
all_d2 <- rbind(all_d2, d2[,c("id","SD","var_pair")])
scat3 <- hw_clean8$scatter$data

SD_number <- c(6,7,8)
SD_count<- c(nrow(d26), nrow(d27), nrow(d28))
SD_info<-as.data.frame(cbind(SD_number, SD_count))

g3 <- ggplot(hw_clean8$scatter$data,aes(x=var1,y=exp(var2))) +
  xlab("Height (cm)")+
  ylab("Weight (kg)")+
  labs(title = paste("Height vs Weight (log); Age 20+ (sample size:", nrow(data), "; \nOutliers detected: SD=6:", nrow(d26),", SD=7:", nrow(d27),", SD=8:", nrow(d28),")"))+
  scale_fill_gradientn(colours=c("grey10","grey10","grey10",colorRampPalette( rev(c("red","orange","gold","cornflowerblue", "blue1")) )(100)),trans="log10", limits=c(1,15000) ) +
  theme_bw()


#----------------------------

All_pairs <- rbind(All_pairs, data.frame(var1="Height", var2="BMI (log)",subsetted="Age 20+",var_pair=4))
hBMI_clean6 <- maha_clean(data$id[which(data$age>=20)],data$height[which(data$age>=20)], log(data$bmi[which(data$age>=20)]), SD=6)
hBMI_clean7 <- maha_clean(data$id[which(data$age>=20)],data$height[which(data$age>=20)], log(data$bmi[which(data$age>=20)]), SD=7)
hBMI_clean8 <- maha_clean(data$id[which(data$age>=20)],data$height[which(data$age>=20)], log(data$bmi[which(data$age>=20)]), SD=8)
cleaned_rows6 <- which(hBMI_clean6$scatter$data$outlier=="detected")
cleaned_rows7 <- which(hBMI_clean7$scatter$data$outlier=="detected")
cleaned_rows8 <- which(hBMI_clean8$scatter$data$outlier=="detected")

d26 <- hBMI_clean6$scatter$data[cleaned_rows6,]
d26$SD <- rep(6, nrow(d26))
d27 <- hBMI_clean7$scatter$data[cleaned_rows7,]
d27$SD <- rep(7, nrow(d27))
d28 <- hBMI_clean8$scatter$data[cleaned_rows8,]
d28$SD <- rep(8, nrow(d28))
d2 <- rbind(d26, d27, d28)
d2$SD<-as.factor(d2$SD)

d2$var_pair <- 4
all_d2 <- rbind(all_d2, d2[,c("id","SD","var_pair")])
scat4 <- hBMI_clean8$scatter$data

SD_number <- c(6,7,8)
SD_count<- c(nrow(d26), nrow(d27), nrow(d28))
SD_info<-as.data.frame(cbind(SD_number, SD_count))

g4 <- ggplot(hBMI_clean8$scatter$data,aes(x=var1,y=exp(var2))) +
  xlab("Height (cm)")+
  ylab("BMI (kg/m^2)")+
  labs(title = paste("Height vs BMI (log) Age 20+ (sample size:", nrow(data), "; \nOutliers detected: SD=6:", nrow(d26),", SD=7:", nrow(d27),", SD=8:", nrow(d28),")"))+
  scale_fill_gradientn(colours=c("grey10","grey10","grey10",colorRampPalette( rev(c("red","orange","gold","cornflowerblue", "blue1")) )(100)),trans="log10", limits=c(1,15000) ) +
  theme_bw()

#----------------------------

All_pairs <- rbind(All_pairs, data.frame(var1="Weight (log)", var2="BMI (log)",subsetted="Age 20+",var_pair=5))
wBMI_clean6 <- maha_clean(data$id[which(data$age>=20)], log(data$weight[which(data$age>=20)]), log(data$bmi[which(data$age>=20)]), SD=6)
wBMI_clean7 <- maha_clean(data$id[which(data$age>=20)], log(data$weight[which(data$age>=20)]), log(data$bmi[which(data$age>=20)]), SD=7)
wBMI_clean8 <- maha_clean(data$id[which(data$age>=20)], log(data$weight[which(data$age>=20)]), log(data$bmi[which(data$age>=20)]), SD=8)
cleaned_rows6 <- which(wBMI_clean6$scatter$data$outlier=="detected")
cleaned_rows7 <- which(wBMI_clean7$scatter$data$outlier=="detected")
cleaned_rows8 <- which(wBMI_clean8$scatter$data$outlier=="detected")

d26 <- wBMI_clean6$scatter$data[cleaned_rows6,]
d26$SD <- rep(6, nrow(d26))
d27 <- wBMI_clean7$scatter$data[cleaned_rows7,]
d27$SD <- rep(7, nrow(d27))
d28 <- wBMI_clean8$scatter$data[cleaned_rows8,]
d28$SD <- rep(8, nrow(d28))
d2 <- rbind(d26, d27, d28)
d2$SD<-as.factor(d2$SD)

d2$var_pair <- 5
all_d2 <- rbind(all_d2, d2[,c("id","SD","var_pair")])
scat5 <- wBMI_clean8$scatter$data

SD_number <- c(6,7,8)
SD_count<- c(nrow(d26), nrow(d27), nrow(d28))
SD_info<-as.data.frame(cbind(SD_number, SD_count))

g5 <- ggplot(wBMI_clean8$scatter$data,aes(x=exp(var1),y=exp(var2))) +
  xlab("Weight (kg)")+
  ylab("BMI (kg/m^2)")+
  labs(title = paste("Weight (log) vs BMI (log) Age 20+ (sample size:", nrow(data), "; \nOutliers detected: SD=6:", nrow(d26),", SD=7:", nrow(d27),", SD=8:", nrow(d28),")"))+
  scale_fill_gradientn(colours=c("grey10","grey10","grey10",colorRampPalette( rev(c("red","orange","gold","cornflowerblue", "blue1")) )(100)),trans="log10", limits=c(1,15000) ) +
  theme_bw()

#----------------------------

All_pairs <- rbind(All_pairs, data.frame(var1="Height", var2="Waist Circumference (log)",subsetted="Age 20+",var_pair=6))
hWC_clean6 <- maha_clean(data$id[which(data$age>=20)],data$height[which(data$age>=20)], log(data$waist[which(data$age>=20)]), SD=6)
hWC_clean7 <- maha_clean(data$id[which(data$age>=20)],data$height[which(data$age>=20)], log(data$waist[which(data$age>=20)]), SD=7)
hWC_clean8 <- maha_clean(data$id[which(data$age>=20)],data$height[which(data$age>=20)], log(data$waist[which(data$age>=20)]), SD=8)
cleaned_rows6 <- which(hWC_clean6$scatter$data$outlier=="detected")
cleaned_rows7 <- which(hWC_clean7$scatter$data$outlier=="detected")
cleaned_rows8 <- which(hWC_clean8$scatter$data$outlier=="detected")

d26 <- hWC_clean6$scatter$data[cleaned_rows6,]
d26$SD <- rep(6, nrow(d26))
d27 <- hWC_clean7$scatter$data[cleaned_rows7,]
d27$SD <- rep(7, nrow(d27))
d28 <- hWC_clean8$scatter$data[cleaned_rows8,]
d28$SD <- rep(8, nrow(d28))
d2 <- rbind(d26, d27, d28)
d2$SD<-as.factor(d2$SD)

d2$var_pair <- 6
all_d2 <- rbind(all_d2, d2[,c("id","SD","var_pair")])
scat6 <- hWC_clean8$scatter$data

SD_number <- c(6,7,8)
SD_count<- c(nrow(d26), nrow(d27), nrow(d28))
SD_info<-as.data.frame(cbind(SD_number, SD_count))

g6 <- ggplot(hWC_clean8$scatter$data,aes(x=var1,y=exp(var2))) +
  xlab("Height (cm)")+
  ylab("Waist Circumference (cm)")+
  labs(title = paste("Height vs Waist Circumference (log) Age 20+ (sample size:", nrow(data), "; \nOutliers detected: SD=6:", nrow(d26),", SD=7:", nrow(d27),", SD=8:", nrow(d28),")"))+
  scale_fill_gradientn(colours=c("grey10","grey10","grey10",colorRampPalette( rev(c("red","orange","gold","cornflowerblue", "blue1")) )(100)),trans="log10", limits=c(1,15000) ) +
  theme_bw()

#----------------------------

All_pairs <- rbind(All_pairs, data.frame(var1="Weight (log)", var2="Waist Circumference (log)",subsetted="Age 20+",var_pair=7))
wWC_clean6 <- maha_clean(data$id[which(data$age>=20)],log(data$weight[which(data$age>=20)]), log(data$waist[which(data$age>=20)]), SD=6)
wWC_clean7 <- maha_clean(data$id[which(data$age>=20)],log(data$weight[which(data$age>=20)]), log(data$waist[which(data$age>=20)]), SD=7)
wWC_clean8 <- maha_clean(data$id[which(data$age>=20)],log(data$weight[which(data$age>=20)]), log(data$waist[which(data$age>=20)]), SD=8)
cleaned_rows6 <- which(wWC_clean6$scatter$data$outlier=="detected")
cleaned_rows7 <- which(wWC_clean7$scatter$data$outlier=="detected")
cleaned_rows8 <- which(wWC_clean8$scatter$data$outlier=="detected")

d26 <- wWC_clean6$scatter$data[cleaned_rows6,]
d26$SD <- rep(6, nrow(d26))
d27 <- wWC_clean7$scatter$data[cleaned_rows7,]
d27$SD <- rep(7, nrow(d27))
d28 <- wWC_clean8$scatter$data[cleaned_rows8,]
d28$SD <- rep(8, nrow(d28))
d2 <- rbind(d26, d27, d28)
d2$SD<-as.factor(d2$SD)

d2$var_pair <- 7
all_d2 <- rbind(all_d2, d2[,c("id","SD","var_pair")])
scat7 <- wWC_clean8$scatter$data

SD_number <- c(6,7,8)
SD_count<- c(nrow(d26), nrow(d27), nrow(d28))
SD_info<-as.data.frame(cbind(SD_number, SD_count))

g7 <- ggplot(wWC_clean6$scatter$data,aes(x=exp(var1),y=exp(var2))) +
  xlab("Weight (kg)")+
  ylab("Waist Circumference (cm)")+
  labs(title = paste("Weight (log) vs Waist Circumference (log) Age 20+ (sample size:", nrow(data), "; \nOutliers detected: SD=6:", nrow(d26),", SD=7:", nrow(d27),", SD=8:", nrow(d28),")"))+
  scale_fill_gradientn(colours=c("grey10","grey10","grey10",colorRampPalette( rev(c("red","orange","gold","cornflowerblue", "blue1")) )(100)),trans="log10", limits=c(1,15000) ) +
  theme_bw()




#########################################
if(FALSE){
  library(data.table)
  #investigate weight=130
  temp <- data[which(data$age>=20),]
  temp <- subset(temp, weight %in% c(120:140))
  #limited to 120-140 but not on waist
  out <- temp %>% group_by(id_study,weight) %>% summarise(n=n()) 
  out2 <- dcast(out, id_study ~ paste0("Wt",weight)) %>% arrange(-Wt130)
  head(out2,20)
  #limit also on waist<110	
  out <- subset(temp, waist<110) %>% group_by(id_study,weight) %>% summarise(n=n()) 
  out2 <- dcast(out, id_study ~ paste0("Wt",weight)) %>% arrange(-Wt130)
  head(out2,20)
  sum(out2$Wt130,na.rm=TRUE)
  subset(out2,is.na(Wt130)==FALSE)
  #75 in total, with 14 from one CHN study but not suspicious
  hist(data$weight[data$id_study=="CHN_2015_CHNS"],nclass=200)
  ## NO ACTION
  
  ### investigate waist==200
  temp <- data[which(data$age>=20),]
  #limited to waist 190-205 
  temp <- subset(temp, waist %in% c(190:205))
  out <- temp %>% group_by(id_study,waist) %>% summarise(n=n()) 
  out2 <- dcast(out, id_study ~ paste0("Waist",waist)) %>% arrange(-Waist200)
  head(out2,20)
  sum(out2$Waist200, na.rm=TRUE) # 110 in total
  sum(subset(out2, grepl("STEPS",id_study,fixed=TRUE))$Waist200, na.rm=TRUE) # 92 are STEPS
  ##
  
  ####### TO LOOK FOR EVIDENCE OF INCHES IN WAIST CIRUCMFERENCE AND TO LOOK AT MAXIMUMS
  #loading steps data  prior to cleaning
  #steps <- readRDS("S:/Projects/HeightProject/Original dataset/Data/Surveys/STEPS/STEPSdata_GLU_BP_chol_formatted_latest.RDS")  # STEPS dataframe
  write.csv(table(subset(steps, waist>200 & waist<8888)$waist), paste0(JBfolder,"over200.csv"))
  
  #200 not actually the maximum
  temp3 <- subset(steps, waist>200 & waist<8888)
  temp3
  
  #all steps 
  pdf(paste0(JBfolder,"All STEPS waist distributions.pdf"),  width = 12, height = 10)
  for(i in seq(1, length(unique(steps$id_study)),6)){
    print(
      ggplot(subset(steps, age>20 &  id_study %in% unique(steps$id_study)[i:(i+5)]) , aes(x=waist)) + 
        geom_vline(xintercept=200, linetype="dashed", colour="gold")+ geom_histogram(binwidth=1) + facet_wrap(~id_study, scales="free_y") + theme_bw() +
        xlab("Waist circumference (cm)") +
        ggtitle("Distribution of waist circumference in STEPS surveys")+  
        coord_cartesian(xlim = c(0,210))  +xlim(0,210)
    )  
  }
  dev.off()
  #repeat with limited axes
  pdf(paste0(JBfolder,"All STEPS waist distributions - zoomed in.pdf"),  width = 12, height = 10)
  for(i in seq(1, length(unique(steps$id_study)),6)){
    print(
      ggplot(subset(steps, age>20 &  id_study %in% unique(steps$id_study)[i:(i+5)]) , aes(x=waist)) + 
        geom_vline(xintercept=200, linetype="dashed", colour="gold")+ geom_histogram(binwidth=1) + facet_wrap(~id_study, scales="free_y") + theme_bw() +
        xlab("Waist circumference (cm)") +
        ggtitle("Distribution of waist circumference in STEPS surveys")+  
        coord_cartesian(xlim = c(0,210), ylim=c(0,50))  +xlim(0,210)  #need to add the second limit to get some (Toonga etc to plot?)
    )  
  }
  dev.off()
  ## WAIST=200 - NO ACTION
  ## WAIST IN INCHES - NO ACTION: DESIRABLE TO REMOVE OR TO RECODE BUT BECAUSE NOT DEFINITELY IDENTIFIABLE THEN LEAVE TO UNIVARIATE AND OR MULTIVARIATE CLEANING.
  
  
  pdf(paste0(JBfolder,"All STEPS waist distributions - before and after cleaning.pdf"),  width = 12, height = 10)
  #list of STEPS studies which show bimodal Waist 
  studies <- c("BHS_2019_STEPS", "BWA_2014_STEPS", "GAB_2009_STEPS", "LBR_2011_STEPS", "LBR_2022_STEPS", "LBY_2009_STEPS","LCA_2020_STEPS","MWI_2009_STEPS","SLE_2009_STEPS","STP_2009_STEPS","TCD_2008_STEPS")
  for(i in seq(1, length(studies),2)){
    grid.arrange(
      
      ggplot(subset(steps, age>20 &  id_study %in% studies[i:(i+1)]) , aes(x=waist)) + 
        geom_histogram(binwidth=1) + facet_wrap(~id_study, scales="free_y") + theme_bw() +
        xlab("Waist circumference (cm)") +
        ggtitle("Distribution of waist circumference in STEPS  before any cleaning (truncated at 210)")+  
        coord_cartesian(xlim = c(0,210))  +xlim(0,210),
      
      ggplot(subset(dat_for_James, age>20 &  id_study %in% studies[i:(i+1)]) , aes(x=waist)) + 
        geom_histogram(binwidth=1) + facet_wrap(~id_study, scales="free_y") + theme_bw() +
        xlab("Waist circumference (cm)") +
        ggtitle("Distribution of waist circumference in STEPS after UniV+MultiV (truncated at 210)")+  
        coord_cartesian(xlim = c(0,210))  +xlim(0,210),
      ncol=1
    )  
  }
  dev.off()
  
  
  
  
  
  
} #FALSE
#########################################






#----------------------------

All_pairs <- rbind(All_pairs, data.frame(var1="Height", var2="Waist to Height Ratio (log)",subsetted="Age 20+",var_pair=8))
hWHR_clean6 <- maha_clean(data$id[which(data$age>=20)],data$height[which(data$age>=20)], log(data$whr[which(data$age>=20)]), SD=6)
hWHR_clean7 <- maha_clean(data$id[which(data$age>=20)],data$height[which(data$age>=20)], log(data$whr[which(data$age>=20)]), SD=7)
hWHR_clean8 <- maha_clean(data$id[which(data$age>=20)],data$height[which(data$age>=20)], log(data$whr[which(data$age>=20)]), SD=8)
cleaned_rows6 <- which(hWHR_clean6$scatter$data$outlier=="detected")
cleaned_rows7 <- which(hWHR_clean7$scatter$data$outlier=="detected")
cleaned_rows8 <- which(hWHR_clean8$scatter$data$outlier=="detected")

d26 <- hWHR_clean6$scatter$data[cleaned_rows6,]
d26$SD <- rep(6, nrow(d26))
d27 <- hWHR_clean7$scatter$data[cleaned_rows7,]
d27$SD <- rep(7, nrow(d27))
d28 <- hWHR_clean8$scatter$data[cleaned_rows8,]
d28$SD <- rep(8, nrow(d28))
d2 <- rbind(d26, d27, d28)
d2$SD<-as.factor(d2$SD)

d2$var_pair <- 8
all_d2 <- rbind(all_d2, d2[,c("id","SD","var_pair")])
scat8 <- hWHR_clean8$scatter$data

SD_number <- c(6,7,8)
SD_count<- c(nrow(d26), nrow(d27), nrow(d28))
SD_info<-as.data.frame(cbind(SD_number, SD_count))

g8 <- ggplot(hWHR_clean8$scatter$data,aes(x=var1,y=exp(var2))) +
  xlab("Height (cm)")+
  ylab("Waist to Height Ratio")+
  labs(title = paste("Height vs Waist to Height Ratio (log) Age 20+ (sample size:", nrow(data), "; \nOutliers detected: SD=6:", nrow(d26),", SD=7:", nrow(d27),", SD=8:", nrow(d28),")"))+
  scale_fill_gradientn(colours=c("grey10","grey10","grey10",colorRampPalette( rev(c("red","orange","gold","cornflowerblue", "blue1")) )(100)),trans="log10", limits=c(1,15000) ) +
  theme_bw()

#----------------------------

All_pairs <- rbind(All_pairs, data.frame(var1="Weight (log)", var2="Waist to Height Ratio (log)",subsetted="Age 20+",var_pair=9))
wWHR_clean6 <- maha_clean(data$id[which(data$age>=20)],log(data$weight[which(data$age>=20)]), log(data$whr[which(data$age>=20)]), SD=6)
wWHR_clean7 <- maha_clean(data$id[which(data$age>=20)],log(data$weight[which(data$age>=20)]), log(data$whr[which(data$age>=20)]), SD=7)
wWHR_clean8 <- maha_clean(data$id[which(data$age>=20)],log(data$weight[which(data$age>=20)]), log(data$whr[which(data$age>=20)]), SD=8)
cleaned_rows6 <- which(wWHR_clean6$scatter$data$outlier=="detected")
cleaned_rows7 <- which(wWHR_clean7$scatter$data$outlier=="detected")
cleaned_rows8 <- which(wWHR_clean8$scatter$data$outlier=="detected")

d26 <- wWHR_clean6$scatter$data[cleaned_rows6,]
d26$SD <- rep(6, nrow(d26))
d27 <- wWHR_clean7$scatter$data[cleaned_rows7,]
d27$SD <- rep(7, nrow(d27))
d28 <- wWHR_clean8$scatter$data[cleaned_rows8,]
d28$SD <- rep(8, nrow(d28))
d2 <- rbind(d26, d27, d28)
d2$SD<-as.factor(d2$SD)

d2$var_pair <- 9
all_d2 <- rbind(all_d2, d2[,c("id","SD","var_pair")])
scat9 <- wWHR_clean8$scatter$data

SD_number <- c(6,7,8)
SD_count<- c(nrow(d26), nrow(d27), nrow(d28))
SD_info<-as.data.frame(cbind(SD_number, SD_count))

g9 <- ggplot(wWHR_clean8$scatter$data,aes(x=exp(var1),y=exp(var2))) +
  xlab("Weight (kg)")+
  ylab("Waist to Height Ratio")+
  labs(title = paste("Weight (log) vs Waist to Height Ratio (log) Age 20+ (sample size:", nrow(data), "; \nOutliers detected: SD=6:", nrow(d26),", SD=7:", nrow(d27),", SD=8:", nrow(d28),")"))+
  scale_fill_gradientn(colours=c("grey10","grey10","grey10",colorRampPalette( rev(c("red","orange","gold","cornflowerblue", "blue1")) )(100)),trans="log10", limits=c(1,15000) ) +
  theme_bw()

#----------------------------

All_pairs <- rbind(All_pairs, data.frame(var1="Waist Circumference (log)", var2="BMI (log)",subsetted="Age 20+",var_pair=10))
WCBMI_clean6 <- maha_clean(data$id[which(data$age>=20)],log(data$waist[which(data$age>=20)]), log(data$bmi[which(data$age>=20)]), SD=6)
WCBMI_clean7 <- maha_clean(data$id[which(data$age>=20)],log(data$waist[which(data$age>=20)]), log(data$bmi[which(data$age>=20)]), SD=7)
WCBMI_clean8 <- maha_clean(data$id[which(data$age>=20)],log(data$waist[which(data$age>=20)]), log(data$bmi[which(data$age>=20)]), SD=8)
cleaned_rows6 <- which(WCBMI_clean6$scatter$data$outlier=="detected")
cleaned_rows7 <- which(WCBMI_clean7$scatter$data$outlier=="detected")
cleaned_rows8 <- which(WCBMI_clean8$scatter$data$outlier=="detected")

d26 <- WCBMI_clean6$scatter$data[cleaned_rows6,]
d26$SD <- rep(6, nrow(d26))
d27 <- WCBMI_clean7$scatter$data[cleaned_rows7,]
d27$SD <- rep(7, nrow(d27))
d28 <- WCBMI_clean8$scatter$data[cleaned_rows8,]
d28$SD <- rep(8, nrow(d28))
d2 <- rbind(d26, d27, d28)
d2$SD<-as.factor(d2$SD)

d2$var_pair <- 10
all_d2 <- rbind(all_d2, d2[,c("id","SD","var_pair")])
scat10 <- WCBMI_clean8$scatter$data

SD_number <- c(6,7,8)
SD_count<- c(nrow(d26), nrow(d27), nrow(d28))
SD_info<-as.data.frame(cbind(SD_number, SD_count))

g10 <- ggplot(WCBMI_clean8$scatter$data,aes(x=exp(var1),y=exp(var2))) +
  xlab("Waist Circumference (cm)")+
  ylab("BMI (kg/m^2)")+
  labs(title = paste("Waist Circumference (log) vs BMI (log) Age 20+ (sample size:", nrow(data), "; \nOutliers detected: SD=6:", nrow(d26),", SD=7:", nrow(d27),", SD=8:", nrow(d28),")"))+
  scale_fill_gradientn(colours=c("grey10","grey10","grey10",colorRampPalette( rev(c("red","orange","gold","cornflowerblue", "blue1")) )(100)),trans="log10", limits=c(1,15000) ) +
  theme_bw()


#----------------------------

All_pairs <- rbind(All_pairs, data.frame(var1="Waist Circumference (log)", var2="Waist to Height Ratio (log)",subsetted="Age 20+",var_pair=11))
WCWHR_clean6 <- maha_clean(data$id[which(data$age>=20)],log(data$waist[which(data$age>=20)]), log(data$whr[which(data$age>=20)]), SD=6)
WCWHR_clean7 <- maha_clean(data$id[which(data$age>=20)],log(data$waist[which(data$age>=20)]), log(data$whr[which(data$age>=20)]), SD=7)
WCWHR_clean8 <- maha_clean(data$id[which(data$age>=20)],log(data$waist[which(data$age>=20)]), log(data$whr[which(data$age>=20)]), SD=8)
cleaned_rows6 <- which(WCWHR_clean6$scatter$data$outlier=="detected")
cleaned_rows7 <- which(WCWHR_clean7$scatter$data$outlier=="detected")
cleaned_rows8 <- which(WCWHR_clean8$scatter$data$outlier=="detected")

d26 <- WCWHR_clean6$scatter$data[cleaned_rows6,]
d26$SD <- rep(6, nrow(d26))
d27 <- WCWHR_clean7$scatter$data[cleaned_rows7,]
d27$SD <- rep(7, nrow(d27))
d28 <- WCWHR_clean8$scatter$data[cleaned_rows8,]
d28$SD <- rep(8, nrow(d28))
d2 <- rbind(d26, d27, d28)
d2$SD<-as.factor(d2$SD)

d2$var_pair <- 11
all_d2 <- rbind(all_d2, d2[,c("id","SD","var_pair")])
scat11 <- WCWHR_clean8$scatter$data

SD_number <- c(6,7,8)
SD_count<- c(nrow(d26), nrow(d27), nrow(d28))
SD_info<-as.data.frame(cbind(SD_number, SD_count))

g11 <- ggplot(WCWHR_clean8$scatter$data,aes(x=exp(var1),y=exp(var2))) +
  xlab("Waist Circumference (cm)")+
  ylab("Waist to Height Ratio")+
  labs(title = paste("Waist Circumference (log) vs Waist to Height Ratio (log) Age 20+ (sample size:", nrow(data), "; \nOutliers detected: SD=6:", nrow(d26),", SD=7:", nrow(d27),", SD=8:", nrow(d28),")"))+
  scale_fill_gradientn(colours=c("grey10","grey10","grey10",colorRampPalette( rev(c("red","orange","gold","cornflowerblue", "blue1")) )(100)),trans="log10", limits=c(1,15000) ) +
  theme_bw()


#----------------------------

All_pairs <- rbind(All_pairs, data.frame(var1="BMI (log)", var2="Waist to Height Ratio (log)",subsetted="Age 20+",var_pair=12))
BMIWHR_clean6 <- maha_clean(data$id[which(data$age>=20)],log(data$bmi[which(data$age>=20)]), log(data$whr[which(data$age>=20)]), SD=6)
BMIWHR_clean7 <- maha_clean(data$id[which(data$age>=20)],log(data$bmi[which(data$age>=20)]), log(data$whr[which(data$age>=20)]), SD=7)
BMIWHR_clean8 <- maha_clean(data$id[which(data$age>=20)],log(data$bmi[which(data$age>=20)]), log(data$whr[which(data$age>=20)]), SD=8)
cleaned_rows6 <- which(BMIWHR_clean6$scatter$data$outlier=="detected")
cleaned_rows7 <- which(BMIWHR_clean7$scatter$data$outlier=="detected")
cleaned_rows8 <- which(BMIWHR_clean8$scatter$data$outlier=="detected")

d26 <- BMIWHR_clean6$scatter$data[cleaned_rows6,]
d26$SD <- rep(6, nrow(d26))
d27 <- BMIWHR_clean7$scatter$data[cleaned_rows7,]
d27$SD <- rep(7, nrow(d27))
d28 <- BMIWHR_clean8$scatter$data[cleaned_rows8,]
d28$SD <- rep(8, nrow(d28))
d2 <- rbind(d26, d27, d28)
d2$SD<-as.factor(d2$SD)

d2$var_pair <- 12
all_d2 <- rbind(all_d2, d2[,c("id","SD","var_pair")])
scat12 <- BMIWHR_clean8$scatter$data

SD_number <- c(6,7,8)
SD_count<- c(nrow(d26), nrow(d27), nrow(d28))
SD_info<-as.data.frame(cbind(SD_number, SD_count))

g12 <- ggplot(BMIWHR_clean8$scatter$data,aes(x=exp(var1),y=exp(var2))) +
  xlab("BMI (kg/m^2)")+
  ylab("Waist to Height Ratio")+
  labs(title = paste("BMI (log) vs Waist to Height Ratio (log) Age 20+ (sample size:", nrow(data), "; \nOutliers detected: SD=6:", nrow(d26),", SD=7:", nrow(d27),", SD=8:", nrow(d28),")"))+
  scale_fill_gradientn(colours=c("grey10","grey10","grey10",colorRampPalette( rev(c("red","orange","gold","cornflowerblue", "blue1")) )(100)),trans="log10", limits=c(1,15000) ) +
  theme_bw()


############################################

#################################  
plot_maha <- function(scatN, gN, N, all_d2){
  temp2 <- merge(all_d2, scatN,  by.x=c("id"), by.y=c("id"), all.x=TRUE)
  temp3 <- scatN[!scatN$id %in% subset(all_d2, SD%in%c(6,7,8))$id,]
  temp4 <- scatN[!scatN$id %in% subset(all_d2, SD%in%c(8))$id,]
  lims <- get_plot_limits(gN +   geom_bin2d(data=scatN  , bins = 130))
  return(grid.arrange(gN +   geom_bin2d(data=scatN  , bins = 130),
                      gN +  geom_bin2d(data=scatN  , bins = 130) + geom_point(data=subset(temp2, var_pair==N) %>% arrange(SD) , aes(color=SD),  size = 0.9)+  
                        scale_colour_manual(values=c("limegreen","pink","cyan")) + ggtitle("\nSD=6,7,8 from this pair")  + xlim(unlist(lims[1:2]))+ ylim(unlist(lims[3:4])),
                      gN +   geom_bin2d(data=temp3  , bins = 130)+ ggtitle("\nExclude SD=6+ from all pairs")  + xlim(unlist(lims[1:2]))+ ylim(unlist(lims[3:4])),
                      gN +   geom_bin2d(data=temp4  , bins = 130)+ ggtitle("\nExclude SD=8 from all pairs")  + xlim(unlist(lims[1:2]))+ ylim(unlist(lims[3:4])) , ncol=2))
}

pdf(paste0(JBfolder,"Multi_plots_update_With All SD v4.pdf"),   # The directory you want to save the file in
    width = 14, # The width of the plot in inches
    height = 10)

plot_maha(scat1,g1,1,all_d2)
plot_maha(scat2,g2,2,all_d2)
plot_maha(scat3,g3,3,all_d2)
plot_maha(scat4,g4,4,all_d2)
plot_maha(scat5,g5,5,all_d2)
plot_maha(scat6,g6,6,all_d2)
plot_maha(scat7,g7,7,all_d2)
plot_maha(scat8,g8,8,all_d2)
plot_maha(scat9,g9,9,all_d2)
plot_maha(scat10,g10,10,all_d2)
plot_maha(scat11,g11,11,all_d2)
plot_maha(scat12,g12,12,all_d2)

dev.off()
##

#List of all id's to be cleaned given by all_d2.
#Example of final datasets possibly only cleaning on some of the pairs
if(FALSE){
  #just to remind what the pairs are so that use correct var_pair for each dataversion
  All_pairs
  
  #Assume using SD=6
  
  #For Ana
  data_MV_BP_only <- data[!data$id %in%  subset(all_d2, var_pair %in% c(1,2) & SD %in% c(6,7,8))$id,]
  dim(data)[1] - dim(data_MV_BP_only)[1]
  
  #For Lakshya
  data_MV_AllAnthro <- data[!data$id %in%  subset(all_d2, var_pair %in% c(3:12) & SD %in% c(6,7,8))$id,]
  dim(data)[1] - dim(data_MV_AllAnthro)[1]
  
  #For James
  data_MV_BPandAllAnthro <- data[!data$id %in%  subset(all_d2, var_pair %in% c(1:12) & SD %in% c(6,7,8))$id,]
  dim(data)[1] - dim(data_MV_BPandAllAnthro)[1]
  
}



print(Sys.time() - t0)
