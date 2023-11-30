#height_to_drop <- c()

# Functionalise this

pdf("Multi_plots_update.pdf",   # The directory you want to save the file in
    width = 12, # The width of the plot in inches
    height = 10)

bp_clean6 <- maha_clean(data$sbp_final[which(data$age>=20 & data$age<60)], log(data$dbp_final[which(data$age>=20 & data$age<60)]), SD=6)
bp_clean7 <- maha_clean(data$sbp_final[which(data$age>=20 & data$age<60)], log(data$dbp_final[which(data$age>=20 & data$age<60)]), SD=7)
bp_clean8 <- maha_clean(data$sbp_final[which(data$age>=20 & data$age<60)], log(data$dbp_final[which(data$age>=20 & data$age<60)]), SD=8)
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

SD_number <- c(6,7,8)
SD_count<- c(nrow(d26), nrow(d27), nrow(d28))
SD_info<-as.data.frame(cbind(SD_number, SD_count))

ggplot(bp_clean8$scatter$data,aes(x=var1,y=exp(var2))) +
  geom_bin2d(bins = 130) +
  xlab("SBP (mmHg)")+
  ylab("DBP (mmHg)")+
  labs(title = paste("SBP vs DBP (log); Age 20-59 (sample size:", nrow(data), "; Outliers detected: SD=6:", nrow(d26),", SD=7:", nrow(d27),", SD=8:", nrow(d28),")"))+
  scale_fill_continuous(type = "viridis") +
  theme_bw()+
  geom_point(data=d2, aes(x=var1, y=exp(var2), color=SD), shape=21, size = 3)

#----------------------------

bp_clean6_old <- maha_clean(data$sbp_final[which(data$age>=60)], log(data$dbp_final[which(data$age>=60)]), SD=6)
bp_clean7_old <- maha_clean(data$sbp_final[which(data$age>=60)], log(data$dbp_final[which(data$age>=60)]), SD=7)
bp_clean8_old <- maha_clean(data$sbp_final[which(data$age>=60)], log(data$dbp_final[which(data$age>=60)]), SD=8)
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

SD_number <- c(6,7,8)
SD_count<- c(nrow(d26), nrow(d27), nrow(d28))
SD_info<-as.data.frame(cbind(SD_number, SD_count))

ggplot(bp_clean8_old$scatter$data,aes(x=var1,y=exp(var2))) +
  geom_bin2d(bins = 130) +
  xlab("SBP (mmHg)")+
  ylab("DBP (mmHG)")+
  labs(title = paste("SBP vs DBP (log) Age 60+ (sample size:", nrow(data), "; Outliers detected: SD=6:", nrow(d26),", SD=7:", nrow(d27),", SD=8:", nrow(d28),")"))+
  scale_fill_continuous(type = "viridis") +
  theme_bw()+
  geom_point(data=d2, aes(x=var1, y=exp(var2), color=SD), shape=21, size = 3) 

#----------------------------

hw_clean6 <- maha_clean(data$height[which(data$age>=20)], log(data$weight[which(data$age>=20)]), SD=6)
hw_clean7 <- maha_clean(data$height[which(data$age>=20)], log(data$weight[which(data$age>=20)]), SD=7)
hw_clean8 <- maha_clean(data$height[which(data$age>=20)], log(data$weight[which(data$age>=20)]), SD=8)
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

SD_number <- c(6,7,8)
SD_count<- c(nrow(d26), nrow(d27), nrow(d28))
SD_info<-as.data.frame(cbind(SD_number, SD_count))

ggplot(hw_clean8$scatter$data,aes(x=var1,y=exp(var2))) +
  geom_bin2d(bins = 130) +
  xlab("Height (cm)")+
  ylab("Weight (kg)")+
  labs(title = paste("Height vs Weight (log); Age 20+ (sample size:", nrow(data), "; Outliers detected: SD=6:", nrow(d26),", SD=7:", nrow(d27),", SD=8:", nrow(d28),")"))+
  scale_fill_continuous(type = "viridis") +
  theme_bw()+
  geom_point(data=d2, aes(x=var1, y=exp(var2), color=SD), shape=21, size = 3)

#----------------------------

hBMI_clean6 <- maha_clean(data$height[which(data$age>=20)], log(data$bmi[which(data$age>=20)]), SD=6)
hBMI_clean7 <- maha_clean(data$height[which(data$age>=20)], log(data$bmi[which(data$age>=20)]), SD=7)
hBMI_clean8 <- maha_clean(data$height[which(data$age>=20)], log(data$bmi[which(data$age>=20)]), SD=8)
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

SD_number <- c(6,7,8)
SD_count<- c(nrow(d26), nrow(d27), nrow(d28))
SD_info<-as.data.frame(cbind(SD_number, SD_count))

ggplot(hBMI_clean8$scatter$data,aes(x=var1,y=exp(var2))) +
  geom_bin2d(bins = 130) +
  xlab("Height (cm)")+
  ylab("BMI (kg/m^2)")+
  labs(title = paste("Height vs BMI (log) Age 20+ (sample size:", nrow(data), "; Outliers detected: SD=6:", nrow(d26),", SD=7:", nrow(d27),", SD=8:", nrow(d28),")"))+
  scale_fill_continuous(type = "viridis") +
  theme_bw()+
  geom_point(data=d2, aes(x=var1, y=exp(var2), color=SD), shape=21, size = 3)

#----------------------------

wBMI_clean6 <- maha_clean(log(data$weight[which(data$age>=20)]), log(data$bmi[which(data$age>=20)]), SD=6)
wBMI_clean7 <- maha_clean(log(data$weight[which(data$age>=20)]), log(data$bmi[which(data$age>=20)]), SD=7)
wBMI_clean8 <- maha_clean(log(data$weight[which(data$age>=20)]), log(data$bmi[which(data$age>=20)]), SD=8)
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

SD_number <- c(6,7,8)
SD_count<- c(nrow(d26), nrow(d27), nrow(d28))
SD_info<-as.data.frame(cbind(SD_number, SD_count))

ggplot(wBMI_clean8$scatter$data,aes(x=exp(var1),y=exp(var2))) +
  geom_bin2d(bins = 130) +
  xlab("Weight (kg)")+
  ylab("BMI (kg/m^2)")+
  labs(title = paste("Weight (log) vs BMI (log) Age 20+ (sample size:", nrow(data), "; Outliers detected: SD=6:", nrow(d26),", SD=7:", nrow(d27),", SD=8:", nrow(d28),")"))+
  scale_fill_continuous(type = "viridis") +
  theme_bw()+
  geom_point(data=d2, aes(x=exp(var1), y=exp(var2), color=SD), shape=21, size = 3)

#----------------------------

hWC_clean6 <- maha_clean(data$height[which(data$age>=20)], log(data$waist[which(data$age>=20)]), SD=6)
hWC_clean7 <- maha_clean(data$height[which(data$age>=20)], log(data$waist[which(data$age>=20)]), SD=7)
hWC_clean8 <- maha_clean(data$height[which(data$age>=20)], log(data$waist[which(data$age>=20)]), SD=8)
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

SD_number <- c(6,7,8)
SD_count<- c(nrow(d26), nrow(d27), nrow(d28))
SD_info<-as.data.frame(cbind(SD_number, SD_count))

ggplot(hWC_clean8$scatter$data,aes(x=var1,y=exp(var2))) +
  geom_bin2d(bins = 130) +
  xlab("Height (cm)")+
  ylab("Waist Circumference (cm)")+
  labs(title = paste("Height vs Waist Circumference (log) Age 20+ (sample size:", nrow(data), "; Outliers detected: SD=6:", nrow(d26),", SD=7:", nrow(d27),", SD=8:", nrow(d28),")"))+
  scale_fill_continuous(type = "viridis") +
  theme_bw()+
  geom_point(data=d2, aes(x=var1, y=exp(var2), color=SD), shape=21, size = 3)

#----------------------------

wWC_clean6 <- maha_clean(log(data$weight[which(data$age>=20)]), log(data$waist[which(data$age>=20)]), SD=6)
wWC_clean7 <- maha_clean(log(data$weight[which(data$age>=20)]), log(data$waist[which(data$age>=20)]), SD=7)
wWC_clean8 <- maha_clean(log(data$weight[which(data$age>=20)]), log(data$waist[which(data$age>=20)]), SD=8)
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

SD_number <- c(6,7,8)
SD_count<- c(nrow(d26), nrow(d27), nrow(d28))
SD_info<-as.data.frame(cbind(SD_number, SD_count))

ggplot(wWC_clean8$scatter$data,aes(x=exp(var1),y=exp(var2))) +
  geom_bin2d(bins = 130) +
  xlab("Weight (kg)")+
  ylab("Waist Circumference (cm)")+
  labs(title = paste("Weight (log) vs Waist Circumference (log) Age 20+ (sample size:", nrow(data), "; Outliers detected: SD=6:", nrow(d26),", SD=7:", nrow(d27),", SD=8:", nrow(d28),")"))+
  scale_fill_continuous(type = "viridis") +
  theme_bw()+
  geom_point(data=d2, aes(x=exp(var1), y=exp(var2), color=SD), shape=21, size = 3)

#----------------------------

hWHR_clean6 <- maha_clean(data$height[which(data$age>=20)], log(data$whr[which(data$age>=20)]), SD=6)
hWHR_clean7 <- maha_clean(data$height[which(data$age>=20)], log(data$whr[which(data$age>=20)]), SD=7)
hWHR_clean8 <- maha_clean(data$height[which(data$age>=20)], log(data$whr[which(data$age>=20)]), SD=8)
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

SD_number <- c(6,7,8)
SD_count<- c(nrow(d26), nrow(d27), nrow(d28))
SD_info<-as.data.frame(cbind(SD_number, SD_count))

ggplot(hWHR_clean8$scatter$data,aes(x=var1,y=exp(var2))) +
  geom_bin2d(bins = 130) +
  xlab("Height (cm)")+
  ylab("Waist to Height Ratio")+
  labs(title = paste("Height vs Waist to Height Ratio (log) Age 20+ (sample size:", nrow(data), "; Outliers detected: SD=6:", nrow(d26),", SD=7:", nrow(d27),", SD=8:", nrow(d28),")"))+
  scale_fill_continuous(type = "viridis") +
  theme_bw()+
  geom_point(data=d2, aes(x=var1, y=exp(var2), color=SD), shape=21, size = 3)


#----------------------------

wWHR_clean6 <- maha_clean(log(data$weight[which(data$age>=20)]), log(data$whr[which(data$age>=20)]), SD=6)
wWHR_clean7 <- maha_clean(log(data$weight[which(data$age>=20)]), log(data$whr[which(data$age>=20)]), SD=7)
wWHR_clean8 <- maha_clean(log(data$weight[which(data$age>=20)]), log(data$whr[which(data$age>=20)]), SD=8)
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

SD_number <- c(6,7,8)
SD_count<- c(nrow(d26), nrow(d27), nrow(d28))
SD_info<-as.data.frame(cbind(SD_number, SD_count))

ggplot(wWHR_clean8$scatter$data,aes(x=exp(var1),y=exp(var2))) +
  geom_bin2d(bins = 130) +
  xlab("Weight (kg)")+
  ylab("Waist to Height Ratio")+
  labs(title = paste("Weight (log) vs Waist to Height Ratio (log) Age 20+ (sample size:", nrow(data), "; Outliers detected: SD=6:", nrow(d26),", SD=7:", nrow(d27),", SD=8:", nrow(d28),")"))+
  scale_fill_continuous(type = "viridis") +
  theme_bw()+
  geom_point(data=d2, aes(x=exp(var1), y=exp(var2), color=SD), shape=21, size = 3)
#----------------------------

WCBMI_clean6 <- maha_clean(log(data$waist[which(data$age>=20)]), log(data$bmi[which(data$age>=20)]), SD=6)
WCBMI_clean7 <- maha_clean(log(data$waist[which(data$age>=20)]), log(data$bmi[which(data$age>=20)]), SD=7)
WCBMI_clean8 <- maha_clean(log(data$waist[which(data$age>=20)]), log(data$bmi[which(data$age>=20)]), SD=8)
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

SD_number <- c(6,7,8)
SD_count<- c(nrow(d26), nrow(d27), nrow(d28))
SD_info<-as.data.frame(cbind(SD_number, SD_count))

ggplot(WCBMI_clean8$scatter$data,aes(x=exp(var1),y=exp(var2))) +
  geom_bin2d(bins = 130) +
  xlab("Waist Circumference (cm)")+
  ylab("BMI (kg/m^2)")+
  labs(title = paste("Waist Circumference (log) vs BMI (log) Age 20+ (sample size:", nrow(data), "; Outliers detected: SD=6:", nrow(d26),", SD=7:", nrow(d27),", SD=8:", nrow(d28),")"))+
  scale_fill_continuous(type = "viridis") +
  theme_bw()+
  geom_point(data=d2, aes(x=exp(var1), y=exp(var2), color=SD), shape=21, size = 3)

#----------------------------

WCWHR_clean6 <- maha_clean(log(data$waist[which(data$age>=20)]), log(data$whr[which(data$age>=20)]), SD=6)
WCWHR_clean7 <- maha_clean(log(data$waist[which(data$age>=20)]), log(data$whr[which(data$age>=20)]), SD=7)
WCWHR_clean8 <- maha_clean(log(data$waist[which(data$age>=20)]), log(data$whr[which(data$age>=20)]), SD=8)
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

SD_number <- c(6,7,8)
SD_count<- c(nrow(d26), nrow(d27), nrow(d28))
SD_info<-as.data.frame(cbind(SD_number, SD_count))

ggplot(WCWHR_clean8$scatter$data,aes(x=exp(var1),y=exp(var2))) +
  geom_bin2d(bins = 130) +
  xlab("Waist Circumference (cm)")+
  ylab("Waist to Height Ratio")+
  labs(title = paste("Waist Circumference (log) vs Waist to Height Ratio (log) Age 20+ (sample size:", nrow(data), "; Outliers detected: SD=6:", nrow(d26),", SD=7:", nrow(d27),", SD=8:", nrow(d28),")"))+
  scale_fill_continuous(type = "viridis") +
  theme_bw()+
  geom_point(data=d2, aes(x=exp(var1), y=exp(var2), color=SD), shape=21, size = 3)

#----------------------------

BMIWHR_clean6 <- maha_clean(log(data$bmi[which(data$age>=20)]), log(data$whr[which(data$age>=20)]), SD=6)
BMIWHR_clean7 <- maha_clean(log(data$bmi[which(data$age>=20)]), log(data$whr[which(data$age>=20)]), SD=7)
BMIWHR_clean8 <- maha_clean(log(data$bmi[which(data$age>=20)]), log(data$whr[which(data$age>=20)]), SD=8)
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

SD_number <- c(6,7,8)
SD_count<- c(nrow(d26), nrow(d27), nrow(d28))
SD_info<-as.data.frame(cbind(SD_number, SD_count))

ggplot(BMIWHR_clean8$scatter$data,aes(x=exp(var1),y=exp(var2))) +
  geom_bin2d(bins = 130) +
  xlab("BMI (kg/m^2)")+
  ylab("Waist to Height Ratio")+
  labs(title = paste("BMI (log) vs Waist to Height Ratio (log) Age 20+ (sample size:", nrow(data), "; Outliers detected: SD=6:", nrow(d26),", SD=7:", nrow(d27),", SD=8:", nrow(d28),")"))+
  scale_fill_continuous(type = "viridis") +
  theme_bw()+
  geom_point(data=d2, aes(x=exp(var1), y=exp(var2), color=SD), shape=21, size = 3)

dev.off()
