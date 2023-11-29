# Clean Weight and BMI together

pdf("Weight_BMI_plots_.pdf",   # The directory you want to save the file in
    width = 12, # The width of the plot in inches
    height = 10)

wBMI_clean_normal <- maha_clean(data$weight, data$bmi)
wBMI_clean$scatter

wBMI_clean_log <- maha_clean(data$weight, data$bmi)
wBMI_clean_log$scatter

wBMI_clean_normal_over_60 <- maha_clean(data$weight[which(data$age >60)], data$bmi[which(data$age >60)])
plot(wBMI_clean_normal_over_60$scatter$data$var1,wBMI_clean_normal_over_60$scatter$data$var2)
plot(wBMI)

wBMI_clean_normal_under_60 <- maha_clean(data$weight[which(data$age >=20 & data$age<60)], data$bmi[which(data$age >=20 & data$age<60)])
wBMI_clean_normal_under_60$scatter



wBMI_clean_log_over_60 <- maha_clean(log(data$weight[which(data$age > 60 )]), log(data$bmi[which(data$age > 60 )]))
wBMI_clean_log_over_60$scatter

wBMI_clean_log_under_60 <- maha_clean(log(data$weight[which(data$age >= 20 & data$age <60)]), log(data$bmi[which(data$age >= 20 & data$age <60)]))
wBMI_clean_log_under_60$scatter
plot(exp(wBMI_clean_normal_over_60$scatter$data$var1),exp(wBMI_clean_normal_over_60$scatter$data$var2))

dev.off()

pdf("BP_Plots.pdf",   # The directory you want to save the file in
    width = 12, # The width of the plot in inches
    height = 11)


bp_clean <- maha_clean(data$sbp_final, data$dbp_final,names_var1 = "SBP",names_var2 = "DBP")
bp_clean$scatter
length(bp_clean$outliers)

bp_clean_over_60 <- maha_clean(data$sbp_final[which(data$age >60)], data$dbp_final[which(data$age >60)])
bp_clean_over_60$scatter
length(bp_clean$outliers)

bp_clean_under_60 <- maha_clean(data$sbp_final[which(data$age <60 & data$age>=20)], data$dbp_final[which(data$age <60 & data$age >=20)])
bp_clean_under_60$scatter
length(bp_clean$outliers)

bp_clean_log <- maha_clean(log(data$sbp_final), log(data$dbp_final))
bp_clean_log$scatter
length(bp_clean$outliers)

bp_clean_over_60_log <- maha_clean(log(data$sbp_final[which(data$age >60)]), log(data$dbp_final[which(data$age >60)]))
bp_clean_over_60_log$scatter
length(bp_clean$outliers)

bp_clean_under_60_log <- maha_clean(log(data$sbp_final[which(data$age <60 & data$age>=20)]), log(data$dbp_final[which(data$age <60 & data$age >=20)]))
bp_clean_under_60_log$scatter
length(bp_clean$outliers)

dev.off()

pdf("Multi_plots.pdf",   # The directory you want to save the file in
    width = 12, # The width of the plot in inches
    height = 10)

bp_clean <- maha_clean(data$sbp_final, log(data$dbp_final))
cleaned_rows <- which(bp_clean$scatter$data$outlier=="detected")
d2 <- bp_clean$scatter$data[cleaned_rows,]
ggplot(bp_clean$scatter$data,aes(x=var1,y=exp(var2))) +
  xlab("SBP")+
  ylab("DBP")+
  ggtitle("SBP vs DBP")+
  geom_bin2d(bins = 130) +
  scale_fill_continuous(type = "viridis") +
  theme_bw()+
  geom_point(data=d2, aes(x=var1, y=exp(var2), color=outlier), shape=21, size = 3)  +
  scale_color_manual(name="", values = c("red", "red"))


hw_clean <- maha_clean(data$height, log(data$weight))
cleaned_rows <- which(hw_clean$scatter$data$outlier=="detected")
d2 <- hw_clean$scatter$data[cleaned_rows,]
ggplot(hw_clean$scatter$data,aes(x=var1,y=exp(var2))) +
  geom_bin2d(bins = 130) +
  xlab("Height")+
  ylab("Weight")+
  ggtitle("Height vs Weight") +
  scale_fill_continuous(type = "viridis") +
  theme_bw()+
  geom_point(data=d2, aes(x=var1, y=exp(var2), color=outlier), shape=21, size = 3)  +
  scale_color_manual(name="", values = c("red", "red"))


# Clean Height and BMI together

#height_to_drop <- c()
#weight
#bmi
#waist circum
#whr waist-to-hip-ratio

hBMI_clean <- maha_clean(data$height, log(data$bmi))
cleaned_rows <- which(hBMI_clean$scatter$data$outlier=="detected")
d2 <- hBMI_clean$scatter$data[cleaned_rows,]
ggplot(hBMI_clean$scatter$data,aes(x=var1,y=exp(var2))) +
  geom_bin2d(bins = 130) +
  xlab("Height")+
  ylab("BMI")+
  ggtitle("Height vs BMI") +
  scale_fill_continuous(type = "viridis") +
  theme_bw()+
  geom_point(data=d2, aes(x=var1, y=exp(var2), color=outlier), shape=21, size = 3)  +
  scale_color_manual(name="", values = c("red", "red"))



# Clean Weight and BMI together

#pdf("Multi_plots2.pdf",   # The directory you want to save the file in
  #  width = 12, # The width of the plot in inches
  #  height = 10)


wBMI_clean <- maha_clean(log(data$weight), log(data$bmi))
cleaned_rows <- which(wBMI_clean$scatter$data$outlier=="detected")
d2 <- wBMI_clean$scatter$data[cleaned_rows,]
ggplot(wBMI_clean$scatter$data,aes(x=exp(var1),y=exp(var2))) +
  geom_bin2d(bins = 130) +
  xlab("Weight")+
  ylab("BMI")+
  ggtitle("Weight vs BMI") +
  scale_fill_continuous(type = "viridis") +
  theme_bw()+
  geom_point(data=d2, aes(x=exp(var1), y=exp(var2), color=outlier), shape=21, size = 3)  +
  scale_color_manual(name="", values = c("red", "red"))

# Clean Height and WC together

hWC_clean <- maha_clean(data$height, log(data$waist))
cleaned_rows <- which(hWC_clean$scatter$data$outlier=="detected")
d2 <- hWC_clean$scatter$data[cleaned_rows,]
ggplot(hWC_clean$scatter$data,aes(x=var1,y=exp(var2))) +
  geom_bin2d(bins = 130) +
  xlab("Height")+
  ylab("Waist Circumference")+
  ggtitle("Height vs Waist Circumference") +
  scale_fill_continuous(type = "viridis") +
  theme_bw()+
  geom_point(data=d2, aes(x=var1, y=exp(var2), color=outlier), shape=21, size = 3)  +
  scale_color_manual(name="", values = c("red", "red"))
# add to height/bmi/wc/whtr lists

# Clean Weight and WC together

wWC_clean <- maha_clean(log(data$weight), log(data$waist))
cleaned_rows <- which(wWC_clean$scatter$data$outlier=="detected")
d2 <- wWC_clean$scatter$data[cleaned_rows,]
ggplot(wWC_clean$scatter$data,aes(x=exp(var1),y=exp(var2))) +
  geom_bin2d(bins = 130) +
  xlab("Weight")+
  ylab("Waist Circumference")+
  ggtitle("Weight vs Waist Circumference") +
  scale_fill_continuous(type = "viridis") +
  theme_bw()+
  geom_point(data=d2, aes(x=exp(var1), y=exp(var2), color=outlier), shape=21, size = 3)  +
  scale_color_manual(name="", values = c("red", "red"))
# add to height/bmi/wc/whtr lists

#dev.off()

# Clean Height and WR together takes long

#pdf("Multi_plots3",   # The directory you want to save the file in
   # width = 12, # The width of the plot in inches
   # height = 10)


hWHR_clean <- maha_clean(data$height, log(data$whr))
cleaned_rows <- which(hWHR_clean$scatter$data$outlier=="detected")
d2 <- hWHR_clean$scatter$data[cleaned_rows,]
ggplot(hWHR_clean$scatter$data,aes(x=var1,y=exp(var2))) +
  geom_bin2d(bins = 130) +
  xlab("Height")+
  ylab("Waist to Height Ratio")+
  ggtitle("Height vs Waist to Height Ratio") +
  scale_fill_continuous(type = "viridis") +
  theme_bw()+
  geom_point(data=d2, aes(x=var1, y=exp(var2), color=outlier), shape=21, size = 3)  +
  scale_color_manual(name="", values = c("red", "red"))
# add to height/bmi/wc/whtr lists

# Clean Weight and WHR together

wWHR_clean <- maha_clean(log(data$weight), log(data$whr))
cleaned_rows <- which(wWHR_clean$scatter$data$outlier=="detected")
d2 <- wWHR_clean$scatter$data[cleaned_rows,]
ggplot(wWHR_clean$scatter$data,aes(x=exp(var1),y=exp(var2))) +
  geom_bin2d(bins = 130) +
  xlab("Weight")+
  ylab("Waist to Height Ratio")+
  ggtitle("Weight vs Waist to Height Ratio") +
  scale_fill_continuous(type = "viridis") +
  theme_bw()+
  geom_point(data=d2, aes(x=exp(var1), y=exp(var2), color=outlier), shape=21, size = 3)  +
  scale_color_manual(name="", values = c("red", "red"))

# Clean WC and BMI together

WCBMI_clean <- maha_clean(log(data$waist), log(data$bmi))
cleaned_rows <- which(WCBMI_clean$scatter$data$outlier=="detected")
d2 <- WCBMI_clean$scatter$data[cleaned_rows,]
ggplot(WCBMI_clean$scatter$data,aes(x=exp(var1),y=exp(var2))) +
  geom_bin2d(bins = 130) +
  xlab("Waist Circumference")+
  ylab("BMI")+
  ggtitle("Waist Circumference vs BMI") +
  scale_fill_continuous(type = "viridis") +
  theme_bw()+
  geom_point(data=d2, aes(x=exp(var1), y=exp(var2), color=outlier), shape=21, size = 3)  +
  scale_color_manual(name="", values = c("red", "red"))

#dev.off()

#pdf("Multi_plots4",   # The directory you want to save the file in
  #  width = 12, # The width of the plot in inches
  #  height = 10)


# Clean WC and WtHR together

WCWHR_clean <- maha_clean(log(data$waist), log(data$whr))
cleaned_rows <- which(WCWHR_clean$scatter$data$outlier=="detected")
d2 <- WCWHR_clean$scatter$data[cleaned_rows,]
ggplot(WCWHR_clean$scatter$data,aes(x=exp(var1),y=exp(var2))) +
  geom_bin2d(bins = 130) +
  xlab("Waist Circumference")+
  ylab("Waist to Height Ratio")+
  ggtitle("Waist Circumference vs Waist to Height Ratio") +
  scale_fill_continuous(type = "viridis") +
  theme_bw()+
  geom_point(data=d2, aes(x=exp(var1), y=exp(var2), color=outlier), shape=21, size = 3)  +
  scale_color_manual(name="", values = c("red", "red"))

# Clean WtHR and BMI together

WCBMI_clean <- maha_clean(log(data$waist), log(data$bmi))
cleaned_rows <- which(WCBMI_clean$scatter$data$outlier=="detected")
d2 <- WCBMI_clean$scatter$data[cleaned_rows,]
ggplot(WCBMI_clean$scatter$data,aes(x=exp(var1),y=exp(var2))) +
  geom_bin2d(bins = 130) +
  xlab("Waist Circumference")+
  ylab("BMI")+
  ggtitle("Waist Circumference vs BMI") +
  scale_fill_continuous(type = "viridis") +
  theme_bw()+
  geom_point(data=d2, aes(x=exp(var1), y=exp(var2), color=outlier), shape=21, size = 3)  +
  scale_color_manual(name="", values = c("red", "red"))

#data<-rbind(data_clean,data_clean1,data_clean2,data_clean3, data_clean4, data_clean5, data_clean6,
#   data_clean7, data_clean8, data_clean9, clean_data10)

#data %>% distinct()

dev.off()
