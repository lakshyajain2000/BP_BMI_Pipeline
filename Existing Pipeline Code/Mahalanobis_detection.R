# NCD-RisC
# Mahalanobis outlier detection 
# July 2021
# Need to edit to make the plots work and to also log and unlog transform the right things at the right time!!!!!

library(rrcov)
library(ggplot2)

variables<-c(data$height,data$weight,"bmi","waist","wht","sbp_final","dbp_final") 
values<-c(1, 0, 0, 0, 0, 0, 1)
log<-as.data.frame(cbind(variables, values))


var1="height"
as.numeric(log$values[which(variables==var1)])

maha_clean <- function(data, var1, var2, level = NA, SD = 8, plot_data = TRUE,names_var1="",names_var2="", title=""){
  # default level is equivalent to being 8 sd away from the mean for normal distribution
  if (is.na(level)) level <- (1-pnorm(SD))*2
  
  data <- as.data.frame(cbind(var1, var2))
  if (as.numeric(log$values[which(variables==var1)])==0){
    data$var1<-log(data$var1)
  }
  if (as.numeric(log$values[which(variables==var2)])==0){
    data$var2<-log(data$var2)
  }
  b    <- covMcd(data, alpha=0.95)    # robust estimate of the covariance   
  
  data$m_dist <- mahalanobis(data, b$center,b$cov)
  
  data$outlier <- "Not detected"
  data[which(data$m_dist > qchisq(1-level, 2)),'outlier'] <- "detected"
  
  cleaned_rows <- which(data$outlier=="detected")
  
  
  if (plot_data & length(cleaned_rows)>0) {
    d2 <- data[cleaned_rows,]
    p <- ggplot(data,aes(x=var1,y=var2)) +
      ggtitle(title) +
          geom_bin2d(bins = 130) +
          scale_fill_continuous(type = "viridis") +
          theme_bw()+
      xlab(names_var1)+
      ylab(names_var2)+
          geom_point(data=d2, aes(x=var1, y=var2, color=outlier), shape=21, size = 3)  +
          scale_color_manual(name="", values = c("red", "red"))
    return(list(scatter = p, outliers = cleaned_rows))
  } else {
    return(list(scatter = NULL, outliers = cleaned_rows))
  }
}





