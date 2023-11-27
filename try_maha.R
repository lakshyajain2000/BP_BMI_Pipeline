
data_test<- data[sample(nrow(data), 10000), ] 

test<-maha_clean(data$dbp1_f, data$sbp1_f)
test$scatter
length(test$outliers)
# removing detected outliers
if (length(test$outliers)>0) data_clean <- data[-test$outliers, ]
