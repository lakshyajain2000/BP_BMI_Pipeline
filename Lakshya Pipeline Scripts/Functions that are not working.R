# Clean anthro variables
# None of these functions seem to be cleaning by age properly - or removing negative values: 
# Need to find bug to replace code in anthro cleaning section 

# Clean height

data$height <- clean_data(data, 'height')


#summary(data$height)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   -1.0   150.0   155.0   156.0   161.5   999.9  240012 
#
#This is what came out 

# Clean weight

data$weight <- clean_data(data, 'weight')

# Calculate missing BMI and WtHR data

data$bmi <- ifelse(is.na(data$bmi), data$weight/((data$height/100)^2), data$bmi) 
data$whr <- ifelse(is.na(data$whr), data$waist/data$hip, data$whr)

# Clean BMI
data$bmi <- clean_data(data, 'bmi')

#Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
#-10000.00     19.61     22.28     24.53     25.77 188000.00    310359 

# Clean WtHR
