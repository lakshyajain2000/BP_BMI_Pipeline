# Clean anthro variables
# None of these functions seem to be cleaning by age properly - or removing negative values: 
# Need to find bug to replace code in anthro cleaning section 

# Clean height

data$height <- clean_data(data, 'height')

#

# Clean weight

data$weight <- clean_data(data, 'weight')

# Calculate missing BMI and WtHR data

data$bmi <- ifelse(is.na(data$bmi), data$weight/((data$height/100)^2), data$bmi) 
data$whr <- ifelse(is.na(data$whr), data$waist/data$hip, data$whr)

# Clean BMI
data$bmi <- clean_data(data, 'bmi')

# Clean WtHR
