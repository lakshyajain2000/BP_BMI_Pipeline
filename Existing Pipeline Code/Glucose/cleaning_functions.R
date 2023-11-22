# Utility functions
library(dplyr)

# clean data
clean_data <- function(data, variable) {
  clean_list <- switch(variable,
                       sex    = clean_sex(data$sex),
                       age    = clean_age(data$age),
                       height = clean_height(data$height, data$age),
                       weight = clean_weight(data$weight, data$age, data$is_pregnant),
                       bmi    = clean_bmi(data$bmi, data$age, data$is_pregnant),
                       is_pregnant = ,
                       drug_diab_insu = ,
                       drug_diab_pill = ,
                       drug_diab = ,
                       self_diab = clean_cat(data[[variable]], variable),
                       fgl    = clean_fgl(data$fgl),
                       hba1c  = clean_hba1c(data$hba1c),
                       {print(paste('Variable', variable, 'is not expected')); c()}
  )
  return(clean_list)
}

clean_height <- function(height, age) {
  clean_list <- c()
  # Clean height according to age group #
  clean_list <- c(clean_list, which((age >= 5 & age < 10) & (height < 60 | height > 180)))
  clean_list <- c(clean_list, which((age >= 10 & age < 15) & (height < 80 | height > 200)))
  clean_list <- c(clean_list, which((age >= 15) & (height < 100 | height > 250)))
  clean_list <- unique(clean_list)
  print(paste("Number of height data recoded as NA:", length(clean_list), "of", sum(!is.na(height))))
  return(clean_list)
}

clean_weight <- function(weight, age, is_pregnant) {
  clean_list <- c()
  # Clean weight according to age group #
  clean_list <- c(clean_list, which((age >= 5 & age < 10) & (weight < 5 | weight > 90)))
  clean_list <- c(clean_list, which((age >= 10 & age < 15) & (weight < 8 | weight > 150)))
  clean_list <- c(clean_list, which((age >= 15) & (weight < 12 | weight > 300)))
  clean_list <- c(clean_list, which(is_pregnant == 1))
  clean_list <- unique(clean_list)
  print(paste("Number of weight data recoded as NA:", length(clean_list), "of", sum(!is.na(weight))))
  return(clean_list)
}

clean_bmi <- function(bmi, age, is_pregnant) {
  clean_list <- c()
  # Clean bmi according to age group #
  clean_list <- c(clean_list, which((age >= 5 & age < 10) & (bmi < 6 | bmi > 40)))
  clean_list <- c(clean_list, which((age >= 10 & age < 15) & (bmi < 8 | bmi > 60)))
  clean_list <- c(clean_list, which((age >= 15) & (bmi < 10 | bmi > 80)))
  clean_list <- c(clean_list, which(is_pregnant == 1))
  clean_list <- unique(clean_list)
  print(paste("Number of bmi data recoded as NA:", length(clean_list), "of", sum(!is.na(bmi))))
  return(clean_list)
}

clean_waist <- function(waist, age, is_pregnant) {
  clean_list <- c()
  # Clean waist according to age group #
  clean_list <- c(clean_list, which((age >= 5 & age < 10) & (waist < 20 | waist > 150)))
  clean_list <- c(clean_list, which((age >= 10 & age < 15) & (waist < 20 | waist > 200)))
  clean_list <- c(clean_list, which((age >= 15) & (waist < 30 | waist > 300)))
  clean_list <- c(clean_list, which(is_pregnant == 1))
  clean_list <- unique(clean_list)
  print(paste("Number of waist data recoded as NA:", length(clean_list), "of", sum(!is.na(waist))))
  return(clean_list)
}

clean_wth <- function(wth, age, is_pregnant) {
  clean_list <- c()
  # Clean wth according to age group #
  clean_list <- c(clean_list, which((age >= 5 & age < 15) & (wth < 0.2 | wth > 1.5)))
  clean_list <- c(clean_list, which((age >= 15) & (wth < 0.2 | wth > 2.0)))
  clean_list <- c(clean_list, which(is_pregnant == 1))
  clean_list <- unique(clean_list)
  print(paste("Number of wth data recoded as NA:", length(clean_list), "of", sum(!is.na(wth))))
  return(clean_list)
}

clean_sbp <- function(sbp, var_name) {
  clean_list <- which(sbp < 70 | sbp > 270)
  print(paste("Number of", var_name, "data recoded as NA:", length(clean_list), "of", sum(!is.na(sbp))))
  return(clean_list)
}

clean_dbp <- function(dbp, var_name) {
  clean_list <- which(dbp < 30 | dbp > 150)
  print(paste("Number of", var_name, "data recoded as NA:", length(clean_list), "of", sum(!is.na(dbp))))
  return(clean_list)
}

clean_sex <- function(sex) {
  clean_list <- which(sex != 1 & sex != 2)
  print(paste("Number of sex data recoded as NA:", length(clean_list), "of", sum(!is.na(sex))))
  return(clean_list)
}

clean_age <- function(age) {
  clean_list <- which(age < 0 | age > 120)
  print(paste("Number of age data recoded as NA:", length(clean_list), "of", sum(!is.na(age))))
  return(clean_list)
}

# clean_continuous <- function(var, var_name, minv, maxv) {
#   clean_list <- which(var < minv | var > maxv)
#   print(paste("Number of", var_name, "data recoded as NA:", length(clean_list), "of", sum(!is.na(var))))
#   return(clean_list)
# }

clean_cat <- function(var, var_name) {
  clean_list <- which(var != 0 & var != 1)
  print(paste("Number of", var_name, "data recoded as NA:", length(clean_list), "of", sum(!is.na(var))))
  return(clean_list)
}

clean_fgl <- function(fgl) {
  clean_list <- which(fgl < 2 | fgl > 30)
  print(paste("Number of FPG data recoded as NA:", length(clean_list), "of", sum(!is.na(fgl))))
  return(clean_list)
}

clean_hba1c <- function(hba1c) {
  clean_list <- which(hba1c < 3 | hba1c > 18)
  print(paste("Number of HbA1c data recoded as NA:", length(clean_list), "of", sum(!is.na(hba1c))))
  return(clean_list)
}
