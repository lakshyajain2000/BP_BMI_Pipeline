## 06062023 Rosie removed latin1 encoding


################################################################################
############################# FUNCTIONS ########################################
################################################################################

#source('S:/Projects/HeightProject/Templates/Code-standardisation/R/data-handling/make_age_groups.R')




## clean_anthro ##
## Cleans variables ("var") according to the criteria specified within the function
## Anthroprometric variables are cleant using age-specific plausible ranges (Updated by Nia in 2018)
clean_anthro <- function(data, var, bmi_only_surveyids = NULL) {

  ### study level number of rows cleaned for each study (used in other RF cleaning)
  print_cleaned <- function(to_clean) {
    print(stringi::stri_c("Percentage Cleaned (No. of cleaned/No. of non-NAs):",to_clean,"(%)"))
    if (length(clnList)==0) {
      print("No records cleaned")
    } else {
      cln.table <- table(data[clnList,]$id_study)/table(data[!is.na(data[to_clean]),]$id_study)*100
      print(sort(round(cln.table[which(cln.table!=Inf&cln.table>10)],2)), decreasing = TRUE)
    }
  }


    if (var == "sex") {
        # If sex is not 1 or 2, set it as NA (preexisting NA values, remain NA)
        message("Cleaning sex variable")
        print(stringi::stri_c("Number of subjects recoded as NA:", length(which(!data$sex %in% c(1, 2)))))
        data$sex[which(!data$sex %in% c(1, 2))] = NA

        #cleaned per study
        clnList <- which(is.na(data$sex))
        sex.na <- table(data[clnList,]$id_study)/table(as.factor(data$id_study))*100
        print("Percentage of NAs in Sex (%)")
        print(sort(round(sex.na[which(sex.na>10)],2)), decreasing = FALSE)

    }
    if (var == "age") {
        # If age is not included within the range [age_min_anthro, age_max_anthro], set
        # age to NA
        message("Cleaning age variable")

      clnList <- which(is.na(data$age))
      age.na <- table(data[clnList,]$id_study)/table(data$id_study)*100
      print("Percentage of NAs in Age (%)")
      print(sort(round(age.na[which(age.na>10)],2)), decreasing = FALSE)


      #number of NA to start
        age_start <- sum(is.na(data$age))
        # Floor age - added by ARM on July-2021
        data$age <- floor(data$age)
        data$age_min_anthro_F <- floor(data$age_min_anthro_F)
        data$age_min_anthro_M <- floor(data$age_min_anthro_M)
        data$age_max_anthro_F <- floor(data$age_max_anthro_F)
        data$age_max_anthro_M <- floor(data$age_max_anthro_M)

        clnList <- with(data,
                        which((sex==1&((is.na(age_max_anthro_M)|age>age_max_anthro_M)|(is.na(age_min_anthro_M)|age<age_min_anthro_M))) |
                                (sex==2&((is.na(age_max_anthro_F)|age>age_max_anthro_F)|(is.na(age_min_anthro_F)|age<age_min_anthro_F))) ) )

        # print out
        age.out <- table(data[clnList,]$id_study)/table(data$id_study)*100
        print("Percentage of Implausible Values (Outside Designed Range) in Age (%)")
        print(sort(round(age.out[which(age.out>10)],2)), decreasing = FALSE)

        data$age[clnList] <- NA
        print(stringi::stri_c("Number of subjects recoded as NA:",  sum(is.na(data$age))- age_start))

    }
    if (var == "pregnant") {
      message("Cleaning pregnant")
      # Pregnant for men (=1) spread across surveys --> all men & preg are considered as men
      preg_old <- data$is_pregnant
      data$is_pregnant <- ifelse(data$is_pregnant == 1 & data$sex == 2 & (data$age >= 10 & data$age <= 49),
                                 1, 0) # Only 1 if female, and pregnant, and age [10-49] # Males, or too young/too old females are set to zero.
      data$is_pregnant <- ifelse(is.na(data$is_pregnant), 0, data$is_pregnant) # if it's NA, set to 0. This was updated Dec-18
      print(stringi::stri_c("Number of subjects recoded as 0:", sum(data$is_pregnant == 0) - length(which(preg_old == 0))))

    }
    if (var == "height") {
        message("Cleaning height variable")

        # Clean height according to age group #
        data$height_clean <- data$height

        clnList <- (which((data$age >= 5 & data$age < 10) & (data$height < 60 | data$height > 180) |
                            (data$age >= 10 & data$age < 15) & (data$height < 80 | data$height > 200) |
                            (data$age >= 15) & (data$height < 100 | data$height > 250) ) )

        data$height_clean[clnList] <- NA
        print(stringi::stri_c("Number of subjects recoded as NA:", sum(is.na(data$height_clean)) - sum(is.na(data$height))))


        print("Percentage of NAs in Height (%)")
        print_cleaned("height")

        overall_clean_list <<- union(overall_clean_list, clnList)

    }

    if (var == "weight") {
        message("Cleaning weight variable")
        # Clean weight according to age group #
        data$weight_clean <- data$weight

        clnList <- (which((data$age >= 5 & data$age < 10) & (data$weight < 5 | data$weight > 90) |
                            (data$age >= 10 & data$age < 15) & (data$weight < 8 | data$weight > 150) |
                            (data$age >= 15) & (data$weight < 12 | data$weight > 300) ) )

        data$weight_clean[clnList] <- NA
        data$weight_clean[which(data$is_pregnant == 1)] <- NA # clean for pregnant
        print(stringi::stri_c("Number of subjects recoded as NA:", sum(is.na(data$weight_clean)) - sum(is.na(data$weight))))


        print("Percentage of NAs in Weight (%)")
        print_cleaned("weight")
        overall_clean_list <<- union(overall_clean_list, clnList)
    }
    if (var == "waist") {
        message("Cleaning waist variable")
        data$waist_clean <- data$waist
        clnList <- (which((data$age >= 5 & data$age < 10) & (data$waist < 20 | data$waist > 150) |
                            (data$age >= 10 & data$age < 15) & (data$waist < 20 | data$waist > 200) |
                            (data$age >= 15) & (data$waist < 30 | data$waist > 300) ) )

        data$waist_clean[clnList] <- NA
        data$waist_clean[which(data$is_pregnant == 1)] <- NA  # clean for pregnant
        print(stringi::stri_c("Number of subjects recoded as NA:", sum(is.na(data$waist_clean)) - sum(is.na(data$waist))))


        print("Percentage of NAs in Waist (%)")
        print_cleaned("waist")

    }
    if (var == "hip") {
        message("Cleaning hip variable")
        data$hip_clean <- data$hip
        clnList <- (which((data$age >= 5 & data$age < 10) & (data$hip < 30 | data$hip > 180) |
                            (data$age >= 10 & data$age < 15) & (data$hip < 30 | data$hip > 300) |
                            (data$age >= 15) & (data$hip < 40 | data$hip > 300) ) )

        data$hip_clean[clnList] <-NA
        data$hip_clean[which(data$is_pregnant == 1)] <- NA  # clean for pregnant
        print(stringi::stri_c("Number of subjects recoded as NA:", sum(is.na(data$hip_clean)) - sum(is.na(data$hip))))

        print("Percentage of NAs in Hip (%)")
        print_cleaned("hip")

    }
    if (var == "whr") {
        message("Cleaning waist-hip-ratio variable")
        data$whr_clean <- data$whr
        clnList <- (which((data$age >= 5 & data$age < 10) & (data$whr < 0.4 | data$whr > 1.8) |
                            (data$age >= 10 & data$age < 15) & (data$whr < 0.4| data$whr > 1.8) |
                            (data$age >= 15) & (data$whr < 0.4 | data$whr > 2) ) )

        data$whr_clean[clnList] <- NA
        data$whr_clean[is.na(data$hip_clean) | is.na(data$waist_clean)] <- NA

        data$whr_clean[which(data$is_pregnant == 1)] <- NA  # clean for pregnant
        print(stringi::stri_c("Number of subjects recoded as NA:", sum(is.na(data$whr_clean)) - sum(is.na(data$whr))))

        print("Percentage of NAs in WHR (%)")
        print_cleaned("whr")

    }
    if (var == "bmi") {
        # Clean BMI according to age group #
        message("Cleaning BMI variable")

        clnList <- which(((data$age >= 5 & data$age < 10) & (data$bmi < 6 | data$bmi > 40)) | ((data$age >= 10 & data$age < 15) & (data$bmi < 8 | data$bmi > 60)) | ((data$age >= 15) & (data$bmi < 10 | data$bmi > 80)))

        data$bmi_clean <- data$bmi
        data$bmi_clean[clnList] <- NA
        data$bmi_clean[which(data$is_pregnant == 1)] <- NA  # clean for pregnant

        if (length(setdiff(c("height_clean", "weight_clean"), names(data)) > 0)) { #
          stop("Variables height_clean or weight_clean are missing")
        } else {
           clnlist2 <- which(((is.na(data$height_clean) | is.na(data$weight_clean)) & !is.na(data$bmi)) & !data$id_study %in% bmi_only_surveyids)
           data$bmi_clean[clnlist2] <- NA
        }

        print(stringi::stri_c("Number of subjects recoded as NA:", sum(is.na(data$bmi_clean)) - sum(is.na(data$bmi))))

        print_cleaned("bmi")

        overall_clean_list <<- union(overall_clean_list, union(clnList, clnlist2))
    }
  if (var == "multiple height") {
    message("Cleaning height 1,2 and 3 variable")

    # Clean height1 according to age group #
    clnList <- (which((data$age >= 5 & data$age < 10) & (data$height1 < 60 | data$height1 > 180) |
                        (data$age >= 10 & data$age < 15) & (data$height1 < 80 | data$height1 > 200) |
                        (data$age >= 15) & (data$height1 < 100 | data$height1 > 250) ) )

    data$height1[clnList] <- NA

    # Clean height2 according to age group #
    clnList <- (which((data$age >= 5 & data$age < 10) & (data$height2 < 60 | data$height2 > 180) |
                        (data$age >= 10 & data$age < 15) & (data$height2 < 80 | data$height2 > 200) |
                        (data$age >= 15) & (data$height2 < 100 | data$height2 > 250) ) )

    data$height2[clnList] <- NA


    # Clean height3 according to age group #
    clnList <- (which((data$age >= 5 & data$age < 10) & (data$height3 < 60 | data$height3 > 180) |
                        (data$age >= 10 & data$age < 15) & (data$height3 < 80 | data$height3 > 200) |
                        (data$age >= 15) & (data$height3 < 100 | data$height3 > 250) ) )

    data$height3[clnList] <- NA

  }
  if (var == "multiple weight") {
    message("Cleaning weight 1,2 and 3 variable")

    # Clean weight31 according to age group #
    clnList <- (which((data$age >= 5 & data$age < 10) & (data$weight1 < 5 | data$weight1 > 90) |
                        (data$age >= 10 & data$age < 15) & (data$weight1 < 8 | data$weight1 > 150) |
                        (data$age >= 15) & (data$weight1 < 12 | data$weight1 > 300) ) )

    data$weight1[clnList] <- NA

    # Clean weight32 according to age group #
    clnList <- (which((data$age >= 5 & data$age < 10) & (data$weight2 < 5 | data$weight2 > 90) |
                        (data$age >= 10 & data$age < 15) & (data$weight2 < 8 | data$weight2 > 150) |
                        (data$age >= 15) & (data$weight2 < 12 | data$weight2 > 300) ) )

    data$weight2[clnList] <- NA

    # Clean weight33 according to age group #
    clnList <- (which((data$age >= 5 & data$age < 10) & (data$weight3 < 5 | data$weight3 > 90) |
                        (data$age >= 10 & data$age < 15) & (data$weight3 < 8 | data$weight3 > 150) |
                        (data$age >= 15) & (data$weight3 < 12 | data$weight3 > 300) ) )

    data$weight3[clnList] <- NA
  }
  if (var == "multiple waist") {
    message("Cleaning waist 1,2 and 3 variable")

    # Clean waist1 according to age group #
    clnList <- (which((data$age >= 5 & data$age < 10) & (data$waist1 < 20 | data$waist1 > 150) |
                        (data$age >= 10 & data$age < 15) & (data$waist1 < 20 | data$waist1 > 200) |
                        (data$age >= 15) & (data$waist1 < 30 | data$waist1 > 300) ) )
    data$waist1[clnList] <- NA

    # Clean waist2 according to age group #
    clnList <- (which((data$age >= 5 & data$age < 10) & (data$waist2 < 20 | data$waist2 > 150) |
                        (data$age >= 10 & data$age < 15) & (data$waist2 < 20 | data$waist2 > 200) |
                        (data$age >= 15) & (data$waist2 < 30 | data$waist2 > 300) ) )
    data$waist2[clnList] <- NA

    # Clean waist3 according to age group #
    clnList <- (which((data$age >= 5 & data$age < 10) & (data$waist3 < 20 | data$waist3 > 150) |
                        (data$age >= 10 & data$age < 15) & (data$waist3 < 20 | data$waist3 > 200) |
                        (data$age >= 15) & (data$waist3 < 30 | data$waist3 > 300) ) )
    data$waist3[clnList] <- NA

  }
  if (var == "multiple hip") {
    message("Cleaning hip 1,2 and 3 variable")

    # Clean hip1 according to age group #
    clnList <- (which((data$age >= 5 & data$age < 10) & (data$hip1 < 30 | data$hip1 > 180) |
                        (data$age >= 10 & data$age < 15) & (data$hip1 < 30 | data$hip1 > 300) |
                        (data$age >= 15) & (data$hip1 < 40 | data$hip1 > 300) ) )
    data$hip1[clnList] <-NA

    # Clean hip2 according to age group #
    clnList <- (which((data$age >= 5 & data$age < 10) & (data$hip2 < 30 | data$hip2 > 180) |
                        (data$age >= 10 & data$age < 15) & (data$hip2 < 30 | data$hip2 > 300) |
                        (data$age >= 15) & (data$hip2 < 40 | data$hip2 > 300) ) )
    data$hip2[clnList] <-NA

    # Clean hip3 according to age group #
    clnList <- (which((data$age >= 5 & data$age < 10) & (data$hip3 < 30 | data$hip3 > 180) |
                        (data$age >= 10 & data$age < 15) & (data$hip3 < 30 | data$hip3 > 300) |
                        (data$age >= 15) & (data$hip3 < 40 | data$hip3 > 300) ) )
    data$hip3[clnList] <-NA
  }

    return(data)
}
################################################################################

## age_range_groups ##
## Creates age range groups: if age >= 5, age_range is min_max, if age < 5 age_range
# is 5-max, else age_range = 999
age_range_groups = function(data) {

    data$age_range_F <- ifelse(data$age_min_anthro_F >= 5, stringi::stri_c(data$age_min_anthro_F,
        "-", data$age_max_anthro_F, sep = ""), ifelse(data$age_min_anthro_F < 5,
        stringi::stri_c("5-", data$age_max_anthro_F, sep = ""), 999))

    data$age_range_M <- ifelse(data$age_min_anthro_M >= 5, stringi::stri_c(data$age_min_anthro_M,
        "-", data$age_max_anthro_M, sep = ""), ifelse(data$age_min_anthro_M < 5,
      stringi::stri_c("5-", data$age_max_anthro_M, sep = ""), 999))

    return(data)
}



################################################################################

## create_age_groups ##
# agemin: if age_min_anthro and age_max_anthro are within a 10-year band agemin = age_min_anthro, else agemin = floor(age).
# Exceptions: if age < 20, agemin = age; if age > 80, agemin = 80
# agemax: if age_min_anthro and age_max_anthro are within a 10-year band agemin = age_max_anthro, else agemax = floor(age) + 9.
# Exceptions: if age < 20, agemax = age
create_age_groups = function(data) {

    data$agemin <- ifelse(data$age < 20, data$age, ifelse(data$age >=
        20 & data$age < 30 & data$age_min_anthro_F >= 20 & data$age_min_anthro_F <
        30 & data$sex == 2, data$age_min_anthro_F, ifelse(data$age >= 30 &
        data$age < 40 & data$age_min_anthro_F >= 30 & data$age_min_anthro_F <
        40 & data$sex == 2, data$age_min_anthro_F, ifelse(data$age >= 40 &
        data$age < 50 & data$age_min_anthro_F >= 40 & data$age_min_anthro_F <
        50 & data$sex == 2, data$age_min_anthro_F, ifelse(data$age >= 50 &
        data$age < 60 & data$age_min_anthro_F >= 50 & data$age_min_anthro_F <
        60 & data$sex == 2, data$age_min_anthro_F, ifelse(data$age >= 60 &
        data$age < 70 & data$age_min_anthro_F >= 60 & data$age_min_anthro_F <
        70 & data$sex == 2, data$age_min_anthro_F, ifelse(data$age >= 70 &
        data$age < 80 & data$age_min_anthro_F >= 70 & data$age_min_anthro_F <
        80 & data$sex == 2, data$age_min_anthro_F, ifelse(data$age >= 80 &
        data$sex == 2, 80, ifelse(data$age >= 20 & data$age < 30 & data$age_min_anthro_M >=
        20 & data$age_min_anthro_M < 30 & data$sex == 1, data$age_min_anthro_M, ifelse(data$age >=
        30 & data$age < 40 & data$age_min_anthro_M >= 30 & data$age_min_anthro_M <
        40 & data$sex == 1, data$age_min_anthro_M, ifelse(data$age >= 40 &
        data$age < 50 & data$age_min_anthro_M >= 40 & data$age_min_anthro_M <
        50 & data$sex == 1, data$age_min_anthro_M, ifelse(data$age >= 50 &
        data$age < 60 & data$age_min_anthro_M >= 50 & data$age_min_anthro_M <
        60 & data$sex == 1, data$age_min_anthro_M, ifelse(data$age >= 60 &
        data$age < 70 & data$age_min_anthro_M >= 60 & data$age_min_anthro_M <
        70 & data$sex == 1, data$age_min_anthro_M, ifelse(data$age >= 70 &
        data$age < 80 & data$age_min_anthro_M >= 70 & data$age_min_anthro_M <
        80 & data$sex == 1, data$age_min_anthro_M, ifelse(data$age >= 80 &
        data$sex == 1, 80, floor(data$age/10) * 10)))))))))))))))

    data$agemax <- ifelse(data$age < 20, data$age, ifelse(data$age >=
        20 & data$age < 30 & data$age_max_anthro_F >= 20 & data$age_max_anthro_F <
        30 & data$sex == 2, data$age_max_anthro_F, ifelse(data$age >= 30 &
        data$age < 40 & data$age_max_anthro_F >= 30 & data$age_max_anthro_F <
        40 & data$sex == 2, data$age_max_anthro_F, ifelse(data$age >= 40 &
        data$age < 50 & data$age_max_anthro_F >= 40 & data$age_max_anthro_F <
        50 & data$sex == 2, data$age_max_anthro_F, ifelse(data$age >= 50 &
        data$age < 60 & data$age_max_anthro_F >= 50 & data$age_max_anthro_F <
        60 & data$sex == 2, data$age_max_anthro_F, ifelse(data$age >= 60 &
        data$age < 70 & data$age_max_anthro_F >= 60 & data$age_max_anthro_F <
        70 & data$sex == 2, data$age_max_anthro_F, ifelse(data$age >= 70 &
        data$age < 80 & data$age_max_anthro_F >= 70 & data$age_max_anthro_F <
        80 & data$sex == 2, data$age_max_anthro_F, ifelse(data$age >= 80 &
        data$sex == 2, data$age_max_anthro_F, ifelse(data$age >= 20 & data$age <
        30 & data$age_max_anthro_M >= 20 & data$age_max_anthro_M < 30 & data$sex ==
        1, data$age_max_anthro_M, ifelse(data$age >= 30 & data$age <
        40 & data$age_max_anthro_M >= 30 & data$age_max_anthro_M < 40 & data$sex ==
        1, data$age_max_anthro_M, ifelse(data$age >= 40 & data$age <
        50 & data$age_max_anthro_M >= 40 & data$age_max_anthro_M < 50 & data$sex ==
        1, data$age_max_anthro_M, ifelse(data$age >= 50 & data$age <
        60 & data$age_max_anthro_M >= 50 & data$age_max_anthro_M < 60 & data$sex ==
        1, data$age_max_anthro_M, ifelse(data$age >= 60 & data$age <
        70 & data$age_max_anthro_M >= 60 & data$age_max_anthro_M < 70 & data$sex ==
        1, data$age_max_anthro_M, ifelse(data$age >= 70 & data$age <
        80 & data$age_max_anthro_M >= 70 & data$age_max_anthro_M < 80 & data$sex ==
        1, data$age_max_anthro_M, ifelse(data$age >= 80 & data$sex == 1, data$age_max_anthro_M,
        9 + floor(data$age/10) * 10)))))))))))))))

    data$age_group <- stringi::stri_c(data$agemin, "-", data$agemax, sep = "")

    data$age_mean <- ifelse(data$agemax - data$agemin == 0, data$agemax, ifelse(data$agemax ==
        200, 84.91, data$agemin + (data$agemax + 1 - data$agemin)/2))

    return(data)

}
################################################################################

## create_metadata ##
create_metadata <- function(data, urban = FALSE) {

    fun1 <- function(x) sum(!is.na(x))

    if (urban) {
        metaA <- ddply(data, .(id_study, is_urban), function(d) data.frame(mid_year = unique(d$mid_year),
            start_year = unique(d$start_year), end_year = unique(d$end_year), iso = unique(d$iso),
            country = unique(d$country), survey = unique(d$survey), survey_short = unique(d$survey_short),
            survey_type = unique(d$survey_type), urban_rural = unique(d$urban_rural),
            age_range_F = unique(d$age_range_F), age_range_M = unique(d$age_range_M)))
        metaB <- reshape(aggregate(cbind(bmi_clean) ~ id_study + sex + is_urban,
            data = data, fun1), idvar = c("id_study", "is_urban"), timevar = "sex",
            direction = "wide")
        metaC <- reshape(aggregate(cbind(weight_clean) ~ id_study + sex + is_urban,
            data = data, fun1), idvar = c("id_study", "is_urban"), timevar = "sex",
            direction = "wide")
        metaD <- reshape(aggregate(cbind(height_clean) ~ id_study + sex + is_urban,
            data = data, fun1), idvar = c("id_study", "is_urban"), timevar = "sex",
            direction = "wide")
        metaE <- reshape(aggregate(cbind(waist_clean) ~ id_study + sex + is_urban,
            data = data, fun1), idvar = c("id_study", "is_urban"), timevar = "sex",
            direction = "wide")
        metaF <- reshape(aggregate(cbind(hip_clean) ~ id_study + sex + is_urban,
            data = data, fun1), idvar = c("id_study", "is_urban"), timevar = "sex",
            direction = "wide")
        metaG <- reshape(aggregate(cbind(whr_clean) ~ id_study + sex + is_urban,
            data = data, fun1), idvar = c("id_study", "is_urban"), timevar = "sex",
            direction = "wide")
        metaH <- merge_recurse(list(metaB, metaC, metaD, metaE, metaF, metaG),
                               by = c("id_study", "is_urban"))

        metadata_final_ur <- merge_recurse(list(metaA, metaH), by = c("id_study", "is_urban"))
        colnames(metadata_final_ur)[c(2, 14:25)] <- c("stratum", "N_bmi_M", "N_bmi_F",
            "N_weight_M", "N_weight_F", "N_height_M", "N_height_F", "N_waist_M",
            "N_waist_F", "N_hip_M", "N_hip_F", "N_whr_M", "N_whr_F")

        metadata_final_ur$age_range_F <- stringi::stri_c("=\"", as.character(metadata_final_ur$age_range_F),
            "\"", sep = "")  # to stop excel converting into dates
        metadata_final_ur$age_range_M <- stringi::stri_c("=\"", as.character(metadata_final_ur$age_range_M),
            "\"", sep = "")
        return(metadata_final_ur)
    } # if urban = FALSE, continue

    metaA <- ddply(data, .(id_study), function(d) data.frame(mid_year = unique(d$mid_year),
        start_year = unique(d$start_year), end_year = unique(d$end_year), iso = unique(d$iso),
        country = unique(d$country), survey = unique(d$survey), survey_short = unique(d$survey_short),
        survey_type = unique(d$survey_type), urban_rural = unique(d$urban_rural),
        age_range_F = unique(d$age_range_F), age_range_M = unique(d$age_range_M)))
    metaB <- reshape(aggregate(bmi_clean ~ id_study + sex, data = data, fun1), idvar = "id_study",
        timevar = "sex", direction = "wide")
    ## aggregate gets reports number of observations different from NA different
    ## reshape splits the dataframe generate by aggreate(), by gender, reporting the
    ## number of observations different from NA, by id_study and gender
    metaC <- reshape(aggregate(weight_clean ~ id_study + sex, data = data, fun1),
        idvar = "id_study", timevar = "sex", direction = "wide")
    metaD <- reshape(aggregate(height_clean ~ id_study + sex, data = data, fun1),
        idvar = "id_study", timevar = "sex", direction = "wide")
    metaE <- reshape(aggregate(waist_clean ~ id_study + sex, data = data, fun1),
        idvar = "id_study", timevar = "sex", direction = "wide")
    metaF <- reshape(aggregate(hip_clean ~ id_study + sex, data = data, fun1), idvar = "id_study",
        timevar = "sex", direction = "wide")
    metaG <- reshape(aggregate(whr_clean ~ id_study + sex, data = data, fun1), idvar = "id_study",
        timevar = "sex", direction = "wide")
    metaH <- merge_recurse(list(metaB, metaC, metaD, metaE, metaF, metaG), by = c("id_study"))

    metadata_final <- merge_recurse(list(metaA, metaH), by = c("id_study"))
    colnames(metadata_final)[13:24] <- c("N_bmi_M", "N_bmi_F", "N_weight_M", "N_weight_F",
        "N_height_M", "N_height_F", "N_waist_M", "N_waist_F", "N_hip_M", "N_hip_F",
        "N_whr_M", "N_whr_F")

    return(metadata_final)
}
################################################################################

## svy_check ##
## Checks whether in tmp[, "var"] there is a mixed of subjects with reported values
# and subjects with missing values for "var"
# December 2022 - Rosie updated to also count participants with samplewts =0
svy_check <- function(tmp, var = "samplewt_anthro") {

  if (any(is.na(tmp[, var]) | tmp[, var]==0) & !all(is.na(tmp[, var])) & var == "samplewt_anthro" & is.numeric(tmp[, var])) {
    na0 <- (is.na(tmp[, var]) | tmp[, var]==0)
    ans <- c(sum(na0),
             round(100*sum(na0)/nrow(tmp), 2), # This was updated Dec-18 to include %NA
             range(na.omit(tmp[, var])),
             stringi::stri_c(tmp$names[(is.na(tmp[, var])| tmp[, var]==0) ], collapse = "//"))
    names(ans) <- c("N_NA","%_NA", "min_sw", "max_sw", "NA_idx")
    return(ans)
  } else if (any(is.na(tmp[, var])) & !all(is.na(tmp[, var])) & var != "samplewt_anthro") {
    ans <- c(sum(is.na(tmp[, var])),
             round(100*sum(is.na(tmp[, var]))/nrow(tmp), 2), # This was updated Dec-18 to include %NA
             stringi::stri_c(tmp$names[(is.na(tmp[, var]))], collapse = "//"))
    names(ans) <- c("N_NA", "%_NA", "NA_idx")
    return(ans)
  }  else {
    return(NULL)
  }}
################################################################################

## clean_svydesign ##
## This function stratifies the data based in: id_study, or id_study//age_mean//sex, or id_study//age_mean//sex//is_urban;
# It then checks whether within a given study there is a mix of subjects with & without (NA) values
# for any survey design variable: psu, stratum, sample weight (each variable is explored separately).
# These studies where there is a mixed of subjects with & without (i.e. NA) values for
# the survey design variables are problematic because the svydesign() function can't deal
# with NA values. Therefore, subjects with NA values within these "problematic studies" must be removed.
clean_svydesign <- function (data, by = "survey") {

    if(grepl("S", rownames(data))[1] == FALSE) { # Make sure we have rownames
        rownames(data) <- stringi::stri_c("S", 1:nrow(data), sep = "")
        data$names <- rownames(data)
    }

    if (by == "age_gender") {
        sw_prob <- ddply(data,.(id_study, age_mean, sex), function(tmp) svy_check(tmp))
        psu_prob <- ddply(data,.(id_study, age_mean, sex), function(tmp) svy_check(tmp, var = "psu"))
        stratum_prob <- ddply(data,.(id_study, age_mean, sex), function(tmp) svy_check(tmp, var = "stratum"))

    } else if (by == "urban"){
        sw_prob <- ddply(data,.(id_study, age_mean, sex, is_urban), function(tmp) svy_check(tmp))
        psu_prob <- ddply(data,.(id_study, age_mean, sex, is_urban), function(tmp) svy_check(tmp, var = "psu"))
        stratum_prob <- ddply(data,.(id_study, age_mean, sex, is_urban), function(tmp) svy_check(tmp, var = "stratum"))

    } else { # Do not stratifiy
        sw_prob <- ddply(data,.(id_study), function(tmp) svy_check(tmp))
        psu_prob <- ddply(data,.(id_study), function(tmp) svy_check(tmp, var = "psu"))
        stratum_prob <- ddply(data,.(id_study), function(tmp) svy_check(tmp, var = "stratum"))
    }

    if (ncol(sw_prob) > 4) {
        sw_prob <- sw_prob[, setdiff(colnames(sw_prob), c("min_sw", "max_sw"))]
    }

    all_prob <- rbind(sw_prob, psu_prob, stratum_prob)

    if (nrow(all_prob) == 0) {
        message("None of the subjects was dropped due to survey design")
        return(data)
    }
    indx <- unlist(strsplit(all_prob$NA_idx, "//"))
    if (sum(as.numeric(all_prob$N_NA)) != length(indx)) {
        stop ("Problem in code")
    }
    indx_wanted <- setdiff(rownames(data), indx)

    to_print <- stringi::stri_c("Number of subjects removed due to survey design:", length(indx))
    data <- data[indx_wanted, ]

    return(data)

}
################################################################################

## bmi_adol ##
# Gets prevalence of bmi for a given bmi category (bmi_cat), and for a given age_sex group (bmi_line)
bmi_adol <- function(bmi_line, bmi_cat, bmi_data) {
    if (grepl("neg", bmi_cat)) {
        res <- which(bmi_data$age_mean == bmi_line$age & bmi_data$sex == bmi_line$sex &
                         bmi_data$bmi_clean < bmi_line[, bmi_cat])
    } else {
        res <- which(bmi_data$age_mean == bmi_line$age & bmi_data$sex == bmi_line$sex &
                         bmi_data$bmi_clean > bmi_line[, bmi_cat])
    }
    return(res)
}

## get bmi_prev ##
# Gives 1 if an individual falls in a given BMI category, else 0 (subjects with NA values in height_data were removed)
get_bmi_prev = function(bmi_data) {

    # prevalences
    bmi_data$prev_bmi12 <- ifelse(bmi_data$bmi_clean < 12, 1,0)
    bmi_data$prev_bmi12_15 <- ifelse(bmi_data$bmi_clean >= 12 & bmi_data$bmi_clean < 15, 1, 0)
    bmi_data$prev_bmi15_185 <- ifelse(bmi_data$bmi_clean >= 15 & bmi_data$bmi_clean < 18.5, 1, 0)
    bmi_data$prev_bmi185_20 <- ifelse(bmi_data$bmi_clean >= 18.5 & bmi_data$bmi_clean < 20, 1, 0)
    bmi_data$prev_bmi20_25 <- ifelse(bmi_data$bmi_clean >= 20 & bmi_data$bmi_clean < 25, 1, 0)
    bmi_data$prev_bmi25_30 <- ifelse(bmi_data$bmi_clean >= 25 & bmi_data$bmi_clean < 30, 1, 0)
    bmi_data$prev_bmi30_35 <- ifelse(bmi_data$bmi_clean >= 30 & bmi_data$bmi_clean < 35, 1, 0)
    bmi_data$prev_bmi35_40 <- ifelse(bmi_data$bmi_clean >= 35 & bmi_data$bmi_clean < 40, 1, 0)
    bmi_data$prev_bmi40 <- ifelse(bmi_data$bmi_clean >= 40, 1, 0)
    bmi_data$prev_bmi25 <- ifelse(bmi_data$bmi_clean >= 25, 1, 0)
    bmi_data$prev_bmi30 <- ifelse(bmi_data$bmi_clean >= 30, 1, 0)
    bmi_data$prev_bmi22 <- ifelse(bmi_data$bmi_clean >= 22, 1, 0)
    bmi_data$prev_bmi23 <- ifelse(bmi_data$bmi_clean >= 23, 1, 0)
    bmi_data$prev_bmi24 <- ifelse(bmi_data$bmi_clean >= 24, 1, 0)
    bmi_data$prev_bmi27 <- ifelse(bmi_data$bmi_clean >= 27, 1, 0)
    bmi_data$prev_bmi275 <- ifelse(bmi_data$bmi_clean >= 27.5, 1, 0)
    bmi_data$prev_bmi28 <- ifelse(bmi_data$bmi_clean >= 28, 1, 0)
    bmi_data$prev_bmi_185_25 <- ifelse(bmi_data$bmi_clean < 25 & bmi_data$bmi_clean >= 18.5, 1, 0)
    bmi_data$prev_bmi_l185 <- ifelse(bmi_data$bmi_clean < 18.5, 1, 0)
    bmi_data$prev_bmi_l16 <- ifelse(bmi_data$bmi_clean < 16, 1, 0)
    bmi_data$prev_bmi_16_17 <- ifelse(bmi_data$bmi_clean < 17 & bmi_data$bmi_clean >= 16, 1, 0)
    bmi_data$prev_bmi_l17 <- ifelse(bmi_data$bmi_clean < 17, 1, 0)
    bmi_data$prev_bmi_l25 <- ifelse(bmi_data$bmi_clean < 25, 1, 0)
    bmi_data$prev_bmi_30_40 <- ifelse(bmi_data$bmi_clean < 40 & bmi_data$bmi_clean >= 30, 1, 0)
    bmi_data$prev_bmi_17_185 <- ifelse(bmi_data$bmi_clean < 18.5 & bmi_data$bmi_clean >= 17, 1,0)

    ## Adolescent cut-offs
    bmi_cutoff <- read.csv("S:/Projects/HeightProject/Original dataset/Anthropometrics/Individual_data/child_adolescent_bmi_cutoffs.csv") ## Update as required
    bmi_rows <- split(bmi_cutoff, f = seq(nrow(bmi_cutoff)))

    idx_neg2sd <- unlist(lapply(bmi_rows, bmi_adol, bmi_cat = "bmi_neg2sd", bmi_data = bmi_data))
    idx_neg1sd <- unlist(lapply(bmi_rows, bmi_adol, bmi_cat = "bmi_neg1sd", bmi_data = bmi_data))
    idx_sd <- unlist(lapply(bmi_rows, bmi_adol, bmi_cat = "bmi_sd", bmi_data = bmi_data))
    idx_2sd <- unlist(lapply(bmi_rows, bmi_adol, bmi_cat = "bmi_2sd", bmi_data = bmi_data))

    bmi_data$prev_bmi_neg2sd <- 0
    bmi_data$prev_bmi_neg2sd[idx_neg2sd] <- 1 # Works even in idx_neg2sd = integer(0)

    bmi_data$prev_bmi_neg1sd <- 0
    bmi_data$prev_bmi_neg1sd[idx_neg1sd] <- 1

    bmi_data$prev_bmi_1sd <- 0
    bmi_data$prev_bmi_1sd[idx_sd] <- 1

    bmi_data$prev_bmi_2sd <- 0
    bmi_data$prev_bmi_2sd[idx_2sd] <- 1

    return(bmi_data)
}
################################################################################

## get_height_prev ##
# Gives 1 if an individual falls in a given height category, else 0 (subjects with NA values in height_data were removed)
get_height_prev = function(height_data) {

    height_data$prev_height120 <- ifelse(height_data$height_clean < 120, 1, 0)
    height_data$prev_height120_140 <- ifelse(height_data$height_clean >= 120 & height_data$height_clean < 140, 1, 0)
    height_data$prev_height140_145 <- ifelse(height_data$height_clean >= 140 & height_data$height_clean < 145, 1, 0)
    height_data$prev_height145_150 <- ifelse(height_data$height_clean >= 145 & height_data$height_clean < 150, 1, 0)
    height_data$prev_height150_155 <- ifelse(height_data$height_clean >= 150 & height_data$height_clean < 155, 1, 0)
    height_data$prev_height155_160 <- ifelse(height_data$height_clean >= 155 & height_data$height_clean < 160, 1, 0)
    height_data$prev_height160_165 <- ifelse(height_data$height_clean >= 160 & height_data$height_clean < 165, 1, 0)
    height_data$prev_height165_170 <- ifelse(height_data$height_clean >= 165 & height_data$height_clean < 170, 1, 0)
    height_data$prev_height170_175 <- ifelse(height_data$height_clean >= 170 & height_data$height_clean < 175, 1, 0)
    height_data$prev_height175_180 <- ifelse(height_data$height_clean >= 175 & height_data$height_clean < 180, 1, 0)
    height_data$prev_height180_185 <- ifelse(height_data$height_clean >= 180 & height_data$height_clean < 185, 1, 0)
    height_data$prev_height185_190 <- ifelse(height_data$height_clean >= 185 & height_data$height_clean < 190, 1, 0)
    height_data$prev_height190_195 <- ifelse(height_data$height_clean >= 190 & height_data$height_clean < 195, 1, 0)
    height_data$prev_height195_200 <- ifelse(height_data$height_clean >= 195 & height_data$height_clean < 200, 1, 0)
    height_data$prev_height200 <- ifelse(height_data$height_clean >= 200, 1, 0)

    return(height_data)
}
################################################################################

## get_summary_bmi ##
# Gets summaries for bmi, and has been adapted from the "sumd()" function from BZ.
# This function uses functionalities from the "survey" R package (svymean(), svydesign())
# For each study (id_study//sex//age) it calculates the mean of bmi (as continuous variable)
# and the mean of each prevalence group, across all subjects included within that
# study. This means that the SE for prevalences, are calcualted as for means

get_summary <- function(tmp, study = NULL) { #

    if (any(grepl("prev_bmi", colnames(tmp)))) {
        var <- "bmi"
    } else {
        var <- "height"
    }
    message (stringi::stri_c(var, "summaries", study))
    print(stringi::stri_c(tmp$id_study[1], tmp$sex[1], tmp$age_mean[1], sep = " "))
    var_clean <- stringi::stri_c(var, "clean", sep = "_")

    ## Set options ##
    options(survey.lonely.psu = "adjust", survey.adjust.domain.lonely = TRUE)

    ## Check survey design variables ##
    if (any(is.na(tmp$samplewt_anthro))) { # If sample weights are missing set them to 1 (NA values not allowed in svydesign())
        if (all(is.na(tmp$samplewt_anthro)) == FALSE) { # Sample weights should be all NA, or all not NA
            stop ("Sample weights are missing ONLY in some subjects")
        }
        tmp$samplewt_anthro <- 1
        res_wt <- TRUE #
    }
    if (any(is.na(tmp$psu))) {
        if (all(is.na(tmp$psu)) == FALSE) { # psu should be all NA, or all not NA
            stop ("psu values are missing ONLY in some subjects")
        }
        res_psu <- FALSE # psu missing
    } else {
        res_psu <- TRUE # psu available
    }
    if (any(is.na(tmp$stratum))) {
        if (all(is.na(tmp$stratum)) == FALSE) { # stratum should be all NA, or all not NA
            stop ("stratum values are missing ONLY in some subjects")
        }
        res_stratum <- FALSE # stratum missing
    } else {
        res_stratum <- TRUE # stratum available
    }

    ## Clean here for single_psu_ssa ## Updated on 27/02/19 by ARM (from line 490 to line 499)
    # Number of different psu (excluding NAs). If the study has only one psu and
    # stratum is not reported, it should be dropped. Else the function svydesign gives an error
    if (length(unique(na.omit(tmp$psu))) == 1) { # Updated on 01/03/20 by ARM
        res_psu <- FALSE # psu not available
        tmp$psu <- NA
        message("Study psu updated to NA because of single psu")
    }
    num.valid <- function(x) sum(!is.na(x) & x != -1) # sums values which are different from NA or -1 (valid subjects)
    n_var <- num.valid(tmp[, var_clean])

    if (n_var > 1) {

        if (sum(res_psu, res_stratum) == 0) { # psu and stratum missing (if sw are not available, they're all 1)
            dsub <- svydesign(id = ~1, strata = NULL, weights = ~samplewt_anthro, data = tmp)
            print("id = ~1, strata = NULL, weights = ~samplewt_anthro") ## Updated on 27/02/19 by ARM (line 505)
        } else if ((sum(res_psu, res_stratum) == 2)) { # we have psu and stratum
            dsub <- svydesign(id = ~psu, strata = ~stratum, weights = ~samplewt_anthro, data = tmp, nest = T)
            print("id = ~psu, strata = ~stratum, weights = ~samplewt_anthro")
        } else if (res_psu & !res_stratum) {# we have psu but not stratum
            dsub <- svydesign(id = ~psu, strata = NULL, weights = ~samplewt_anthro, data = tmp, nest = T)
            print("id = ~psu, strata = ~NULL, weights = ~samplewt_anthro")
        } else if (!res_psu & res_stratum) {
            dsub <- svydesign(id = ~1, strata = ~stratum, weights = ~samplewt_anthro, data = tmp, nest = T)
            print("id = ~1, strata = ~stratum, weights = ~samplewt_anthro")
        }

        var_m <- data.frame(svymean(~tmp[, var_clean], dsub, na.rm = T))  # gives mean and standard error
        var_sd <- as.numeric(sqrt(coef(svyvar(~tmp[, var_clean], dsub, na.rm = T)))) # gives only one value

        if (var == "bmi") {
            colnames(tmp) <- gsub("bmi_", "bmi", colnames(tmp))
            colnames(tmp) <- gsub("bmi", "bmi_", colnames(tmp))
        } else {
            colnames(tmp) <- gsub("height_", "height", colnames(tmp))
            colnames(tmp) <- gsub("height", "height_", colnames(tmp))
        }

        prev_names <- grep("prev_", colnames(tmp), value = TRUE)
        res <- as.data.frame(matrix(ncol = (length(prev_names)*2 + 4), nrow = 1)) # empty df
        prev_cols <- unlist(lapply(prev_names, function(x) c(stringi::stri_c("se", x, sep = "_"), x)))
        var_cols <- stringi::stri_c(c("N", "mean", "se", "sd"), var, sep = "_")
        colnames(res) <- c(var_cols, prev_cols)

        res[, var_cols] <- c(n_var, var_m[1, 1], var_m[1, 2], var_sd)

        for (prev in prev_names) {
            ans <- data.frame(svymean(~tmp[, prev], dsub, na.rm = T))
            res[, prev] <- ans[1, 1]
            res[, stringi::stri_c("se", prev, sep = "_")] <- ans[1, 2]
        }

    } else {
        message("Study excluded as n <= 1")
        res <- NULL
    }

    return(res)
}
################################################################################

### get_summary_parallel ## similar to get summary, but in parallel
# https://stackoverflow.com/questions/28503208/doparallel-error-in-r-error-in-serializedata-nodecon-error-writing-to-con
get_summary_parallel <- function(data, get_summary = get_summary, study = NULL,
                                 ur = FALSE, ncore = 35) {
    start_time <- Sys.time()
    cores <- detectCores()
    #cI <- makeCluster(cores[1]-1, outfile = "debug.txt") # to be checked
    cI <- makeCluster(ncore, outfile = "debug.txt") # to be checked
    if (ur) {
        data$split <- stringi::stri_c(data$id_study, data$is_urban, data$sex, data$age_mean, sep =" ")
    } else {
        data$split <- stringi::stri_c(data$id_study, data$sex, data$age_mean, sep =" ")
    }
    splits <- unique(data$split)

    registerDoParallel(cI)
    summary0 <- foreach(i=1:length(splits), .packages="survey") %dopar% get_summary(subset(data, split == splits[i]), study = study)
    stopCluster(cI)
    print(Sys.time() - start_time)
    ind_null <- sapply(summary0, function(x) is.null(x))

    if(ur) {
        idents <- data.frame(matrix(unlist(strsplit(splits," ")), ncol = 4, byrow = TRUE))
        names(idents) <- c("id_study","stratum", "sex","age_mean")
        idents$stratum <- as.character(idents$stratum)
    } else {
        idents <- data.frame(matrix(unlist(strsplit(splits," ")), ncol = 3, byrow = TRUE))
        names(idents) <- c("id_study","sex","age_mean")
    }
    idents$id_study <- as.character(idents$id_study)
    idents$sex <- as.numeric(as.character(idents$sex))
    idents$age_mean <- as.numeric(as.character(idents$age_mean))
    idents <- idents[!ind_null, ]
    summary <- cbind(idents, do.call(rbind, summary0))
    summary <- summary[order(summary$id_study, summary$sex, summary$age_mean), ]
    rownames(summary) <- NULL
    return(summary)
}
################################################################################

## save_data ## Saves summaries and metadata
save_data <- function(dataset, directory = NULL, date, other_old = NULL) { ## ARM: 14/06/19 - Updated to account for non-stratified data in summary data

    if (is.null(directory)) {
        directory <- getwd()
    }
    old_dir <- getwd()
    name <- deparse(substitute(dataset)) # transform var name into string

    ## In individual level data, add old dataset
    if (!is.null(other_old)) {
        dataset <- unique(rbind(dataset, other_old %>% filter(!id_study %in% dataset$id_study)))
    }

    ## Sort dataset by id_study
    dataset <- arrange(dataset, id_study)

    if(any(grepl("metadata", name, ignore.case = TRUE))) {
        if(!any(grepl("ur", name, ignore.case = TRUE))) {
            setwd(directory)

            write.csv(dataset, stringi::stri_c(study, "metadatafinal_latest.csv", sep = ""))
            setwd(stringi::stri_c(directory, "old", sep = "/"))

            write.csv(dataset, stringi::stri_c(study, "metadatafinal_", date, ".csv", sep = ""))
        } else {
            setwd(directory)
            if(any(grepl('nonstrat', name, ignore.case= TRUE))){
                write.csv(dataset, stringi::stri_c(study, "_NONSTRATIFIED", "metadatafinal_ur_latest.csv", sep = ""))

            } else {

              write.csv(dataset, stringi::stri_c(study, "metadatafinal_ur_latest.csv", sep = ""))
            }
            setwd(stringi::stri_c(directory, "old", sep = "/"))
            if(any(grepl('nonstrat', name, ignore.case= TRUE))){

                write.csv(dataset, stringi::stri_c(study, "_NONSTRATIFIED", "metadatafinal_ur", date, ".csv", sep = ""))
            } else {

              write.csv(dataset, stringi::stri_c(study, "metadatafinal_ur", date, ".csv", sep = ""))
            }
        }
        setwd(old_dir)
        return("Done!")
    }
    if(any(grepl("height", name, ignore.case = TRUE))) {
        if(!any(grepl("ur", name, ignore.case = TRUE))) {
            setwd(directory)
            write.csv(dataset, stringi::stri_c(study, "summary_height_latest.csv", sep = ""))

            setwd(stringi::stri_c(directory, "old", sep = "/"))
            write.csv(dataset, stringi::stri_c(study, "summary_height_", date, ".csv", sep = ""))

        } else {
            setwd(directory)
            if(any(grepl('nonstrat', name, ignore.case= TRUE))){
                write.csv(dataset, stringi::stri_c(study,  "_NONSTRATIFIED", "summary_height_ur_latest.csv", sep = ""))

            } else {

               write.csv(dataset, stringi::stri_c(study, "summary_height_ur_latest.csv", sep = ""))
            }
            setwd(stringi::stri_c(directory, "old", sep = "/"))
            if(any(grepl('nonstrat', name, ignore.case= TRUE))){
                write.csv(dataset, stringi::stri_c(study, "_NONSTRATIFIED", "summary_height_ur_", date, ".csv", sep = ""))

            } else {
                write.csv(dataset, stringi::stri_c(study, "summary_height_ur_", date, ".csv", sep = ""))

            }
        }
        setwd(old_dir)
        return("Done!")
    }
    if(any(grepl("bmi", name, ignore.case = TRUE))) {
        if(!any(grepl("ur", name, ignore.case = TRUE))) {
            setwd(directory)
            write.csv(dataset, stringi::stri_c(study, "summary_bmi_latest.csv", sep = ""))

            setwd(stringi::stri_c(directory, "old", sep = "/"))
            write.csv(dataset, stringi::stri_c(study, "summary_bmi_", date, ".csv", sep = ""))

        } else {
            setwd(directory)
            if(any(grepl('nonstrat', name, ignore.case= TRUE))){
                write.csv(dataset, stringi::stri_c(study, "_NONSTRATIFIED", "summary_bmi_ur_latest.csv", sep = ""))

            } else {
                write.csv(dataset, stringi::stri_c(study, "summary_bmi_ur_latest.csv", sep = ""))

            }
            setwd(stringi::stri_c(directory, "old", sep = "/"))
            if(any(grepl('nonstrat', name, ignore.case= TRUE))){
                write.csv(dataset, stringi::stri_c(study, "_NONSTRATIFIED", "summary_bmi_ur_", date, ".csv", sep = ""))

            } else {
                write.csv(dataset, stringi::stri_c(study, "summary_bmi_ur_", date, ".csv", sep = ""))

            }
        }
        setwd(old_dir)
        return("Done!")
    }
}

## clean code ##
#library(formatR)
# tidy_eval("M:/Data cleaning/Height_data_update/Anthropometrics_cleaning_functions_tidy.R",
#           file = "M:/Data cleaning/Height_data_update/Anthropometrics_cleaning_functions_tidy2.R", width.cutoff = 80)

