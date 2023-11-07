
#### Calculating metrics ####
## Define DM status (repeated for each predicted values)
define_DM_status <- function(data) {

    # 0. Diagnosed DM among all participants regardless of biomarker availability 20230526
    data$DM_diag <- data$diagnosis

    # 991. Treated DM among all participants regardless of biomarker availability
    data$DM_med <- data$medication

    # 992. Diagnosed DM among all participants regardless of biomarker availability
    data$DM_med_over_diag <- NA
    data$DM_med_over_diag[which(data$DM_diag == 1)] <- 0
    data$DM_med_over_diag[which(data$DM_diag == 1 & data$DM_med == 1)] <- 1
    data$DM_med_over_diag[is.na(data$DM_med)] <- NA

    # Utilities
    # 2. DM defined as: Fasting glucose>=7.0 mmol/L OR diagnosis OR medication
    data$DM_fgl_70_self_med <- NA
    data$DM_fgl_70_self_med[with(data,which(fgl_clean>=7|new_med==1))] <- 1
    data$DM_fgl_70_self_med[with(data,which(fgl_clean<7&new_med==0))] <- 0
    data$DM_fgl_70_self_med[with(data,which(is.na(fgl_clean) | is.na(new_med)))] <- NA

    # 4. DM defined as: HbA1c>=6.5% OR diagnosis OR medication
    data$DM_hba1c_65_self_med <- NA
    data$DM_hba1c_65_self_med[with(data,which(hba1c_clean>=6.5|new_med==1))] <- 1
    data$DM_hba1c_65_self_med[with(data,which(hba1c_clean<6.5&new_med==0))] <- 0
    data$DM_hba1c_65_self_med[with(data,which(is.na(hba1c_clean) | is.na(new_med)))] <- NA

    # Main diabetes prevalence
    data$DM_fgl_70_self <- ifelse(is.na(data$diagnosis), NA, data$DM_fgl_70_self_med)
    data$DM_hba1c_65_self <- ifelse(is.na(data$diagnosis), NA, data$DM_hba1c_65_self_med)

    # define new metric with CWed data at individual level
    ### From FPG ###
    # Total diabetes in complete set
    # 40. DM defined as: Fasting glucose>=7.0 mmol/L OR CWed HbA1c>=6.5% OR diagnosis OR medication
    data$DM_fgl_70_cwhba1c_65_self_med <- data$DM_fgl_70_self_med
    list <- which(!is.na(data$predicted_a1c) & data$DM_fgl_70_cwhba1c_65_self_med == 0)
    data$DM_fgl_70_cwhba1c_65_self_med[list] <- data$predicted_a1c[list]  # predicted probability
    data$DM_fgl_70_cwhba1c_65_self_med[data$study_id %in% fpg_a1c_diag_studies] <- NA  # remove for studies where the calculation does not make sense
    data$DM_fgl_70_cwhba1c_65_self_med[!data$study_id %in% predict_a1c_studies] <- NA  # studies without diagnosis are not CWed

    # utility
    data$DM_fgl_70_cwhba1c_65_self <- ifelse(is.na(data$diagnosis), NA, data$DM_fgl_70_cwhba1c_65_self_med)

    # Undiagnosed diabetes prevalence in complete set
    # 401. DM defined as: fasting glucose>=7.0 mmol/L OR CWed HbA1c>=6.5% AND undiagnosed
    data$undiagDM_fgl_70_cwhba1c_65_undiag <- data$DM_fgl_70_cwhba1c_65_self
    data$undiagDM_fgl_70_cwhba1c_65_undiag[which(data$undiagDM_fgl_70_cwhba1c_65_undiag == 1 & data$diagnosis == 1)] <- 0
    data$undiagDM_fgl_70_cwhba1c_65_undiag[data$study_id %in% fpg_a1c_diag_studies] <- NA  # remove for studies where the calculation does not make sense
    data$undiagDM_fgl_70_cwhba1c_65_undiag[!data$study_id %in% predict_a1c_studies] <- NA  # studies without diagnosis are not CWed

    # Undiagnosed diabetes by components in complete set
    # 41. Undiagnosed, fasting glucose >=7.0 mmol/L AND CWed HbA1c<6.5%
    data$DM_fgl_70_and_cwhba1c_l65_undiag <- NA
    data$DM_fgl_70_and_cwhba1c_l65_undiag[!is.na(data$DM_fgl_70_self)] <- 0
    list <- which(data$fgl_clean >=7 & data$diagnosis == 0 & !is.na(data$predicted_a1c))
    data$DM_fgl_70_and_cwhba1c_l65_undiag[list] <- 1 - data$predicted_a1c[list]
    data$DM_fgl_70_and_cwhba1c_l65_undiag[data$study_id %in% fpg_a1c_diag_studies] <- NA  # remove for studies where the calculation does not make sense
    data$DM_fgl_70_and_cwhba1c_l65_undiag[!data$study_id %in% predict_a1c_studies] <- NA  # studies without diagnosis are not CWed

    # 42. Undiagnosed, fasting glucose <7.0 mmol/L AND CWed HbA1c>=6.5%
    data$DM_fgl_l70_and_cwhba1c_65_undiag <- NA
    data$DM_fgl_l70_and_cwhba1c_65_undiag[!is.na(data$DM_fgl_70_self)] <- 0
    list <- which(data$fgl_clean <7 & data$diagnosis == 0 & !is.na(data$predicted_a1c))
    data$DM_fgl_l70_and_cwhba1c_65_undiag[list] <- data$predicted_a1c[list]
    data$DM_fgl_l70_and_cwhba1c_65_undiag[data$study_id %in% fpg_a1c_diag_studies] <- NA  # remove for studies where the calculation does not make sense
    data$DM_fgl_l70_and_cwhba1c_65_undiag[!data$study_id %in% predict_a1c_studies] <- NA  # studies without diagnosis are not CWed

    # 43. Undiagnosed, fasting glucose >=7.0 mmol/L AND CWed HbA1c>=6.5%
    data$DM_fgl_70_and_cwhba1c_65_undiag <- NA
    data$DM_fgl_70_and_cwhba1c_65_undiag[!is.na(data$DM_fgl_70_self)] <- 0
    list <- which(data$fgl_clean >=7 & data$diagnosis == 0 & !is.na(data$predicted_a1c))
    data$DM_fgl_70_and_cwhba1c_65_undiag[list] <- data$predicted_a1c[list]
    data$DM_fgl_70_and_cwhba1c_65_undiag[data$study_id %in% fpg_a1c_diag_studies] <- NA  # remove for studies where the calculation does not make sense
    data$DM_fgl_70_and_cwhba1c_65_undiag[!data$study_id %in% predict_a1c_studies] <- NA  # studies without diagnosis are not CWed


    ### From HbA1c ###
    # Total diabetes in complete set
    # 44. DM defined as: CWed fasting glucose>=7.0 mmol/L OR HbA1c>=6.5% OR diagnosis OR medication
    data$DM_cwfgl_70_hba1c_65_self_med <- data$DM_hba1c_65_self_med
    list <- which(!is.na(data$predicted_fpg) & data$DM_cwfgl_70_hba1c_65_self_med == 0)
    data$DM_cwfgl_70_hba1c_65_self_med[list] <- data$predicted_fpg[list]
    data$DM_cwfgl_70_hba1c_65_self_med[data$study_id %in% fpg_a1c_diag_studies] <- NA  # remove for studies where the calculation does not make sense
    data$DM_cwfgl_70_hba1c_65_self_med[!data$study_id %in% predict_fpg_studies] <- NA  # studies without diagnosis are not CWed

    # utility
    data$DM_cwfgl_70_hba1c_65_self <- ifelse(is.na(data$diagnosis), NA, data$DM_cwfgl_70_hba1c_65_self_med)

    # Undiagnosed diabetes prevalence in complete set
    # 441. DM defined as: CWed fasting glucose>=7.0 mmol/L OR HbA1c>=6.5% AND undiagnosed
    data$undiagDM_cwfgl_70_hba1c_65_undiag <- data$DM_cwfgl_70_hba1c_65_self
    data$undiagDM_cwfgl_70_hba1c_65_undiag[which(data$undiagDM_cwfgl_70_hba1c_65_undiag == 1 & data$diagnosis == 1)] <- 0
    data$undiagDM_cwfgl_70_hba1c_65_undiag[data$study_id %in% fpg_a1c_diag_studies] <- NA  # remove for studies where the calculation does not make sense
    data$undiagDM_cwfgl_70_hba1c_65_undiag[!data$study_id %in% predict_fpg_studies] <- NA  # studies without diagnosis are not CWed

    # Undiagnosed diabetes components in complete set
    # 45. Undiagnosed, CWed fasting glucose >=7.0 mmol/L AND HbA1c<6.5%
    data$DM_cwfgl_70_and_hba1c_l65_undiag <- NA
    data$DM_cwfgl_70_and_hba1c_l65_undiag[!is.na(data$DM_hba1c_65_self)] <- 0
    list <- which(data$hba1c_clean <6.5 & data$diagnosis == 0 & !is.na(data$predicted_fpg))
    data$DM_cwfgl_70_and_hba1c_l65_undiag[list] <- data$predicted_fpg[list]
    data$DM_cwfgl_70_and_hba1c_l65_undiag[data$study_id %in% fpg_a1c_diag_studies] <- NA  # remove for studies where the calculation does not make sense
    data$DM_cwfgl_70_and_hba1c_l65_undiag[!data$study_id %in% predict_fpg_studies] <- NA  # studies without diagnosis are not CWed

    # 46. Undiagnosed, CWed fasting glucose <7.0 mmol/L AND HbA1c>=6.5%
    data$DM_cwfgl_l70_and_hba1c_65_undiag <- NA
    data$DM_cwfgl_l70_and_hba1c_65_undiag[!is.na(data$DM_hba1c_65_self)] <- 0
    list <- which(data$hba1c_clean >=6.5 & data$diagnosis == 0 & !is.na(data$predicted_fpg))
    data$DM_cwfgl_l70_and_hba1c_65_undiag[list] <- 1 - data$predicted_fpg[list]
    data$DM_cwfgl_l70_and_hba1c_65_undiag[data$study_id %in% fpg_a1c_diag_studies] <- NA  # remove for studies where the calculation does not make sense
    data$DM_cwfgl_l70_and_hba1c_65_undiag[!data$study_id %in% predict_fpg_studies] <- NA  # studies without diagnosis are not CWed

    # 47. Undiagnosed, CWed fasting glucose >=7.0 mmol/L AND HbA1c>=6.5%
    data$DM_cwfgl_70_and_hba1c_65_undiag <- NA
    data$DM_cwfgl_70_and_hba1c_65_undiag[!is.na(data$DM_hba1c_65_self)] <- 0
    list <- which(data$hba1c_clean >=6.5 & data$diagnosis == 0 & !is.na(data$predicted_fpg))
    data$DM_cwfgl_70_and_hba1c_65_undiag[list] <- data$predicted_fpg[list]
    data$DM_cwfgl_70_and_hba1c_65_undiag[data$study_id %in% fpg_a1c_diag_studies] <- NA  # remove for studies where the calculation does not make sense
    data$DM_cwfgl_70_and_hba1c_65_undiag[!data$study_id %in% predict_fpg_studies] <- NA  # studies without diagnosis are not CWed


    ## Combine predicted and raw data
    # Fill missing data both ways: fpglist or a1clist no longer matters
    # 50. DM defined as: Fasting glucose>=7.0 mmol/L OR HbA1c >=6.5% OR CWed FPG >=7.0 mmol/L OR CWed HbA1c>=6.5% OR diagnosis OR medication
    data$DM_fgl_70_hba1c_65_cwfgl_70_cwhba1c_65_self_med <- NA
    data$DM_fgl_70_hba1c_65_cwfgl_70_cwhba1c_65_self_med[which(data$fgl_clean>=7 | data$hba1c_clean>=6.5 | data$new_med==1)] <- 1
    data$DM_fgl_70_hba1c_65_cwfgl_70_cwhba1c_65_self_med[which(data$fgl_clean<7 & data$hba1c_clean<6.5 & data$new_med==0)] <- 0
    data$DM_fgl_70_hba1c_65_cwfgl_70_cwhba1c_65_self_med[which(is.na(data$fgl_clean) & is.na(data$hba1c_clean))] <- NA
    data$DM_fgl_70_hba1c_65_cwfgl_70_cwhba1c_65_self_med[is.na(data$new_med)] <- NA
    list <- which(data$new_med==0 & is.na(data$fgl_clean) & data$hba1c_clean<6.5 & is.na(data$DM_fgl_70_hba1c_65_cwfgl_70_cwhba1c_65_self_med))
    data$DM_fgl_70_hba1c_65_cwfgl_70_cwhba1c_65_self_med[list] <- data$predicted_fpg[list]
    list <- which(data$new_med==0 & is.na(data$hba1c_clean) & data$fgl_clean<7 & is.na(data$DM_fgl_70_hba1c_65_cwfgl_70_cwhba1c_65_self_med))
    data$DM_fgl_70_hba1c_65_cwfgl_70_cwhba1c_65_self_med[list] <- data$predicted_a1c[list]
    # remove values if a study does not have all of the components for this calculation to make sense
    data$DM_fgl_70_hba1c_65_cwfgl_70_cwhba1c_65_self_med[!data$study_id %in% fpg_a1c_diag_studies] <- NA

    # Utility
    data$DM_fgl_70_hba1c_65_cwfgl_70_cwhba1c_65_self <- ifelse(is.na(data$diagnosis), NA, data$DM_fgl_70_hba1c_65_cwfgl_70_cwhba1c_65_self_med)

    # 501. Undiagnosed DM defined as: Fasting glucose>=7.0 mmol/L OR HbA1c >=6.5% OR CWed FPG >=7.0 mmol/L OR CWed HbA1c>=6.5% AND undiagnosed
    data$undiagDM_fgl_70_hba1c_65_cwfgl_70_cwhba1c_65_undiag <- data$DM_fgl_70_hba1c_65_cwfgl_70_cwhba1c_65_self
    data$undiagDM_fgl_70_hba1c_65_cwfgl_70_cwhba1c_65_undiag[which(data$DM_fgl_70_hba1c_65_cwfgl_70_cwhba1c_65_self == 1 & data$diagnosis == 1)] <- 0

    # 51. Undiagnosed, fasting glucose OR CWed fasting glucose >=7.0 mmol/L AND HbA1c or CWed HbA1c <6.5%
    data$DM_fgl_70_cwfgl_70_and_hba1c_l65_cwhba1c_l65_undiag <- NA
    data$DM_fgl_70_cwfgl_70_and_hba1c_l65_cwhba1c_l65_undiag[!is.na(data$DM_fgl_70_hba1c_65_cwfgl_70_cwhba1c_65_self)] <- 0
    list <- which(!is.na(data$DM_fgl_70_hba1c_65_cwfgl_70_cwhba1c_65_self) & data$diagnosis == 0 & data$hba1c_clean <6.5 & !is.na(data$predicted_fpg))
    data$DM_fgl_70_cwfgl_70_and_hba1c_l65_cwhba1c_l65_undiag[list] <- data$predicted_fpg[list]
    list <- which(!is.na(data$DM_fgl_70_hba1c_65_cwfgl_70_cwhba1c_65_self) & data$diagnosis == 0 & data$fgl_clean >=7 & !is.na(data$predicted_a1c))
    data$DM_fgl_70_cwfgl_70_and_hba1c_l65_cwhba1c_l65_undiag[list] <- 1 - data$predicted_a1c[list]
    list <- which(!is.na(data$DM_fgl_70_hba1c_65_cwfgl_70_cwhba1c_65_self) & data$diagnosis == 0 & data$fgl_clean >=7 & data$hba1c_clean <6.5)
    data$DM_fgl_70_cwfgl_70_and_hba1c_l65_cwhba1c_l65_undiag[list] <- 1

    # 51. Undiagnosed, fasting glucose OR CWed fasting glucose <7.0 mmol/L AND HbA1c or CWed HbA1c >=6.5%
    data$DM_fgl_l70_cwfgl_l70_and_hba1c_65_cwhba1c_65_undiag <- NA
    data$DM_fgl_l70_cwfgl_l70_and_hba1c_65_cwhba1c_65_undiag[!is.na(data$DM_fgl_70_hba1c_65_cwfgl_70_cwhba1c_65_self)] <- 0
    list <- which(!is.na(data$DM_fgl_70_hba1c_65_cwfgl_70_cwhba1c_65_self) & data$diagnosis == 0 & data$hba1c_clean >=6.5 & !is.na(data$predicted_fpg))
    data$DM_fgl_l70_cwfgl_l70_and_hba1c_65_cwhba1c_65_undiag[list] <- 1 - data$predicted_fpg[list]
    list <- which(!is.na(data$DM_fgl_70_hba1c_65_cwfgl_70_cwhba1c_65_self) & data$diagnosis == 0 & data$fgl_clean <7 & !is.na(data$predicted_a1c))
    data$DM_fgl_l70_cwfgl_l70_and_hba1c_65_cwhba1c_65_undiag[list] <- data$predicted_a1c[list]
    list <- which(!is.na(data$DM_fgl_70_hba1c_65_cwfgl_70_cwhba1c_65_self) & data$diagnosis == 0 & data$fgl_clean <7 & data$hba1c_clean >=6.5)
    data$DM_fgl_l70_cwfgl_l70_and_hba1c_65_cwhba1c_65_undiag[list] <- 1

    # 52. Undiagnosed, fasting glucose OR CWed fasting glucose >=7.0 mmol/L AND HbA1c or CWed HbA1c >=6.5%
    data$DM_fgl_70_cwfgl_70_and_hba1c_65_cwhba1c_65_undiag <- NA
    data$DM_fgl_70_cwfgl_70_and_hba1c_65_cwhba1c_65_undiag[!is.na(data$DM_fgl_70_hba1c_65_cwfgl_70_cwhba1c_65_self)] <- 0
    list <- which(!is.na(data$DM_fgl_70_hba1c_65_cwfgl_70_cwhba1c_65_self) & data$diagnosis == 0 & data$hba1c_clean >=6.5 & !is.na(data$predicted_fpg))
    data$DM_fgl_70_cwfgl_70_and_hba1c_65_cwhba1c_65_undiag[list] <- data$predicted_fpg[list]
    list <- which(!is.na(data$DM_fgl_70_hba1c_65_cwfgl_70_cwhba1c_65_self) & data$diagnosis == 0 & data$fgl_clean >=7 & !is.na(data$predicted_a1c))
    data$DM_fgl_70_cwfgl_70_and_hba1c_65_cwhba1c_65_undiag[list] <- data$predicted_a1c[list]
    list <- which(!is.na(data$DM_fgl_70_hba1c_65_cwfgl_70_cwhba1c_65_self) & data$diagnosis == 0 & data$fgl_clean >=7 & data$hba1c_clean >=6.5)
    data$DM_fgl_70_cwfgl_70_and_hba1c_65_cwhba1c_65_undiag[list] <- 1


    ## Newly diagnosed diabetes among those with diagnosis == 0

    # 54. Undiagnosed, fasting glucose >=7.0 mmol/L or HbA1c >=6.5%, among people with diagnosis == 0
    # use samplewt_glu because it is the more restrictive except in a small number of studies and none of them don't have special weights
    data$DM_fgl_70_hba1c_65_over_no_diag <- NA
    data$DM_fgl_70_hba1c_65_over_no_diag[which(data$diagnosis == 0 & !is.na(data$fgl_clean) & !is.na(data$hba1c_clean))] <- 0
    data$DM_fgl_70_hba1c_65_over_no_diag[which(data$DM_fgl_70_hba1c_65_over_no_diag == 0 & (data$fgl_clean >= 7 | data$hba1c_clean >= 6.5))] <- 1

    # 55. Undiagnosed, CWed fasting glucose >=7.0 mmol/L or HbA1c >=6.5%, among people with diagnosis == 0
    data$DM_cwfgl_70_hba1c_65_over_no_diag <- NA
    data$DM_cwfgl_70_hba1c_65_over_no_diag[which(data$diagnosis == 0 & !is.na(data$predicted_fpg) & !is.na(data$hba1c_clean))] <- 0
    data$DM_cwfgl_70_hba1c_65_over_no_diag[which(data$DM_cwfgl_70_hba1c_65_over_no_diag == 0 & data$hba1c_clean >= 6.5)] <- 1
    list <- which(data$DM_cwfgl_70_hba1c_65_over_no_diag == 0 & data$hba1c_clean < 6.5)
    data$DM_cwfgl_70_hba1c_65_over_no_diag[list] <- data$predicted_fpg[list]
    data$DM_cwfgl_70_hba1c_65_over_no_diag[!data$study_id %in% predict_fpg_studies] <- NA  # studies without diagnosis are not CWed

    # 56. Undiagnosed, fasting glucose >=7.0 mmol/L or CWed HbA1c >=6.5%, among people with diagnosis == 0
    data$DM_fgl_70_cwhba1c_65_over_no_diag <- NA
    data$DM_fgl_70_cwhba1c_65_over_no_diag[which(data$diagnosis == 0 & !is.na(data$predicted_a1c) & !is.na(data$fgl_clean))] <- 0
    data$DM_fgl_70_cwhba1c_65_over_no_diag[which(data$DM_fgl_70_cwhba1c_65_over_no_diag == 0 & data$fgl_clean >= 7)] <- 1
    list <- which(data$DM_fgl_70_cwhba1c_65_over_no_diag == 0 & data$fgl_clean < 7)
    data$DM_fgl_70_cwhba1c_65_over_no_diag[list] <- data$predicted_a1c[list]
    data$DM_fgl_70_cwhba1c_65_over_no_diag[!data$study_id %in% predict_a1c_studies] <- NA  # studies without diagnosis are not CWed

    # 57. Undiagnosed, fasting glucose or CWed fasting glucose >=7.0 mmol/L or HbA1c or CWed HbA1c >=6.5%, among people with diagnosis == 0
    data$DM_fgl_70_cwfgl_70_hba1c_65_cwhba1c_65_over_no_diag <- NA
    data$DM_fgl_70_cwfgl_70_hba1c_65_cwhba1c_65_over_no_diag[which(data$diagnosis == 0 & (!is.na(data$fgl_clean) | !is.na(data$predicted_fpg)) & (!is.na(data$hba1c_clean) | !is.na(data$predicted_a1c)))] <- 0
    data$DM_fgl_70_cwfgl_70_hba1c_65_cwhba1c_65_over_no_diag[which(data$DM_fgl_70_cwfgl_70_hba1c_65_cwhba1c_65_over_no_diag == 0 & (data$fgl_clean >= 7 | data$hba1c_clean >=6.5))] <- 1
    list <- which(data$DM_fgl_70_cwfgl_70_hba1c_65_cwhba1c_65_over_no_diag == 0 & !is.na(data$predicted_fpg))
    data$DM_fgl_70_cwfgl_70_hba1c_65_cwhba1c_65_over_no_diag[list] <- data$predicted_fpg[list]
    list <- which(data$DM_fgl_70_cwfgl_70_hba1c_65_cwhba1c_65_over_no_diag == 0 & !is.na(data$predicted_a1c))
    data$DM_fgl_70_cwfgl_70_hba1c_65_cwhba1c_65_over_no_diag[list] <- data$predicted_a1c[list]
    # remove values if a study does not have all of the components for this calculation to make sense
    data$DM_fgl_70_cwfgl_70_hba1c_65_cwhba1c_65_over_no_diag[!data$study_id %in% fpg_a1c_diag_studies] <- NA

    return(data)
}

## Get point estimates using mean predicted probability
## accounting for sampling design
summarise_data <- function(tmp) {
    if (length(unique(tmp$dsub))> 1) stop(paste('More than one sample design in study', paste(unique(tmp$study_id), collapse = ' ')))
    print(unique(as.character(tmp$study_id)))

    my_design <- switch(unique(tmp$dsub),
                        both = svydesign(ids = ~psu, strata = ~stratum, weights = ~samplewt, data = tmp, nest = TRUE),
                        psuonly = svydesign(ids = ~psu, strata = NULL, weights = ~samplewt, data = tmp),
                        stratonly = svydesign(ids = ~1, strata = ~stratum, weights = ~samplewt, data = tmp),
                        neither = svydesign(ids = ~1, strata = NULL, weights = ~samplewt, data = tmp)
    )
    prev_result <- reduce(lapply(lists, function(my_var) {
        res0 <- svyby(as.formula(paste0('~',my_var)), by = ~study_id + sex + age_group + age_mean, FUN = svymean, design = my_design, na.rm=TRUE)
        names(res0)[5:6] <- c('prev','se')
        res <- tmp %>%
            filter(!is.na(!!sym(my_var))) %>%
            group_by(study_id, sex, age_group, age_mean) %>%
            summarise(n = n()) %>%
            suppressMessages()
        res <- full_join(res, res0) %>% suppressMessages()

        res$prev[is.na(res$n)] <- NA
        res$se[is.na(res$n)] <- NA
        res$n[is.na(res$n)] <- 0

        names(res)[5:7] <- paste0(c('n_', 'prev_','se_'), my_var)
        return(res)
    }), full_join) %>% suppressMessages()

    ratio_result <- reduce(lapply(ratio_lists, function(my_var) {
        res0 <- svyby(as.formula(paste0('~',my_var)), by = ~study_id + sex + age_group + age_mean, denominator = as.formula(paste0('~', denom_lists[my_var])), FUN = svyratio, design = my_design, na.rm=TRUE)
        names(res0)[5:6] <- c('prev','se')
        ns <- prev_result %>%
            select(study_id, sex, age_group, age_mean, paste0(c('n_','prev_'), denom_lists[my_var]))
        names(ns)[5:6] <- c('prev', 'n')
        res <- ns %>%
            mutate(n = n * prev) %>%  # nominal sample size for ratios - n * prev of the denominator variable
            select(-prev) %>%
            full_join(res0) %>% suppressMessages()

        res$se[is.na(res$n)] <- NA

        # consistent with naming convention
        names(res)[5:7] <- paste0(c('n_', 'prev_','se_'), my_var)
        names(res) <- gsub('_DM_', '_ratio_', names(res))
        names(res) <- gsub('undiag$', 'over_undiagDM', names(res))

        return(res)
    }), full_join) %>% suppressMessages()

    result <- full_join(prev_result, ratio_result) %>% suppressMessages()
    return(result)
}

## Get uncertainty of prevalence using draws of predicted probability
## without accounting for PSU or stratum - only sampling variance is not included in the prediction using individual draw
summarise_data_draw <- function(d, ncore = 1) {
    if (ncore == 1) {
        registerDoSEQ()
    } else {
        cl = makeCluster(ncore, outfile = 'debug1.txt')
        registerDoParallel(cl)
    }
    summary0 <- rbindlist(foreach(draw = predicted_draws, .packages = c("dtplyr","tidyverse"),
                                  .export = c('lists', 'ratio_lists', 'denom_lists', 'fpg_a1c_diag_studies', 'predict_fpg_studies', 'predict_a1c_studies', 'define_DM_status')
    ) %dopar% {
        d$predicted_fpg <- d$predicted_a1c <- NA
        d$predicted_fpg <- draw$fpg[d$draw_id]
        d$predicted_a1c <- draw$a1c[d$draw_id]

        d <- define_DM_status(d)
        dt <- lazy_dt(d)
        prev <- dt %>%
            group_by(study_id, sex, age_mean, age_group) %>%
            summarise_at(all_of(lists), .funs = funs(weighted.mean(., samplewt, na.rm=TRUE))) %>%
            as_tibble()
        n <- dt %>%
            group_by(study_id, sex, age_mean, age_group) %>%
            summarise_at(all_of(lists), .funs = funs(sum(.>0, na.rm=TRUE))) %>%
            as_tibble()
        res <- prev %>% mutate(metric = 'prev') %>%
            bind_rows(
                n %>% mutate(metric = 'n')
            ) %>%
            gather(variable, value, all_of(lists)) %>%
            spread(metric, value)

        ratio_res <- do.call(rbind, lapply(ratio_lists, function(ratio) {
            tmp <- res %>% filter(variable == ratio)
            tmp$prev <- tmp$prev / res$prev[res$variable == denom_lists[ratio]]
            tmp$variable = gsub('undiag', 'over_undiagDM', paste0('ratio_', ratio))
            tmp$n <- res$prev[res$variable == denom_lists[ratio]] * res$n[res$variable == denom_lists[ratio]]
            return(tmp)
        }))

        res <- rbind(res, ratio_res) %>% filter(!is.na(prev))
        res$draw <- draw$draw_num
        return(res)

    }, fill = TRUE)

    if (ncore > 1) stopCluster(cl)

    return(summary0)
}
