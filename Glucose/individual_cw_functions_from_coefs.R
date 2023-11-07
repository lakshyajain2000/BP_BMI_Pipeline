invlogit <- arm::invlogit
rmvnorm <- mvtnorm::rmvnorm

transform_data <- function(d) {
    # d$Superregion <- countrylist$Superregion[match(d$iso, countrylist$iso)]
    d$Superregion <- "Southeast Asia, East Asia and the Pacific"
    d$age.var <- (d$age - 50) / 15
    d$a1c.var <- (d$hba1c_clean - 5.5) / 0.7
    d$fpg.var <- (d$fgl_clean - 5.5) / 1
    d$bmi.var <- (d$bmi_clean - 26.5) / 5
    d$year.var <- (d$mid_year - 2011) / 5
    d$Superregion <- factor(d$Superregion, levels = srs)
    d$sex <- factor(d$sex, levels = c('female', 'male'))

    d$handheld_a1c_cat <- ifelse(is.na(d$handheld_a1c), NA, ifelse(d$handheld_a1c == 1, 'Handheld', 'Lab'))
    d$handheld_a1c_cat <- factor(d$handheld_a1c_cat, levels = c('Lab', 'Handheld'))
    d$handheld_fpg_cat <- ifelse(is.na(d$handheld_fpg), NA, ifelse(d$handheld_fpg == 1, 'Handheld', 'Lab'))
    d$handheld_fpg_cat <- factor(d$handheld_fpg_cat, levels = c('Lab', 'Handheld'))

    # d$study_id <- 'NA'  # use the study random effect to predict the missing data for studies used to fit the CW models
    return(d[,c('sex','age.var','fpg.var','a1c.var','year.var','Superregion','handheld_fpg_cat','handheld_a1c_cat','study_id','bmi.var')])
}


cw_predict <- function(dat, studies, mod_obj, N = 5000, seed = 2022) {
    # Fixed effect
    if (N > 5000) stop('Cannot simulate for more than 5000 iterations')
    fixef <- mod_obj$fixefs[1:N,]
    boot <- fixef %*% t(as.matrix(dat))
    unique_studies <- unique(studies)

    set.seed(seed)

    # Study RE - individuals from the same study get the same REs
    for (i in 1:length(unique_studies)) {
        ranef_i <- rnorm(n = N, mean = 0, sd = mod_obj$ranef_sd)
        boot[,studies == unique_studies[i]] <- boot[,studies == unique_studies[i]] + ranef_i
    }

    boot <- t(invlogit(boot))

    return(boot)
}
