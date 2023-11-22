
library(tidyverse)

source("/Volumes/HeightProject/Templates/Code-standardisation/R/data-handling/cleaning_functions.R")

d <- readRDS("/Volumes/HeightProject/Original dataset/Data/Surveys/Extracted survey/Merged/individual_extracted_latest.RDS")
steps <- readRDS("/Volumes/HeightProject/Original dataset/Data/Surveys/STEPS/STEPSdata_GLU_BP_chol_formatted_latest.RDS")
dhs <- readRDS("/Volumes/HeightProject/Original dataset/Data/Surveys/DHS/DHS-formatted_latest.RDS")

for (v in c('psu','stratum','edu','birth_y','birth_m','is_pregnant','is_urban'))
    steps[[v]] <- as.numeric(steps[[v]])

data <- bind_rows(steps, dhs) %>% select(
    id_study, survey_type, mid_year, age, sex,
    contains('height'), contains('weight'), contains('waist'), contains('hip'),
    fgl, unit_gl, hba1c, unit_hba1c,
    tc, unit_tc, hdl, unit_hdl, ldl, unit_ldl, trg, unit_trg,
    starts_with('sbp'), starts_with('dbp'), unit_bp,
    is_pregnant, is_plasma
)
data <- d %>% select(
    id_study, survey_type, mid_year, age, sex,
    contains('height'), contains('weight'), contains('waist'), contains('hip'),
    fgl, unit_gl, hba1c, unit_hba1c,
    tc, unit_tc, hdl, unit_hdl, ldl, unit_ldl, trg, unit_trg,
    starts_with('sbp'), starts_with('dbp'), unit_bp,
    is_pregnant, is_plasma
) %>% bind_rows(data) %>%
    as_tibble()

for (v in c('sex','age','is_pregnant','is_plasma')) {
    data[[v]] <- clean_data(data, v)
}

for (v in setdiff(names(data), c('id_study','survey_type','mid_year','age','sex','is_pregnant','is_plasma'))) {
    if (grepl('unit', v)) next
    data[[paste0(v, '_clean')]] <- clean_data(data, v)
}

dd <- data %>% filter(survey_type == 'National')


