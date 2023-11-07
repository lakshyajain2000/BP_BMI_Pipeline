### 11 February 2015
### Mariachiara Di Cesare, Bin Zhou (with Vasilis' help)
### this script reads all the files in the folder and rbind them based on column names
### 2018 11 30 Honor Bixby added line to check for files missing id_study.
## 20181203, Bin Zhou added variable names/class checks
## 20190627, Bin Zhou added last modified date of the csvs to the data frame
## 20200206, Bin added merging log
## 20211104, Bin added correction of id variable
## 20220107. Bin added correction of id when duplicates exist within study
## 20221102  Rosie added encoding to read.csv as other wise getting error trying to read micro symbol in units
## 20230606  Rosie removed encoding to read.csv
## 20230607 Rosie added lists of variables not to flag in merging log


library(plyr)
library(data.table)
date <- format(Sys.Date(),"%Y %m %d")

sink(paste0("S:/Projects/HeightProject/Original dataset/Data/Surveys/Extracted survey/Merged/Merging log_",date,".txt"))

setwd("S:/Projects/HeightProject/Original dataset/Data/Surveys/Extracted survey/merged/")
outdir <- "S:/Projects/HeightProject/Original dataset/Data/Surveys/Extracted survey/merged/"

#### Merge individual surveys ####
filenames <- list.files("S:/Projects/HeightProject/Original dataset/Data/Surveys/Extracted survey/", pattern="*.csv", full.names=TRUE)

std_names_list <- read.csv("S:/Projects/HeightProject/Original dataset/Data/Surveys/__Extraction Template/standard_variable_names.csv")

mod_time_list <- data.frame()

duplists <- c()
datalist <- lapply(filenames, function(file) {
         temp_dataset <- read.csv(file, na.strings = c(".", "", "NA"))
		 #check for files missing id_study variable
		 if(sum(names(temp_dataset)=='id_study')==0) {
		     message(file)
		     stop("File missing id_study: check extraction!!")
		 }
         if(sum(is.na(temp_dataset$id_study))!=0) {
		     message(file)
		     stop("File has missing in id_study: check extraction!!")
		 }
         # remove variables with completely NA values to avoid erroneous behavior in rbind.fill()
         namesNA <- names(which( sapply(temp_dataset,function(x)all(is.na(x))) ))
         temp_dataset <- temp_dataset[ !names(temp_dataset) %in% namesNA ]
         temp_dataset$X<-NULL; temp_dataset$X_merge<-NULL

         # rewrite id to 1:nrow when there are duplicates in the raw data
         # convert id variable to characters to avoid coercing into numerical
         if ('id' %in% names(temp_dataset)) {
            temp_dataset$id <- as.character(temp_dataset$id)

            check_list <- aggregate(temp_dataset$id, list(temp_dataset$id_study), function(x) any(duplicated(x)))
            dup_list <- check_list[check_list[,2], 1]
            if (length(dup_list)>0) {
                duplists <<- c(duplists, dup_list)
                for (study in dup_list) temp_dataset$id[temp_dataset$id_study == study] <- as.character(1:sum(temp_dataset$id_study == study))
            }
         }

         # add time last modified
         temp_dataset$mod_time <- file.mtime(file)
         mod_time_list <<- rbind(mod_time_list, data.frame(id_study = unique(temp_dataset$id_study),mod_time = file.mtime(file))) # "<<-" writes in outer space global variable

         return(temp_dataset)
    })
print('ID variable regenerated for the following studies due to duplicated ID exsiting in original data')
print(paste(duplists, collapse = ' '))

dataset <- do.call(rbind.fill, datalist)
write.csv(mod_time_list, paste0(outdir, "Time_modified_for_latest_csvs.csv"), row.names=FALSE)
write.csv(mod_time_list, paste0(outdir, "old/Time_modified_for_latest_csvs_",date,".csv"), row.names=FALSE)

## ARM 30/04/19 ## If not enough RAM
# datalist1 <- datalist[1:(length(datalist)/2)]
# datalist2 <- datalist[(1 + length(datalist)/2): length(datalist)]
# remove(datalist)
# dataset1 <- do.call(rbind.fill, datalist1)
# remove(datalist1)
# dataset2 <- do.call(rbind.fill, datalist2)
# remove(datalist2)
# dataset <- rbind.fill(dataset1, dataset2)

## Check for duplicated study IDs
ids <- lapply(datalist, function(dat) unique(as.character(dat$id_study)))
id <- unlist(ids)
if (sum(duplicated(id))>0) {
    print(id[duplicated(id)])
    stop("Check merging log for duplicated study IDs!!")
} else {
    print("No duplicated study IDs found")
}


## Check for numeric variables that are extracted as non-numeric types (eg factors)
print("Check if numerical variables are in non-numeric formats:")
numeric_var_list <- as.character(std_names_list$Name[which(std_names_list$Type=="numeric")])
var_class_check <- lapply(datalist, function(dat) {
                        classes <- lapply(dat[, names(dat)%in% numeric_var_list], class)
                        non_numeric_list <- names(classes[!classes%in% c("numeric", "integer")])
                        if (length(non_numeric_list)>0) {
                            print(unique(as.character(dat$id_study)))
                            print(non_numeric_list)
                        }
                        return(non_numeric_list)
                    })

# drop unused columns to save memory 20170402
dataset$zero1 <- dataset$zero2 <- dataset$zero3 <- NULL
dataset$age_range_bp_M <- dataset$age_range_bp_F <- NULL
dataset$pack_year <- dataset$pack_years <- NULL
dataset$post_strat <- dataset$proxy_resp <- NULL
dataset$cig_per_day <- dataset$smoke_num_curr_other <- NULL
dataset$passive_smoke <- dataset$smoke_100 <- dataset$smoke_age <- NULL

# Add whole survey exclusion indicator
dataset$excluded <- 0

#### Correction of data and variable names ####
setnames(dataset,"ha1c","hba1c")
setnames(dataset,"unit_ha1c","unit_hba1c")
setnames(dataset,"device_ha1c","device_hba1c")

#### Assign sample weight for ppg, hba1c, trg and ldl if current value is NA ####
## TO CORRECT 12 JULY 2021
ppg_wtlist   <- which(is.na(dataset$samplewt_ppg)&!(dataset$survey_short=="NHANES"&dataset$iso=="USA"))
hba1c_wtlist <- which(is.na(dataset$samplewt_hba1c)&!(dataset$survey_short=="NHANES"&dataset$iso=="USA"))
trg_wtlist   <- which(is.na(dataset$samplewt_trg)&!(dataset$survey_short=="NHANES"&dataset$iso=="USA"))
ldl_wtlist   <- which(is.na(dataset$samplewt_ldl)&!(dataset$survey_short=="NHANES"&dataset$iso=="USA"))
dataset$samplewt_ppg[ppg_wtlist]     <- dataset$samplewt_glu[ppg_wtlist]
dataset$samplewt_hba1c[hba1c_wtlist] <- dataset$samplewt_glu[hba1c_wtlist]
dataset$samplewt_trg[trg_wtlist]     <- dataset$samplewt_chol[trg_wtlist]
dataset$samplewt_ldl[ldl_wtlist]     <- dataset$samplewt_chol[ldl_wtlist]

## unit_gl in DHS are all blank instead of NA

#### Standardise factor levels for unit variables ####
# standardise mg% and mg/dl to mg/dL
dataset$unit_gl [which(dataset$unit_gl=="mg%"|dataset$unit_gl=="mg/dl")]   <- "mg/dL"
dataset$unit_ppg[which(dataset$unit_ppg=="mg%"|dataset$unit_ppg=="mg/dl")] <- "mg/dL"
dataset$unit_tc [which(dataset$unit_tc=="mg%"|dataset$unit_tc=="mg/dl")]   <- "mg/dL"
dataset$unit_hdl[which(dataset$unit_hdl=="mg%"|dataset$unit_hdl=="mg/dl")] <- "mg/dL"
dataset$unit_ldl[which(dataset$unit_ldl=="mg%"|dataset$unit_ldl=="mg/dl")] <- "mg/dL"
dataset$unit_trg[which(dataset$unit_trg=="mg%"|dataset$unit_trg=="mg/dl")] <- "mg/dL"
# standardise mmol/l to mmol/L, added 9 Feb 2015
dataset$unit_gl [which(dataset$unit_gl=="mmol/l")]   <- "mmol/L"
dataset$unit_ppg[which(dataset$unit_ppg=="mmol/l")]   <- "mmol/L"
dataset$unit_tc [which(dataset$unit_tc=="mmol/l")]   <- "mmol/L"
dataset$unit_hdl[which(dataset$unit_hdl=="mmol/l")]   <- "mmol/L"
dataset$unit_ldl[which(dataset$unit_ldl=="mmol/l")]   <- "mmol/L"
dataset$unit_trg[which(dataset$unit_trg=="mmol/l")]   <- "mmol/L"
# add "EXCLUDE" as a new level
dataset$unit_height <- factor(dataset$unit_height, levels=c("cm","EXCLUDE"))
dataset$unit_weight <- factor(dataset$unit_weight, levels=c("kg","EXCLUDE"))
dataset$unit_waist  <- factor(dataset$unit_waist, levels=c("cm","EXCLUDE"))
dataset$unit_hip    <- factor(dataset$unit_hip, levels=c("cm","EXCLUDE"))
dataset$unit_gl     <- factor(dataset$unit_gl, levels=c("mmol/L","mg/dL","EXCLUDE"))
dataset$unit_ppg    <- factor(dataset$unit_ppg, levels=c("mmol/L","mg/dL","EXCLUDE"))
dataset$unit_hba1c  <- factor(dataset$unit_hba1c, levels=c("%","mmol/mol","EXCLUDE"))
dataset$unit_tc     <- factor(dataset$unit_tc, levels=c("mmol/L","mg/dL","EXCLUDE","SPECIAL"))
dataset$unit_hdl    <- factor(dataset$unit_hdl, levels=c("mmol/L","mg/dL","EXCLUDE","SPECIAL"))
dataset$unit_ldl    <- factor(dataset$unit_ldl, levels=c("mmol/L","mg/dL","EXCLUDE","SPECIAL"))
dataset$unit_trg    <- factor(dataset$unit_trg, levels=c("mmol/L","mg/dL","EXCLUDE","SPECIAL"))
# add new variable unit_bp
dataset$unit_bp     <- factor("mmHg", levels=c("mmHg","EXCLUDE"))

#### Mark Exclusions ####
#save(dataset,file=paste(outdir,'individual_extracted_beforeE', date,'.RData',sep=''))
excl_list_gl    <- c("GBR_1999_HSE","GBR_2004_HSE","MEX_2000_ENSA","PAK_1992_NHSP","MOZ_2005_STEPS","MEX_1993_ENEC","IRL_2007_SLAN","BRA_2015_EPIFLORIPA","NLD_1993_LASA","NLD_2003_LASA","ZAF_2015_HAALSI", "MEX_2012_ENSANUT","TUN_2016_THES","ITA_2002_ASTI","IRN_2011_YES")
excl_list_hba1c <- c("ITA_1993_ILSA","ITA_1996_ILSA","ITA_2001_ILSA","MUS_2009_MNCS","KOR_1998_KNHANES","KOR_2001_KNHANES","GBR_1993_HSE","GBR_1994_HSE","KOR_2005_KNHANES","KOR_2007_KNHANES","KOR_2008_KNHANES","KOR_2009_KNHANES","KOR_2010_KNHANES","ITA_1990_BRUNECKstudy","ITA_1995_BRUNECKstudy","ITA_1997_PROVA","USA_1991_NHANES","IND_2001_NDBC","NRU_1994_NIDDM","FRA_2000_3C","CHN_2011_BES","CHL_2010_ENS","GBR_1999_NSHD","GBR_1999_BRHS", "GBR_2000_HSE", "FIN_1997_Oulu35", "MEX_2006_ENSANUT","DEU_1999_SHIP","ITA_2002_ASTI","CHN_2016_Henan","JPN_1988_HiS")
special_list_hba1c <- c("MEX_2012_ENSANUT","JAM_2008_JHLS","IRN_2011_YES")  # measured among diabetics only
excl_list_ppg   <- c("MEX_2000_ENSA","TUR_1990_TARF","TUR_1998_TARF","TUR_2000_TARF","TUR_2013_TARF","TWN_1995_NAHSIT","IND_2007_CIEMS","IND_1989_citRamachandran","IND_1995_citRamachandran","IND_1997_citRamachandran","IND_2000_citRamachandran","USA_1961_NHES","CMR_1999_ENHIP_urban","CMR_1999_ENHIP_rural", "FIN_1991_Oulu35", "FIN_1997_Oulu35","DEU_2010_SHIPTREND")
excl_list_bp    <- c("BRA_2009_MetS", "USA_2006_HRS", "USA_2008_HRS", "USA_2011_HRS", "USA_2012_HRS", "USA_2014_HRS", "USA_2017_HRS", "USA_2006_NSHAP", "USA_2011_NSHAP", "USA_2016_NSHAP")
## HRS and NSHAP temporarily excluded: clarifying with survey contact about unusually high DBP levels 20200801/20210125
# excl_list_anthro <- c("")

# dataset$unit_height/weight <- "EXCLUDE_FOR_TREND"

dataset$unit_gl   [ which(dataset$id_study %in% excl_list_gl)]    <- "EXCLUDE"
dataset$unit_hba1c[ which(dataset$id_study %in% c(excl_list_hba1c,special_list_hba1c))] <- "EXCLUDE"
dataset$unit_ppg  [ which(dataset$id_study %in% excl_list_ppg)]   <- "EXCLUDE"
dataset$unit_bp   [ which(dataset$id_study %in% excl_list_bp)]    <- "EXCLUDE"
# dataset$unit_bp   [ which(dataset$id_study %in% excl_list_bp)]    <- "EXCLUDE"

excl_list_chol  <- c("MOZ_2005_STEPS", "IRN_2011_YES")
excl_list_ldl   <- c(excl_list_chol)
excl_list_trg   <- c(excl_list_ldl) #add if only trg to be excluded
#cretae list for hdl
spcl_list_chol  <- c()  # calculate prevalence only, but not mean; "BRA_2005_HOB" and "PAK_1992_NHSP" removed after discussion 08012015

dataset$unit_tc   [ which(dataset$id_study %in% excl_list_chol)]  <- "EXCLUDE"
dataset$unit_hdl  [ which(dataset$id_study %in% excl_list_chol)]  <- "EXCLUDE"
dataset$unit_ldl  [ which(dataset$id_study %in% excl_list_ldl)]   <- "EXCLUDE"
dataset$unit_trg  [ which(dataset$id_study %in% excl_list_trg)]   <- "EXCLUDE"
# dataset$unit_tc   [ which(dataset$id_study %in% spcl_list_chol)]  <- "SPECIAL"
# dataset$unit_hdl  [ which(dataset$id_study %in% spcl_list_chol)]  <- "SPECIAL"

# dataset <- data.frame(dataset)

#### Output ####
saveRDS(dataset, file = paste0(outdir,'individual_extracted_latest.RDS'))
file.copy(from = paste0(outdir,'individual_extracted_latest.RDS'), to = paste0(outdir,'old/individual_extracted_', date,'.RDS'), overwrite = TRUE)

# check consistency of id_study with its components
print("inconsistent iso")
unique(dataset$id_study[dataset$iso!=substr(as.character(dataset$id_study),1,3)])
print("inconsistent mid_year")
unique(dataset$id_study[dataset$mid_year!=as.numeric(substr(as.character(dataset$id_study),5,8))])
print("inconsistent survey_short")
unique(dataset$id_study[dataset$survey_short!=substr(as.character(dataset$id_study),10,nchar(as.character(dataset$id_study)))])

# check id_study in exclusion lists
print("inconsistent id_study in exclusion lists")
lists <- c(excl_list_gl,excl_list_hba1c,excl_list_ppg,excl_list_bp,excl_list_chol,excl_list_ldl,excl_list_trg)
unique(lists[!lists%in%dataset$id_study])

# check if survey_type and urban_rural are coded correctly
print("non-standard coding for survey_type")
unique(dataset$id_study[!dataset$survey_type %in% c("National","Subnational","Community","mixed","Mixed")])
print("survey_type coded as mixed but have not been checked")
known_mixed <- c("CHN_2008_CLHLS", "CHN_2012_CLHLS", "CHN_2014_CLHLS", "CHN_2018_CLHLS", "IRN_2017_IRCAP", "CAN_1996_CaMos")
setdiff(unique(dataset$id_study[dataset$survey_type %in% c("mixed","Mixed")]), known_mixed)
print("non-standard coding for urban_rural")
unique(dataset$id_study[!dataset$urban_rural %in% c("urban","rural","both","mixed","Mixed")])
print("urban_rural coded as mixed but have not been checked")
known_mixed <- c("COL_2010_STEPS")
setdiff(unique(dataset$id_study[dataset$urban_rural %in% c("mixed","Mixed")]), known_mixed)

# check if there are duplicated variable names
print("Checking if any measurement variables has '.1' in names, indicating duplicated columns in extraction")
for (v in c("bmi","height","weight","waist","hip","sbp","dbp","tc","ldl","hdl","trg","fgl","ppg","hba1c","ha1c")) {
    grep(v, names(dataset), value= TRUE)
}

# check if any non-standard variable names
ignore_list <- c("self_hyper_gp", "self_cvd_angina", "self_cvd_mi", "self_cvd_ihd" ,"self_cvd_stroke",  "smokeless_curr", "ICD_codes1",  "ICD_codes2", "ICD", "ICD_codes3", "other_ATC",  "drug_diab_class",  "drink_freq",  "drug_cvd", "drink_num_curr", "smoke_num_curr", "drug_hyper_12mos", "self_hyper_new12mos", "doctor_visit_12mos", "meas_bp_12mos","drug_hyper_type")
other_measure_list <- c( "apolipoproteina", "unit_apolipoproteina", "apolipoproteinb", "unit_apolipoproteinb", "proteinuria_category", "urea", "unit_urea")

no_flag <-c(ignore_list, other_measure_list)

nonstd_name <- setdiff(names(dataset)[!names(dataset)%in% std_names_list$Name & !names(dataset) %in% no_flag], "mod_time")
if (length(nonstd_name)==0) print("No non-standard variable names found") else {
    for (i in nonstd_name) {
        print(i)
        print(unique(dataset$id_study[!is.na(dataset[,i])]))
    }
}

# check missingness in sex
print("Unique values in sex variable")
table(dataset$sex, exclude=NULL)
sex.na <- as.character(unique(dataset$id_study[which(is.na(dataset$sex))])) # all studies with at least one particpant with sex = NA
sex.n <- ddply(dataset ,.(id_study), function(tmp) length(unique(tmp$sex)))
print("number of unique values in all studies with NA: problem if any with <3 values")
table(sex.n[which(sex.n$id_study%in%sex.na),2] )
print(sex.n[which(sex.n$id_study%in%sex.na),][sex.n[which(sex.n$id_study%in%sex.na),2]<3,1])
print("SLV_2013_GSHS checked: OK")
#check_sex <- ddply(dataset,.(id_study), function(x) sum( is.na(x) )) # check no survey has only NA values for sex
sink()
