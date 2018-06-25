# ====================================================================================== #
# 
#                             *** DATA SANITIZATION ****
# 
#
# Author(s): Rado,
#
# --- GOAL ---
# The purpose here is to clean, cleanse, wrangle, etc. data into a form that are ready
# for the next phases (aggreation on the master file level => feature generation).
#
# --- INPUTS---
# - application_train.csv 
# - application_test.csv 
# - bureau.csv
# - bureau_balance.csv 
# - previous_application.csv
# - POS_CASH_balance.csv
# - instalments_payment.csv
# - credit_card_balance.csv
#
# --- OUTPUTS ---
# As a general remark, please put "s_" prefix to the output file, to indicate that it is 
# a "sanitized" file, e.g. "s_application_tran.csv".
# 
# ====================================================================================== #
rm(list = ls())

source("_utils.R")
library(dplyr)
library(stringr)

# ----------------------------------------------------------------------------------------
#                                  GLOBAL SETTINGS
# ----------------------------------------------------------------------------------------
maxLevels <- 25 # maximum number of level that a categorical variable can have.


# ----------------------------------------------------------------------------------------
#                                  application_train.csv
# ----------------------------------------------------------------------------------------
datta <- read.csv("..//01_raw_data/application_train.csv", header = T)

## 1. add varible name pre-fixed according to variable types (see ../00_organisation/README_variable_types.txt)
ids           <- c('SK_ID_CURR')
target        <- c('TARGET')
dates         <- NULL
indicators    <- unique(c(colnames(datta)[grepl("FLAG", colnames(datta))]
                 , colnames(datta)[grepl("REG_REGION", colnames(datta))]
                 , colnames(datta)[grepl("LIVE_REGION", colnames(datta))]
                 , colnames(datta)[grepl("REG_CITY", colnames(datta))]
                 , colnames(datta)[grepl("LIVE_CITY", colnames(datta))]
                 ))

categoricalSuggested  <- suggestCategorical(datta, maxLevels)
categoricalSuggested  <- categoricalSuggested[!(categoricalSuggested %in% c(target, indicators))] 
print(categoricalSuggested)

categoricalNominal <- c('NAME_CONTRACT_TYPE'
                        , 'CODE_GENDER'
                        , 'NAME_TYPE_SUITE'
                        , 'NAME_INCOME_TYPE'
                        , 'NAME_EDUCATION_TYPE'
                        , 'NAME_FAMILY_STATUS'
                        , 'NAME_HOUSING_TYPE'
                        , 'OCCUPATION_TYPE'
                        , 'ORGANIZATION_TYPE'
                        , 'FONDKAPREMONT_MODE'
                        , 'HOUSETYPE_MODE'
                        , 'WALLSMATERIAL_MODE'
                        , 'EMERGENCYSTATE_MODE'
                        , '')

categoricalOrdinal <- c('WEEKDAY_APPR_PROCESS_START'
                        , 'REGION_RATING_CLIENT'
                        , 'REGION_RATING_CLIENT_W_CITY'
                        , 'HOUR_APPR_PROCESS_START'
                        , 'AMT_REQ_CREDIT_BUREAU_HOUR'
                        , 'AMT_REQ_CREDIT_BUREAU_DAY'
                        , 'AMT_REQ_CREDIT_BUREAU_WEEK'
                        , 'AMT_REQ_CREDIT_BUREAU_QRT')

quantitative  <- colnames(datta)[!(colnames(datta) %in% c(ids, target, indicators, dates, categoricalNominal, categoricalOrdinal))]

check <- c(ids, target,dates,indicators,quantitative,categoricalNominal,categoricalOrdinal)
check <- as.data.frame(table(check)) %>% dplyr::arrange(desc(Freq))

datta <- extendColumnNamesByTypes(datta
                                  , ids = ids
                                  , target = target
                                  , dates = dates
                                  , indicators = indicators
                                  , quantitative = quantitative
                                  , categoricalNominal = categoricalNominal
                                  , categoricalOrdinal = categoricalOrdinal)
## Economic-sense-based:
# i.) Day cannot be < 0 and larger than 100 years => 36500 days.
daysCols <- colnames(dplyr::select(datta, contains('DAYS')))
for(varName in daysCols){
  datta[which(datta[,varName] > 365*100),varName] <- NA
  datta[which(datta[,varName] < -365*100),varName] <- NA
}

## Technical:
# Get rid of commas in the level-names. SM binning cannot work when commas in the names
for(var in colnames(dplyr::select(datta, starts_with('CN_'), starts_with('CO_')))){
  print(paste0(Sys.time(),"Sanitizing level-names for: ", var, "..."))
  datta[,var] <- stringr::str_replace_all(as.character(datta[,var])," ","_")
  datta[,var] <- stringr::str_replace_all(as.character(datta[,var]),",","")
}

## 2. MISSING VALUES IMPUTATION
# 2.i) target
any(is.na(datta$T_TARGET)) 
  # - no need

# 2.ii) IDs
any(is.na(datta$ID_SK_ID_CURR)) 
# - no need

# 2.iii) INDICATORs
which(sapply(dplyr::select(datta, starts_with("I_")), function(x) any(is.na(x))) == TRUE)
# - no need

# 2.iv) QUANTITATIVE
which(sapply(dplyr::select(datta, starts_with("Q_")), function(x) any(is.na(x))) == TRUE)
datta <- impute(datta, varType = 'Q_', values = list(0, "mean"), keepOriginal = FALSE)

# 2.v) CATEGORICAL NOMINAL
which(sapply(dplyr::select(datta, starts_with("CN_")), function(x) any(is.na(x))) == TRUE)
  # - no need

# 2.v) CATEGORICAL ORDINAL
which(sapply(dplyr::select(datta, starts_with("CO_")), function(x) any(is.na(x))) == TRUE)
datta <- impute(datta, varType = 'CO_', values = list("missing"), keepOriginal = FALSE)

# final check
any(sapply(datta, function(x) is.na(x)))

## 3.i OUTLIERS INDICATION
datta <- indicateOutliers(datta)

## 3.ii CENSORING
# outlier information is already captured in dedicated outlier indicator columns
# , now distribution are censored such that all observations fall between 3 STD
# -> to improve binning algos later on
outlier_indicators <- colnames(dplyr::select(datta, ends_with('_Out')))
for(oi in outlier_indicators){
  if(any(datta[,oi] == 1)){
    original_var <- substr(oi,3,nchar(oi))
    original_var <- substr(original_var,1,nchar(original_var)-4)
    print(paste0('Generating trimmed version of: ',original_var,'...'))
    
    minn <- min(datta[datta[,oi] == 0,original_var], na.rm = T)
    maxx <- max(datta[datta[,oi] == 0,original_var], na.rm = T)
    
    #create 'trimmed' variable
    new_namee <- paste0(original_var, '_Trimmed')
    datta[,new_namee] <- datta[,original_var]
    if(any((datta[,oi]==1 & scale(datta[, original_var]) < 0))){
      datta[(datta[,oi]==1 & scale(datta[, original_var]) < 0) ,new_namee]  <- minn  
    }
    if(any((datta[,oi]==1 & scale(datta[, original_var]) >= 0))){
      datta[(datta[,oi]==1 & scale(datta[, original_var]) >= 0) ,new_namee] <- maxx
    }
    
    # drop original variable with outliers
    datta[,original_var] <- NULL
  }
}
## Checking for quantitative variables with only few levels
# Some variables may seem quantitative, but in fact, only a couple of distinct values exist -> cast to Categorical (otherwise problems in binning)
qnames <- colnames(dplyr::select(datta, starts_with("Q_")))
summary <- data.frame()
for(varName in qnames) summary <- rbind(summary, data.frame(varName = varName, uniqueValues = length(unique(datta[,varName]))))
summary <- dplyr::arrange(summary, uniqueValues)
View(dplyr::filter(summary, uniqueValues <= maxLevels))
# turn the low-unique-values one into categorical nominal
# TODO: RADO:This should be done better, some should actually be ordinal...
toBeCat <- dplyr::filter(summary, uniqueValues <= maxLevels)[,1]
colnames(datta)[colnames(datta) %in% toBeCat] <- gsub('Q_','CN_',toBeCat)

## 4. RARE-LEVEL INDICATION
# TODO: RADO

## 5. SAVE
write.table(datta, file = "./sanitized_data/s_application_train.csv", sep = ",")
