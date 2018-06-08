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

## 2. MISSING VALUES IMPUTATION
# 2.i) target
any(is.na(datta$T_TARGET)) 
  # - no need

# 2.ii) IDs
any(is.na(datta$ID_SK_ID_CURR)) 
# - no need

# 2.iii) INDICATORs
any(sapply(dplyr::select(datta, starts_with("I_")), function(x) sum(is.na(x))))
# - no need

# 2.iv) QUANTITATIVE
any(sapply(dplyr::select(datta, starts_with("Q_")), function(x) sum(is.na(x))))
datta <- impute(datta, varType = 'Q_', values = list(0, "mean"), keepOriginal = TRUE)

# 2.v) CATEGORICAL NOMINAL
any(sapply(dplyr::select(datta, starts_with("CN_")), function(x) sum(is.na(x))))
  # - no need

# 2.v) CATEGORICAL ORDINAL
any(sapply(dplyr::select(datta, starts_with("CQ_")), function(x) sum(is.na(x))))
  # - no need

## 3. OUTLIERS INDICATION
datta <- indicateOutliers(datta)

## 4. RARE-LEVEL INDICATION
# TODO: RADO

## 5. SAVE
write.table(datta, file = "./sanitized_data/s_application_train.csv", sep = ",")
