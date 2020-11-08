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

sanitizeApplicationData <- function(inputFile, outputFile, maxLevels = 25, scope = 'train'){
  datta <- read.csv(inputFile, header = T)
  
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
    print(paste0(Sys.time()," Sanitizing level-names for: ", var, "..."))
    datta[,var] <- stringr::str_replace_all(as.character(datta[,var])," ","_")
    datta[,var] <- stringr::str_replace_all(as.character(datta[,var]),",","")
  }
  
  ## 2. MISSING VALUES IMPUTATION
  # 2.i) target
  if(scope == 'train'){
    if(length(which(sapply(dplyr::select(datta, starts_with("T_")), function(x) any(is.na(x))) == TRUE)) > 0)
      datta <- impute(datta, varType = 'T_', values = list(0), keepOriginal = FALSE)  
  }
  
  # 2.ii) IDs
  if(length(which(sapply(dplyr::select(datta, starts_with("ID_")), function(x) any(is.na(x))) == TRUE)) > 0)
    datta <- impute(datta, varType = 'T_', values = list(0), keepOriginal = FALSE)  

  # 2.iii) INDICATORs
  if(length(which(sapply(dplyr::select(datta, starts_with("I_")), function(x) any(is.na(x))) == TRUE)) > 0)
    datta <- impute(datta, varType = 'I_', values = list(0), keepOriginal = FALSE)
  
  # 2.iv) QUANTITATIVE
  if(length(which(sapply(dplyr::select(datta, starts_with("Q_")), function(x) any(is.na(x))) == TRUE)) > 0)
    datta <- impute(datta, varType = 'Q_', values = list(0, "mean"), keepOriginal = FALSE)
  
  # 2.v) CATEGORICAL NOMINAL
  if(length(which(sapply(dplyr::select(datta, starts_with("CN_")), function(x) any(is.na(x))) == TRUE)) > 0)
    datta <- impute(datta, varType = 'CN_', values = list("missing"), keepOriginal = FALSE)
  
  # 2.v) CATEGORICAL ORDINAL
  if(length(which(sapply(dplyr::select(datta, starts_with("CO_")), function(x) any(is.na(x))) == TRUE)) > 0)
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
  if(scope == 'train'){
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
    save(toBeCat, file = 'fromQuantitativeToCategorical.RData')
  } else {
    load("fromQuantitativeToCategorical.RData")
    colnames(datta)[colnames(datta) %in% toBeCat] <- gsub('Q_','CN_',toBeCat)
  }
  
  
  ## 4. RARE-LEVEL INDICATION
  # TODO: RADO
  
  ## 5. SAVE
  print("Writting the output file...")
  write.table(datta, file = outputFile, sep = ",")
  print('Over and out!')
}
sanitizeApplicationData_prev <- function(inputFile, outputFile, maxLevels = 25){
  datta <- read.csv(inputFile, header = T)
  
  ## 1. add varible name pre-fixed according to variable types (see ../00_organisation/README_variable_types.txt)
  ids           <- c('SK_ID_CURR', 'SK_ID_PREV')
  dates         <- NULL
  target        <- NULL
  indicators    <- unique(c(colnames(datta)[grepl("FLAG", colnames(datta))]
                            , colnames(datta)[grepl("NFLAG", colnames(datta))]
  ))
  
  categoricalSuggested  <- suggestCategorical(datta, maxLevels)
  categoricalSuggested  <- categoricalSuggested[!(categoricalSuggested %in% c(target, indicators))] 
  print(categoricalSuggested)
  
  categoricalNominal <- c('NAME_CONTRACT_TYPE'
                          , 'NAME_CASH_LOAN_PURPOSE'
                          , 'NAME_CONTRACT_STATUS'
                          , 'NAME_PAYMENT_TYPE'
                          , 'CODE_REJECT_REASON'
                          , 'NAME_TYPE_SUITE'
                          , 'NAME_CLIENT_TYPE'
                          , 'NAME_GOODS_CATEGORY'
                          , 'NAME_PORTFOLIO'
                          , 'NAME_PRODUCT_TYPE'
                          , 'CHANNEL_TYPE'
                          , 'NAME_SELLER_INDUSTRY'
                          , 'NAME_YIELD_GROUP'
                          , 'PRODUCT_COMBINATION'
  )
  
  categoricalOrdinal <- c('WEEKDAY_APPR_PROCESS_START'
                          ,'HOUR_APPR_PROCESS_START')
  
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
    print(paste0(Sys.time()," Sanitizing level-names for: ", var, "..."))
    datta[,var] <- stringr::str_replace_all(as.character(datta[,var])," ","_")
    datta[,var] <- stringr::str_replace_all(as.character(datta[,var]),",","")
  }
  
  ## 2. AGGREGATE ON SK_ID_CURR LEVEL
  datta   <- castVariables(datta)
  datta_agg <- data.frame(ID_SK_ID_CURR = unique(datta[,c('ID_SK_ID_CURR')]))
  
  doCountsCategorical <- function(inputData, var){
    inputData <- inputData %>% dplyr::select(ID_SK_ID_CURR, ID_SK_ID_PREV, !!rlang::sym(var))
    inputData[,var] <- as.factor(paste0('Q_PREV_APP_',as.character(inputData[,var])))
    inputData <- inputData %>% 
      dplyr::group_by(ID_SK_ID_CURR, !!rlang::sym(var)) %>% 
      dplyr::summarize(CNT = length(ID_SK_ID_PREV)) %>% 
      reshape2::dcast(paste0('ID_SK_ID_CURR ~',eval(var)), fill = 0, value.var = "CNT")
    
    return(inputData)
  }
  
  for(c in colnames(dplyr::select(datta, starts_with('CN_'), starts_with('CO_')))){
    print(paste0(Sys.time(),'| Counting and spreading categorical : ', c))
    datta_agg <- dplyr::left_join(x = datta_agg
                                  , y = doCountsCategorical(datta, c)
                                  , by = c('ID_SK_ID_CURR'))  
  }
  
  doQuantitative <- function(inputData, var){
    inputData[,var] <- as.numeric(inputData[,var])
    inputData <- inputData %>% 
      dplyr::select(ID_SK_ID_CURR, !!rlang::sym(var)) %>%
      dplyr::group_by(ID_SK_ID_CURR) %>%
      dplyr::summarize(PREV_APP_MAX = max(!!rlang::sym(var), na.rm = TRUE)
                       , PREV_APP_MIN = min(!!rlang::sym(var), na.rm = TRUE)
                       , PREV_APP_MEAN = mean(!!rlang::sym(var), na.rm = TRUE)
                       , PREV_APP_MEDIAN = median(!!rlang::sym(var), na.rm = TRUE))
    
    colnames(inputData)[colnames(inputData) == 'PREV_APP_MAX'] <- paste0('Q_PREV_APP_MAX_', var)
    colnames(inputData)[colnames(inputData) == 'PREV_APP_MIN'] <- paste0('Q_PREV_APP_MIN_', var)
    colnames(inputData)[colnames(inputData) == 'PREV_APP_MEAN'] <- paste0('Q_PREV_APP_MEAN_', var)
    colnames(inputData)[colnames(inputData) == 'PREV_APP_MEDIAN'] <- paste0('Q_PREV_APP_MEDIAN_', var)
    
    return(inputData)
  }
  
  for(c in colnames(dplyr::select(datta, starts_with('Q_')))){
    print(paste0(Sys.time(),'| Aggregating quantitative : ', c))
    datta_agg <- dplyr::left_join(x = datta_agg
                                  , y = doQuantitative(datta, c)
                                  , by = c('ID_SK_ID_CURR'))  
  }
  
  doIndicators <- function(inputData, var){
    inputData[,var] <- as.numeric(as.factor(inputData[,var])) - 1
    inputData <- inputData %>% 
      dplyr::select(ID_SK_ID_CURR, !!rlang::sym(var)) %>%
      dplyr::mutate(non = abs(inputData[,var] - 1)) %>%
      dplyr::group_by(ID_SK_ID_CURR) %>%
      dplyr::summarize(N_1 = sum(!!rlang::sym(var), na.rm = TRUE)
                       , N_0 = sum(non, na.rm = TRUE)
                       , frac = (sum(!!rlang::sym(var), na.rm = TRUE)/length(ID_SK_ID_CURR)))
    
    colnames(inputData)[colnames(inputData) == 'N_1'] <- paste0('Q_N_1_', var)
    colnames(inputData)[colnames(inputData) == 'N_0'] <- paste0('Q_N_0_', var)
    colnames(inputData)[colnames(inputData) == 'frac'] <- paste0('Q_frac_', var)
    
    return(inputData)
  }
  
  for(c in colnames(dplyr::select(datta, starts_with('I_')))){
    print(paste0(Sys.time(),'| Aggregating indicators : ', c))
    datta_agg <- dplyr::left_join(x = datta_agg
                                  , y = doIndicators(datta, c)
                                  , by = c('ID_SK_ID_CURR'))  
  }
  
  ## 3. MISSING VALUES IMPUTATION
  # 3.iii) INDICATORs
  if(length(which(sapply(dplyr::select(datta_agg, starts_with("I_")), function(x) any(is.na(x))) == TRUE)) > 0)
    datta_agg <- impute(datta_agg, varType = 'I_', values = list(0), keepOriginal = FALSE)
  
  # 3.iv) QUANTITATIVE
  if(length(which(sapply(dplyr::select(datta_agg, starts_with("Q_")), function(x) any(is.na(x))) == TRUE)) > 0)
    datta_agg <- impute(datta_agg, varType = 'Q_', values = list(0, "mean"), keepOriginal = FALSE)
  
  # 3.v) CATEGORICAL NOMINAL
  if(length(which(sapply(dplyr::select(datta_agg, starts_with("CN_")), function(x) any(is.na(x))) == TRUE)) > 0)
    datta_agg <- impute(datta_agg, varType = 'CN_', values = list("missing"), keepOriginal = FALSE)
  
  # 3.v) CATEGORICAL ORDINAL
  if(length(which(sapply(dplyr::select(datta_agg, starts_with("CO_")), function(x) any(is.na(x))) == TRUE)) > 0)
    datta_agg <- impute(datta_agg, varType = 'CO_', values = list("missing"), keepOriginal = FALSE)
  
  # final check
  any(sapply(datta_agg, function(x) is.na(x)))
  
  ## 3.i OUTLIERS INDICATION
  #datta_agg <- indicateOutliers(datta_agg)
  
  ## 3.ii CENSORING
  # outlier information is already captured in dedicated outlier indicator columns
  # , now distribution are censored such that all observations fall between 3 STD
  # -> to improve binning algos later on
  # outlier_indicators <- colnames(dplyr::select(datta_agg, ends_with('_Out')))
  # for(oi in outlier_indicators){
  #   if(any(datta_agg[,oi] == 1)){
  #     original_var <- substr(oi,3,nchar(oi))
  #     original_var <- substr(original_var,1,nchar(original_var)-4)
  #     print(paste0('Generating trimmed version of: ',original_var,'...'))
  #     
  #     minn <- min(datta_agg[datta_agg[,oi] == 0,original_var], na.rm = T)
  #     maxx <- max(datta_agg[datta_agg[,oi] == 0,original_var], na.rm = T)
  #     
  #     #create 'trimmed' variable
  #     new_namee <- paste0(original_var, '_Trimmed')
  #     datta_agg[,new_namee] <- datta_agg[,original_var]
  #     if(any((datta_agg[,oi]==1 & scale(datta_agg[, original_var]) < 0))){
  #       datta_agg[(datta_agg[,oi]==1 & scale(datta_agg[, original_var]) < 0) ,new_namee]  <- minn  
  #     }
  #     if(any((datta_agg[,oi]==1 & scale(datta_agg[, original_var]) >= 0))){
  #       datta_agg[(datta_agg[,oi]==1 & scale(datta_agg[, original_var]) >= 0) ,new_namee] <- maxx
  #     }
  #     
  #     # drop original variable with outliers
  #     datta_agg[,original_var] <- NULL
  #   }
  # }
  # if(scope == 'train'){
  #   ## Checking for quantitative variables with only few levels
  #   # Some variables may seem quantitative, but in fact, only a couple of distinct values exist -> cast to Categorical (otherwise problems in binning)
  #   qnames <- colnames(dplyr::select(datta, starts_with("Q_")))
  #   summary <- data.frame()
  #   for(varName in qnames) summary <- rbind(summary, data.frame(varName = varName, uniqueValues = length(unique(datta[,varName]))))
  #   summary <- dplyr::arrange(summary, uniqueValues)
  #   View(dplyr::filter(summary, uniqueValues <= maxLevels))
  #   # turn the low-unique-values one into categorical nominal
  #   # TODO: RADO:This should be done better, some should actually be ordinal...
  #   toBeCat <- dplyr::filter(summary, uniqueValues <= maxLevels)[,1]
  #   colnames(datta)[colnames(datta) %in% toBeCat] <- gsub('Q_','CN_',toBeCat)
  #   save(toBeCat, file = 'fromQuantitativeToCategorical_appPrev.RData')
  # } else {
  #   load("fromQuantitativeToCategorical_appPrev.RData")
  #   colnames(datta)[colnames(datta) %in% toBeCat] <- gsub('Q_','CN_',toBeCat)
  # }
  
  ## 4. RARE-LEVEL INDICATION
  # TODO: RADO
  ## get rid of column names that contain dots
  colnames(datta_agg) <- stringr::str_replace_all(colnames(datta_agg), "\\.", "")
  colnames(datta_agg) <- stringr::str_replace_all(colnames(datta_agg),"-","_")
  colnames(datta_agg) <- stringr::str_replace_all(colnames(datta_agg),"/","")
  colnames(datta_agg) <- stringr::str_replace_all(colnames(datta_agg),"\\+","")
  colnames(datta_agg) <- stringr::str_replace_all(colnames(datta_agg),"\\(","")
  colnames(datta_agg) <- stringr::str_replace_all(colnames(datta_agg),"\\)","")
  colnames(datta_agg) <- stringr::str_replace_all(colnames(datta_agg),":","")
  
  ## 6. SAVE
  print("Writting the output file...")
  write.table(datta_agg, file = outputFile, sep = ",")
  print('Over and out!')
}

# ----------------------------------------------------------------------------------------
#                                  GLOBAL SETTINGS
# ----------------------------------------------------------------------------------------
maxLevels <- 25 # maximum number of level that a categorical variable can have.


# ----------------------------------------------------------------------------------------
#                                  SCRIPT EXECUTION
# ----------------------------------------------------------------------------------------

sanitizeApplicationData(inputFile = "..//01_raw_data/application_train.csv"
                        , outputFile = "./sanitized_data/s_application_for_regBased_train.csv"
                        , maxLevels = maxLevels
                        , scope = 'train')

sanitizeApplicationData(inputFile = "..//01_raw_data/application_test.csv"
                        , outputFile = "./sanitized_data/s_application_for_regBased_test.csv"
                        , maxLevels = maxLevels
                        , scope = 'test')

sanitizeApplicationData_prev(inputFile = "..//01_raw_data/previous_application.csv"
                        , outputFile = "./sanitized_data/s_prev_application_for_regBased.csv"
                        , maxLevels = maxLevels)

