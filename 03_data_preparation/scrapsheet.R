rm(list = ls())

maxLevels <- 25
inputFile <- "..//01_raw_data/previous_application.csv"
scope     <- 'train'

source("_utils.R")
require('dplyr')

## _______________________ THE FUNCTION __________________________________________________________________________

  
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

## 2. MISSING VALUES IMPUTATION
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
  save(toBeCat, file = 'fromQuantitativeToCategorical_appPrev.RData')
} else {
  load("fromQuantitativeToCategorical_appPrev.RData")
  colnames(datta)[colnames(datta) %in% toBeCat] <- gsub('Q_','CN_',toBeCat)
}

## 4. RARE-LEVEL INDICATION
# TODO: RADO

## 5. AGGREGATE ON SK_ID_CURR LEVEL
datta   <- castVariables(datta)
datta_agg <- data.frame(ID_SK_ID_CURR = unique(datta[,c('ID_SK_ID_CURR')]))

doCountsCategorical <- function(inputData, var){
  inputData <- inputData %>% dplyr::select_('ID_SK_ID_CURR', 'ID_SK_ID_PREV', eval(var))
  inputData[,var] <- as.factor(paste0('PREV_APP_',as.character(inputData[,var])))
  inputData <- inputData %>% 
    dplyr::group_by_('ID_SK_ID_CURR', eval(var)) %>% 
    dplyr::summarize(CNT = length(ID_SK_ID_PREV)) %>% 
    reshape2::dcast(paste0('ID_SK_ID_CURR ~',eval(var)), fill = 0, value.var = "CNT")
  
  return(inputData)
}

for(c in colnames(datta)[grepl('CN_',colnames(datta))]){
  print(paste0(Sys.time(),'| Counting and spreading categorical : ', c))
  datta_agg <- dplyr::left_join(x = datta_agg
                                , y = doCountsCategorical(datta, c)
                                , by = c('ID_SK_ID_CURR'))  
}


## 6. SAVE
print("Writting the output file...")
write.table(datta, file = outputFile, sep = ",")
print('Over and out!')

 


