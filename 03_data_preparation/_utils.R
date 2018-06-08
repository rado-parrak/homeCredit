# ====================================================================================== #
# 
#                             *** Utility functions ****
# 
#
# Author(s): Rado,
# ====================================================================================== #
library(dplyr)

# Suggest which variables might be categorical
suggestCategorical <- function(inputData, maxLevels){
  
  # 1) look at factors
  factors   <- colnames(inputData)[unlist(lapply(inputData, is.factor))]
  
  # 2) look at numerics too
  numerics <- NULL
  aux     <- colnames(inputData)[unlist(lapply(inputData, is.numeric))]
  for(colname in aux){
    if(length(unique(inputData[,colname])) < maxLevels)
      numerics <- c(numerics, colname)
  }
  
  return(c(factors, numerics))
  
}

# Suggest which variables might be categorical
extendColumnNamesByTypes <- function(inputData, ids, target, dates, indicators,quantitative, categoricalNominal, categoricalOrdinal){
  if(!is.null(ids)){
    for(i in ids){
      colnames(inputData)[colnames(inputData) == i] <- paste0("ID_", i)     
    }
  }
  if(!is.null(target)){
    for(i in target){
      colnames(inputData)[colnames(inputData) == i] <- paste0("T_", i)     
    }
  }
  if(!is.null(dates)){
    for(i in dates){
      colnames(inputData)[colnames(inputData) == i] <- paste0("DT_", i)     
    }
  }
  if(!is.null(quantitative)){
    for(i in quantitative){
      colnames(inputData)[colnames(inputData) == i] <- paste0("Q_", i)     
    }
  }
  if(!is.null(categoricalNominal)){
    for(i in categoricalNominal){
      colnames(inputData)[colnames(inputData) == i] <- paste0("CN_", i)     
    }
  }
  if(!is.null(categoricalOrdinal)){
    for(i in categoricalOrdinal){
      colnames(inputData)[colnames(inputData) == i] <- paste0("CO_", i)     
    }
  }
  if(!is.null(indicators)){
    for(i in indicators){
      colnames(inputData)[colnames(inputData) == i] <- paste0("I_", i)     
    }
  }

  return(inputData)
}

# IMPUTE
impute  <- function(inputData, varType, values, keepOriginal = TRUE){
  
  # find those where it makes sense:
  toBeImputed <- names(sapply(dplyr::select(datta, starts_with(varType)), function(x) sum(is.na(x))))
  
  for(var in toBeImputed){
    print(paste0('Imputing variable: ', var))
    counter <- 0
    for(value in values){
      counter <- counter + 1
      if(value[[1]] == "mean"){
        varName <- paste0(var,"_IMP_v",counter)
        inputData[ ,varName] <- ifelse(is.na(inputData[ ,var]),mean(inputData[ ,var], na.rm=TRUE),inputData[ ,var])         
      } else if(value[[1]] == "missing"){
        
      } else{
        varName <- paste0(var,"_IMP_v",counter)
        inputData[ ,varName] <- ifelse(is.na(inputData[ ,var]),value[[1]],inputData[ ,var]) 
      }
    }
    if(!keepOriginal){
      inputData <- inputData[,-varName]
    }
  }
  
  return(inputData)
}

# INDICATE OUTLIERS
indicateOutliers <- function(inputData){
  quantData <- dplyr::select(inputData, starts_with("Q_"))
  for(var in colnames(quantData)){
    print(paste0('Indicating outliers for: ', var))
    varName <- paste0("I_",var,"_Out")
    inputData[ ,varName] <- ifelse( abs(scale(inputData[ ,var])) > 3, 1,0)
  }
  return(inputData)
}


