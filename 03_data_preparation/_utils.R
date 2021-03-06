# ====================================================================================== #
# 
#                             *** Utility functions ****
# 
#
# Author(s): Rado,
# ====================================================================================== #
if(!require(dplyr)){
  install.packages("dplyr")
}
if(!require(smbinning)){
  install.packages("smbinning")
}
library(dplyr)
library(smbinning)

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
  toBeImputed <- names(sapply(dplyr::select(inputData, starts_with(varType)), function(x) sum(is.na(x))))
  
  for(var in toBeImputed){
    print(paste0('Imputing variable: ', var))
    counter <- 0
    inputData[, paste0('I_NA_',var)] <- ifelse(is.na(inputData[ ,var]), 1, 0)
    for(value in values){
      counter <- counter + 1
      if(value[[1]] == "mean"){
        varName <- paste0(var,"_IMP_v",counter)
        inputData[ ,varName] <- ifelse(is.na(inputData[ ,var]),mean(inputData[ ,var], na.rm=TRUE),inputData[ ,var])         
      } else if(value[[1]] == "missing"){
        varName <- paste0(var,"_IMP_v",counter)
        if(is.numeric(inputData[ ,var])){
          inputData[ ,varName] <- ifelse(is.na(inputData[ ,var]),-999,inputData[ ,var])
        } else{
          inputData[ ,varName] <- ifelse(is.na(inputData[ ,var]),'missing',inputData[ ,var])
        }
      } else{
        varName <- paste0(var,"_IMP_v",counter)
        inputData[ ,varName] <- ifelse(is.na(inputData[ ,var]),value[[1]],inputData[ ,var]) 
      }
    }
    if(!keepOriginal){
      inputData[,var] <- NULL
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

# CAST VARIABLES
castVariables <- function(inputData){
  
  # intervals / quantitative:
  qnames        <- colnames(dplyr::select(inputData, dplyr::starts_with('Q_')))
  qnames        <- c(qnames, colnames(dplyr::select(inputData, dplyr::starts_with('ID_'))))
  qnames        <- c(qnames, colnames(dplyr::select(inputData, dplyr::starts_with('T_'))))
  outputData_q  <- as.data.frame(sapply(inputData[,qnames], as.numeric))
  
  if(length(qnames)>0)
    outputData  <- outputData_q
  
  # factors: Target, Indicators, Categorical Nominal 
  fnames        <- colnames(dplyr::select(inputData, dplyr::starts_with('I_')))
  fnames        <- c(fnames, colnames(dplyr::select(inputData, dplyr::starts_with('CN_'))))
  outputData_f  <- as.data.frame(sapply(inputData[,fnames], as.factor))
  
  if(length(fnames)>0)
    outputData  <- cbind(outputData, outputData_f)
  
  # ordinals:
  # TODO: See how this can be done such that we're sure that the order is correct
  onames        <- colnames(dplyr::select(inputData, dplyr::starts_with('CO_'), dplyr::starts_with('B_')))
  outputData_o  <- as.data.frame(sapply(inputData[,onames], as.ordered))
  
  if(length(onames)>0)
    outputData  <- cbind(outputData, outputData_o)
  
  allnames <- c(qnames,fnames,onames)
  print(paste0(length(allnames), ' variables casted from ', length(colnames(inputData)),'!'))
  
  return(outputData)
}

# doSmBinning wraper for paralelization
doSmBinning <- function(type,varName, inputData,pVal,IVtresh, filePath){
  set.seed(21)
  cat(paste0(Sys.time()," | SM Binning, calculating WOE and IV for: ", varName, '\n'), file = filePath, append = TRUE)
  browser()
  res <- tryCatch(
    if(type == 'q'){
      inputData[,varName] <- as.numeric(inputData[,varName])
      smbinning(df=inputData, y="T_TARGET",x=varName,p=pVal)
    } else if(type == 'c'){
      inputData[,varName] <- as.factor(inputData[,varName])
      smbinning.factor(df=inputData, y="T_TARGET",x=varName,maxcat = 70)
    }
    , error=function(e) NULL)

  if(!is.null(res)){
    if(res != "No significant splits"){
      if(res$iv >= IVtresh){
        return(res)
      }    
    }  
  }
   
}

# doWoeBinning wraper for paralelization
doWoeBinning <- function(varName, inputData, pVal, IVtresh, minPercClass, filePath){
  set.seed(21)
  cat(paste0(Sys.time()," | WOE Binning, calculating WOE and IV for: ", varName, '\n'), file = filePath, append = TRUE)
  inputData[,varName] <- as.numeric(inputData[,varName])
  
  res <- tryCatch(woe.binning(inputData, 'T_TARGET', varName, min.perc.total = pVal, min.perc.class = minPercClass, event.class = 1)
                  , error=function(e) NULL)
  
  if(!is.null(res)){
    if(res[[2]]$iv.total.final[1] >= IVtresh){
      return(res)
    } 
  }
  
}