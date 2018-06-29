# ====================================================================================== #
# 
#                             *** CUSTOM FEATURE GENERATION ****
# 
#
# Author(s): Rado,
#
# --- GOAL ---
# Feature generation script
#
# --- INPUTS---
# - s_application_for_XXX_train.csv
# - s_application_for_XXX_test.csv
#
# --- OUTPUTS ---
# - predictor_base_XXX_train.csv
# - predictor_base_XXX_test.csv
# ====================================================================================== #

# Clean workspace and load libs:
rm(list = ls())
library(plyr)
library(dplyr)
library(smbinning)
library(woeBinning)
library(ggplot2)
library(doParallel)
library(InformationValue)
source('_utils.R')

preparePredictorBase <- function(inputFile, outputFile, scope = 'train', modelType = 'regressionBased', settings){
  
  sdata <- read.csv(inputFile, header = T)
  sdata <- castVariables(sdata)
  
  # --------------------------------------------------------------------------------------------------
  #                                 --- SELECTING IMPUTATION versions ---
  # --------------------------------------------------------------------------------------------------
  if(scope == 'train'){
    if(modelType %in% c('regressionBased', 'treeBased')){
      imps <- colnames(dplyr::select(sdata, contains('_IMP')))
      non_imps <- colnames(sdata)[!(colnames(sdata) %in% imps)] 
      
      toKeep <- non_imps
      
      # get unique 'core' names:
      coreNames <- unique(sapply(imps, function(x) gsub('_IMP.*',"",x)))
      for(coreName in coreNames){
        print(paste0('Selecting imputation version for: ', coreName))
        versions <- imps[sapply(imps, function(x) startsWith(x,coreName))]
        versionComp <- data.frame()
        for(version in versions){
          versionComp <- rbind(versionComp, data.frame(version, IV = InformationValue::IV(X = sdata[,version], Y = sdata[,'T_TARGET'])))
        }
        versionComp <- dplyr::arrange(versionComp, desc(IV))
        print(as.character(versionComp$version)[1])
        toKeep <- c(toKeep, as.character(versionComp$version)[1])
        rm(versions, versionComp)
      }
    }
    save(toKeep, file =paste0('toKeepFromImputation_',modelType,'.RData'))
  } else if(scope == 'scoring'){
    load(paste0('toKeepFromImputation_',modelType,'.RData'))
    toKeep <- toKeep[-which(toKeep == 'T_TARGET')]
  }
  sdata <- sdata[,unique(toKeep)]
  # --------------------------------------------------------------------------------------------------
  #                                     --- BINNING ---
  # --------------------------------------------------------------------------------------------------
  if(modelType == 'regressionBased'){
    if(scope == 'train'){
      # ----- (a) Quantitative variables -----
      sdata_q   <- dplyr::select(sdata, starts_with("Q_")) %>% dplyr::mutate(T_TARGET = sdata$T_TARGET)
      qnames    <- colnames(sdata_q)[colnames(sdata_q) != "T_TARGET"]
      results_q <- list()
      cl        <- makeCluster(detectCores() - 1)
      registerDoParallel(cl)
      
      nParts <- 7
      log_con1 <- file("SmBinningLog.log", open = "a")
      
      for(j in 1:nParts){ #this outer loop is there due to heap insuficiency
        print(paste0('Start: ', Sys.time()))
        minn <- (j-1)*ceiling(ncol(sdata_q)/nParts) + 1
        maxx <- min((j)*ceiling(ncol(sdata_q)/nParts),length(qnames))
        print(paste0('=== Chunk >> ', minn, ':', maxx, ' from ', length(qnames),'. ==='))
        
        aux_smBinning <- foreach(i = minn:maxx, .packages = c('smbinning')) %dopar% {
          source('_utils.R', local = TRUE)
          doSmBinning('q',qnames[[i]], sdata_q, settings$pVal, settings$IVtresh, './SmBinningLog.log')
        }
        
        if(j == 1){
          results_q   <- aux_smBinning
        } else{
          results_q   <- c(results_q, aux_smBinning)
        }
        results_q <- plyr::compact(results_q)
        
        rm(aux_smBinning)
        print(paste0('End: ', Sys.time()))
      }
      stopCluster(cl)
      close(log_con1)
      
      # save the interesting vars from 'smBinning' to IV container
      ivContainer       <- list()
      for(res in results_q){
        ivContainer[[colnames(sdata_q)[res$col_id]]][['sm']]$IV             <- res$iv
        ivContainer[[colnames(sdata_q)[res$col_id]]][['sm']]$cuts           <- res$cuts
        ivContainer[[colnames(sdata_q)[res$col_id]]][['sm']]$binningMethod  <- 'smBinning'
      }
      
      # ----- (b) Categorical variables -----
      sdata_c   <- dplyr::select(sdata, starts_with("CN_"), starts_with("CO_")) %>% dplyr::mutate(T_TARGET = sdata$T_TARGET)
      cnames    <- colnames(sdata_c)[colnames(sdata_c) != "T_TARGET"]
      results_c <- list()
      cl        <- makeCluster(detectCores() - 1)
      
      registerDoParallel(cl)
      nParts <- 6
      log_con1 <- file("SmBinningLog.log", open = "a")
      
      for(j in 1:nParts){ #this outer loop is there due to heap insuficiency
        print(paste0('Start: ', Sys.time()))
        minn <- (j-1)*ceiling(ncol(sdata_c)/nParts) + 1
        maxx <- min((j)*ceiling(ncol(sdata_c)/nParts),length(cnames))
        print(paste0('=== Chunk >> ', minn, ':', maxx, ' from ', length(cnames),'. ==='))
        
        aux_smBinning <- foreach(i = minn:maxx, .packages = c('smbinning')) %dopar% {
          source('_utils.R', local = TRUE)
          doSmBinning('c',cnames[[i]], sdata_c, settings$pVal, settings$IVtresh, './SmBinningLog.log')
        }
        
        if(j == 1){
          results_c   <- aux_smBinning
        } else{
          results_c   <- c(results_c, aux_smBinning)
        }
        results_c <- plyr::compact(results_c)
        
        rm(aux_smBinning)
        print(paste0('End: ', Sys.time()))
      }
      stopCluster(cl)
      close(log_con1)
      
      # save the interesting vars from 'smBinning' to IV container
      for(res in results_c){
        ivContainer[[colnames(sdata_c)[res$col_id]]][['sm']]$IV             <- res$iv
        ivContainer[[colnames(sdata_c)[res$col_id]]][['sm']]$cuts           <- res$cuts
        ivContainer[[colnames(sdata_c)[res$col_id]]][['sm']]$binningMethod  <- 'smBinning'
      }
      
      # ----- (C) Indicators variables -----
      sdata_i <- dplyr::select(sdata, starts_with("I_")) %>%
        dplyr::mutate(T_TARGET = sdata$T_TARGET)
      
      inames                <- colnames(sdata_i)[colnames(sdata_i) != "T_TARGET"]
      for(var in inames){
        print(paste0("Calculating Information Value for: ", var))
        IV <- InformationValue::IV(X=sdata_i[,var], Y=sdata_i[,'T_TARGET'], valueOfGood = 1)
        print(paste0('IV: ', as.character(IV[[1]])))
        if(IV[[1]] > settings$IVtresh){
          ivContainer[[var]][['noBinning']]$IV <- IV[[1]]
          ivContainer[[var]][['noBinning']]$binningMethod <- 'no binning'
        }
      }
      # ----- (D) PLOT -----
      counter     <- 0
      dataForPlot <- data.frame()
      for(el in ivContainer){
        counter <- counter +1
        for(method in names(el)){
          dataForPlot <- rbind(dataForPlot, data.frame(variable = paste0(names(ivContainer)[counter], ' | ', method)
                                                       , IV = el[[method]]$IV))
        }
      }
      
      print(ggplot(data = dataForPlot, aes(x = reorder(variable, IV), y = IV)) +
              geom_bar(stat = 'identity') +
              coord_flip())
      
      toKeepFromBinning <- names(ivContainer)
      save(results_c, results_q, file = './smbinning_results.RData')
      save(toKeepFromBinning, file =paste0('toKeepFromBinning_',modelType,'.RData'))
    } else if(scope == 'scoring'){
      load(paste0('toKeepFromBinning_',modelType,'.RData'))
      load('smbinning_results.RData')
    }
    
  }
  
  # --------------------------------------------------------------------------------------------------
  #                                     --- PRE-SELECTION ---
  # --------------------------------------------------------------------------------------------------
  if(modelType == 'regressionBased'){
    # 1) take target and id
    if(scope == 'train'){
      output <- sdata[,c("T_TARGET", "ID_SK_ID_CURR")]  
    } else if(scope == 'scoring'){
      output <- data.frame(ID_SK_ID_CURR = sdata$ID_SK_ID_CURR)
    }
    
    
    # 2) take original variables from the top IV-selected
    output <- cbind(output, sdata[,toKeepFromBinning])
    
    # 3.i) add SM binning of these
    qnames_sm <- sapply(results_q, function(y) y$x)
    for(namee in qnames_sm){
      newName           <- paste0('B_',namee,'_sm')
      output[,newName]  <- paste0("Bin_", findInterval(sdata[,namee], results_q[[which(qnames_sm == namee)]]$cuts))  
    }
  }
  if(modelType == 'treeBased'){
    output <- sdata
  }
  
  
  # --------------------------------------------------------------------------------------------------
  #                                     --- SAVING ---
  # --------------------------------------------------------------------------------------------------
    # save output
  write.csv(output, file=outputFile, row.names = F)
}

# ----------------------------------------------------------------------------------------
#                                  GLOBAL SETTINGS
# ----------------------------------------------------------------------------------------
settings <- data.frame(pVal = 0.05
                       , IVtresh = 0.05
                       , minPerClass = 0.01)

# ----------------------------------------------------------------------------------------
#                                  SCRIPT EXECUTION
# ----------------------------------------------------------------------------------------
# 1) Regression-based models:
preparePredictorBase(inputFile = './sanitized_data/s_application_for_regBased_train.csv'
                     , outputFile = './predictor_base/predictor_base_regBased_train.csv'
                     , scope = 'train'
                     , modelType = 'regressionBased'
                     , settings)

preparePredictorBase(inputFile = './sanitized_data/s_application_for_regBased_test.csv'
                     , outputFile = './predictor_base/predictor_base_regBased_test.csv'
                     , scope = 'scoring'
                     , modelType = 'regressionBased'
                     , settings)


# 1) Tree-based models:
preparePredictorBase(inputFile = './sanitized_data/s_application_for_regBased_train.csv'
                                  , outputFile = './predictor_base/predictor_base_treeBased_train.csv'
                                  , scope = 'train'
                                  , modelType = 'treeBased'
                                  , settings)

preparePredictorBase(inputFile = './sanitized_data/s_application_for_regBased_test.csv'
                     , outputFile = './predictor_base/predictor_base_treeBased_test.csv'
                     , scope = 'test'
                     , modelType = 'treeBased'
                     , settings)