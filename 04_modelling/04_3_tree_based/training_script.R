# ====================================================================================== #
# 
#                             *** BENCHMARK MODEL - TRAINING SCRIPT ****
# 
#
# Author(s): Rado,
#
# --- GOAL ---
# Train a benchmark model. The benchmark model is a simple stepwise logistic regression.
#
# --- INPUTS---
# predictor_base_train.csv
#
# --- OUTPUTS ---
# 1.) benchmark_model.RData
# 2.) Update of the "./model_leaderboard.csv"
# 
# ====================================================================================== #
rm(list = ls())

#libs & proprietary functions:
library(caret)
library(e1071)
library(pROC)
library(ggplot2)
library(mlbench)
library(plotROC)
source('../../03_data_preparation/_utils.R')

# --------------------------------------------------------------------------------------
# Preparatory
# --------------------------------------------------------------------------------------
# settings
corCutOff <- 0.6

# load data
tdata <- read.csv("../../03_data_preparation/predictor_base/predictor_base_treeBased_train.csv")

#drop ID
tdata <- dplyr::select(tdata, -one_of('ID_SK_ID_CURR'))

# cast variables to correct variables types (see './00_organisation/README_variable_types.txt')
tdata <- castVariables(tdata)

tdata <- tdata[,c('T_TARGET',toKeep)]
tdata$T_TARGET <- as.factor(tdata$T_TARGET)
levels(tdata$T_TARGET) <- make.names(levels(factor(tdata$T_TARGET)))

# !!!!! DIRTY   !!!!!
# TODO: To be fixed in the data preparation
# CN_ORGANIZATION_TYPE has 58 levels, rf can only hangle 53..
tdata[,colnames(tdata) == 'CN_ORGANIZATION_TYPE'] <- NULL 
# !!!!!!!!!!!!!!!!!!!

set.seed(21)
trainIndex <- caret::createDataPartition(tdata$T_TARGET, p = .7, list = FALSE, times = 1)
dtrain <- tdata[trainIndex,]
dtest  <- tdata[-trainIndex,]

# --------------------------------------------------------------------------------------
# RADNDOM FORREST
# --------------------------------------------------------------------------------------
npos <- length(dtrain$T_TARGET[dtrain$T_TARGET == 'X1'])
rfTuneGrid <- expand.grid(mtry = c(25, 40, 55))

tc <- trainControl(method="repeatedcv"
                   , number = 5
                   , repeats = 1
                   , classProbs = T
                   , summaryFunction = twoClassSummary
                   , sampling = 'down'
                   , savePredictions = T)

rf <-   train(x = dplyr::select(dtrain, -one_of('T_TARGET'))
              , y = dtrain$T_TARGET
              , method = 'rf'
              , metric = 'ROC'
              , trControl = tc
              , verbose = F
              
              # rf-specific settings:
              , tuneGrid = rfTuneGrid
              , nodesize = round(0.025*npos*2)
              , ntree = 100
              , strata=dtrain$T_TARGET)

rf$results
caret::varImp(rf, scale = T)

# Select a parameter setting
selectedIndices <- rf$pred$mtry == 40

# ploc AUC for TRAIN:
g <- ggplot(rf$pred[selectedIndices, ], aes(m=X1, d=factor(obs, levels = c("X1", "X0")))) + 
  geom_roc(n.cuts=0) + 
  coord_equal() +
  style_roc()
g + annotate("text", x=0.75, y=0.25, label=paste("AUC =", round((calc_auc(g))$AUC, 4)))


fit  <- predict(rf, newdata = dtrain, type = 'prob')
pred <- predict(rf, newdata = dtest, type = 'prob')

rocFit  <- pROC::roc(predictor=fit$X1, response=dtrain$T_TARGET)
rocPred <- pROC::roc(predictor=pred$X1, response=dtest$T_TARGET)

# ================ Save to the MODEL LOG ================ 
modelSummary <- data.frame(DATE = paste0(Sys.Date(), " | ", Sys.time()) 
                           , MODEL_TYPE = 'randomForest'
                           , MODEL_VERSION = 0
                           , MODELLER = 'Rado'
                           , TRAIN_AUC = as.numeric(rocFit$auc)
                           , TEST_AUX = as.numeric(rocPred$auc))

if(file.exists('../model_log.csv')){
  modelSummaryOld <- read.table('../model_log.csv', sep=',', header = T)
  modelSummary <- rbind(modelSummaryOld,modelSummary)
}
write.table(modelSummary, file = '../model_log.csv', sep = ',', row.names = FALSE)

# ================ Save the benchmark model ================ 
save(rf, file='rf_v0.RData')


