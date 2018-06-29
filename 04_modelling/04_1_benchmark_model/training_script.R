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
source('../../03_data_preparation/_utils.R')

# settings
corCutOff <- 0.6

# load data
tdata <- read.csv("../../03_data_preparation/predictor_base/predictor_base_regBased_train.csv")

#drop ID
tdata <- dplyr::select(tdata, -one_of('ID_SK_ID_CURR'))

# cast variables to correct variables types (see './00_organisation/README_variable_types.txt')
tdata <- castVariables(tdata)

# drop columns with NAs (glm cannot treat these anyways)
toKeep  <- colnames(tdata)[!unlist(lapply(tdata, function(x) any(is.na(x))))]
tdata   <- tdata[,toKeep]

# drop (near-)zero-variance
nzv <- nearZeroVar(tdata, saveMetrics= TRUE)
tdata <- tdata[,!(colnames(tdata) %in% row.names(nzv[nzv$zeroVar,]))]

# drop correlated variables
cor_input <- dplyr::select(tdata, -one_of('T_TARGET'))
for(namee in colnames(cor_input)){
  if(is.factor(cor_input[,namee])){
    cor_input[,namee] <- as.numeric(cor_input[,namee])
  }
}
corr <- cor(cor_input)
toKeep <- colnames(cor_input)[caret::findCorrelation(corr, cutoff = corCutOff, verbose = TRUE, exact = ncol(corr) < 100)]

tdata <- tdata[,c('T_TARGET',toKeep)]
tdata$T_TARGET <- as.factor(tdata$T_TARGET)
levels(tdata$T_TARGET) <- make.names(levels(factor(tdata$T_TARGET)))

set.seed(21)
trainIndex <- caret::createDataPartition(tdata$T_TARGET, p = .7, list = FALSE, times = 1)
train <- tdata[trainIndex,]
test  <- tdata[-trainIndex,]


fitControl <- trainControl(method = "repeatedcv"
                           , number = 10
                           , repeats = 1
                           , classProbs = TRUE
                           , summaryFunction = twoClassSummary)

benchmarkModel <- train(T_TARGET ~ ., data = train,
                 method = "glm",
                 metric = 'ROC',
                 trControl = fitControl,
                 family = binomial())

summary(benchmarkModel)
benchmarkModel$results

pred <- predict(benchmarkModel, test, type = 'prob')
hist(pred$X1)

roc <- pROC::roc(predictor=pred$X1, response=test$T_TARGET)

# ================ Save to the MODEL LOG ================ 
modelSummary <- data.frame(DATE = paste0(Sys.Date(), " | ", Sys.time()) 
                           , MODEL_TYPE = 'benchmark - GLM'
                           , MODEL_VERSION = 0
                           , MODELLER = 'Rado'
                           , TRAIN_AUC = benchmarkModel$results$ROC
                           , TEST_AUX = as.numeric(roc$auc))

if(file.exists('../model_log.csv')){
  modelSummaryOld <- read.table('../model_log.csv', sep=',', header = T)
  modelSummary <- rbind(modelSummaryOld,modelSummary)
}
write.table(modelSummary, file = '../model_log.csv', sep = ',', row.names = FALSE)

# ================ Save the benchmark model ================ 
save(benchmarkModel, file='benchmarkModel_glm_v0.RData')


