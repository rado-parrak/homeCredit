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
source('../../03_data_preparation/_utils.R')

# settings
corCutOff <- 0.6

# load data
tdata <- read.csv("../../03_data_preparation/predictor_base/predictor_base_train.csv")

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

set.seed(21)
trainIndex <- caret::createDataPartition(tdata$T_TARGET, p = .7, list = FALSE, times = 1)
train <- tdata[trainIndex,]
test  <- tdata[-trainIndex,]


fitControl <- trainControl(method = "repeatedcv"
                           , number = 10
                           , repeats = 1
                           , classProbs = TRUE)

benchmarkModel <- train(T_TARGET ~ ., data = train, 
                 method = "glm",
                 metric = 'ROC',
                 trControl = fitControl,
                 verbose = FALSE)


