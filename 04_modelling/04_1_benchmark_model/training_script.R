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

# drop correlated variables
cor_input <- dplyr::select(tdata, -one_of('T_TARGET')) 
cor <- cor(cor_input, use = 'pairwise.complete.obs', method = 'pearson')
toKeep <- colnames(cor_input)[caret::findCorrelation(cor, cutoff = corCutOff, verbose = TRUE, exact = TRUE)]