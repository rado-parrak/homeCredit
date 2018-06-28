# ====================================================================================== #
# 
#                             *** BENCHMARK MODEL - TRAINING SCRIPT ****
# 
#
# Author(s): Rado,
#
# --- GOAL ---
# Score the 'test' set using the benchmark model
#
# --- INPUTS---
# predictor_base_test.csv
#
# --- OUTPUTS ---
# 
# ====================================================================================== #

rm(list = ls())
library(e1071)
source('../../03_data_preparation/_utils.R')

# 1.) load the data
datta <- read.table(file="../../03_data_preparation/predictor_base/predictor_base_regBased_test.csv", sep = ',', header = T)
datta <- castVariables(datta)

# 2.) load the benchmark model
bechnmarkModel <- load("../../04_modelling/04_1_benchmark_model/benchmarkModel_glm_v0.RData")

# 3.) score
probs <- predict(benchmarkModel, newdata = datta,type = 'prob')

# 4.) submission-sheet
submissionSheet <- data.frame(SK_ID_CURR = as.integer(datta$ID_SK_ID_CURR), TARGET = probs$X1)

# 5.) write table
date    <- Sys.Date()
author  <- 'Rado'
model   <- 'benchmarkReg'
version <- '0'

fileName <- paste(date, model, version, author, sep="_")

write.csv(submissionSheet, file=paste0('../99_submission_sheets/',fileName,'.csv'), row.names = F)