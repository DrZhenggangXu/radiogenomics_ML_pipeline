getwd()
rm(list = ls())
library(dplyr)
library(tidyverse)
library(pROC)
library(glmnet)
library(caret)
data <- read.csv('tcia.csv') 
x <- data[,2:ncol(data)]
y <- data[,'label']

## Recursive Feature Elimination
control <- rfeControl(functions=gamFuncs, 
                      method="repeatedcv",
                      repeats=10,
                      number=5,
                      verbose=FALSE)
set.seed(123) 
rfe_results <- rfe(x, y, 
                   sizes=c(1:ncol(x)), 
                   family = 'binomial',
                   rfeControl=control)
print(rfe_results)

## Model Building
features <- rfe_results$optVariables
print(features)
fitControl <- trainControl(
  method = 'repeatedcv', 
  repeats = 10,
  number = 5,                      
  savePredictions = 'final',       
  search = "grid",
  classProbs = T,                  
  summaryFunction=twoClassSummary,  
  sampling = 'smote')
Grid <-  expand.grid(alpha = 0, 
                     lambda = seq(0.001, 1, length = 100))
set.seed(123)  
model_ridge = train(data[features],y,
                 method = "glmnet",
                 family = "binomial", 
                 tuneGrid = Grid,
                 metric = "ROC",
                 trControl = fitControl)
model_ridge$finalModel 
model_ridge$bestTune 
model_ridge$results 
model_ridge
