getwd()
rm(list = ls())
library(dplyr)
library(tidyverse)
library(pROC)
library(LogicReg)
library(caret)
data <- read.csv('tcia.csv')
x <- data[,2:ncol(data)]
y <- data[,'label']

## Recursive Feature Elimination
control <- rfeControl(functions=lrFuncs, 
                      method="repeatedcv",
                      repeats=10,
                      number=5,
                      verbose=FALSE)
set.seed(123) 
rfe_results <- rfe(x, y, 
                   sizes=c(1:ncol(x)), 
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
  classProbs = T,                  
  summaryFunction=twoClassSummary,  
  sampling = 'smote')
set.seed(123)  
model_lr = train(data[,features],y,
                 method = "logreg",
                 family = "binomial", 
                 metric = "ROC",
                 trControl = fitControl)
model_lr$finalModel 
model_lr$bestTune 
model_lr$results 
model_lr
