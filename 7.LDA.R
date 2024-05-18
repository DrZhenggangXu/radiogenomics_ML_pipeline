getwd()
rm(list = ls())
library(dplyr)
library(tidyverse)
library(pROC)
library(caret)
library(MASS)
data <- read.csv('tcia.csv')
x <- data[,2:ncol(data)]
y <- data[,'label']

## Recursive Feature Elimination
control <- rfeControl(functions=ldaFuncs, 
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
model_lda = train(data[features],y,
                 method = "lda",
                 metric = "ROC",
                 trControl = fitControl)
model_lda$bestTune
model_lda$finalModel
model_lda
