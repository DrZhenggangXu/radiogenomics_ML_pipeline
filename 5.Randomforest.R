getwd()
rm(list = ls())
library(dplyr)
library(tidyverse)
library(pROC)
library(caret)
library(randomForest)
data <- read.csv('tcia.csv')
x <- data[,2:ncol(data)]
y <- data[,'label']

## Recursive Feature Elimination
control <- rfeControl(functions=rfFuncs, 
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
  search = "grid",
  classProbs = T,                  
  summaryFunction=twoClassSummary,  
  sampling = 'smote')
Grid <-  expand.grid(mtry= c(1:length(features)))
set.seed(123) 
model_rf = train(data[,features],y,
                 method = "rf",
                 tuneGrid = Grid,
                 metric = "ROC",
                 trControl = fitControl)
model_rf$bestTune
model_rf$finalModel
model_rf
