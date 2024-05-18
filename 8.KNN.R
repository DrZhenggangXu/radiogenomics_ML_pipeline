getwd()
rm(list = ls())
library(dplyr)
library(tidyverse)
library(pROC)
library(caret)
data <- read.csv('tcia.csv') 
x <- data[,2:ncol(data)]
y <- data[,'label']

## Recursive Feature Elimination
control <- rfeControl(functions=caretFuncs, 
                      method="repeatedcv",
                      repeats=10,
                      number=5,
                      verbose=FALSE)
set.seed(123) 
rfe_results <- rfe(x, y, 
                   sizes=c(1:ncol(x)), 
                   rfeControl=control,
                   method = 'knn')
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
Grid <-  expand.grid(k = c(1:5)) 
set.seed(123)  
model_knn = train(data[features],y,
                 method = "knn",
                 tuneGrid = Grid,
                 metric = "ROC",
                 trControl = fitControl)
model_knn$bestTune
model_knn$finalModel
model_knn
