getwd()
rm(list = ls())
library(dplyr)
library(tidyverse)
library(pROC)
library(rpart)
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
                   method='rpart')
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
tuneGrid <- expand.grid(.cp = seq(0.01, 0.1, by = 0.01))
set.seed(123)  
model_rpart = train(data[features],y,
                 method = "rpart",
                 tuneGrid = tuneGrid,
                 metric = "ROC",
                 trControl = fitControl)
model_rpart$bestTune
model_rpart$finalModel
model_rpart
