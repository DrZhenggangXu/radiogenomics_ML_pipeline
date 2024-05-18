getwd()
rm(list = ls())
library(ggplot2)
library(dplyr)
library(tidyverse)
library(pROC)
library(klaR)
library(caret)
data <- read.csv('tcia.csv')
x <- data[,2:ncol(data)]
y <- data[,'label']

## Recursive Feature Elimination
control <- rfeControl(functions=nbFuncs, 
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
Grid <-  expand.grid(
  usekernel = c(TRUE, FALSE),
  fL = seq(0,5,0.5), 
  adjust = seq(0,5,0.5))
set.seed(123) 
model_nb = train(data[features],y,
                 method = "nb",
                 tuneGrid = Grid,
                 metric = "ROC",
                 trControl = fitControl)
model_nb$bestTune
model_nb$finalModel
model_nb
