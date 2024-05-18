## Model Comparisons
library(caret)
resamps <- resamples(list(LR = model_lr,
                          LASSO = model_lasso,
                          Ridge = model_ridge,
                          LDA = model_lda,
                          Elastic = model_elastic,
                          RF = model_rf,
                          SVM = model_svm,
                          NB = model_nb,
                          KNN = model_knn,
                          CART = model_rpart
                          ))
resamps
summary(resamps)
theme1 <- trellis.par.get()
theme1$plot.symbol$col = rgb(.2, .2, .2, .4)
theme1$plot.symbol$pch = 16
theme1$plot.line$col = rgb(1, 0, 0, .7)
theme1$plot.line$lwd <- 2
pdf('model_comparisons.pdf',width = 12,height = 6)
trellis.par.set(caretTheme())
dotplot(resamps, metric = "ROC")
dev.off()
