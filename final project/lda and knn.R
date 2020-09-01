library(mlr)
library(fastDummies)
set.seed(20190421)

# Set dummy varaibles
nis_dummy <- dummy_cols(nis_clean_new[,-which(colnames(nis_clean_new) == "DIED")])[,-c(1:ncol(nis_clean_new))]

for (i in 1:ncol(nis_clean_new)) {
  name <- colnames(nis_clean_new)[i]
  if (is.numeric(nis_clean_new$name)){
    nis_dummy <- cbind(nis_dummy, nis_clean_new[,i]) 
  }
}
nis_dummy <- cbind(nis_dummy, nis_clean_new$DIED)

for (i in 1:ncol(nis_dummy)) {
  newname <- strsplit(colnames(nis_dummy)[i], "_")[[1]]
  colnames(nis_dummy)[i] <- paste(newname[1:length(newname)], collapse = "")
}
colnames(nis_dummy)[ncol(nis_dummy)] <- "DIED"

# Split data
nis_tsk <- makeClassifTask(id = "NIS", data = nis_dummy, target = "DIED")
split_desc <- makeResampleDesc(method = 'Holdout', stratify = T, split = 0.7)
split <- makeResampleInstance(split_desc, task = nis_tsk)
nis_tsk_train <- split$train.inds[[1]]
nis_tsk_test <- split$test.inds[[1]]

# KNN
## KNN will take longer time, but can use test data to validate the code.
Kvals <- makeParamSet(makeDiscreteParam("k", values = 1:30))
nis_CV <- makeResampleDesc("CV", iters = 10L, stratify=TRUE)
ctrl <- makeTuneControlGrid()
nis_knn_tune <- tuneParams("classif.knn", subsetTask(nis_tsk, subset = nis_tsk_train), resampling = nis_CV, par.set = Kvals, control = ctrl, measures = mmce)

nis_knn_learner <- makeLearner("classif.knn", id = "nis knn", fix.factors.prediction = T, k = nis_knn_tune$x$k)
nis_knn <- train(nis_knn_learner, task = nis_tsk, subset = nis_tsk_train)

nis_knn_perdict <- predict(nis_knn, newdata = nis_dummy[nis_tsk_test,])
calculateConfusionMatrix(nis_knn_perdict)
performance(nis_knn_predict, measures = list(auc, mmce))

# LDA
## variable 114 appears to be constant within groups. If we use less data, there will be more variables appear to be constant. Use the whole data to train.
nis_lda_tsk <- makeClassifTask(id = "nis lda", data = nis_dummy[nis_tsk_train,-114], target = "DIED")
nis_lda_learner <- makeLearner("classif.lda", fix.factors.prediction = T, predict.type = "prob")
nis_lda_cv <- crossval(nis_test_lda_learner, nis_lda_tsk, iters = 10L, stratify = T, measures = list(auc,mmce))

nis_lda <- train(nis_lda_learner, nis_lda_tsk)
nis_lda_predict <- predict(nis_lda, newdata = nis_dummy[nis_tsk_test,-114])
calculateConfusionMatrix(nis_lda_predict)
performance(nis_lda_predict, measures = list(auc, mmce))