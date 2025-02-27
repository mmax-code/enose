library(mlr3verse)
library(mlr3)
library(mlr3learners)
library(mlr3tuning)
library(mlr3tuningspaces)
library(ranger)
library(mlr3filters)
library(ggplot2)


source("...")

set.seed(1)
data = final
rows = sample(nrow(data))
data = data[rows,]


data = final
data$label = as.factor(data$label)
rownames(data) = data$id
data$id = NULL
task_enose = TaskClassif$new(id = "id", backend = data, target = "label")
task_enose



################################################################################
# Nested Resampling for Performance Eval only 
################################################################################

### RF

learner = lrn("classif.ranger", predict_type = "prob")
tuning_space = lts("classif.ranger.default")
learner$param_set$values = tuning_space$values

at = auto_tuner(
  tuner = tnr("random_search"),
  learner = learner,
  resampling = rsmp("cv", folds = 7),
  measure = msr("classif.auc"),
  term_evals = 1000
)

outer_resampling = rsmp("cv", folds = 5)

rf = resample(task_enose, at, outer_resampling, store_models = TRUE)

rf$aggregate()
rf$score()
extract_inner_tuning_results(rf)
res = rf$predictions()

res_list = list()
for (i in  1:5){
  res = rf$predictions()[[i]]
  ce = res$score(measures = msr("classif.ce"))
  sens = res$score(measures = msr("classif.sensitivity"))
  spec = res$score(measures = msr("classif.specificity"))
  tnr = res$score(measures = msr("classif.fpr"))
  tpr = res$score(measures = msr("classif.fnr"))
  auc = res$score(measures = msr("classif.auc"))
  res_list[[i]] = c(ce,sens,spec,tnr,tpr, auc)
}

ce = mean(sapply(res_list,"[[",1))
sens = mean(sapply(res_list,"[[",2))
spec = mean(sapply(res_list,"[[",3))
tnr = mean(sapply(res_list,"[[",4))
tpr = mean(sapply(res_list,"[[",5))
auc = mean(sapply(res_list,"[[",6))



### Glmnet


learner = lrn("classif.glmnet")

learner$param_set$ids()
as.data.table(learner$param_set)

learner = lrn("classif.glmnet", predict_type = "prob")
tuning_space = lts("classif.glmnet.default")
learner$param_set$values = tuning_space$values


at = auto_tuner(
  tuner = tnr("random_search"),
  learner = learner,
  resampling = rsmp("cv", folds = 7),
  measure = msr("classif.auc"),
  term_evals = 10
)

outer_resampling = rsmp("cv", folds = 5)

glm = resample(task_enose, at, outer_resampling, store_models = TRUE)


glm$aggregate()
glm$score()
extract_inner_tuning_results(glm)
res = glm$predictions()

res_list = list()
for (i in  1:5){
  res = glm$predictions()[[i]]
  ce = res$score(measures = msr("classif.ce"))
  sens = res$score(measures = msr("classif.sensitivity"))
  spec = res$score(measures = msr("classif.specificity"))
  tnr = res$score(measures = msr("classif.fpr"))
  tpr = res$score(measures = msr("classif.fnr"))
  auc = res$score(measures = msr("classif.auc"))
  res_list[[i]] = c(ce,sens,spec,tnr,tpr, auc)
}

ce = mean(sapply(res_list,"[[",1))
sens = mean(sapply(res_list,"[[",2))
spec = mean(sapply(res_list,"[[",3))
tnr = mean(sapply(res_list,"[[",4))
tpr = mean(sapply(res_list,"[[",5))
auc = mean(sapply(res_list,"[[",6))



################################################################################
# final automatic tuning RF, GLMNet, SVM
################################################################################

### RF

learner = lrn("classif.ranger", predict_type = "prob")
tuning_space = lts("classif.ranger.default")
learner$param_set$values = tuning_space$values

at = auto_tuner(
  tuner = tnr("random_search"),
  learner = learner,
  resampling = rsmp("cv", folds = 5),
  measure = msr("classif.auc"),
  term_evals = 1000
)

at$train(task_enose)
at$model



### Glmnet

learner = lrn("classif.glmnet", predict_type = "prob")
tuning_space = lts("classif.glmnet.default")
learner$param_set$values = tuning_space$values


at = auto_tuner(
  tuner = tnr("random_search"),
  learner = learner,
  resampling = rsmp("cv", folds = 5),
  measure = msr("classif.auc"),
  term_evals = 1000
)


at$train(task_enose)
at$model


### SVM

learner = lrn("classif.svm", predict_type = "prob")
tuning_space = lts("classif.svm.default")
learner$param_set$values = tuning_space$values
learner$param_set$values$type = "C-classification"

at = auto_tuner(
  tuner = tnr("random_search"),
  learner = learner,
  resampling = rsmp("cv", folds = 5),
  measure = msr("classif.auc"),
  term_evals = 1000
)

at$train(task_enose)
at$model