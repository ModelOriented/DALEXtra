context("funnel_measure")

library("mlr")
task <- makeRegrTask(
  id = "R",
  data = apartments,
  target = "m2.price"
)

learner_lm <- makeLearner(
  "regr.lm"
)

model_lm <- train(learner_lm, task)
explainer_lm <- explain_mlr(model_lm, apartmentsTest, apartmentsTest$m2.price, label = "LM")

learner_rf <- makeLearner(
  "regr.randomForest"
)

model_rf <- train(learner_rf, task)
explainer_rf <- explain_mlr(model_rf, apartmentsTest, apartmentsTest$m2.price, label = "RF")

learner_gbm <- makeLearner(
  "regr.gbm"
)

model_gbm <- train(learner_gbm, task)
explainer_gbm <- explain_mlr(model_gbm, apartmentsTest, apartmentsTest$m2.price, label = "GBM")

titanic_factorized <- titanic_imputed
titanic_factorized$survived <- as.factor(titanic_factorized$survived)
task_classification <- makeClassifTask(
  id = "R",
  data = titanic_factorized,
  target = "survived"
)

task_multiclassification <- makeClassifTask(
  id = "R",
  data = HR,
  target = "status"
)

learner_rf_classif <- makeLearner("classif.randomForest", predict.type = "prob")
model_rf_classif <- train(learner_rf_classif, task_classification)
explainer_rf_classif <- explain_mlr(model_rf_classif, titanic_factorized, titanic_imputed$survived, label = "RF")

learner_rpart_classif <- makeLearner("classif.rpart", predict.type = "prob")
model_rpart_classif <- train(learner_rpart_classif, task_classification)
explainer_rpart_classif <- explain_mlr(model_rpart_classif, titanic_factorized, titanic_imputed$survived, label = "RF")

learner_rf_multiclassif <- makeLearner("classif.randomForest", predict.type = "prob")
model_rf_multiclassif <- train(learner_rf_multiclassif, task_multiclassification)
explainer_rf_multiclassif <- explain_mlr(model_rf_multiclassif, HR, HR$status, label = "RF")

learner_rpart_multiclassif <- makeLearner("classif.rpart", predict.type = "prob")
model_rpart_multiclassif <- train(learner_rpart_multiclassif, task_multiclassification)
explainer_rpart_multiclassif <- explain_mlr(model_rpart_multiclassif, HR, HR$status, label = "RF")


test_that("funnel_measure parameters", {

funnel_measure(explainer_lm, list(explainer_rf, explainer_gbm),
               nbins = 5, measure_function = DALEX::loss_root_mean_square)





  expect_error(funnel_measure(model_lm, list(explainer_rf, explainer_gbm),
                              nbins = 5, measure_function = DALEX::loss_root_mean_square))

  expect_true(is.list(funnel_measure(explainer_lm, list(explainer_rf, explainer_gbm),
                                     nbins = 5)))

  expect_identical(class(funnel_measure(explainer_lm, list(explainer_rf, explainer_gbm),
                                        nbins = 5)
  ), "funnel_measure")

  plot_data <- funnel_measure(explainer_lm, list(explainer_rf, explainer_gbm),
                              nbins = 5, measure_function = DALEX::loss_root_mean_square)
  expect_is(plot(plot_data), "list")

})

test_that("funnel_measure_classif", {
  expect_error(funnel_measure(explainer_rf_classif, explainer_rpart_classif,
                              nbins = 5), NA)

})

test_that("funnel_measure_classif", {
  expect_error(funnel_measure(explainer_rf_multiclassif, explainer_rpart_multiclassif,
                              nbins = 5), NA)

})




