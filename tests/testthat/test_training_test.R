context("training_test_comparison")

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
explainer_lm <- explain_mlr(model_lm, apartmentsTest, apartmentsTest$m2.price, label = "LM", verbose = FALSE)

learner_rf <- makeLearner(
  "regr.randomForest"
)

model_rf <- train(learner_rf, task)
explainer_rf <- explain_mlr(model_rf, apartmentsTest, apartmentsTest$m2.price, label = "RF", verbose = FALSE)

learner_gbm <- makeLearner(
  "regr.gbm"
)

model_gbm <- train(learner_gbm, task)
explainer_gbm <- explain_mlr(model_gbm, apartmentsTest, apartmentsTest$m2.price, label = "GBM", verbose = FALSE)


test_that("trainig_test parameters", {

  expect_error(training_test_comparison(model_lm, list(explainer_rf, explainer_gbm), apartments, apartments$m2.price))

  expect_true(is.list(training_test_comparison(explainer_lm, list(explainer_rf, explainer_gbm), apartments, apartments$m2.price)))

  expect_identical(class(training_test_comparison(explainer_lm, list(explainer_rf, explainer_gbm), apartments, apartments$m2.price)), "training_test_comparison")

  plot_data <- training_test_comparison(explainer_lm, list(explainer_rf, explainer_gbm), apartments, apartments$m2.price)
  expect_is(plot(plot_data), "gg")

})

test_that("funnel_measure_classif", {
  expect_error(training_test_comparison(explainer_rf_classif, explainer_rpart_classif, titanic_imputed, titanic_imputed$survived), NA)

})

test_that("funnel_measure_classif", {
  expect_error(training_test_comparison(explainer_rf_multiclassif, explainer_rpart_multiclassif, HR, HR$status), NA)

})




