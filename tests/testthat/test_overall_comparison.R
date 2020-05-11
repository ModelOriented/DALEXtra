context("overall_comaprison")

library("mlr")
task <- mlr::makeRegrTask(
  id = "R",
  data = apartments,
  target = "m2.price"
)
learner_lm <- mlr::makeLearner(
  "regr.lm"
)
model_lm <- mlr::train(learner_lm, task)
explainer_lm <- explain_mlr(model_lm, apartmentsTest, apartmentsTest$m2.price, label = "LM", verbose = FALSE)

learner_rf <- mlr::makeLearner(
  "regr.randomForest"
)
model_rf <- mlr::train(learner_rf, task)
explainer_rf <- explain_mlr(model_rf, apartmentsTest, apartmentsTest$m2.price, label = "RF", verbose = FALSE)

learner_gbm <- mlr::makeLearner(
  "regr.gbm"
)
model_gbm <- mlr::train(learner_gbm, task)
explainer_gbm <- explain_mlr(model_gbm, apartmentsTest, apartmentsTest$m2.price, label = "gbm", verbose = FALSE)


test_that("test overall regression", {

  expect_error(overall_comparison(model_lm, list(explainer_rf, explainer_gbm), type = "regression"))

  expect_true(is.list(overall_comparison(explainer_lm, list(explainer_rf, explainer_gbm), type = "regression")))

  expect_identical(class(overall_comparison(explainer_lm, list(explainer_rf, explainer_gbm), type = "regression")), "overall_comparison")

  plot_data <- overall_comparison(explainer_lm, list(explainer_rf, explainer_gbm), type = "regression")
  expect_is(plot(plot_data), "list")

})



test_that("test overall classif", {

  expect_true(is.list(overall_comparison(explainer_rf_classif, explainer_rpart_classif, type = "classification")))

  expect_identical(class(overall_comparison(explainer_rf_classif, explainer_rpart_classif, type = "classification")), "overall_comparison")

  plot_data <- overall_comparison(explainer_rf_classif, explainer_rpart_classif, type = "classification")
  expect_is(plot(plot_data), "list")

})




