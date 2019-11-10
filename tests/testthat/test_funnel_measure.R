context("Check funnel_measure() function")

task <- mlr::makeRegrTask(
  id = "R",
  data = apartments,
  target = "m2.price"
)
learner_lm <- mlr::makeLearner(
  "regr.lm"
)
model_lm <- mlr::train(learner_lm, task)
explainer_lm <- explain_mlr(model_lm, apartmentsTest, apartmentsTest$m2.price, label = "LM")

learner_rf <- mlr::makeLearner(
  "regr.randomForest"
)
model_rf <- mlr::train(learner_rf, task)
explainer_rf <- explain_mlr(model_rf, apartmentsTest, apartmentsTest$m2.price, label = "RF")

learner_gbm <- mlr::makeLearner(
  "regr.gbm"
)
model_gbm <- mlr::train(learner_gbm, task)
explainer_gbm <- explain_mlr(model_gbm, apartmentsTest, apartmentsTest$m2.price, label = "GBM")


funnel_measure(explainer_lm, list(explainer_rf, explainer_gbm),
               nbins = 5, measure_function = DALEX::loss_root_mean_square)




test_that("funnel_measure parameters", {
  expect_error(funnel_measure(model_lm, list(explainer_rf, explainer_gbm),
                              nbins = 5, measure_function = DALEX::loss_root_mean_square)
  )
})

test_that("funnel_measure parameters", {
  expect_true(is.list(funnel_measure(explainer_lm, list(explainer_rf, explainer_gbm),
                              nbins = 5, measure_function = DALEX::loss_root_mean_square)
  ))
})

test_that("funnel_measure parameters", {
  expect_identical(class(funnel_measure(explainer_lm, list(explainer_rf, explainer_gbm),
                                         nbins = 5, measure_function = DALEX::loss_root_mean_square)
  ), "funnel_measure")
})