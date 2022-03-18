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
  "regr.ranger"
)

model_rf <- train(learner_rf, task)
explainer_rf <- explain_mlr(model_rf, apartmentsTest, apartmentsTest$m2.price, label = "RF")

learner_gbm <- makeLearner(
  "regr.gbm"
)

model_gbm <- train(learner_gbm, task)
explainer_gbm <- explain_mlr(model_gbm, apartmentsTest, apartmentsTest$m2.price, label = "GBM")


test_that("funnel_measure parameters", {

funnel_measure(explainer_lm, list(explainer_rf, explainer_gbm),
               nbins = 5, measure_function = DALEX::loss_root_mean_square, show_info = FALSE)





  expect_error(funnel_measure(model_lm, list(explainer_rf, explainer_gbm),
                              nbins = 5, measure_function = DALEX::loss_root_mean_square, show_info = FALSE))

  expect_true(is.list(funnel_measure(explainer_lm, list(explainer_rf, explainer_gbm),
                                     nbins = 5, show_info = FALSE)))

  expect_identical(class(funnel_measure(explainer_lm, list(explainer_rf, explainer_gbm),
                                        nbins = 5, show_info = FALSE)
  ), "funnel_measure")

  plot_data <- funnel_measure(explainer_lm, list(explainer_rf, explainer_gbm),
                              nbins = 5, measure_function = DALEX::loss_root_mean_square, show_info = FALSE)
  expect_is(plot(plot_data), "list")

})

test_that("funnel_measure_classif", {
  expect_error(funnel_measure(explainer_rf_classif, explainer_rpart_classif,
                              nbins = 5, show_info = FALSE), NA)

})

test_that("funnel_measure_classif", {
  expect_error(funnel_measure(explainer_rf_multiclassif, explainer_rpart_multiclassif,
                              nbins = 5, show_info = FALSE), NA)

})




