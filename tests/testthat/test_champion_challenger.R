context("champion_challenger")

library("mlr")
library("DALEXtra")
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
explainer_gbm <- explain_mlr(model_gbm, apartmentsTest, apartmentsTest$m2.price, label = "GBM", verbose = FALSE)


plot_data_1 <- funnel_measure(explainer_lm, list(explainer_rf, explainer_gbm),
                            nbins = 5, measure_function = DALEX::loss_root_mean_square)
plot_data_2 <- training_test_comparison(explainer_lm, list(explainer_rf, explainer_gbm), training_data = apartments, training_y = apartments$m2.price)
plot_data_3 <- overall_comparison(explainer_lm, list(explainer_rf, explainer_gbm), type = "regression")

fi <- ingredients::feature_importance(explainer_rf)

report_data <- list(plot_data_1, plot_data_2, plot_data_3, feature_importance = fi)


test_that("Report generates without errors", {
  expect_error(champion_challenger(report_data, dot_size = 3), NA)

})
