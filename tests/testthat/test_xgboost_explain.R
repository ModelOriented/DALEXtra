context("explain_mlr")

library("DALEXtra")
library("mlr")
library("xgboost")

test_that("creating explainer classif", {
  data <- titanic_imputed[,-8]
  model <- xgboost(data, as.factor(titanic_imputed$survived), nrounds = 10, objective = "binary:logistic", verbosity = 0)
  explainer_1 <- explain_xgboost(model, data = titanic_imputed[,-8], titanic_imputed$survived, verbose = FALSE)
  expect_is(explainer_1, "explainer")
  expect_is(explainer_1$y_hat, "numeric")


  explainer_2 <- explain_xgboost(model, data = data, titanic_imputed$survived, verbose = FALSE)
  expect_is(explainer_2, "explainer")
  expect_is(explainer_2$y_hat, "numeric")

})

test_that("creating explainer regr", {

  data <- apartments[,-6]
  model <- xgboost(data, apartments$m2.price, nrounds = 10, objective = "reg:squarederror", verbosity = 0)
  explainer_3 <- explain_xgboost(model, data = apartments[,-6], apartments$m2.price, verbose = FALSE,  encode_function = function(data) {
    as.matrix(createDummyFeatures(data))
  })
  expect_is(explainer_3, "explainer")
  expect_is(explainer_3$y_hat, "numeric")


  explainer_4 <- explain_xgboost(model, data = data, apartments$m2.price, verbose = FALSE)
  expect_is(explainer_4, "explainer")
  expect_is(explainer_4$y_hat, "numeric")

})

test_that("creating explainer multi", {

  data <- HR[,-6]
  target <- HR$status
  model <- xgboost(data, HR$status, nrounds = 10, objective = "multi:softprob", verbosity = 0)
  explainer_5 <- explain_xgboost(model, data = HR[,-6], target, verbose = TRUE)
  expect_is(explainer_5, "explainer")
  expect_is(explainer_5$y_hat, "matrix")


  explainer_6 <- explain_xgboost(model, data = data, target, verbose = FALSE)
  expect_is(explainer_6, "explainer")
  expect_is(explainer_6$y_hat, "matrix")

})

