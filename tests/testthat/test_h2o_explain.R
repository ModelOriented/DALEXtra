context("explain_h2o")

source("objects_for_tests.R")



test_that("creating explainer classif", {
  skip_if_no_java()
  titanic_test <- read.csv(system.file("extdata", "titanic_test.csv", package = "DALEXtra"))
  titanic_train <- read.csv(system.file("extdata", "titanic_train.csv", package = "DALEXtra"))
  library("DALEX")
  h2o::h2o.init()
  h2o::h2o.no_progress()
  titanic_h2o <- h2o::as.h2o(titanic_train)
  titanic_h2o["survived"] <- h2o::as.factor(titanic_h2o["survived"])
  titanic_test_h2o <- h2o::as.h2o(titanic_test)
  model <- h2o::h2o.gbm(
    training_frame = titanic_h2o,
    y = "survived",
    distribution = "bernoulli",
    ntrees = 500,
    max_depth = 4,
    min_rows =  12,
    learn_rate = 0.001
  )
  explainer <- explain_h2o(model, titanic_test[,1:17], titanic_test[,18])
  expect_is(explainer, "explainer")
  expect_is(explainer$y_hat, "numeric")

})

test_that("creating explainer regr", {
  skip_if_no_java()
  titanic_test <- read.csv(system.file("extdata", "titanic_test.csv", package = "DALEXtra"))
  titanic_train <- read.csv(system.file("extdata", "titanic_train.csv", package = "DALEXtra"))
  library("DALEX")
  h2o::h2o.init()
  h2o::h2o.no_progress()
  titanic_h2o <- h2o::as.h2o(titanic_train)
  titanic_h2o["survived"] <- h2o::as.factor(titanic_h2o["survived"])
  titanic_test_h2o <- h2o::as.h2o(titanic_test)
  model <- h2o::h2o.gbm(
    training_frame = titanic_h2o,
    y = "fare",
    ntrees = 500,
    max_depth = 4,
    min_rows =  12,
    learn_rate = 0.001
  )
  explainer <- explain_h2o(model, predict_function = yhat, titanic_test, titanic_test$fare)
  expect_is(explainer, "explainer")
  expect_is(explainer$y_hat, "numeric")

})


test_that("y is numeric", {
  skip_if_no_java()
  titanic_test <- read.csv(system.file("extdata", "titanic_test.csv", package = "DALEXtra"))
  titanic_train <- read.csv(system.file("extdata", "titanic_train.csv", package = "DALEXtra"))
  library("DALEX")
  h2o::h2o.init()
  h2o::h2o.no_progress()
  titanic_h2o <- h2o::as.h2o(titanic_train)
  titanic_h2o["survived"] <- h2o::as.factor(titanic_h2o["survived"])
  titanic_test_h2o <- h2o::as.h2o(titanic_test)
  model <- h2o::h2o.gbm(
    training_frame = titanic_h2o,
    y = "survived",
    distribution = "bernoulli",
    ntrees = 500,
    max_depth = 4,
    min_rows =  12,
    learn_rate = 0.001
  )
  explainer <- explain_h2o(model, titanic_test[,1:17], titanic_test_h2o["survived"])
  expect_is(explainer$y, "numeric")
  h2o::h2o.shutdown(prompt = FALSE)

})

test_that("Assert when no h2o",{
  titanic_test <- read.csv(system.file("extdata", "titanic_test.csv", package = "DALEXtra"))
  titanic_train <- read.csv(system.file("extdata", "titanic_train.csv", package = "DALEXtra"))
  expect_error(yhat.H2OBinomialModel("b", titanic_test))
  expect_error(yhat.H2ORegressionModel("b", titanic_test))
})

