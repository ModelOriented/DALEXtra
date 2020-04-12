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
  h2o::h2o.shutdown(prompt = FALSE)
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
  h2o::h2o.shutdown(prompt = FALSE)
})

test_that("automl example", {
  skip_if_no_java()
  data <- DALEX::titanic_imputed

  # init h2o
  h2o::h2o.init()

  # split the data
  h2o_split <- h2o::h2o.splitFrame(h2o::as.h2o(data))
  train <- h2o_split[[1]]
  test <- as.data.frame(h2o_split[[2]])

  # h2o automl takes target as factor
  train$survived <- h2o::as.factor(train$survived)

  # fit a model
  automl <- h2o::h2o.automl(y = "survived",
                            training_frame = train,
                            max_runtime_secs = 15)

  # stop h2o progress printing
  h2o::h2o.no_progress()

  # create an explainer for the model
  explainer <- DALEXtra::explain_h2o(automl,
                                     data = test,
                                     y = test$survived,
                                     label = "h2o")
  
  testthat::expect_is(explainer, "explainer")
  testthat::expect_false(class(explainer$model)=="H2OAutoML")
  h2o::h2o.shutdown(prompt = FALSE)
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

