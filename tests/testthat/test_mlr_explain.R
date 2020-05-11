context("explain_mlr")

source("objects_for_tests.R")



test_that("creating explainer classif", {
  titanic_test <- read.csv(system.file("extdata", "titanic_test.csv", package = "DALEXtra"))
  titanic_train <- read.csv(system.file("extdata", "titanic_train.csv", package = "DALEXtra"))
  library("mlr")
  task <- mlr::makeClassifTask(
    id = "R",
    data = titanic_train,
    target = "survived"
  )
  learner <- mlr::makeLearner(
    "classif.gbm",
    par.vals = list(
      distribution = "bernoulli",
      n.trees = 500,
      interaction.depth = 4,
      n.minobsinnode = 12,
      shrinkage = 0.001,
      bag.fraction = 0.5,
      train.fraction = 1
    ),
    predict.type = "prob"
  )
  gbm <- mlr::train(learner, task)
  explainer <- explain_mlr(gbm, titanic_test[,1:17], titanic_test[,18], verbose = FALSE)
  expect_is(explainer, "explainer")
  expect_is(explainer$y_hat, "numeric")

})

test_that("creating explainer regr", {
  titanic_test <- read.csv(system.file("extdata", "titanic_test.csv", package = "DALEXtra"))
  titanic_train <- read.csv(system.file("extdata", "titanic_train.csv", package = "DALEXtra"))
  library("mlr")
  task <- mlr::makeRegrTask(
    id = "R",
    data = titanic_train,
    target = "fare"
  )
  learner <- mlr::makeLearner(
    "regr.gbm",
    par.vals = list(
      n.trees = 500,
      interaction.depth = 4,
      n.minobsinnode = 12,
      shrinkage = 0.001,
      bag.fraction = 0.5,
      train.fraction = 1
    )
  )
  gbm <- mlr::train(learner, task)
  explainer <- explain_mlr(gbm, data = titanic_test, predict_function = yhat, y = titanic_test$fare, verbose = FALSE)
  expect_is(explainer, "explainer")
  expect_is(explainer$y_hat, "numeric")

})

test_that("Assert when no mlr",{
  titanic_test <- read.csv(system.file("extdata", "titanic_test.csv", package = "DALEXtra"))
  titanic_train <- read.csv(system.file("extdata", "titanic_train.csv", package = "DALEXtra"))
  a <- list()
  a$task.desc$type <- "b"
  expect_error(yhat.WrappedModel(a, titanic_test))
})

