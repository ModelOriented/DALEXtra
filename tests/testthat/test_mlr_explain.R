context("explain_mlr")

source("objects_for_tests.R")



test_that("creating explainer", {
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
  explainer <- explain_mlr(gbm, titanic_test[,1:17], titanic_test[,18])
  expect_is(explainer, "explainer")
  expect_is(explainer$y_hat, "numeric")

})

