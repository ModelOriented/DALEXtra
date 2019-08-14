context("champion_challenger")

source("objects_for_tests.R")

test_that("Error does not occur when create report", {
  skip_if_no_conda()
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
  explainer_mlr <- explain_mlr(gbm, titanic_test[,1:17], titanic_test[,18])-> explainermlr
  explainer_scikit <- explain_scikitlearn(system.file("extdata",
                                                      "scikitlearn.pkl",
                                                      package = "DALEXtra"),
                                          yml = system.file("extdata",
                                                            "scikitlearn_unix.yml",
                                                            package = "DALEXtra"),
                                          data = titanic_test[,1:17],
                                          y = titanic_test$survived)

  skip("Test temporarily unavaliable due to other packages problems")
  expect_error(champion_challenger(explainer_mlr, explainer_scikit, type = "classification",
                         variable = c("fare", "age")), NA)

})

test_that("If checks", {
  expect_error(champion_challenger(explainer_mlr, explainer_scikit))
  expect_error(champion_challenger("1", 2))
})
