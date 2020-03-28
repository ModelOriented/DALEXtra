context("test prints")

source("objects_for_tests.R")

test_that("prints", {
  titanic_test <- read.csv(system.file("extdata", "titanic_test.csv", package = "DALEXtra"))
  titanic_train <- read.csv(system.file("extdata", "titanic_train.csv", package = "DALEXtra"))
  skip_because_conda_conf_needed()
  explainer <- explain_scikitlearn(system.file("extdata", "scikitlearn.pkl", package = "DALEXtra"),
                                     yml = system.file("extdata", "testing_environment.yml", package = "DALEXtra"),
                                     data = titanic_test[,1:17], y = titanic_test$survived)
  expect_success(expect_output(print(explainer$param_set), NULL))

})
