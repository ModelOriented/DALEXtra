context("explain_keras")

source("objects_for_tests.R")

test_that("creating explainer", {
 skip_because_conda_conf_needed()
 create_env(system.file("extdata", "testing_environment.yml", package = "DALEXtra"))
 test_data <- read.csv("https://raw.githubusercontent.com/jbrownlee/Datasets/master/pima-indians-diabetes.data.csv", sep = ",")
 explainer_1 <- explain_keras(system.file("extdata", "keras.pkl", package = "DALEXtra"),
                                    condaenv = "myenv", data = test_data[,1:8], y = test_data[,9])

 explainer_2 <- explain_keras(system.file("extdata", "keras.pkl", package = "DALEXtra"),
                                    yml = system.file("extdata", "testing_environment.yml", package = "DALEXtra"),
                                    data = test_data[,1:8], y = test_data[,9])

 expect_is(explainer_1, "explainer")
 expect_is(explainer_1$y_hat, "numeric")
 expect_is(explainer_2, "explainer")
 expect_is(explainer_2$y_hat, "numeric")

})

test_that("if check", {
  expect_error(explain_keras("path.pkl",
                                   condaenv = "conda",
                                   env = "env"),
               "Only one argument from condaenv and env can be different from NULL", fixed = TRUE)
  expect_error(explain_keras("path.pkl",
                                   env = "wrong_env"))


})




