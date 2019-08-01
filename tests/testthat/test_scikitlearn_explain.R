context("explain_scikitlearn")

source("objects_for_tests.R")

test_that("creating explainer", {
  skip_if_no_conda()
  if(!"myenv" %in% reticulate::conda_list()$name){
       create_env(system.file("extdata", "scikitlearn_unix.yml", package = "DALEXtra"))
  }
    titanic_test <- read.csv(system.file("extdata", "titanic_test.csv", package = "DALEXtra"))
    explainer_1 <- explain_scikitlearn(system.file("extdata", "scikitlearn.pkl", package = "DALEXtra"),
    condaenv = "myenv", data = titanic_test[,1:17], y = titanic_test$survived)

    skip_if_no_conda()
    explainer_2 <- explain_scikitlearn(system.file("extdata", "scikitlearn.pkl", package = "DALEXtra"),
                                     yml = system.file("extdata", "scikitlearn_unix.yml", package = "DALEXtra"),
                                     data = titanic_test[,1:17], y = titanic_test$survived)

    expect_is(explainer_1, "explainer")
    expect_is(explainer_1$y_hat, "numeric")
    expect_is(explainer_2, "explainer")
    expect_is(explainer_2$y_hat, "numeric")

})

test_that("if check", {
  expect_error(explain_scikitlearn("path.pkl",
                                   condaenv = "conda",
                                   env = "env"),
               "Only one argument from condaenv and env can be different from NULL", fixed = TRUE)
  expect_error(explain_scikitlearn("path.pkl",
                                   env = "env"))
  expect_error(explain_scikitlearn(system.file("extdata", "scikitlearn.pkl", package = "DALEXtra"),
                                   condaenv = conda_list()$name[1]))

})

test_that("env change error", {

  skip_if_no_conda()
  if(!"myenv" %in% reticulate::conda_list()$name){
    create_env(system.file("extdata", "scikitlearn_unix.yml", package = "DALEXtra"))
  }
  py_discover_config()
  expect_error(explain_scikitlearn(system.file("extdata", "scikitlearn.pkl", package = "DALEXtra"),
                                   condaenv = conda_list()$name[1]))

})



