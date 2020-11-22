context("explain_scikitlearn")

source("objects_for_tests.R")

test_that("creating explainer", {
  skip_because_conda_conf_needed()
  if(!"myenv" %in% reticulate::conda_list()$name){
       create_env(system.file("extdata", "testing_environment.yml", package = "DALEXtra"))
  }
    titanic_test <- read.csv(system.file("extdata", "titanic_test.csv", package = "DALEXtra"))
    explainer_1 <- explain_scikitlearn(system.file("extdata", "scikitlearn.pkl", package = "DALEXtra"),
    condaenv = "myenv", data = titanic_test[,1:17], y = titanic_test$survived, verbose = FALSE)

    explainer_2 <- explain_scikitlearn(system.file("extdata", "scikitlearn.pkl", package = "DALEXtra"),
                                     yml = system.file("extdata", "testing_environment.yml", package = "DALEXtra"),
                                     data = titanic_test[,1:17], y = titanic_test$survived, verbose = FALSE)

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


})





