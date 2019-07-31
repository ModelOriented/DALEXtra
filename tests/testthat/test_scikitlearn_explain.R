context("explain_scikitlearn")

source("objects_for_tests.R")

test_that("creating explainer", {
  if("myenv" %in% reticulate::conda_list()$name){
       titanic_test <- read.csv(system.file("extdata", "titanic_test.csv", package = "DALEXtra"))
       explainer <- explain_scikitlearn(system.file("extdata", "scikitlearn.pkl", package = "DALEXtra"),
       condaenv = "myenv", data = titanic_test[,1:17], y = titanic_test$survived)
  } else{
    skip_if_windows()
    skip_if_no_conda()
    titanic_test <- read.csv(system.file("extdata", "titanic_test.csv", package = "DALEXtra"))
    explainer <- explain_scikitlearn(system.file("extdata", "scikitlearn.pkl", package = "DALEXtra"),
                                     yml = system.file("extdata", "scikitlearn_unix.yml", package = "DALEXtra"),
                                     condaenv = paste(sub('[/][^/]+$', '', reticulate::conda_binary()), "/..", sep = "") , data = titanic_test[,1:17], y = titanic_test$survived)
  }
  expect_is(explainer, "explainer")
  expect_is(explainer$y_hat, "numeric")

})

test_that("if check", {
  expect_error(explain_scikitlearn("path.pkl",
                                   condaenv = "conda",
                                   env = "env"),
               "Only one argument from condaenv and env can be different from NULL", fixed = TRUE)

})


