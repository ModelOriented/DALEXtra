context("create_env")

source("objects_for_tests.R")

test_that("creating env", {
  skip_if_windows()
  skip_if_no_conda()
  if("myenv" %in% reticulate::conda_list()$name){
    reticulate::conda_remove("myenv")
    name <- create_env(system.file("extdata", "scikitlearn_unix.yml", package = "DALEXtra"),
                       condaenv = paste(sub('[/][^/]+$', '', reticulate::conda_binary()), "/..", sep = ""))
  } else{
      name <- create_env(system.file("extdata", "scikitlearn_unix.yml", package = "DALEXtra"),
                 condaenv = paste(sub('[/][^/]+$', '', reticulate::conda_binary()), "/..", sep = ""))
  }
  expect_is(name, "character")


})
