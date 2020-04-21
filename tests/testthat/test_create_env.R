context("create_env")

source("objects_for_tests.R")

test_that("creating env", {
  skip_because_conda_conf_needed()
  reticulate::use_condaenv("myenv")
  if ("myenv" %in% reticulate::conda_list()$name) {
    reticulate::conda_remove("myenv")
    name <-
      create_env(
        system.file("extdata", "testing_environment.yml", package = "DALEXtra")
        )

  } else{
    name <-
      create_env(
        system.file("extdata", "testing_environment.yml", package = "DALEXtra"),
      )
  }
  expect_is(name, "character")


})
test_that("if check", {
  skip_because_conda_conf_needed()
  reticulate::use_condaenv("myenv")
  if (.Platform$OS.type == "unix") {
    expect_success(expect_message(create_env(yml = system.file("extdata", "testing_environment.yml", package = "DALEXtra")),
                                  "not specified"))
  }
  if ("myenv" %in% reticulate::conda_list()$name){
    expect_success(expect_message(create_env(yml = system.file("extdata", "testing_environment.yml", package = "DALEXtra")),
                                  "exists"))
  }
})

test_that("errors checks", {
  skip_because_conda_conf_needed()
  skip_if_windows()
  if ("myenv" %in% reticulate::conda_list()$name) {
    reticulate::conda_remove("myenv")
  }
    expect_error(create_env(
      system.file("extdata", "scikitlearn.yml", package = "DALEXtra"),
      condaenv = "wrong_path"

    ))
})



