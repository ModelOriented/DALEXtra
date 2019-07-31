context("create_env")

source("objects_for_tests.R")

test_that("creating env", {
  skip_if_no_conda()
  if ("myenv" %in% reticulate::conda_list()$name) {
    reticulate::conda_remove("myenv")
    name <-
      create_env(
        system.file("extdata", "scikitlearn_unix.yml", package = "DALEXtra")
        )

  } else{
    name <-
      create_env(
        system.file("extdata", "scikitlearn_unix.yml", package = "DALEXtra"),
      )
  }
  expect_is(name, "character")


})
test_that("if check", {
  if (.Platform$OS.type == "unix") {
    expect_success(expect_message(create_env(yml = system.file("extdata", "scikitlearn_unix.yml", package = "DALEXtra")),
                                  "not specified"))
  }
  if ("myenv" %in% reticulate::conda_list()$name){
    expect_success(expect_message(create_env(yml = system.file("extdata", "scikitlearn_unix.yml", package = "DALEXtra")),
                                  "exists"))
  }
})



