context("explain_mljar")

source("objects_for_tests.R")

test_that("creating explainer", {
  titanic_test <- read.csv(system.file("extdata", "titanic_test.csv", package = "DALEXtra"))
  load(system.file("extdata", "mljar.RData", package = "DALEXtra"))
  explainer <- explain_mljar(model, "Project title", data = titanic_test[,1:17], y = titanic_test[,18], verbose = FALSE, precalculate = FALSE)

  expect_is(explainer, "explainer")

})
