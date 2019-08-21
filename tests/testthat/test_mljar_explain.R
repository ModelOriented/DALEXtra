context("explain_mljar")

source("objects_for_tests.R")

test_that("creating explainer", {
  load(system.file("extdata", "mljar.RData", package = "DALEXtra"))
  explainer <- explain_mljar(model, "Project title")

  expect_is(explainer, "explainer")

})
