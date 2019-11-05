context("explain_mlr")

library("DALEXtra")
library("mlr3")

test_that("creating explainer classif", {
  titanic_imputed$survived <- as.factor(titanic_imputed$survived)
  task_classif <- TaskClassif$new(id = "1", backend = titanic_imputed, target = "survived")
  learner_classif <- lrn("classif.rpart", predict_type = "prob")
  learner_classif$train(task_classif)
  explainer_classif <- explain_mlr3(learner_classif, data = titanic_imputed, y = as.numeric(as.character(titanic_imputed$survived)))
  expect_is(explainer_classif, "explainer")
  expect_is(explainer_classif$y_hat, "numeric")

})

test_that("creating explainer regr", {

  task_regr <- TaskRegr$new(id = "2", backend = apartments, target = "m2.price")
  learner_regr <- lrn("regr.rpart")
  learner_regr$train(task_regr)
  explainer_regr <- explain_mlr3(learner_regr, data = apartments, apartments$m2.price)
  expect_is(explainer_regr, "explainer")
  expect_is(explainer_regr$y_hat, "numeric")

})

