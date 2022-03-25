context("predict_surrogate")

# based on https://ema.drwhy.ai/LIME.html
library(ranger)
library(DALEX)

titanic_imputed <- DALEX::titanic_imputed
titanic_rf <- ranger(survived~., titanic_imputed, probability = TRUE)

johnny_d <- data.frame(
  class="1st", gender="male", age=8, sibsp=0, parch=0, fare=72, embarked="Southampton"
)

johnny_d_faulty <- data.frame(
  class="1st", gender="male", age=8, sibsp=0, parch=0, fare=72, embarked="Southampton",
  survived=0
)


titanic_rf_exp <- DALEX::explain(titanic_rf, titanic_imputed[,-8], titanic_imputed[,8])

model_type.dalex_explainer <- DALEXtra::model_type.dalex_explainer
predict_model.dalex_explainer <- DALEXtra::predict_model.dalex_explainer


lime_johnny_d <- predict_surrogate(explainer = titanic_rf_exp, 
                                   new_observation = johnny_d, 
                                   n_features = 3, 
                                   n_permutations = 1000,
                                   type = "lime")
lime_johnny_d_faulty <- predict_surrogate(explainer = titanic_rf_exp, 
                                          new_observation = johnny_d_faulty, 
                                          n_features = 3, 
                                          n_permutations = 1000,
                                          type = "lime")

testthat::test_that("normal case", {
  testthat::expect_is((as.data.frame(lime_johnny_d)), "data.frame")
  testthat::expect_is(plot(lime_johnny_d), "ggplot")
})

testthat::test_that("faulty case", {
  testthat::expect_is((as.data.frame(lime_johnny_d_faulty)), "data.frame")
  testthat::expect_is(plot(lime_johnny_d_faulty), "ggplot")
})