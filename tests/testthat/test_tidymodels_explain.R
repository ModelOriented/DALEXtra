library("tidymodels")
library("recipes")
data <- titanic_imputed
data$survived <- as.factor(data$survived)
rec <- recipe(survived ~ ., data = data) %>%
       step_normalize(fare)
model <- decision_tree(tree_depth = 25) %>%
         set_engine("rpart") %>%
         set_mode("classification")

wflow <- workflow() %>%
         add_recipe(rec) %>%
         add_model(model)


model_fitted <- wflow %>%
                fit(data = data)

test_that("explain_tidymodels works for workflow", {
  expect_error(explainer_classif <- explain_tidymodels(model_fitted, data = titanic_imputed, y = titanic_imputed$survived), NA)
  expect_is(explainer_classif, "explainer")
  expect_is(explainer_classif$y_hat, "numeric")
  
})

model2 <- decision_tree(tree_depth = 2) %>%
  set_engine("rpart") %>%
  set_mode("classification")


library(stacks)



wfs <- workflow_set(
  preproc = list(rec),  
  models = list(model, model2), 
  cross = TRUE)

wfs_rs <- workflow_map(
  wfs,
  "fit_resamples",
  resamples = titanic_folds,
  control = control_grid(
    save_pred = TRUE,
    parallel_over = "everything",
    save_workflow = TRUE            
    )
  )

wfs_stack <- 
  stacks() %>% 
  add_candidates(wfs_rs)
blend_ens <- blend_predictions(wfs_stack, penalty = 10^seq(-2, 0, length = 10))
ens_fit <- fit_members(blend_ens)

test_that("explain_tidymodels works for stack", {
  expect_error(explainer_stack <- explain_tidymodels(ens_fit, data = titanic_imputed, y = titanic_imputed$survived), NA)
  expect_is(explainer_stack, "explainer")
  expect_is(explainer_stack$y_hat, "numeric")
  
})

