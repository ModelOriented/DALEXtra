library("reticulate")
library("mlr")

# helper function to skip tests if we don't have the conda
skip_because_conda_conf_needed <- function() {
  if (!"CONDA_TEST" %in% names(Sys.getenv())) {
    skip("Conda test env needed for tests")
  }
}
skip_if_windows <- function() {
  if (.Platform$OS.type == "windows")
    skip("Test with unix")
}

skip_if_unix <- function() {
  if (.Platform$OS.type == "unix")
    skip("Test with windows")
}

# skip_if_no_mljar <- function() {
#   if (!"MLJAR_TOKEN" %in% names(Sys.getenv())) {
#     skip("MLJAR_TOKEN entry needed for tests")
#   }
# }

skip_if_no_java <- function() {
  if (!"JAVA" %in% names(Sys.getenv())) {
    skip("JAVA entry needed for tests")
  }
}

skip_if_osx <- function() {
# Needed becasue conda cannot remove pacakges while github actions osx build
  if (Sys.info()["sysname"] == "Darwin") {
    skip("Cannot test it with osx")
  }
}

titanic_factorized <- titanic_imputed
titanic_factorized$survived <- as.factor(titanic_factorized$survived)
task_classification <- makeClassifTask(
  id = "R",
  data = titanic_factorized,
  target = "survived"
)

task_multiclassification <- makeClassifTask(
  id = "R",
  data = HR,
  target = "status"
)

learner_rf_classif <- makeLearner("classif.randomForest", predict.type = "prob")
model_rf_classif <- train(learner_rf_classif, task_classification)
explainer_rf_classif <- explain_mlr(model_rf_classif, titanic_factorized, titanic_imputed$survived, label = "RF", verbose = FALSE)

learner_rpart_classif <- makeLearner("classif.rpart", predict.type = "prob")
model_rpart_classif <- train(learner_rpart_classif, task_classification)
explainer_rpart_classif <- explain_mlr(model_rpart_classif, titanic_factorized, titanic_imputed$survived, label = "RF", verbose = FALSE)

learner_rf_multiclassif <- makeLearner("classif.randomForest", predict.type = "prob")
model_rf_multiclassif <- train(learner_rf_multiclassif, task_multiclassification)
explainer_rf_multiclassif <- explain_mlr(model_rf_multiclassif, HR, HR$status, label = "RF", verbose = FALSE)

learner_rpart_multiclassif <- makeLearner("classif.rpart", predict.type = "prob")
model_rpart_multiclassif <- train(learner_rpart_multiclassif, task_multiclassification)
explainer_rpart_multiclassif <- explain_mlr(model_rpart_multiclassif, HR, HR$status, label = "RF", verbose = FALSE)
