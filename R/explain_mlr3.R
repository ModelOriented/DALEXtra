#' Create explainer from your mlr model
#'
#' DALEX is designed to work with various black-box models like tree ensembles, linear models, neural networks etc.
#' Unfortunately R packages that create such models are very inconsistent. Different tools use different interfaces to train, validate and use models.
#' One of those tools, which is one of the most popular one is mlr3 package. We would like to present dedicated explain function for it.
#'
#'
#' @param model object - a fitted learner created with \code{mlr3}.
#' @param data data.frame or matrix - data that was used for fitting. If not provided then will be extracted from the model. Data should be passed without target column (this shall be provided as the \code{y} argument). NOTE: If target variable is present in the \code{data}, some of the functionalities my not work properly.
#' @param y numeric vector with outputs / scores. If provided then it shall have the same size as \code{data}
#' @param weights numeric vector with sampling weights. By default it's \code{NULL}. If provided then it shall have the same length as \code{data}
#' @param predict_function function that takes two arguments: model and new data and returns numeric vector with predictions
#' @param predict_function_target_column Character or numeric containing either column name or column number in the model prediction object of the class that should be considered as positive (ie. the class that is associated with probability 1). If NULL, the second column of the output will be taken for binary classification. For a multiclass classification setting that parameter cause switch to binary classification mode with 1 vs others probabilities.
#' @param residual_function function that takes three arguments: model, data and response vector y. It should return a numeric vector with model residuals for given data. If not provided, response residuals (\eqn{y-\hat{y}}) are calculated.
#' @param ... other parameters
#' @param label character - the name of the model. By default it's extracted from the 'class' attribute of the model
#' @param verbose if TRUE (default) then diagnostic messages will be printed.
#' @param precalculate if TRUE (default) then 'predicted_values' and 'residuals' are calculated when explainer is created.
#' @param colorize if TRUE (default) then \code{WARNINGS}, \code{ERRORS} and \code{NOTES} are colorized. Will work only in the R console.
#' @param model_info a named list (\code{package}, \code{version}, \code{type}) containg information about model. If \code{NULL}, \code{DALEX} will seek for information on it's own.
#' @param type type of a model, either \code{classification} or \code{regression}. If not specified then \code{type} will be extracted from \code{model_info}.
#'
#' @return explainer object (\code{\link[DALEX]{explain}}) ready to work with DALEX
#'
#' @import DALEX
#' @importFrom stats predict
#' @importFrom DALEX yhat
#'
#' @rdname explain_mlr3
#' @export
#' @examples
#'library("DALEXtra")
#' library(mlr3)
#' titanic_imputed$survived <- as.factor(titanic_imputed$survived)
#' task_classif <- TaskClassif$new(id = "1", backend = titanic_imputed, target = "survived")
#' learner_classif <- lrn("classif.rpart", predict_type = "prob")
#' learner_classif$train(task_classif)
#' explain_mlr3(learner_classif, data = titanic_imputed,
#'              y = as.numeric(as.character(titanic_imputed$survived)))
#'
#'
#' task_regr <- TaskRegr$new(id = "2", backend = apartments, target = "m2.price")
#' learner_regr <- lrn("regr.rpart")
#' learner_regr$train(task_regr)
#' explain_mlr3(learner_regr, data = apartments, apartments$m2.price)
#'


explain_mlr3 <-
  function(model,
           data = NULL,
           y = NULL,
           weights = NULL,
           predict_function = NULL,
           predict_function_target_column = NULL,
           residual_function = NULL,
           ...,
           label = NULL,
           verbose = TRUE,
           precalculate = TRUE,
           colorize = TRUE,
           model_info = NULL,
           type = NULL) {
    explain(
      model,
      data = data,
      y = y,
      weights = weights,
      predict_function = predict_function,
      predict_function_target_column = predict_function_target_column,
      residual_function = residual_function,
      ...,
      label = label,
      verbose = verbose,
      precalculate = precalculate,
      colorize = colorize,
      model_info = model_info,
      type = type
    )



  }
