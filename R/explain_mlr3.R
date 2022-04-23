#' Create explainer from your mlr model
#'
#' DALEX is designed to work with various black-box models like tree ensembles, linear models, neural networks etc.
#' Unfortunately R packages that create such models are very inconsistent. Different tools use different interfaces to train, validate and use models.
#' One of those tools, which is one of the most popular one is mlr3 package. We would like to present dedicated explain function for it.
#'
#'
#' @inheritParams DALEX::explain
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
           colorize = !isTRUE(getOption('knitr.in.progress')),
           model_info = NULL,
           type = NULL) {
    explain(
      mlr3::as_learner(model),
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
