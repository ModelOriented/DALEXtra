#' Create explainer from your xgboost model
#'
#' DALEX is designed to work with various black-box models like tree ensembles, linear models, neural networks etc.
#' Unfortunately R packages that create such models are very inconsistent. Different tools use different interfaces to train, validate and use models.
#' One of those tools, we would like to make more accessible is the xgboost package.
#'
#'
#' @inheritParams DALEX::explain
#'
#' @return explainer object (\code{\link[DALEX]{explain}}) ready to work with DALEX
#'
#' @import DALEX
#' @importFrom DALEX yhat
#'
#'
#' @examples
#' \donttest{
#' library("xgboost")
#' library("DALEXtra")
#' # 8th column is target that has to be omitted in X data
#' data <- titanic_imputed[,-8]
#' y <- titanic_imputed$survived
#' model <- xgboost(data, as.factor(y), nrounds = 10,
#'                  objective = "binary:logistic", nthreads = 1)
#' 
#' explainer_1 <- explain_xgboost(model, data = titanic_imputed[,-8],
#'                                titanic_imputed$survived)
#' plot(predict_parts(explainer_1, titanic_imputed[1,-8]))
#' }
#'
#' @rdname explain_xgboost
#' @export

explain_xgboost <-
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
