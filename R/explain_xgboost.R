#' Create explainer from your xgboost model
#'
#' DALEX is designed to work with various black-box models like tree ensembles, linear models, neural networks etc.
#' Unfortunately R packages that create such models are very inconsistent. Different tools use different interfaces to train, validate and use models.
#' One of those tools, we would like to make more accessible is the xgboost package.
#'
#'
#' @inheritParams DALEX::explain
#' @param encode_function function(data, ...) that if executed with \code{data} 
#' parameters returns encoded dataframe that was used to fit model. Xgboost does 
#' not handle factors on it's own so such function is needed to acquire better explanations.
#' @param true_labels a vector of \code{y} before encoding.
#'
#' @return explainer object (\code{\link[DALEX]{explain}}) ready to work with DALEX
#'
#' @import DALEX
#' @importFrom DALEX yhat
#'
#'
#' @examples
#' library("xgboost")
#' library("DALEXtra")
#' library("mlr")
#' # 8th column is target that has to be omitted in X data
#' data <- as.matrix(createDummyFeatures(titanic_imputed[,-8]))
#' model <- xgboost(data, titanic_imputed$survived, nrounds = 10,
#'                  params = list(objective = "binary:logistic"),
#'                 prediction = TRUE)
#' # explainer with encode functiom
#' explainer_1 <- explain_xgboost(model, data = titanic_imputed[,-8],
#'                                titanic_imputed$survived,
#'                                encode_function = function(data) {
#'  as.matrix(createDummyFeatures(data))
#' })
#' plot(predict_parts(explainer_1, titanic_imputed[1,-8]))
#'
#' # explainer without encode function
#' explainer_2 <- explain_xgboost(model, data = data, titanic_imputed$survived)
#' plot(predict_parts(explainer_2, data[1,,drop = FALSE]))
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
           type = NULL,
           encode_function = NULL,
           true_labels = NULL) {

    attr(model, "encoder") <- encode_function
    if (!is.null(true_labels)) {
      attr(model, "true_labels") <- true_labels
      y <- true_labels
    }


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
