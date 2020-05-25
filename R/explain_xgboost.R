#' Create explainer from your xgboost model
#'
#' DALEX is designed to work with various black-box models like tree ensembles, linear models, neural networks etc.
#' Unfortunately R packages that create such models are very inconsistent. Different tools use different interfaces to train, validate and use models.
#' One of those tools, we would like to make more accessible is xgboost.
#'
#'
#' @param model object - a model to be explained
#' @param data data.frame or matrix - data that was used for fitting. If not provided then will be extracted from the model. Data should be passed without target column (this shall be provided as the \code{y} argument). NOTE: If target variable is present in the \code{data}, some of the functionalities my not work properly.
#' @param y numeric vector with outputs / scores. If provided then it shall have the same size as \code{data}. For classif task has to be numerci in range [0, nclasses)
#' @param weights numeric vector with sampling weights. By default it's \code{NULL}. If provided then it shall have the same length as \code{data}
#' @param predict_function function that takes two arguments: model and new data and returns numeric vector with predictions
#' @param residual_function function that takes three arguments: model, data and response vector y. It should return a numeric vector with model residuals for given data. If not provided, response residuals (\eqn{y-\hat{y}}) are calculated.
#' @param ... other parameters
#' @param label character - the name of the model. By default it's extracted from the 'class' attribute of the model
#' @param verbose if TRUE (default) then diagnostic messages will be printed
#' @param precalculate if TRUE (default) then 'predicted_values' and 'residuals' are calculated when explainer is created.
#' @param colorize if TRUE (default) then \code{WARNINGS}, \code{ERRORS} and \code{NOTES} are colorized. Will work only in the R console.
#' @param model_info a named list (\code{package}, \code{version}, \code{type}) containg information about model. If \code{NULL}, \code{DALEX} will seek for information on it's own.
#' @param type type of a model, either \code{classification} or \code{regression}. If not specified then \code{type} will be extracted from \code{model_info}.
#' @param encode_function fuction(data, ...) that if executed with \code{data} parameters returns encoded dataframe that was used to fit model. Xgboost does not handle factors on it's own so such function is needed to aquire better explanations.
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
           residual_function = NULL,
           ...,
           label = NULL,
           verbose = TRUE,
           precalculate = TRUE,
           colorize = TRUE,
           model_info = NULL,
           type = NULL,
           encode_function = NULL) {

    attr(model, "encoder") <- encode_function

    explain(
      model,
      data = data,
      y = y,
      weights = weights,
      predict_function = predict_function,
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
