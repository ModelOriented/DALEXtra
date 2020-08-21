#' Create explainer from your tidymodels workflow.
#'
#' DALEX is designed to work with various black-box models like tree ensembles, linear models, neural networks etc.
#' Unfortunately R packages that create such models are very inconsistent. Different tools use different interfaces to train, validate and use models.
#' One of those tools, which is one of the most popular one is tidymodels package. We would like to present dedicated explain function for it.
#'
#'
#' @param model object - a fitted workflow created with \code{mlr3}.
#' @param data data.frame or matrix - data that was used for fitting. Data should be passed without target column (this shall be provided as the \code{y} argument). NOTE: If target variable is present in the \code{data}, some of the functionalities my not work properly. Tibble will be converted into data.frame
#' @param y numeric vector with outputs / scores. If provided then it shall have the same size as \code{data}
#' @param weights numeric vector with sampling weights. By default it's \code{NULL}. If provided then it shall have the same length as \code{data}
#' @param predict_function function that takes two arguments: model and new data and returns numeric vector with predictions
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
#' @rdname explain_tidymodels
#' @export
#' @examples
#' library("DALEXtra")
#' library("tidymodels")
#' library("recipes")
#' rec <- recipe(survived ~ ., data = titanic_imputed) %>%
#'        step_mutate(survived = as.factor(survived)) %>%
#'        prep()
#' model <- decision_tree(tree_depth = 25) %>%
#'          set_engine("rpart") %>%
#'          set_mode("classification")
#'
#' wflow <- workflow() %>%
#'          add_recipe(rec) %>%
#'          add_model(model)
#'
#'
#' model_fitted <- wflow %>%
#'                 fit(data = titanic_imputed)
#'
#' explain_tidymodels(model_fitted, data = titanic_imputed, y = titanic_imputed$survived)
#'
#'



explain_tidymodels <-
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
           adjust_data = TRUE
  ) {

    if (!model$trained) {
      stop("Only trained workflows can be passed to explain function")
    }

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
