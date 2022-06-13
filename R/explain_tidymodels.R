#' Create explainer from your tidymodels workflow.
#'
#' DALEX is designed to work with various black-box models like tree ensembles, linear models, neural networks etc.
#' Unfortunately R packages that create such models are very inconsistent. Different tools use different interfaces to train, validate and use models.
#' One of those tools, which is one of the most popular one is the tidymodels package. We would like to present dedicated explain function for it.
#'
#'
#' @inheritParams DALEX::explain
#'
#' @return explainer object (\code{\link[DALEX]{explain}}) ready to work with DALEX
#'
#' @import DALEX
#' @importFrom DALEX yhat
#'
#' @rdname explain_tidymodels
#' @export
#' @examples
#' library("DALEXtra")
#' library("tidymodels")
#' library("recipes")
#' data <- titanic_imputed
#' data$survived <- as.factor(data$survived)
#' rec <- recipe(survived ~ ., data = data) %>%
#'        step_normalize(fare)
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
#'                 fit(data = data)
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
           predict_function_target_column = NULL,
           residual_function = NULL,
           ...,
           label = NULL,
           verbose = TRUE,
           precalculate = TRUE,
           colorize = !isTRUE(getOption('knitr.in.progress')),
           model_info = NULL,
           type = NULL) {

    if (inherits(model, "workflow") && !model$trained) {
      stop("Only trained workflows can be passed to explain function")
    }

    # for classification models do not calculate residuals (default)
    # as this my rise some not needed warnings
    if (is.null(residual_function) && isTRUE(model$spec$mode == "classification")) {
        residual_function <- function(m, d, y, predict_function) 0
    }

    if (!is.null(type) && type == "class") {
      type = "classification"
      predict_function <- function(X.model, newdata, ...)
          predict(X.model, newdata, type = "class")
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
