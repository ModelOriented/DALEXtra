#' Create explainer from your h2o model
#'
#' DALEX is designed to work with various black-box models like tree ensembles, linear models, neural networks etc.
#' Unfortunately R packages that create such models are very inconsistent. Different tools use different interfaces to train, validate and use models.
#' One of those tools, we would like to make more accessible is H2O.
#'
#'
#' @param model object - a model to be explained
#' @param data data.frame or matrix - data that was used for fitting. If not provided then will be extracted from the model. Data should be passed without target column (this shall be provided as the \code{y} argument). NOTE: If target variable is present in the \code{data}, some of the functionalities my not work properly.
#' @param y numeric vector with outputs / scores. If provided then it shall have the same size as \code{data}
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
#'
#' @return explainer object (\code{\link[DALEX]{explain}}) ready to work with DALEX
#'
#' @import DALEX
#' @importFrom DALEX yhat
#'
#'
#' @examples
#' \donttest{
#' 
#' # load packages and data
#' library(h2o)
#' library(DALEXtra)
#' 
#' data <- DALEX::titanic_imputed
#' 
#' # init h2o
#' h2o.init()
#' 
#' # split the data
#' h2o_split <- h2o.splitFrame(as.h2o(data))
#' train <- h2o_split[[1]]
#' test <- as.data.frame(h2o_split[[2]])
#' 
#' # h2o automl takes target as factor
#' train$survived <- as.factor(train$survived)
#' 
#' # fit a model
#' automl <- h2o.automl(y = "survived",
#'                      training_frame = train,
#'                      max_runtime_secs = 30)
#' 
#' # stop h2o progress printing
#' h2o.no_progress()
#' 
#' # create an explainer for the model
#' explainer <- explain_h2o(automl,
#'                          data = test,
#'                          y = test$survived,
#'                          label = "h2o")
#'
#' 
#' titanic_test <- read.csv(system.file("extdata", "titanic_test.csv", package = "DALEXtra"))
#' titanic_train <- read.csv(system.file("extdata", "titanic_train.csv", package = "DALEXtra"))
#' titanic_h2o <- h2o::as.h2o(titanic_train)
#' titanic_h2o["survived"] <- h2o::as.factor(titanic_h2o["survived"])
#' titanic_test_h2o <- h2o::as.h2o(titanic_test)
#' model <- h2o::h2o.gbm(
#' training_frame = titanic_h2o,
#' y = "survived",
#' distribution = "bernoulli",
#' ntrees = 500,
#' max_depth = 4,
#' min_rows =  12,
#' learn_rate = 0.001
#' )
#' explain_h2o(model, titanic_test[,1:17], titanic_test[,18])
#' 
#' h2o::h2o.shutdown(prompt = FALSE)
#' }
#' @rdname explain_h2o
#' @export

explain_h2o <-
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
           type = NULL) {
    
    if (class(y) == "H2OFrame") {
      y <- as.numeric(as.vector(y))
    }
    
    if (class(model) == "H2OAutoML") {
      message("`model` argument is a H2OAutoML class object. model@leader will be extracted.\n")
      model <- model@leader
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
