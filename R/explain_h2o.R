#' Create explainer from your h2o model
#'
#' DALEX is designed to work with various black-box models like tree ensembles, linear models, neural networks etc.
#' Unfortunately R packages that create such models are very inconsistent. Different tools use different interfaces to train, validate and use models.
#' One of those tools, we would like to make more accessible is H2O.
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
#'
#'
#' # load packages and data
#' library(h2o)
#' library(DALEXtra)
#'
#' # data <- DALEX::titanic_imputed
#'
#' # init h2o
#'  cluster <- try(h2o::h2o.init())
#' if (!inherits(cluster, "try-error")) {
#' # stop h2o progress printing
#'  h2o.no_progress()
#'
#' # split the data
#' # h2o_split <- h2o.splitFrame(as.h2o(data))
#' # train <- h2o_split[[1]]
#' # test <- as.data.frame(h2o_split[[2]])
#' # h2o automl takes target as factor
#' # train$survived <- as.factor(train$survived)
#'
#' # fit a model
#' # automl <- h2o.automl(y = "survived",
#' #                   training_frame = train,
#' #                    max_runtime_secs = 30)
#'
#'
#' # create an explainer for the model
#' # explainer <- explain_h2o(automl,
#' #                        data = test,
#' #                         y = test$survived,
#' #                          label = "h2o")
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
#' try(h2o.shutdown(prompt = FALSE))
#'  }
#' }
#' @rdname explain_h2o
#' @export

explain_h2o <-
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

    if (inherits(y, "H2OFrame")) {
      y <- as.vector(y)
    }

    if (inherits(model, "H2OAutoML")) {
      message("`model` argument is a H2OAutoML class object. model@leader will be extracted.\n")
      model <- model@leader
    }

    if (inherits(data, "H2OFrame")) {
      data <- as.data.frame(data)
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
