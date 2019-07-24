#' Create explainer from your mlr model
#'
#' DALEX is designed to work with various black-box models like tree ensembles, linear models, neural networks etc.
#' Unfortunately R packages that create such models are very inconsistent. Different tools use different interfaces to train, validate and use models.
#' One of those tools, which is one of the most popular one is mlr package. We would like to present dedicated explain function for it.
#'
#'
#' @param model object - a model to be explained
#' @param data data.frame or matrix - data that was used for fitting. If not provided then will be extracted from the model
#' @param y numeric vector with outputs / scores. If provided then it shall have the same size as \code{data}
#' @param predict_function function that takes two arguments: model and new data and returns numeric vector with predictions
#' @param residual_function function that takes three arguments: model, data and response vector y. It should return a numeric vector with model residuals for given data. If not provided, response residuals (\eqn{y-\hat{y}}) are calculated.
#' @param ... other parameters
#' @param label character - the name of the model. By default it's extracted from the 'class' attribute of the model
#' @param verbose if TRUE (default) then diagnostic messages will be printed
#'
#' @return explainer object ready to work with DALEX
#'
#' @importFrom DALEX explain
#' @importFrom stats predict
#'
#'
#' @rdname explain_mlr
#' @export
#' @examples
#' \dontrun{
#' titanic_test <- read.csv(system.file("extdata", "titanic_test.csv", package = "DALEXtra"))
#' titanic_train <- read.csv(system.file("extdata", "titanic_train.csv", package = "DALEXtra"))
#' library("mlr")
#' task <- mlr::makeClassifTask(
#' id = "R",
#' data = titanic_train,
#' target = "survived"
#' )
#' learner <- mlr::makeLearner(
#'   "classif.gbm",
#'   par.vals = list(
#'     distribution = "bernoulli",
#'     n.trees = 5000,
#'     interaction.depth = 4,
#'     n.minobsinnode = 12,
#'     shrinkage = 0.001,
#'     bag.fraction = 0.5,
#'     train.fraction = 1
#'   ),
#'   predict.type = "prob"
#' )
#' gbm <- mlr::train(learner, task)
#' explain_mlr(gbm, titanic_test[,1:17], titanic_test[,18])
#'
#'}

explain_mlr <-
  function(model,
           data = NULL,
           y = NULL,
           predict_function = NULL,
           residual_function = NULL,
           ...,
           label = NULL,
           verbose = TRUE) {

    switch(model$task.desc$type,
           "classif" = {
             predict_function <- function(X.model, newdata, ...) {
               pred <- predict(X.model, newdata = newdata)
               response <- pred$data[, 1]
               response

             }
           },
           "regr" = {
             predict_function <- function(X.model, newdata, ...) {
               pred <- predict(X.model, newdata = newdata)
               response <- pred$data[, 1]
               response
             }
           },
           stop("Model is not explainable mlr object"))

    explain(
      model,
      data = data,
      y = y,
      predict_function = predict_function,
      residual_function = residual_function,
      label = label,
      verbose = verbose,
      ... = ...
    )


  }
