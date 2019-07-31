#' Wrapper over the predict function
#'
#' These functios are default predict functions.
#' Each function returns a single numeric score for each new observation.
#'
#' Currently supported packages are:
#' \itemize{
#' \item `mlr` see more in explain_mlr
#' \item `h2o` see more in explain_h2o
#' }
#'
#' @param X.model object - a model to be explained
#' @param newdata data.frame or matrix - observations for prediction
#' @param ... other parameters that will be passed to the predict function
#'
#' @return An numeric vector of predictions
#' @rdname yhat
#' @export



#' @rdname yhat
#' @export
yhat_mlr <- function(X.model, newdata, ...) {
  switch(X.model$task.desc$type,
         "classif" = {
           pred <- predict(X.model, newdata = newdata)
           response <- pred$data[, 1]
           response
         },
         "regr" = {
           pred <- predict(X.model, newdata = newdata)
           response <- pred$data[, 1]
           response
         },
         stop("Model is not explainable mlr object"))
}

#' @rdname yhat
#' @export
yhat_h2o <- function(X.model, newdata, ...) {
  switch(
    class(X.model),
    "H2ORegressionModel" = {
      if (!class(newdata) == "H2OFrame") {
        newdata <- h2o::as.h2o(newdata)
      }
      as.vector(h2o::h2o.predict(X.model, newdata = newdata))

    },
    "H2OBinomialModel" = {
      if (!class(newdata) == "H2OFrame") {
        newdata <- h2o::as.h2o(newdata)
      }

      res <-
        as.data.frame(h2o::h2o.predict(X.model, newdata = newdata))
      res$p1

    },
    stop("Model is not explainable h2o object")


  )
}
