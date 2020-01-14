#' Wrapper over the predict function
#'
#' These functios are default predict functions.
#' Each function returns a single numeric score for each new observation.
#' Those functions are very important since informations from many models have to be extracted with various techniques.
#'
#' Currently supported packages are:
#' \itemize{
#' \item \code{mlr} see more in \code{\link{explain_mlr}}
#' \item \code{h2o} see more in \code{\link{explain_h2o}}
#' \item \code{scikit-learn} see more in \code{\link{explain_scikitlearn}}
#' \item \code{keras} see more in \code{\link{explain_keras}}
#' \item \code{mljar} see more in \code{\link{explain_mljar}}
#' \item \code{mlr3} see more in \code{\link{explain_mlr3}}
#' }
#'
#' @param X.model object - a model to be explained
#' @param newdata data.frame or matrix - observations for prediction
#' @param ... other parameters that will be passed to the predict function
#'
#' @return An numeric vector of predictions



#' @rdname yhat
#' @export
yhat.WrappedModel <- function(X.model, newdata, ...) {
  switch(X.model$task.desc$type,
         "classif" = {
           pred <- predict(X.model, newdata = newdata)
           if ("truth" %in% colnames(pred$data)){
             response <- pred$data[, 3]
           } else {
             response <- pred$data[, 2]
           }
           response
         },
         "regr" = {
           pred <- predict(X.model, newdata = newdata)
           response <- pred$data$response
           response

         },
         stop("Model is not explainable mlr object"))
}

yhat.h2o <- function(X.model, newdata, ...) {
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

#' @rdname yhat
#' @export
yhat.H2ORegressionModel <- yhat.h2o

#' @rdname yhat
#' @export
yhat.H2OBinomialModel <- yhat.h2o

#' @rdname yhat
#' @export
yhat.scikitlearn_model <- function(X.model, newdata, ...) {
  if ("predict_proba" %in% names(X.model)) {
    # we take second cloumn which indicates probability of `1` to adapt to DALEX predict functions (yhat). If output is one column it will be taken
    success <-
      try(pred <-  X.model$predict_proba(newdata)[, 2], silent = TRUE)
    if (class(success) == "try-error") {
      pred <-  X.model$predict_proba(newdata)[, 1]
    }
  } else {
    pred <-  X.model$predict(newdata)
  }
  pred
}

#' @rdname yhat
#' @export
yhat.keras <- function(X.model, newdata, ...) {
  if ("predict_proba" %in% names(X.model)) {
    # We take first column due to keras not storing matrix when binary classification
    pred <-  X.model$predict_proba(newdata)[, 1]
  } else {
    pred <-  X.model$predict(newdata)
  }
  pred
}

#' @rdname yhat
#' @export
yhat.mljar_model <- function(X.model, newdata, ...) {
  unlist(mljar::mljar_predict(model = X.model, x_pred = newdata, project_title = X.model$project), use.names = FALSE)
}

#' @rdname yhat
#' @export
yhat.LearnerRegr <- function(X.model, newdata, ...) {
  predict(X.model, newdata = newdata, ...)
}

#' @rdname yhat
#' @export
yhat.LearnerClassif <- function(X.model, newdata, ...) {
  predict(X.model, newdata = newdata, predict_type = "prob", ...)[,1]
}
