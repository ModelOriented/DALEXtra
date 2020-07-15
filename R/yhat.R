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
#' \item \code{mlr3} see more in \code{\link{explain_mlr3}}
#' \item \code{xgboost} see more in \code{\link{explain_xgboost}}
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
           if (X.model$learner$predict.type != "prob") {
             return(pred$data$response)
           }
           if ("truth" %in% colnames(pred$data)){
             if (ncol(pred$data) == 4) {
               response <- pred$data[, 3]
             } else {
               response <- pred$data[, -c(1, ncol(pred$data))]
               names(response) <- normalize_mlr_names(names(response))
             }

           } else {
             if (ncol(pred$data) == 3) {
               response <- pred$data[, 2]
             } else {
               response <- pred$data[, -ncol(pred$data)]
               names(response) <- normalize_mlr_names(names(response))
             }
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
      ret <- as.data.frame(h2o::h2o.predict(X.model, newdata = newdata))
      if ("predict" %in% colnames(ret)) {
        ret <- ret [,3]
      } else {
        ret <- ret[,2]
      }
      ret

    },
    "H2OMultinomialModel" = {
      if (!class(newdata) == "H2OFrame") {
        newdata <- h2o::as.h2o(newdata)
      }
      ret <- as.data.frame(h2o::h2o.predict(X.model, newdata = newdata))
      colnames(ret) <- normalize_h2o_names(colnames(ret))
      ret[,-1]
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
yhat.H2OMultinomialModel <- yhat.h2o

#' @rdname yhat
#' @export
yhat.scikitlearn_model <- function(X.model, newdata, ...) {
  if ("predict_proba" %in% names(X.model)) {
    pred <-  X.model$predict_proba(newdata)
    if (ncol(pred) == 2) {
      pred <- pred[,2]
    } else {
      colnames(pred) <- 0:(ncol(pred)-1)
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
    pred <-  X.model$predict_proba(newdata)
    if (ncol(pred) == 1) {
      pred <- as.numeric(pred)
    } else if (ncol(pred) == 2) {
      pred <- as.numeric(pred[,2])
    } else {
      colnames(pred) <- 0:(ncol(pred)-1)
    }
  } else {
    pred <-  X.model$predict(newdata)
  }
  pred
}

#' yhat.mljar_model <- function(X.model, newdata, ...) {
#'   unlist(mljar::mljar_predict(model = X.model, x_pred = newdata, project_title = X.model$project), use.names = FALSE)
#' }

#' @rdname yhat
#' @export
yhat.LearnerRegr <- function(X.model, newdata, ...) {
  X.model$predict_newdata(newdata, ...)$response
}

#' @rdname yhat
#' @export
yhat.LearnerClassif <- function(X.model, newdata, ...) {
  pred <- X.model$predict_newdata(newdata)

  # return probabilities for class: 1
  response <- pred$prob
  if (ncol(response) == 2) {
    response <- response[,2]
  }
  response
}

#' @rdname yhat
#' @export
yhat.GraphLearner <- function(X.model, newdata, ...) {
  if ("prob" %in% X.model$predict_types) {
    pred <- X.model$predict_newdata(newdata)
    # return probabilities for class: 1
    response <- pred$prob
    if (ncol(response) == 2) {
      response <- response[,2]
    }
    response
  } else {
    X.model$predict_newdata(newdata, ...)$response
  }
}

#' @rdname yhat
#' @export
yhat.xgb.Booster <- function(X.model, newdata, ...) {
  if (!is.null(attr(X.model, "encoder"))) {
    newdata <- attr(X.model, "encoder")(newdata)
  }

  if (X.model$params$objective == "multi:softprob") {
    if (!is.null(attr(X.model, "true_labels"))) {
      col_names <- levels(as.factor(attr(X.model, "true_labels")))
    } else {
      col_names <- 0:(X.model$params$num_class-1)
    }
    p <- predict(X.model, newdata, type="response")
    ret <- matrix(p, ncol = X.model$params$num_class, byrow = TRUE)
    colnames(ret) <- col_names
  } else if (X.model$params$objective == "multi:softprob") {
    stop("Please use objective\"multi:softmax\" to get probability output")
  } else if (X.model$params$objective == "binary:logistic") {
    ret <- predict(X.model, newdata, type="response")
  } else if (X.model$params$objective == "binary:logitraw" | X.model$params$objective == "binary:hinge") {
    stop("Please use objective\"binary:logistic\" to get probability output")
  } else {
    ret <- predict(X.model, newdata, type="response")
  }
  ret
}


normalize_h2o_names <- function(names) {
  ret <- sapply(names, FUN = function(x) {
    tmp <- strsplit(x, "p")
    if (!is.na(tmp[[1]][2])) {
      return(tmp[[1]][2])
    } else {
      return(x)
    }
  })
  names(ret) <- NULL
  ret
}

normalize_mlr_names <- function(names) {
  ret <- sapply(names, FUN = function(x) {
    tmp <- strsplit(x, "prob.")
    if (!is.na(tmp[[1]][2])) {
      return(tmp[[1]][2])
    } else {
      return(x)
    }
  })
  names(ret) <- NULL
  ret
}
