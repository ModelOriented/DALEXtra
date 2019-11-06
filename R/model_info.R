#' Exract info from model
#'
#' This generic function let user extract base information about model. The function returns a named list of class \code{model_info} that
#' contain about package of model, version and task type. For wrappers like \code{mlr} or \code{caret} both, package and wrapper inforamtion
#' are stored
#'
#' @param model - model object
#' @param ... - another arguments
#'
#' Currently supported packages are:
#' \itemize{
#' \item \code{mlr} models created with \code{mlr} package
#' \item \code{h2o} models created with \code{h2o} package
#' \item \code{scikit-learn} models created with \code{scikit-learn} pyhton library and accesed via \code{reticulate}
#' \item \code{keras} models created with \code{keras} pyhton library and accesed via \code{reticulate}
#' \item \code{mljar} models created with \code{mljar} API and accesed via \code{mljar} R package
#' \item \code{mlr3} models created with \code{mlr3} package
#' }
#'
#' @return A named list of class \code{model_info}


#' @rdname model_info
#' @export
model_info.WrappedModel <- function(model, ...) {
  switch(model$task.desc$type,
         "classif" = {
           type <- "classification"
         },
         "regr" = {
           type <- "regression"
         },
         stop("Model is not explainable mlr object"))
  package_wrapper <- "mlr"
  ver_wrapper <- as.character(utils::packageVersion("mlr"))
  package <- model$learner$package
  ver <- as.character(utils::packageVersion(package))
  model_info <- list(package = c(wrapper = package_wrapper, package = package), ver = c(wrapper = ver_wrapper, package = ver), type = type)
  class(model_info) <- "model_info"
  model_info
}

model_info.h2o <- function(model, ...) {
  switch(
    class(model),
    "H2ORegressionModel" = {
      type <- "regression"
    },
    "H2OBinomialModel" = {
      type <- "classification"
    },
    stop("Model is not explainable h2o object")
  )
  package_wrapper <- "h2o"
  ver_wrapper <- as.character(utils::packageVersion("h2o"))
  package <- model@algorithm
  ver <- "unkown"
  model_info <- list(package = c(wrapper = package_wrapper, package = package), ver = c(wrapper = ver_wrapper, package = ver), type = type)
  class(model_info) <- "model_info"
  model_info
}

#' @rdname model_info
#' @export
model_info.H2ORegressionModel <- model_info.h2o

#' @rdname model_info
#' @export
model_info.H2OBinomialModel <- model_info.h2o

#' @rdname model_info
#' @export
model_info.scikitlearn_model <- function(model, ...) {
  if ("predict_proba" %in% names(model)) {
    type <- "classification"
  } else {
    type <- "regression"
  }
  package_wrapper <- "reticulate"
  ver_wrapper <- as.character(utils::packageVersion("reticulate"))
  package <- "scikit-learn"
  ver <- "unkown"
  model_info <- list(package = c(wrapper = package_wrapper, package = package), ver = c(wrapper = ver_wrapper, package = ver), type = type)
  class(model_info) <- "model_info"
  model_info
}

#' @rdname model_info
#' @export
model_info.keras <- function(model, ...) {
  if ("predict_proba" %in% names(model)) {
    type <- "classification"
  } else {
    type <- "regression"
  }
  package_wrapper <- "reticulate"
  ver_wrapper <- as.character(utils::packageVersion("reticulate"))
  package <- "keras"
  ver <- "unkown"
  model_info <- list(package = c(wrapper = package_wrapper, package = package), ver = c(wrapper = ver_wrapper, package = ver), type = type)
  class(model_info) <- "model_info"
  model_info
}

#' @rdname model_info
#' @export
model_info.mljar_model <- function(model, ...) {
  type <- "regression"
  package_wrapper <- "mljar"
  ver_wrapper <- as.character(utils::packageVersion("mljar"))
  package <- "mljar"
  ver <- "unkown"
  model_info <- list(package = c(wrapper = package_wrapper, package = package), ver = c(wrapper = ver_wrapper, package = ver), type = type)
  class(model_info) <- "model_info"
  model_info
}

#' @rdname model_info
#' @export
model_info.LearnerRegr <- function(model, ...) {
  type <- "regression"
  package_wrapper <- "mlr3"
  ver_wrapper <- as.character(utils::packageVersion("mlr3"))
  package <- model$packages
  ver <- as.character(utils::packageVersion(package))
  model_info <- list(package = c(wrapper = package_wrapper, package = package), ver = c(wrapper = ver_wrapper, package = ver), type = type)
  class(model_info) <- "model_info"
  model_info
}

#' @rdname model_info
#' @export
model_info.LearnerClassif <- function(model, ...) {
  type <- "classification"
  package_wrapper <- "mlr3"
  ver_wrapper <- as.character(utils::packageVersion("mlr3"))
  package <- model$packages
  ver <- as.character(utils::packageVersion(package))
  model_info <- list(package = c(wrapper = package_wrapper, package = package), ver = c(wrapper = ver_wrapper, package = ver), type = type)
  class(model_info) <- "model_info"
  model_info
}

