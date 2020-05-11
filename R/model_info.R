#' Exract info from model
#'
#' This generic function let user extract base information about model. The function returns a named list of class \code{model_info} that
#' contain about package of model, version and task type. For wrappers like \code{mlr} or \code{caret} both, package and wrapper inforamtion
#' are stored
#'
#' @param model - model object
#' @param is_multiclass - if TRUE and task is classification, then multitask classification is set. Else is omitted. If \code{model_info}
#' was executed withing \code{explain} function. DALEX will recognize subtype on it's own. @param is_multiclass
#' @param ... - another arguments
#'
#' Currently supported packages are:
#' \itemize{
#' \item \code{mlr} models created with \code{mlr} package
#' \item \code{h2o} models created with \code{h2o} package
#' \item \code{scikit-learn} models created with \code{scikit-learn} pyhton library and accesed via \code{reticulate}
#' \item \code{keras} models created with \code{keras} pyhton library and accesed via \code{reticulate}
#' \item \code{mlr3} models created with \code{mlr3} package
#' }
#'
#' @return A named list of class \code{model_info}


#' @rdname model_info
#' @export
model_info.WrappedModel <- function(model, is_multiclass = FALSE, ...) {
  if (model$task.desc$type == "classif" & is_multiclass) {
    type <- "multiclass"
  } else if (model$task.desc$type == "classif" & !is_multiclass) {
    type <- "classification"
  } else {
    type <- "regression"
  }
  package_wrapper <- "mlr"
  ver_wrapper <- get_pkg_ver_safe(package_wrapper)
  package <- model$learner$package
  ver <- get_pkg_ver_safe(package)
  model_info <- list(package = c(wrapper = package_wrapper, package = package), ver = c(wrapper = ver_wrapper, package = ver), type = type)
  class(model_info) <- "model_info"
  model_info
}

model_info.h2o <- function(model, is_multiclass = FALSE, ...) {
  switch(
    class(model),
    "H2ORegressionModel" = {
      type <- "regression"
    },
    "H2OBinomialModel" = {
      type <- "classification"
    },
    "H2OMultinomialModel" = {
      type <- "multiclass"
    },
    stop("Model is not explainable h2o object")
  )
  package_wrapper <- "h2o"
  ver_wrapper <- get_pkg_ver_safe(package_wrapper)
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
model_info.H2OMultinomialModel <- model_info.h2o

#' @rdname model_info
#' @export
model_info.scikitlearn_model <- function(model, is_multiclass = FALSE, ...) {
  if ("predict_proba" %in% names(model) & is_multiclass) {
    type <- "multiclass"
  } else if ("predict_proba" %in% names(model) & !is_multiclass) {
    type <- "classification"
  }else {
    type <- "regression"
  }
  package_wrapper <- "reticulate"
  ver_wrapper <- get_pkg_ver_safe(package_wrapper)
  package <- "scikit-learn"
  ver <- "unkown"
  model_info <- list(package = c(wrapper = package_wrapper, package = package), ver = c(wrapper = ver_wrapper, package = ver), type = type)
  class(model_info) <- "model_info"
  model_info
}

#' @rdname model_info
#' @export
model_info.keras <- function(model, is_multiclass = FALSE, ...) {
  if ("predict_proba" %in% names(model) & is_multiclass) {
    type <- "multiclass"
  } else if ("predict_proba" %in% names(model) & !is_multiclass) {
    type <- "classification"
  }else {
    type <- "regression"
  }
  package_wrapper <- "reticulate"
  ver_wrapper <- get_pkg_ver_safe(package_wrapper)
  package <- "keras"
  ver <- "unkown"
  model_info <- list(package = c(wrapper = package_wrapper, package = package), ver = c(wrapper = ver_wrapper, package = ver), type = type)
  class(model_info) <- "model_info"
  model_info
}


# model_info.mljar_model <- function(model, is_multiclass = FALSE, ...) {
#   type <- "regression"
#   package_wrapper <- "mljar"
#   ver_wrapper <- as.character(utils::packageVersion("mljar"))
#   package <- "mljar"
#   ver <- "unkown"
#   model_info <- list(package = c(wrapper = package_wrapper, package = package), ver = c(wrapper = ver_wrapper, package = ver), type = type)
#   class(model_info) <- "model_info"
#   model_info
# }

#' @rdname model_info
#' @export
model_info.LearnerRegr <- function(model, is_multiclass = FALSE, ...) {
  type <- "regression"
  package_wrapper <- "mlr3"
  ver_wrapper <- get_pkg_ver_safe(package_wrapper)
  package <- model$packages
  ver <- get_pkg_ver_safe(package)
  model_info <- list(package = c(wrapper = package_wrapper, package = package), ver = c(wrapper = ver_wrapper, package = ver), type = type)
  class(model_info) <- "model_info"
  model_info
}

#' @rdname model_info
#' @export
model_info.LearnerClassif <- function(model, is_multiclass = FALSE, ...) {
  if (is_multiclass) {
    type <- "multiclass"
  } else {
    type <- "classification"
  }
  package_wrapper <- "mlr3"
  ver_wrapper <- get_pkg_ver_safe(package_wrapper)
  package <- model$packages
  ver <- get_pkg_ver_safe(package)
  model_info <- list(package = c(wrapper = package_wrapper, package = package), ver = c(wrapper = ver_wrapper, package = ver), type = type)
  class(model_info) <- "model_info"
  model_info
}

get_pkg_ver_safe <- function(package) {
  ver <- try(as.character(utils::packageVersion(package)), silent = TRUE)
  if (class(ver) == "try-error") {
    ver <- "Unknown"
  }
  ver
}

