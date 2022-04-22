#' Exract info from model
#'
#' This generic function let user extract base information about model. The function returns a named list of class \code{model_info} that
#' contain about package of model, version and task type. For wrappers like \code{mlr} or \code{caret} both, package and wrapper information
#' are stored
#'
#' @param model - model object
#' @param is_multiclass - if TRUE and task is classification, then multitask classification is set. Else is omitted. If \code{model_info}
#' was executed withing \code{explain} function. DALEX will recognize subtype on it's own. @param is_multiclass
#' @param ... - another arguments
#'
#' @details 
#' Currently supported packages are:
#' \itemize{
#' \item \code{mlr} models created with \code{mlr} package
#' \item \code{h2o} models created with \code{h2o} package
#' \item \code{scikit-learn} models created with \code{scikit-learn} Python library and accessed via \code{reticulate}
#' \item \code{keras} models created with \code{keras} Python library and accessed via \code{reticulate}
#' \item \code{mlr3} models created with \code{mlr3} package
#' \item \code{xgboost} models created with \code{xgboost} package
#' \item \code{tidymodels} models created with \code{tidymodels} package
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
      if (!is.null(attr(model, "predict_function_target_column"))) {
       type <- "classification"
      } else {
        type <- "multiclass"
      }
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

#' @rdname model_info
#' @export
model_info.GraphLearner <- function(model, is_multiclass = FALSE, ...) {
  if ("prob" %in% model$predict_types) {
    return(model_info.LearnerClassif(model, is_multiclass))
  } else {
    return(model_info.LearnerRegr(model, is_multiclass))
  }
}

#' @rdname model_info
#' @export
model_info.xgb.Booster <- function(model, is_multiclass = FALSE, ...) {
  task <- strsplit(model$params$objective, ":", fixed = TRUE)[[1]][1]
  if (task == "multi") {
    if (!is.null(attr(model, "predict_function_target_column"))) {
      type <- "classification"
    } else {
      type <- "multiclass"
    }
  } else if (task == "binary") {
    type <- "classification"
  } else if (task == "reg") {
    type <- "regression"
  } else {
    stop("Task has to be either multi, binary or reg")
  }
  package <- "xgboost"
  ver <- get_pkg_ver_safe(package)
  model_info <- list(package = package, ver = ver, type = type)
  class(model_info) <- "model_info"
  model_info
}

#' @rdname model_info
#' @export
model_info.workflow <- function(model, is_multiclass = FALSE, ...) {
  model <- model$fit$fit
  if (model$spec$mode == "classification" & is_multiclass) {
    type <- "multiclass"
  } else if (model$spec$mode == "classification" & !is_multiclass) {
    type <- "classification"
  } else {
    type <- "regression"
  }
  package_wrapper <- "tidymodels"
  ver_wrapper <- get_pkg_ver_safe(package_wrapper)
  package <- model$spec$method$libs
  ver <- get_pkg_ver_safe(package)
  model_info <- list(package = c(wrapper = package_wrapper, package = package), ver = c(wrapper = ver_wrapper, package = ver), type = type)
  class(model_info) <- "model_info"
  model_info
}

#' @rdname model_info
#' @export
model_info.model_stack <- function(model, is_multiclass = FALSE, ...) {
  if (model$mode == "classification" & is_multiclass) {
    type <- "multiclass"
  } else if (model$mode == "classification" & !is_multiclass) {
    type <- "classification"
  } else {
    type <- "regression"
  }
  
  package <- "stacks"
  ver <- get_pkg_ver_safe(package)
  model_info <- list(package = package, ver = ver, type = type)  
  class(model_info) <- "model_info"
  model_info
}



get_pkg_ver_safe <- function(package) {
  ver <- try(as.character(utils::packageVersion(package)), silent = TRUE)
  if (inherits(ver, "try-error")) {
    ver <- "Unknown"
  }
  ver
}

