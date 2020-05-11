# Here are functions that are not supposed to be exported


# Function loads objects and handles possible errors
dalex_load_object <- function(path, mode) {
  tryCatch(
    model <- reticulate::py_load_object(path),
    error = function(e) {
      if (grepl("UnicodeDecodeError", e)) {
        warning(paste(e), call. = FALSE)
        warning(
          "There is a problem with encoding mismatch. You are probably using Python 3.x and are  trying to load Python 2.7 object.
              Try to use virtual environment with matching encoding.",
          call. = FALSE
        )
      } else if (grepl("No module", e)) {
        warning(paste(e), call. = FALSE)
        warning(
          "Your Python environment is missing some modules. Please install them using \n conda install name_of_missing_module \n or \n pip install name_of_missing_module " ,
          call. = FALSE
        )
      }
      else{
        warning(paste(e), call. = FALSE)
      }
      stop(
        "Yours environment has to match environment where pickle file was created. It also includes encoding, python version, and libraries version. Specifying .yml file or path to virtual environment may help. For more information look warnings() and then ?explain_scikitlearn",
        call. = FALSE
      )
    }
  )
  class(model) <- c(class(model), mode)
  model
}

# Function sets as working virtual env
prepeare_env <- function(yml, condaenv, env){
  if (!is.null(condaenv) & !is.null(env)) {
    stop("Only one argument from condaenv and env can be different from NULL")
  }
  if (!is.null(yml)) {
    name <- create_env(yml, condaenv)
    tryCatch(
      reticulate::use_condaenv(name, required = TRUE),
      error = error_mes
    )
  }
  if (!is.null(condaenv) & is.null(yml)) {
    tryCatch(
      reticulate::use_condaenv(condaenv, required = TRUE),
      error = error_mes
    )
  }
  if (!is.null(env)) {
    tryCatch(
      reticulate::use_virtualenv(env, required = TRUE),
      error = error_mes
    )
  }
}


# Error message
error_mes <- function(e) {
  warning(e, call. = FALSE)
  stop(
    "reticulate is unable to set new environment. Specified envirnonment does not exists or connection cannot be established due to already using other python.exe, please install environment or restart R session. See warnings() for original error",
    call. = FALSE
  )
}

is_conda <- function() {
  conda <- try(reticulate::conda_binary(), silent = TRUE)
  class(conda) != "try-error"
}

set_measure_function <- function(champion, challengers) {
  if (all(sapply(challengers, function(x){
    x$model_info$type == "multiclass"
  })) & champion$model_info$type == "multiclass") {
    measure_function <- loss_cross_entropy
  } else if (all(sapply(challengers, function(x){
    x$model_info$type == "classification"
  })) & champion$model_info$type == "classification") {
    measure_function <- loss_one_minus_auc
  } else if (all(sapply(challengers, function(x){
    x$model_info$type == "regression"
  })) & champion$model_info$type == "regression") {
    measure_function <- loss_root_mean_square
  } else {
    stop("Champion and all of challengers don't share same task type")
  }
  measure_function
}
