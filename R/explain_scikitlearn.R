#' Wrapper for Python Scikit-Learn Models
#'
#' scikit-learn models may be loaded into R environment like any other Python object. This function helps to inspect performance of Python model
#' and compare it with other models, using R tools like DALEX. This function creates an object that is easy accessible R version of scikit-learn model
#' exported from Python via pickle file.
#'
#'
#' @param path a path to the pickle file. Can be used without other arguments if you are sure that active Python version match pickle version.
#' @param yml a path to the yml file. Conda virtual env will be recreated from this file. If OS is Windows conda has to be added to the PATH first
#' @param condaenv If yml param is provided, a path to the main conda folder. If yml is null, a name of existing conda environment.
#' @param env A path to python virtual environment.
#' @param data test data set that will be passed to explainer.
#' @param y vector that will be passed to explainer.
#' @param predict_function predict function that will be passed into explainer. If NULL, default will be used.
#' @param residual_function residual function that will be passed into explainer. If NULL, default will be used.
#' @param label label that will be passed into explainer. If NULL, default will be used.
#' @param verbose bool that will be passed into explainer. If NULL, default will be used.
#' @param precalculate if TRUE (default) then 'predicted_values' and 'residuals' are calculated when explainer is created. This will happenn also if 'verbose' is TRUE.
#' @param ... other parameters
#'
#'
#' @author Szymon Maksymiuk
#'
#'
#' @return An object of the class 'explainer'. Possible extraction of object 'scikitlearn_model' via explainer$model
#'
#' scikitlearn_model is a list with following fields:
#'
#' \itemize{
#' \item \code{model} it is original model received vie reticiulate function. Use it for computations.
#' \item \code{predict_function} predict function extracted from original model. It is adjusted to DALEX demands and therfore fully compatibile.
#' \item \code{type} type model, classification or regression
#' \item \code{params} object of class `scikitlearn_set` which in fact is list that consist of parameters of our model.
#' \item \code{label} name of model
#'
#' }
#'
#' \bold{Example of Python code}\cr
#'
#' from pandas import DataFrame, read_csv \cr
#' import pandas as pd\cr
#' import pickle\cr
#' import sklearn.ensemble\cr
#' model = sklearn.ensemble.GradientBoostingClassifier() \cr
#' model = model.fit(titanic_train_X, titanic_train_Y)\cr
#' pickle.dump(model, open("gbm.pkl", "wb"), protocol = 2)\cr
#' \cr
#' \cr
#' In order to export environment into .yml, activating virtual env via \code{activate name_of_the_env} and execution of the following shell command is necessary \cr
#' \code{conda env export > environment.yml}\cr
#' \cr
#'
#' \bold{Errors use case}\cr
#' Here is shortened version of solution for specific errors \cr
#' \cr
#' \bold{There already exists environment with a name specified by given .yml file}\cr
#' You have two ways of solving that issue. Both connected with anaconda prompt. First is removing conda env with command: \cr
#' \code{conda env remove --name myenv}\cr
#' And execute function once again. Second is updating env via: \cr
#' \code{conda env create -f environment.yml}\cr
#' \cr
#' \bold{Conda cannot find specified packages at channels you have provided.}\cr
#' That error may be casued by a lot of things. Of of those is that specified version is too old to be avaialble from offcial conda repo.
#' Edit Your .yml file and add link to proper repository at channels section.\cr
#' \cr
#' Issue may be also connected with the platform. If model was created on the platform with different OS yo may need to remove specific version from .yml file.\cr
#' \code{- numpy=1.16.4=py36h19fb1c0_0}\cr
#' \code{- numpy-base=1.16.4=py36hc3f5095_0}\cr
#' In the example above You have to remove \code{=py36h19fb1c0_0} and \code{=py36hc3f5095_0} \cr
#' If some packages are not availbe for anaconda at all, use pip statement\cr
#' \cr
#' If .yml file seems not to work, virtual env can be created manually using anaconda promt. \cr
#' \code{conda create -n name_of_env python=3.4} \cr
#' \code{conda install -n name_of_env name_of_package=0.20} \cr
#'
#'
#' @import DALEX
#' @import reticulate
#' @importFrom utils head
#'
#'
#' @examples
#' library("DALEXtra")
#' library("DALEX")
#' if(is_conda()) {
#'    # Explainer build (Keep in mind that 18th column is target)
#'    titanic_test <- read.csv(system.file("extdata", "titanic_test.csv", package = "DALEXtra"))
#'    # Keep in mind that when pickle is being built and loaded,
#'    # not only Python version but libraries versions has to match aswell
#'    explainer <- explain_scikitlearn(system.file("extdata", "scikitlearn.pkl", package = "DALEXtra"),
#'    yml = system.file("extdata", "scikitlearn_unix.yml", package = "DALEXtra"),
#'    data = titanic_test[,1:17], y = titanic_test$survived)
#'    plot(model_performance(explainer))
#'
#'    # Predictions with newdata
#'    explainer$model$predict_function(explainer$model, titanic_test[1:10,1:17])
#'
#' } else {
#'   print('Conda is required.')
#' }
#'
#' @rdname explain_scikitlearn
#' @export
#'
explain_scikitlearn <-
  function(path,
           yml = NULL,
           condaenv = NULL,
           env = NULL,
           data = NULL,
           y = NULL,
           predict_function = NULL,
           residual_function = NULL,
           ...,
           label = NULL,
           verbose = TRUE,
           precalculate = TRUE) {
    if (!is.null(condaenv) & !is.null(env)) {
      stop("Only one argument from condaenv and env can be different from NULL")
    }

    if (!is.null(yml)) {
      name <- create_env(yml, condaenv)
      tryCatch(
        reticulate::use_condaenv(name, required = TRUE),
        error = function(e) {
          warning(e, call. = FALSE)
          stop(
            "reticulate is unable to set new environment due to already using other python.exe, please restart R session. See warnings() for original error",
            call. = FALSE
          )
        }
      )

    }

    if (!is.null(condaenv) & is.null(yml)) {
      tryCatch(
        reticulate::use_condaenv(condaenv, required = TRUE),
        error = function(e) {
          warning(e, call. = FALSE)
          stop(
            "reticulate is unable to set new environment. Specified envirnonment does not exists or connection cannot be established due to already using other python.exe, please install environment or restart R session. See warnings() for original error",
            call. = FALSE
          )
        }
      )



    }

    if (!is.null(env)) {
      tryCatch(
        reticulate::use_virtualenv(env, required = TRUE),
        error = function(e) {
          warning(e, call. = FALSE)
          stop(
            "reticulate is unable to set new environment. Specified envirnonment does not exists or connection cannot be established due to already using other python.exe, please install environment or restart R session. See warnings() for original error",
            call. = FALSE
          )
        }
      )
    }


    model <- dalex_load_object(path)

    # params are represented as one long string
    params <- model$get_params
    # taking first element since strsplit() returns list of vectors
    params <- strsplit(as.character(params), split = ",")[[1]]
    # replacing blanks and other signs that we don't need and are pasted with params names
    params <-
      gsub(params,
           pattern = "\n",
           replacement = "",
           fixed = TRUE)
    params <-
      gsub(params,
           pattern = " ",
           replacement = "",
           fixed = TRUE)
    # splitting after "=" mark and taking first element (head(n = 1L)) provides as with params names
    params <- lapply(strsplit(params, split = "="), head, n = 1L)
    # removing name of function from the first parameter
    params[[1]] <- strsplit(params[[1]], split = "\\(")[[1]][2]
    # setting freshly extracted parameters names as labels for list
    names(params) <- as.character(params)
    #extracting parameters value
    params <- lapply(params, function(x) {
      do.call("$", list(model, x))
    })

    class(params) <- "scikitlearn_set"

    #name of our model
    label <- strsplit(as.character(model), split = "\\(")[[1]][1]

    if ("predict_proba" %in% names(model)) {
      predict_function <- function(model, newdata) {
        # we take second cloumn which indicates probability of `1` to adapt to DALEX predict functions (yhat)
        model$model$predict_proba(newdata)[, 2]
      }
      type = "classification"
    } else{
      predict_function <- function(model, newdata) {
        model$model$predict(newdata)
      }
      type = "regression"
    }

    scikitlearn_model <- list(
      label = label,
      type = type,
      params = params,
      predict_function = predict_function,
      model = model
    )
    class(scikitlearn_model) <- "scikitlearn_model"

    explain(
      model = scikitlearn_model,
      data = data,
      y = y,
      predict_function = scikitlearn_model$predict_function,
      residual_function = residual_function,
      ... = ...,
      label = label,
      verbose = verbose
    )

  }
