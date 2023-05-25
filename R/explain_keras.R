#' Wrapper for Python Keras Models
#'
#' Keras models may be loaded into R environment like any other Python object. This function helps to inspect performance of Python model
#' and compare it with other models, using R tools like DALEX. This function creates an object that is easily accessible R version of Keras model
#' exported from Python via pickle file.
#'
#'
#' @param path a path to the pickle file. Can be used without other arguments if you are sure that active Python version match pickle version.
#' @param yml a path to the yml file. Conda virtual env will be recreated from this file. If OS is Windows conda has to be added to the PATH first
#' @param condaenv If yml param is provided, a path to the main conda folder. If yml is null, a name of existing conda environment.
#' @param env A path to python virtual environment.
#' @inheritParams DALEX::explain
#'
#' @author Szymon Maksymiuk
#'
#'
#' @return An object of the class 'explainer'.
#'
#' \bold{Example of Python code available at documentation \code{\link{explain_scikitlearn}}}\cr
#'
#' \bold{Errors use case}\cr
#' Here is shortened version of solution for specific errors \cr
#' \cr
#' \bold{There already exists environment with a name specified by given .yml file}\cr
#' If you provide .yml file that in its header contains name exact to name of environment that already exists, existing will be set active without changing it. \cr
#' You have two ways of solving that issue. Both connected with anaconda prompt. First is removing conda env with command: \cr
#' \code{conda env remove --name myenv}\cr
#' And execute function once again. Second is updating env via: \cr
#' \code{conda env create -f environment.yml}\cr
#' \cr
#' \bold{Conda cannot find specified packages at channels you have provided.}\cr
#' That error may be caused by a lot of things. One of those is that specified version is too old to be available from the official conda repo.
#' Edit Your .yml file and add link to proper repository at channels section.\cr
#' \cr
#' Issue may be also connected with the platform. If model was created on the platform with different OS yo may need to remove specific version from .yml file.\cr
#' \code{- numpy=1.16.4=py36h19fb1c0_0}\cr
#' \code{- numpy-base=1.16.4=py36hc3f5095_0}\cr
#' In the example above You have to remove \code{=py36h19fb1c0_0} and \code{=py36hc3f5095_0} \cr
#' If some packages are not available for anaconda at all, use pip statement\cr
#' \cr
#' If .yml file seems not to work, virtual env can be created manually using anaconda promt. \cr
#' \code{conda create -n name_of_env python=3.4} \cr
#' \code{conda install -n name_of_env name_of_package=0.20} \cr
#'
#'
#' @import DALEX
#'
#' @examples
#' 
#' library("DALEXtra")
#' \dontrun{
#' 
#' if (Sys.info()["sysname"] != "Darwin") {
#'    # Explainer build (Keep in mind that 9th column is target)
#'    create_env(system.file("extdata", "testing_environment.yml", package = "DALEXtra"))
#'    test_data <-
#'    read.csv(
#'    "https://raw.githubusercontent.com/jbrownlee/Datasets/master/pima-indians-diabetes.data.csv",
#'    sep = ",")
#'    # Keep in mind that when pickle is being built and loaded,
#'    # not only Python version but libraries versions has to match aswell
#'    explainer <- explain_keras(system.file("extdata", "keras.pkl", package = "DALEXtra"),
#'    condaenv = "myenv",
#'    data = test_data[,1:8], y = test_data[,9])
#'    plot(model_performance(explainer))
#' 
#'    # Predictions with newdata
#'    predict(explainer, test_data[1:10,1:8])
#' }
#'
#'}
#'
#' @rdname explain_keras
#' @export

explain_keras <-
  function(path,
           yml = NULL,
           condaenv = NULL,
           env = NULL,
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

    prepeare_env(yml, condaenv, env)

    model <- dalex_load_object(path, "keras")

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
