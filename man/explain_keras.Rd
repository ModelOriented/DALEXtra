% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/explain_keras.R
\name{explain_keras}
\alias{explain_keras}
\title{Wrapper for Python Keras Models}
\usage{
explain_keras(
  path,
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
  colorize = !isTRUE(getOption("knitr.in.progress")),
  model_info = NULL,
  type = NULL
)
}
\arguments{
\item{path}{a path to the pickle file. Can be used without other arguments if you are sure that active Python version match pickle version.}

\item{yml}{a path to the yml file. Conda virtual env will be recreated from this file. If OS is Windows conda has to be added to the PATH first}

\item{condaenv}{If yml param is provided, a path to the main conda folder. If yml is null, a name of existing conda environment.}

\item{env}{A path to python virtual environment.}

\item{data}{data.frame or matrix - data which will be used to calculate the explanations. If not provided, then it will be extracted from the model. Data should be passed without a target column (this shall be provided as the \code{y} argument). NOTE: If the target variable is present in the \code{data}, some of the functionalities may not work properly.}

\item{y}{numeric vector with outputs/scores. If provided, then it shall have the same size as \code{data}}

\item{weights}{numeric vector with sampling weights. By default it's \code{NULL}. If provided, then it shall have the same length as \code{data}}

\item{predict_function}{function that takes two arguments: model and new data and returns a numeric vector with predictions.   By default it is \code{yhat}.}

\item{predict_function_target_column}{Character or numeric containing either column name or column number in the model prediction object of the class that should be considered as positive (i.e. the class that is associated with probability 1). If NULL, the second column of the output will be taken for binary classification. For a multiclass classification setting, that parameter cause switch to binary classification mode with one vs others probabilities.}

\item{residual_function}{function that takes four arguments: model, data, target vector y and predict function (optionally). It should return a numeric vector with model residuals for given data. If not provided, response residuals (\eqn{y-\hat{y}}) are calculated. By default it is \code{residual_function_default}.}

\item{...}{other parameters}

\item{label}{character - the name of the model. By default it's extracted from the 'class' attribute of the model}

\item{verbose}{logical. If TRUE (default) then diagnostic messages will be printed}

\item{precalculate}{logical. If TRUE (default) then \code{predicted_values} and \code{residual} are calculated when explainer is created.
This will happen also if \code{verbose} is TRUE. Set both \code{verbose} and \code{precalculate} to FALSE to omit calculations.}

\item{colorize}{logical. If TRUE (default) then \code{WARNINGS}, \code{ERRORS} and \code{NOTES} are colorized. Will work only in the R console. Now by default it is \code{FALSE} while knitting and \code{TRUE} otherwise.}

\item{model_info}{a named list (\code{package}, \code{version}, \code{type}) containing information about model. If \code{NULL}, \code{DALEX} will seek for information on it's own.}

\item{type}{type of a model, either \code{classification} or \code{regression}. If not specified then \code{type} will be extracted from \code{model_info}.}
}
\value{
An object of the class 'explainer'.

\bold{Example of Python code available at documentation \code{\link{explain_scikitlearn}}}\cr

\bold{Errors use case}\cr
Here is shortened version of solution for specific errors \cr
\cr
\bold{There already exists environment with a name specified by given .yml file}\cr
If you provide .yml file that in its header contains name exact to name of environment that already exists, existing will be set active without changing it. \cr
You have two ways of solving that issue. Both connected with anaconda prompt. First is removing conda env with command: \cr
\code{conda env remove --name myenv}\cr
And execute function once again. Second is updating env via: \cr
\code{conda env create -f environment.yml}\cr
\cr
\bold{Conda cannot find specified packages at channels you have provided.}\cr
That error may be caused by a lot of things. One of those is that specified version is too old to be available from the official conda repo.
Edit Your .yml file and add link to proper repository at channels section.\cr
\cr
Issue may be also connected with the platform. If model was created on the platform with different OS yo may need to remove specific version from .yml file.\cr
\code{- numpy=1.16.4=py36h19fb1c0_0}\cr
\code{- numpy-base=1.16.4=py36hc3f5095_0}\cr
In the example above You have to remove \code{=py36h19fb1c0_0} and \code{=py36hc3f5095_0} \cr
If some packages are not available for anaconda at all, use pip statement\cr
\cr
If .yml file seems not to work, virtual env can be created manually using anaconda promt. \cr
\code{conda create -n name_of_env python=3.4} \cr
\code{conda install -n name_of_env name_of_package=0.20} \cr
}
\description{
Keras models may be loaded into R environment like any other Python object. This function helps to inspect performance of Python model
and compare it with other models, using R tools like DALEX. This function creates an object that is easily accessible R version of Keras model
exported from Python via pickle file.
}
\examples{

library("DALEXtra")
\dontrun{

if (Sys.info()["sysname"] != "Darwin") {
   # Explainer build (Keep in mind that 9th column is target)
   create_env(system.file("extdata", "testing_environment.yml", package = "DALEXtra"))
   test_data <-
   read.csv(
   "https://raw.githubusercontent.com/jbrownlee/Datasets/master/pima-indians-diabetes.data.csv",
   sep = ",")
   # Keep in mind that when pickle is being built and loaded,
   # not only Python version but libraries versions has to match aswell
   explainer <- explain_keras(system.file("extdata", "keras.pkl", package = "DALEXtra"),
   condaenv = "myenv",
   data = test_data[,1:8], y = test_data[,9])
   plot(model_performance(explainer))

   # Predictions with newdata
   predict(explainer, test_data[1:10,1:8])
}

}

}
\author{
Szymon Maksymiuk
}
