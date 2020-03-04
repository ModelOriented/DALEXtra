#' Create explainer from your mljar model
#'
#' DALEX is designed to work with various black-box models like tree ensembles, linear models, neural networks etc.
#' Unfortunately packages that create such models are very inconsistent between many platforms and programming languages. Different tools use different interfaces to train, validate and use models.
#' One of those tools, we would like to make more accessible is mljar.
#'
#'
#' @param model object - a mljar model to be explained
#' @param project_title character - a name of project_title  in which model was built. Without it predictions are unreachable.
#' @param data data.frame or matrix - data that was used for fitting. If not provided then will be extracted from the model. Data should be passed without target column (this shall be provided as the \code{y} argument). NOTE: If target variable is present in the \code{data}, some of the functionalities my not work properly.
#' @param y numeric vector with outputs / scores. If provided then it shall have the same size as \code{data}
#' @param weights numeric vector with sampling weights. By default it's \code{NULL}. If provided then it shall have the same length as \code{data}
#' @param predict_function function that takes two arguments: model and new data and returns numeric vector with predictions
#' @param residual_function function that takes three arguments: model, data and response vector y. It should return a numeric vector with model residuals for given data. If not provided, response residuals (\eqn{y-\hat{y}}) are calculated.
#' @param ... other parameters
#' @param label character - the name of the model. By default it's extracted from the 'class' attribute of the model
#' @param verbose if TRUE (default) then diagnostic messages will be printed.
#' @param precalculate if TRUE (default) then 'predicted_values' and 'residuals' are calculated when explainer is created. This will happenn also if 'verbose' is TRUE
#' @param colorize if TRUE (default) then \code{WARNINGS}, \code{ERRORS} and \code{NOTES} are colorized. Will work only in the R console.
#' @param model_info a named list (\code{package}, \code{version}, \code{type}) containg information about model. If \code{NULL}, \code{DALEX} will seek for information on it's own.
#'
#' @return explainer object (\code{\link[DALEX]{explain}}) ready to work with DALEX
#'
#' @import DALEX
#' @importFrom DALEX yhat
#' @importFrom utils read.csv
#'
#'
#' @examples
#' \dontrun{
#' library("DALEXtra")
#' library(mljar)
#' titanic_test <- read.csv(system.file("extdata", "titanic_test.csv", package = "DALEXtra"))
#'
#' model <- mljar_fit(titanic_test[,1:17], titanic_test[,18],
#'                    proj_title="Project title", exp_title="experiment title",
#'                    algorithms = c("logreg"), metric = "logloss")
#' # It Works
#' explainer <- explain_mljar(model, project_title = "Project title",
#'                            data = titanic_test[,1:17], y = titanic_test[,18])
#' # But it works aswell
#' explainer <- explain_mljar(model, project_title = "Project title",
#'                            verbose = FALSE, precalculate = FALSE)
#' }


# explain_mljar <-
#   function(model,
#            project_title,
#            data = NULL,
#            y = NULL,
#            weights = NULL,
#            predict_function = NULL,
#            residual_function = NULL,
#            ...,
#            label = NULL,
#            verbose = TRUE,
#            precalculate = TRUE,
#            colorize = TRUE,
#            model_info = NULL) {
#
#     if (!"MLJAR_TOKEN" %in% names(Sys.getenv())) {
#       stop(
#         "In order to use this function it is necessary to ad \"MLJAR_TOEKN\" to Your environmental variables. For more info see How to is it? section at https://github.com/mljar/mljar-api-python/blob/master/README.md"
#       )
#     }
#
#     # Temporary solution until new version of mljar arrive on CRAN
#     class(model) <- "mljar_model"
#
#     # If model was created in current session of system, we may try to extract data
#     if (is.null(data)) {
#       message("Data not specified, trying extract default...")
#       err <-
#         try(data <- read.csv(mljar::get_datasets(model$hid)$datasets[[1]]$file_name),
#             silent = TRUE)
#       if (class(err) == "try-error") {
#         stop("Extracting default data failed")
#       } else {
#         message("Done")
#         if (is.null(y)) {
#           y <- data["target"]
#         }
#       }
#     }
#
#     # mljar does not store information about project title and we cannot do predictions without it. Therefore we expand model with that field
#     model$project <- project_title
#     explain(
#       model,
#       data = data,
#       y = y,
#       weights = weights,
#       predict_function = predict_function,
#       residual_function = residual_function,
#       ...,
#       label = label,
#       verbose = verbose,
#       precalculate = precalculate,
#       colorize = colorize,
#       model_info = model_info
#     )
#   }
