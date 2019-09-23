#' Compare performance of model between training and test set
#'
#' Function \code{training_test_comparison} calculates performance of the provided model based on specified measure function.
#' Response of the model is caluclated based on test data, extracted from the explainer and training data, provided by the user.
#' Output can be easily shown with \code{print} or \code{plot} function.
#'
#' @param champion - explainer of champion model.
#' @param challengers - explainer of challenger model or list of explainers.
#' @param training_data - data without target column that will be passed to predict function and then to measure function. Keep in mind that
#'                        they have to differ from data passed to an explainer.
#' @param training_y - target column for \code{training_data}
#' @param measure_function - measure function that calculates performance of model based on true observation and prediction.
#'                           Order of parameters is important and should be (y, y_hat). By default it is RMSE.
#'
#' @return An object of the class \code{training_test_comparison}.
#'
#' It is a named list containig:
#' \itemize{
#' \item \code{data} data.frame with following columns
#'   \itemize{
#'   \item \code{measure_test} performance on test set
#'   \item \code{measure_train} performance on training set
#'   \item \code{label} label of explainer
#'   \item \code{type} flag that indicates if explainer was passed as champion or as challenger.
#' }
#' \item \code{models_info} data.frame containig inforamtion about models used in analysys
#' }

#'
#' @rdname trainig_test_comparison
#' @export
#'
#' @examples
#' library(DALEXtra)
#' titanic_train <- read.csv(system.file("extdata", "titanic_train.csv", package = "DALEXtra"))
#' titanic_test <- read.csv(system.file("extdata", "titanic_test.csv", package = "DALEXtra"))
#'
#' h2o::h2o.init()
#' h2o::h2o.no_progress()
#' titanic_h2o <- h2o::as.h2o(titanic_train)
#' titanic_h2o["survived"] <- h2o::as.factor(titanic_h2o["survived"])
#' titanic_test_h2o <- h2o::as.h2o(titanic_test)
#' model <- h2o::h2o.gbm(
#'   training_frame = titanic_h2o,
#'   y = "survived",
#'   distribution = "bernoulli",
#'   ntrees = 500,
#'   max_depth = 4,
#'   min_rows =  12,
#'   learn_rate = 0.001
#' )
#' explainer_h2o <- explain_h2o(model, titanic_test[,1:17], titanic_test[,18])
#'
#' explainer_scikit <- explain_scikitlearn(system.file("extdata",
#'                                                     "scikitlearn.pkl",
#'                                                     package = "DALEXtra"),
#'                                         yml = system.file("extdata",
#'                                                           "testing_environment.yml",
#'                                                           package = "DALEXtra"),
#'                                         data = titanic_test[,1:17],
#'                                         y = titanic_test$survived)
#'
#' library("mlr")
#' task <- mlr::makeClassifTask(
#'   id = "R",
#'   data = titanic_train,
#'   target = "survived"
#' )
#' learner <- mlr::makeLearner(
#'   "classif.gbm",
#'   par.vals = list(
#'     distribution = "bernoulli",
#'     n.trees = 500,
#'     interaction.depth = 4,
#'     n.minobsinnode = 12,
#'     shrinkage = 0.001,
#'     bag.fraction = 0.5,
#'     train.fraction = 1
#'   ),
#'   predict.type = "prob"
#' )
#' gbm <- mlr::train(learner, task)
#' explainer_mlr <- explain_mlr(gbm, titanic_test[,1:17], titanic_test[,18])
#'
#' data <- training_test_comparison(explainer_scikit, list(explainer_h2o, explainer_mlr),
#'                                  training_data = titanic_train[,-18],
#'                                  training_y = titanic_train[,18])
#' plot(data)

training_test_comparison <- function(champion,
                                     challengers,
                                     training_data,
                                     training_y,
                                     measure_function = DALEX::loss_root_mean_square) {

  if (class(challengers) == "explainer") {
    challengers <- list(challengers)
  }

  if (any(sapply(challengers, function(x) {
    class(x) != "explainer"
  })) & class(champion) != "explainer") {
    stop("Champion and all of challengers has to be explainer objects")
  }

  if (is.null(champion$data)) {
    stop("Data argument has to be passed with explainer")
  }

  ret <- data.frame()

  models_info <- data.frame(label = champion$label, class = class(champion$model)[1], type = "Champion", stringsAsFactors = FALSE)
  for (e in challengers) {
    models_info <- rbind(models_info,
                         list(label = e$label, class = class(e$model)[1], type = "Challenger"),
                         stringsAsFactors = FALSE)
  }

  for (explainer in challengers) {
    pred_test <- predict(explainer, explainer$data)
    pred_train <- predict(explainer, training_data)
    measure_test <- measure_function(explainer$y, pred_test)
    measure_train <- measure_function(training_y, pred_train)
    ret <-
      rbind(
        ret,
        list(
          "measure_test" = measure_test,
          "measure_train" = measure_train,
          "label" = explainer$label,
          "type" = "Challenger"
        ),
        stringsAsFactors = FALSE
      )
  }

  pred_test_champion <- predict(champion, champion$data)
  pred_train_champion <- predict(champion, training_data)
  measure_test_champion <- measure_function(champion$y, pred_test_champion)
  measure_train_champion <- measure_function(training_y, pred_train_champion)
  ret <-
    rbind(
      ret,
      list(
        "measure_test" = measure_test_champion,
        "measure_train" = measure_train_champion,
        "label" = champion$label,
        "type" = "Champion"
      ),
      stringsAsFactors = FALSE
    )
  ret <- list(data = ret, models_info = models_info)
  class(ret) <- c("training_test_comparison")
  ret
}

#' Print funnel_measure object
#'
#' @param x an object of class \code{funnel_measure}
#' @param ... other parameters
#'
#' @export
#' @examples
#' library(DALEXtra)
#' titanic_train <- read.csv(system.file("extdata", "titanic_train.csv", package = "DALEXtra"))
#' titanic_test <- read.csv(system.file("extdata", "titanic_test.csv", package = "DALEXtra"))
#'
#' h2o::h2o.init()
#' h2o::h2o.no_progress()
#' titanic_h2o <- h2o::as.h2o(titanic_train)
#' titanic_h2o["survived"] <- h2o::as.factor(titanic_h2o["survived"])
#' titanic_test_h2o <- h2o::as.h2o(titanic_test)
#' model <- h2o::h2o.gbm(
#'   training_frame = titanic_h2o,
#'   y = "survived",
#'   distribution = "bernoulli",
#'   ntrees = 500,
#'   max_depth = 4,
#'   min_rows =  12,
#'   learn_rate = 0.001
#' )
#' explainer_h2o <- explain_h2o(model, titanic_test[,1:17], titanic_test[,18])
#'
#' explainer_scikit <- explain_scikitlearn(system.file("extdata",
#'                                                     "scikitlearn.pkl",
#'                                                     package = "DALEXtra"),
#'                                         yml = system.file("extdata",
#'                                                           "testing_environment.yml",
#'                                                           package = "DALEXtra"),
#'                                         data = titanic_test[,1:17],
#'                                         y = titanic_test$survived)
#'
#' library("mlr")
#' task <- mlr::makeClassifTask(
#'   id = "R",
#'   data = titanic_train,
#'   target = "survived"
#' )
#' learner <- mlr::makeLearner(
#'   "classif.gbm",
#'   par.vals = list(
#'     distribution = "bernoulli",
#'     n.trees = 500,
#'     interaction.depth = 4,
#'     n.minobsinnode = 12,
#'     shrinkage = 0.001,
#'     bag.fraction = 0.5,
#'     train.fraction = 1
#'   ),
#'   predict.type = "prob"
#' )
#' gbm <- mlr::train(learner, task)
#' explainer_mlr <- explain_mlr(gbm, titanic_test[,1:17], titanic_test[,18])
#'
#' data <- training_test_comparison(explainer_scikit, list(explainer_h2o, explainer_mlr),
#'                                  training_data = titanic_train[,-18],
#'                                  training_y = titanic_train[,18])
#' print(data)


print.training_test_comparison <- function(x, ...) {
  cat("Training test data head:\n")
  print(head(x$data))
  cat("Models Info\n")
  print(head(x$models_info))
}
