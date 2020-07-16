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
#' library("mlr")
#' library("DALEXtra")
#' task <- mlr::makeRegrTask(
#'  id = "R",
#'   data = apartments,
#'    target = "m2.price"
#' )
#'  learner_lm <- mlr::makeLearner(
#'  "regr.lm"
#' )
#' model_lm <- mlr::train(learner_lm, task)
#' explainer_lm <- explain_mlr(model_lm, apartmentsTest, apartmentsTest$m2.price, label = "LM")
#'
#' learner_rf <- mlr::makeLearner(
#' "regr.randomForest"
#' )
#' model_rf <- mlr::train(learner_rf, task)
#' explainer_rf <- explain_mlr(model_rf, apartmentsTest, apartmentsTest$m2.price, label = "RF")
#'
#' learner_gbm <- mlr::makeLearner(
#' "regr.gbm"
#' )
#' model_gbm <- mlr::train(learner_gbm, task)
#' explainer_gbm <- explain_mlr(model_gbm, apartmentsTest, apartmentsTest$m2.price, label = "GBM")
#'
#' data <- training_test_comparison(explainer_lm, list(explainer_gbm, explainer_rf),
#'                                  training_data = apartments,
#'                                  training_y = apartments$m2.price)
#' plot(data)

training_test_comparison <- function(champion,
                                     challengers,
                                     training_data,
                                     training_y,
                                     measure_function = NULL) {

  if (class(challengers) == "explainer") {
    challengers <- list(challengers)
  }

  if (any(sapply(challengers, function(x) {
    class(x) != "explainer"
  })) | class(champion) != "explainer") {
    stop("Champion and all of challengers has to be explainer objects")
  }

  if (is.null(champion$data)) {
    stop("Data argument has to be passed with explainer")
  }

  if (is.null(measure_function)) {
    measure_function <- set_measure_function(champion, challengers)
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
#' library("mlr")
#' library("DALEXtra")
#' task <- mlr::makeRegrTask(
#'  id = "R",
#'   data = apartments,
#'    target = "m2.price"
#' )
#'  learner_lm <- mlr::makeLearner(
#'  "regr.lm"
#' )
#' model_lm <- mlr::train(learner_lm, task)
#' explainer_lm <- explain_mlr(model_lm, apartmentsTest, apartmentsTest$m2.price, label = "LM")
#'
#' learner_rf <- mlr::makeLearner(
#' "regr.randomForest"
#' )
#' model_rf <- mlr::train(learner_rf, task)
#' explainer_rf <- explain_mlr(model_rf, apartmentsTest, apartmentsTest$m2.price, label = "RF")
#'
#' learner_gbm <- mlr::makeLearner(
#' "regr.gbm"
#' )
#' model_gbm <- mlr::train(learner_gbm, task)
#' explainer_gbm <- explain_mlr(model_gbm, apartmentsTest, apartmentsTest$m2.price, label = "GBM")
#'
#' data <- training_test_comparison(explainer_lm, list(explainer_gbm, explainer_rf),
#'                                  training_data = apartments,
#'                                  training_y = apartments$m2.price)
#' print(data)


print.training_test_comparison <- function(x, ...) {
  cat("Training test data head:\n")
  print(head(x$data))
  cat("Models Info\n")
  print(head(x$models_info))
}
