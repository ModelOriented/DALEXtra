#' Compare champion with challengers globally
#'
#' The function creates objects that present global model performance using various measures. Those date can be easily
#' plotted with \code{plot} function. It uses \code{auditor} package to create \code{\link[auditor]{model_performance}} of all passed
#' explainers. Keep in mind that type of task has to be specified.
#'
#' @param champion - explainer of champion model.
#' @param challengers - explainer of challenger model or list of explainers.
#' @param type - type of the task. Either classification or regression
#'
#' @return An object of the class overall_comparison
#'
#'  It is a named list containing following fields:
#' \itemize{
#' \item \code{radar} list of \code{\link[auditor]{model_performance}} objects and other parameters that will be passed to generic \code{plot} function
#' \item \code{accordance} data.frame object of champion responses and challenger's corresponding to them. Used to plot accordance.
#' \item \code{models_info} data.frame containing information about models used in analysis
#' }
#'
#' @rdname overall_comparison
#' @export
#'
#' @examples
#' \donttest{
#' library("DALEXtra")
#' library("mlr")
#' task <- mlr::makeRegrTask(
#'   id = "R",
#'   data = apartments,
#'   target = "m2.price"
#' )
#' learner_lm <- mlr::makeLearner(
#'   "regr.lm"
#' )
#' model_lm <- mlr::train(learner_lm, task)
#' explainer_lm <- explain_mlr(model_lm, apartmentsTest, apartmentsTest$m2.price, label = "LM")
#'
#' learner_rf <- mlr::makeLearner(
#'   "regr.ranger"
#' )
#' model_rf <- mlr::train(learner_rf, task)
#' explainer_rf <- explain_mlr(model_rf, apartmentsTest, apartmentsTest$m2.price, label = "RF")
#'
#' learner_gbm <- mlr::makeLearner(
#'   "regr.gbm"
#' )
#' model_gbm <- mlr::train(learner_gbm, task)
#' explainer_gbm <- explain_mlr(model_gbm, apartmentsTest, apartmentsTest$m2.price, label = "gbm")
#'
#' data <- overall_comparison(explainer_lm, list(explainer_gbm, explainer_rf), type = "regression")
#' plot(data)
#' }

overall_comparison <- function(champion, challengers, type) {
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

  if (is.null(champion$y_hat)) {
    stop("Explain function has to be run with precalculate TRUE")
  }
  models_info <- data.frame(label = champion$label, class = class(champion$model)[1], type = "Champion", stringsAsFactors = FALSE)
  for (e in challengers) {
    models_info <- rbind(models_info,
                         list(label = e$label, class = class(e$model)[1], type = "Challenger"),
                         stringsAsFactors = FALSE)
  }

  if (type == "classification") {
    radar_args <- lapply(challengers, auditor::model_performance, score = NULL, new_score = new_scores)
    radar_args$object <- auditor::model_performance(champion, score = NULL, new_score = new_scores)
    radar_args$verbose <- FALSE
    yhats <- NULL
    for (e in challengers) {
      yhats <- rbind(yhats,
               data.frame("Champion" = champion$y_hat,
                          "Challenger" = e$y_hat,
                          "Label" = e$label),
               stringsAsFactors = FALSE)
    }
    ret <- list("radar" = radar_args, "accordance" = yhats, "models_info" = models_info)
  } else if (type == "regression") {
    radar_args <- lapply(challengers, auditor::model_performance)
    radar_args$object <- auditor::model_performance(champion)
    radar_args$verbose <- FALSE
    yhats <- NULL
    for (e in challengers) {
     yhats <- rbind(yhats,
              data.frame("Champion" = champion$y_hat,
                         "Challenger" = e$y_hat,
                         "Label" = e$label),
              stringsAsFactors = FALSE)
    }
    ret <- list("radar" = radar_args, "accordance" = yhats, "models_info" = models_info)
  } else {
    stop("Task has to be either classification or regression")
  }
   class(ret) <- "overall_comparison"
   ret
}

#' Print overall_comparison object
#'
#' @param x an object of class \code{overall_comparison}
#' @param ... other parameters
#'
#' @export
#' @examples
#' \donttest{
#' library("DALEXtra")
#' library("mlr")
#' task <- mlr::makeRegrTask(
#'   id = "R",
#'   data = apartments,
#'   target = "m2.price"
#' )
#' learner_lm <- mlr::makeLearner(
#'   "regr.lm"
#' )
#' model_lm <- mlr::train(learner_lm, task)
#' explainer_lm <- explain_mlr(model_lm, apartmentsTest, apartmentsTest$m2.price, label = "LM")
#'
#' learner_rf <- mlr::makeLearner(
#'   "regr.ranger"
#' )
#' model_rf <- mlr::train(learner_rf, task)
#' explainer_rf <- explain_mlr(model_rf, apartmentsTest, apartmentsTest$m2.price, label = "RF")
#'
#' learner_gbm <- mlr::makeLearner(
#'   "regr.gbm"
#' )
#' model_gbm <- mlr::train(learner_gbm, task)
#' explainer_gbm <- explain_mlr(model_gbm, apartmentsTest, apartmentsTest$m2.price, label = "gbm")
#'
#' data <- overall_comparison(explainer_lm, list(explainer_gbm, explainer_rf), type = "regression")
#' print(data)
#' }

print.overall_comparison <- function(x, ...) {
   cat("Radar Args: ", length(x$radar)-1, "model_performances detected\n")
   cat("Accordance table head\n")
   print(head(x$accordance))
   cat("Models Info\n")
   print(head(x$models_info))
}

confusionmatrix <- function(explainer) {
  yhat <- as.numeric(explainer$y_hat > 0.5)
  TP <- sum(yhat[yhat == 1] == explainer$y[yhat == 1])
  FP <- length(yhat[yhat == 1]) - TP
  TN <- sum(yhat[yhat == 0] == explainer$y[yhat == 0])
  FN <- length(yhat[yhat == 0]) - TN
  list(
    "TP" = TP,
    "FP" = FP,
    "TN" = TN,
    "FN" = FN
  )
}

new_scores <- list(
  "1-auc" = function(au) {
    1 - auditor::score(au, score = "auc")$score
  },
  "1-acc" = function(au) {
    conf <- confusionmatrix(au)
    1 - (conf$TP + conf$TN) / (conf$TP + conf$FP + conf$TN + conf$FN)
  },
  "1-precission" = function(au) {
    conf <- confusionmatrix(au)
    1 - conf$TP / (conf$TP + conf$FP)
  },
  "1-recall" = function(au) {
    conf <- confusionmatrix(au)
    1 - conf$TP / (conf$TP + conf$FN)
  },
  "1-specificity" = function(au) {
    conf <- confusionmatrix(au)
    1 - conf$TN / (conf$TN + conf$FP)
  },
  "1-F1" = function(au) {
    conf <- confusionmatrix(au)
    1 - (2 * (conf$TP / (conf$TP + conf$FP)) * (conf$TP / (conf$TP + conf$FN))) /
      (conf$TP / (conf$TP + conf$FN) + conf$TP / (conf$TP + conf$FP))
  }
)
