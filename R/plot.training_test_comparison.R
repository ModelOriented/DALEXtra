#' Plot and compare performance of model between training and test set
#'
#' Function \code{plot.training_test_comparison} plots dependecy between model performance on test and trainig dataset based on
#' \code{training_test_comparison} object. Green line indicates \code{y = x} line.
#'
#' @param x - object created with \code{\link{training_test_comparison}} function.
#' @param ... - other parameters
#'
#' @return ggplot object
#'
#' @import ggplot2
#'
#' @rdname plot.trainig_test_comparison
#' @export
#'
#' @examples
#'
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
#'

plot.training_test_comparison <- function(x, ...) {
  training_test_comparison <- x
  if(!"training_test_comparison" %in% class(training_test_comparison)) stop("Data is not a training_test_comparison object")
  data <- training_test_comparison$data
  ggplot(data, aes_string(x = "measure_train", y = "measure_test")) +
    geom_abline(
      slope = 1,
      intercept = 0,
      size = 1,
      color = "#8bdcbe",
      show.legend = TRUE
    ) +
    geom_point(aes_string(colour = "type")) +
    scale_color_manual(values = c("Challenger" = "orange", "Champion" = "black")) +
    ggrepel::geom_text_repel(aes_string(label = "label"), color = "#371ea3") +
    xlim(c(
      min(data$measure_train) - 0.05,
      max(data$measure_train) + 0.05
    )) +
    ylim(c(min(data$measure_test) - 0.05,
           max(data$measure_test) + 0.05
    )) +
    labs(x = "Train set score", y = "Test set score", colour = "") +
    theme_drwhy()
}
