#' Plot function for overall_comparison
#'
#' The function plots data created with \code{\link{overall_comparison}}. For radar plot it uses auditor's
#' \code{\link[auditor]{plot_radar}}. Keep in mind that the function creates two plots returned as list.
#'
#' @param x - data created with \code{\link{overall_comparison}}
#' @param ... - other parameters
#'
#' @return A named list of ggplot objects.
#'
#' It consists of:
#' \itemize{
#' \item \code{radar_plot} plot created with \code{\link[auditor]{plot_radar}}
#' \item \code{accordance_plot} accordance plot of responses. OX axis stand for champion response, while OY for one of challengers
#'                              responses. Colour indicates on challenger.
#' }
#'
#' @importFrom graphics plot
#'
#' @rdname plot.overall_comparison
#' @export
#'
#' @examples
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
#'   "regr.randomForest"
#' )
#' model_rf <- mlr::train(learner_rf, task)
#' explainer_rf <- explain_mlr(model_rf, apartmentsTest, apartmentsTest$m2.price, label = "RF")
#'
#' learner_gbm <- mlr::makeLearner(
#'   "regr.gbm"
#' )
#' model_gbm<- mlr::train(learner_gbm, task)
#' explainer_gbm <- explain_mlr(model_gbm, apartmentsTest, apartmentsTest$m2.price, label = "GBM")
#'
#' data <- overall_comparison(explainer_lm, list(explainer_gbm, explainer_rf), type = "regression")
#' plot(data)

plot.overall_comparison <- function(x, ...) {
  overall_comparison <- x
  if (!requireNamespace("auditor")) {
    stop("Please download auditor package to access that functionallity")
  }
  data <- overall_comparison
  p <- do.call(plot, data$radar)

  q <- ggplot(data = data$accordance, aes_string(x = data$accordance$Champion, y = data$accordance$Challenger, colour = data$accordance$Label))+
    geom_point()+
    geom_abline(slope = 1, intercept = 0, size = 1, color = "#8bdcbe", show.legend = TRUE)+
    labs(x = "Champion response",
         y = "Challenger response",
         colour = "Challengers")+
     theme_drwhy()

  list("radar_plot" = p, "accordance_plot" = q)
}
