#' Funnel plot for difference in measures
#'
#' Function \code{plot.funnel_measure} creates funnel plot of differences in measures for two models across variable areas.
#' It uses data created with 'funnel_measure' function.
#'
#' @param x - funnel_measure object created with \code{\link{funnel_measure}} function.
#' @param ... - other parameters
#' @param dot_size - size of the dot on plots. Passed to \code{\link[ggplot2]{geom_point}}.
#'
#' @return ggplot object
#'
#' @import ggplot2
#' @rdname plot.funnel_measure
#' @export
#' @examples
#' library("mlr")
#' library("DALEXtra")
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
#' model_gbm <- mlr::train(learner_gbm, task)
#' explainer_gbm <- explain_mlr(model_gbm, apartmentsTest, apartmentsTest$m2.price, label = "GBM")
#'
#'
#' plot_data <- funnel_measure(explainer_lm, list(explainer_rf, explainer_gbm),
#'                             nbins = 5, measure_function = DALEX::loss_root_mean_square)
#' plot(plot_data)


plot.funnel_measure <- function(x, ..., dot_size = 4){
  funnel_measure <- x
  if(!"funnel_measure" %in% class(funnel_measure)) stop("Data is not a funnel_measure object")
    data <- funnel_measure$data
    champion_label <- funnel_measure$models_info[funnel_measure$models_info$type == "Champion",]$label
    p <- ggplot(data = data, aes_string(x = "Measure", y = "Variable")) +
    scale_y_discrete() +
    geom_point(aes_string(color = "Challenger"), size = dot_size) +
    ggrepel::geom_text_repel(aes_string(label = "Label"), color = "#371ea3") +
    geom_vline(xintercept = 0,  color = "#f05a71", size=0.5) +
    xlim(c(-max(abs(data$Measure)), max(abs(data$Measure)))) +
    xlab(paste("Champion (", champion_label, ") and Challengers measure difference", sep = ""))+
    ylab("")+
    labs(title = "Funnel Plot",
         subtitle = paste("For every colour, dot on the right side of red line means that Champion (",
                          champion_label,
                          ") is better. \nDot on the left means that one of the Challengers is better than Champion (",
                          champion_label,
                          ")",
                          sep = "")) +
    theme_drwhy()
    p

    #8bdcbe
}
