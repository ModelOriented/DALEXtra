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
#' \donttest{
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
#' }


plot.funnel_measure <- function(x, ..., dot_size = 0.5){
  funnel_measure <- x
  Variable <- Measure <- Measure_min <- Measure_max <- Category <- Text <- L <- NULL
  if(!"funnel_measure" %in% class(funnel_measure)) stop("Data is not a funnel_measure object")
    champion_label <- funnel_measure$models_info[funnel_measure$models_info$type == "Champion",]$label
    challenger_label <- funnel_measure$models_info[funnel_measure$models_info$type == "Challenger",]$label
    funnel_measure$data$Measure_min <- unlist(lapply(funnel_measure$data$Measure, function(x){
      list("Measure_min" = min(0, x))
      }
    ))
    funnel_measure$data$Measure_max <- unlist(lapply(funnel_measure$data$Measure, function(x){
      list("Measure_max" = max(0, x))
      }
    ))
    p <- lapply(challenger_label, function(y){
      data <- funnel_measure$data
      data = data[data$Challenger == y, ]
      data$Text <- unlist(lapply(unique(data$Variable), function(x){
        tmp <- data[data$Variable == x, ]
        ifelse(tmp$Measure == max(tmp$Measure), 1,
               ifelse(tmp$Measure == min(tmp$Measure) & tmp$Measure != max(tmp$Measure), -1, 0))
        }
      ))
      data$L <- ifelse(data$Text == -1 | data$Text == 1, data$Label, "")

      ggplot(data, aes(x = Variable, y = Measure)) +
        geom_pointrange(aes(ymin = Measure_min, ymax = Measure_max, color = Category),
                        position = position_dodge2(width = 0.75), size =
                          dot_size) +
        scale_color_manual(values = c(colors_discrete_drwhy(3))) +
        geom_hline(yintercept = 0, color = "#371ea3", size=1) +
        ylim(c(-max(abs(data$Measure))-20, max(abs(data$Measure))+20)) +
        ylab(paste("Champion (", champion_label, ") and Challengers measure difference", sep = "")) +
        xlab("") +
        labs(title = "Funnel Plot",
             subtitle = paste("For every category, dot on the right side of violet line means that Champion (",
                              champion_label,
                              ") is better. \nDot on the left means that one of the Challengers is better than Champion (",
                              champion_label,
                              ")",
                              sep = "")) +
        theme_drwhy() +
        theme(panel.grid = element_blank()) +
        geom_vline(xintercept = seq(1.5, 1.5 + length(unique(data$Variable))), linetype = "dotdash", color = "#371ea3")+
        geom_text(aes(y = Text * max(abs(Measure)), label = L),
                  hjust = 0.5, vjust = 0.5,
                  position = position_dodge2(width = 0.75),
                  color = "#371ea3") +
        coord_flip()

    })
    names(p) <- unlist(lapply(challenger_label, function(x){paste0("challanger_", x)}))
    p
}
