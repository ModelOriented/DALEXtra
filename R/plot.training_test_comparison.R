#' Plot and compare performance of model between training and test set
#'
#' Function \code{plot.training_test_comparison} plots dependecy between model performance on test and trainig set based on
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

plot.training_test_comparison <- function(x, ...) {
  training_test_comparison <- x
  if(!"training_test_comparison" %in% class(training_test_comparison)) stop("Data is not a training_test_comparison object")
  data <- training_test_comparison$data
  ggplot(data, aes(x = "measure_train", y = "measure_test")) +
    geom_abline(
      slope = 1,
      intercept = 0,
      size = 1,
      color = "#8bdcbe",
      show.legend = TRUE
    ) +
    geom_point(aes(colour = "type")) +
    scale_color_manual(values = c("Challenger" = "orange", "Champion" = "black")) +
    ggrepel::geom_text_repel(aes(label = "label"), color = "#371ea3") +
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
