#' Caluculate difference in performance in models across different categories
#'
#' Function \code{funnel_measure} allows users to compare two models based on their explainers. It partitions dataset on which models were builded
#' and creates categories according to quantiles of columns in \code{parition data}. \code{nbins} parameter determinates number of qunatiles.
#' For each category difference in provided measure is being calculated. Positive value of that differnece means that Champion model
#' has better performance in specified category, while negative value means that one of the Challengers was better. Function allows
#' to compare multiple Challengers at once.
#'
#' @param champion - explainer of champion model.
#' @param challengers - explainer of challenger model or list of explainers.
#' @param measure_function - measure function that calculates performance of model based on true observation and prediction.
#'                           Order of parameters is important and should be (y, y_hat). The measure calculated by the function
#'                           should have the property that lower score value indicates better model. If NULL, RMSE will be used for regression,
#'                           one minus auc for classification and crossentropy for multiclass classification.
#' @param nbins - Number of qunatiles (partition points) for numeric columns. In case when more than one qunatile have the same value, there will be less partition points.
#' @param partition_data - Data by which test dataset will be paritioned for computation. Can be either data.frame or character vector.
#'                         When second is passed, it has to indicate names of columns that will be extracted fromm test data.
#'                         By default full test data. If data.frame, number of rows has to be equal to number of rows in test data.
#' @param cutoff - Threshold for categorical data. Entries less frequent than specified value will be merged into one category.
#' @param cutoff_name - Name for new category that arised after merging entries less frequent than \code{cutoff}
#' @param factor_conversion_threshold - Numeric columns with lower number of unique values than value of this parameter will be treated as factors
#' @param categories - a named list of variable names that will be plotted in a different colour. By deafault it is partitioned on Explanatory, External and Target.
#' @param show_info - Logical value indicating if progress bar should be shown.
#'
#' @return An object of the class \code{funnel_measure}
#'
#' It is a named list containing following fields:
#' \itemize{
#' \item \code{data} data.frame that consists of columns:
#'    \itemize{
#'    \item \code{Variable} Variable according to which partitions were made
#'    \item \code{Measure} Difference in measures. Positive value indicates that champion was better, while negative that challenger.
#'    \item \code{Label} String that defines subset of \code{Variable} values (partition rule).
#'    \item \code{Challenger} Label of challenger explainer that was used in \code{Measure}
#'    \item \code{Category} a category of the variable passed to function
#'    }
#' \item \code{models_info} data.frame containig inforamtion about models used in analysys
#' }
#'
#' @rdname funnel_measure
#' @export
#'
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @importFrom stats quantile
#'
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


funnel_measure <-
  function(champion,
           challengers,
           measure_function = NULL,
           nbins = 5,
           partition_data = champion$data,
           cutoff = 0.01,
           cutoff_name = "Other",
           factor_conversion_threshold = 7,
           show_info = TRUE,
           categories = NULL) {
    data <- champion$data

    if (class(challengers) == "explainer") {
      challengers <- list(challengers)
    }

    switch (class(partition_data),
      "character" = {
        partition_data <- data[partition_data]
      },
      "data.frame" = {},
      stop("Wrong format of partition_data. Has to be either character vector indicating columns from Champion's data or a data.frame")
    )

    if (any(sapply(challengers, function(x) {
      class(x) != "explainer"
    })) | class(champion) != "explainer") {
      stop("Champion and all of challengers has to be explainer objects")
    }

    if (is.null(measure_function)) {
      measure_function <- set_measure_function(champion, challengers)
    }


    models_info <- data.frame(label = champion$label, class = class(champion$model)[1], type = "Champion", stringsAsFactors = FALSE)
    for (e in challengers) {
      models_info <- rbind(models_info,
                           list(label = e$label, class = class(e$model)[1], type = "Challenger"),
                           stringsAsFactors = FALSE)
    }
    #It iterates progress bar and col_names
    col_index <- 1
    y <- champion$y
    ret <- data.frame()
    col_names <- colnames(partition_data)
    if (show_info) {
      pb = txtProgressBar(
        min = 0,
        max = ncol(partition_data),
        initial = 0,
        style = 3
      )
    }
    for (col in partition_data) {
      if (is.character(col)) {
        col <- as.factor(col)
      }
      if (length(unique(col)) < factor_conversion_threshold) {
        col <- as.factor(col)
      }
      if (is.numeric(col)) {
        #Separate case for first entry as it has to be equal on the both sides of compratment due to semi constant variables
        quantiles <-
          round(quantile(col, probs = seq(0, 1, length.out = nbins+1)), 2)
        scoring_data <-
          data[(quantiles[1] <= col & col <= quantiles[2]),]
        scoring_y <-
          y[(quantiles[1] <= col & col <= quantiles[2])]
        champion_pred <- predict(champion, scoring_data)
        challengers_pred <- lapply(challengers, function(x) {
          predict(x, scoring_data)
        })
        diff <- lapply(challengers_pred, function(x) {
          measure_function(scoring_y, champion_pred) - measure_function(scoring_y, x)
        })
        for (d in 1:length(diff)) {
          ret <-
            rbind(
              ret,
              list(
                "Variable" = col_names[col_index],
                # Negative diff in order to have dots where champion is better on the right side
                "Measure" = -diff[[d]],
                "Label" = ifelse(
                  quantiles[1] == quantiles[2],
                  quantiles[1],
                  paste("[", quantiles[1], ", ", quantiles[2], "]", sep = "")
                ),
                "Challenger" = challengers[[d]]$label
              ),
              stringsAsFactors = FALSE
            )
        }
        #From 2 becasue first qunatile was in upper case. To (length(quantiles) - 1) becasue we have i+1 in the loop.
        for (i in 2:(length(quantiles) - 1)) {
          scoring_data <-
            data[(quantiles[i] < col & col <= quantiles[i + 1]),]
          scoring_y <-
            y[(quantiles[i] < col & col <= quantiles[i + 1])]
          #In case of empty compartment
          if (length(scoring_y) == 0)
            next()
          champion_pred <- predict(champion, scoring_data)
          challengers_pred <- lapply(challengers, function(x) {
            predict(x, scoring_data)
          })
          diff <- lapply(challengers_pred, function(x) {
            measure_function(scoring_y, champion_pred) - measure_function(scoring_y, x)
          })
          for (d in 1:length(diff)) {
            ret <-
              rbind(
                ret,
                list(
                  "Variable" = col_names[col_index],
                  # Negative diff in order to have dots where champion is better on the right side
                  "Measure" = -diff[[d]],
                  "Label" = ifelse(
                    quantiles[i] == quantiles[i + 1],
                    quantiles[i] ,
                    paste("(", quantiles[i], ", ", quantiles[i+1], "]", sep = "")
                  ),
                  "Challenger" = challengers[[d]]$label
                ),
                stringsAsFactors = FALSE
              )
          }
        }
      } else if (is.factor(col)) {
        col <- as.character(col)

        if (length(unique(col)) > 4) {
          freq <- table(col) / length(col)
          names_to_cut <- names(freq[freq < cutoff])
          for (name in names_to_cut) {
            col[col == name] <- cutoff_name
          }
        }
        for (level in sort(unique(col))) {
          scoring_data <-
            data[col == level,]
          scoring_y <-
            y[col == level]
          champion_pred <- predict(champion, scoring_data)
          challengers_pred <- lapply(challengers, function(x) {
            predict(x, scoring_data)
          })
          diff <- lapply(challengers_pred, function(x) {
            measure_function(scoring_y, champion_pred) - measure_function(scoring_y, x)
          })
          for (d in 1:length(diff)) {
            ret <-
              rbind(
                ret,
                list(
                  "Variable" = col_names[col_index],
                  # Negative diff in order to have dots where champion is better on the right side
                  "Measure" = -diff[[d]],
                  "Label" = level,
                  "Challenger" = challengers[[d]]$label
                ),

                stringsAsFactors = FALSE
              )
          }
        }
      } else {
        stop(paste("Not recognizable column type"), col_names[col_index])
      }

      if (show_info) setTxtProgressBar(pb, col_index)

      col_index <- col_index + 1
    }
    if (is.null(categories)) {
      data_names <- names(partition_data)
      features <- data_names[data_names %in% names(champion$data)]
      target_check <-  apply(partition_data, 2, function(x) {
        all(as.character(x) == as.character(champion$y))
      })
      target <- NULL
      if (any(target_check)) {
        target <- data_names[target_check]
      }
      other_variables <- setdiff(data_names, c(features, target))
      categories <- list(Explanatory = features, Target = target, External = other_variables)
    }
    ret$Category <- ""
    for (i in 1:length(categories)) {
      if (length(categories[[i]]) > 0){
        for (variable in categories[[i]]) {
          ret[ret$Variable == variable,]$Category <- names(categories)[i]
        }
      }
    }
    # Check if they are Variables without category
    if (length(ret[ret$Category == "",]$Category) > 0){
      ret[ret$Category == "",]$Category <- "Other Variables"
    }
    ret <- list(data = ret, models_info = models_info)
    names(ret$data$Label) <- NULL
    class(ret) <- c("funnel_measure")
    ret
  }

#' Print funnel_measure object
#'
#' @param x an object of class \code{funnel_measure}
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
#' plot_data <- funnel_measure(explainer_lm, list(explainer_rf, explainer_gbm),
#'                             nbins = 5, measure_function = DALEX::loss_root_mean_square)
#' print(plot_data)
#' }

print.funnel_measure <- function(x, ...) {
  cat("Funnel measure head:\n")
  print(head(x$data))
  cat("Models Info\n")
  print(head(x$models_info))
}


