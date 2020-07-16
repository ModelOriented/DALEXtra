#' Compare machine learning models
#'
#' Determining if one model is better than the other one is a difficult task. Mostly because there is a lot of fields that have to be
#' covered to make such a judgemnt. Overall performance, performance on the crucial subset, distribution of residuals, those are only
#' few among many ideas related to that issue. Following function allow user to create a report based on various sections. Each says something different
#' about relation between champion and challengers. \code{DALEXtra} package share 3 base sections which are \code{\link{funnel_measure}}
#' \code{\link{overall_comparison}} and \code{\link{training_test_comparison}} but any object that has generic \code{plot} function can
#' be inculded at report.
#'
#'
#' @param sections - list of sections to be attached to report. Could be sections available with DALEXtra which are \code{\link{funnel_measure}}
#' \code{\link{training_test_comparison}}, \code{\link{overall_comparison}} or any other explanation that can work with \code{plot} function. Please
#' provide name for not standard sections, that will be presented as section titles. Oterwise class of the object will be used.
#' @param dot_size - dot_size argument passed to \code{\link{plot.funnel_measure}} if \code{\link{funnel_measure}} section present
#' @param output_dir_path - path to directory where Report should be created. By default it is current working directory.
#' @param output_name - name of the Report. By default it is "Report"
#' @param model_performance_table - If TRUE and \code{\link{overall_comparison}} section present, table of scores will be displayed.
#' @param title - Title for report, by default it is "ChampionChallenger".
#' @param author - Author of , report. By default it is current user name.
#' @param ... - other parameters passed to rmarkdown::render.
#'
#' @return rmarkdown report
#'
#' @examples
#' \donttest{
#' library("mlr")
#' library("DALEXtra")
#' task <- mlr::makeRegrTask(
#'  id = "R",
#'   data = apartments,
#'    target = "m2.price"
#'  )
#'  learner_lm <- mlr::makeLearner(
#'    "regr.lm"
#'  )
#'  model_lm <- mlr::train(learner_lm, task)
#'  explainer_lm <- explain_mlr(model_lm, apartmentsTest, apartmentsTest$m2.price, label = "LM")
#'
#'  learner_rf <- mlr::makeLearner(
#'  "regr.randomForest"
#'  )
#'  model_rf <- mlr::train(learner_rf, task)
#'  explainer_rf <- explain_mlr(model_rf, apartmentsTest, apartmentsTest$m2.price, label = "RF")
#'
#'  learner_gbm <- mlr::makeLearner(
#'  "regr.gbm"
#'  )
#'  model_gbm <- mlr::train(learner_gbm, task)
#'  explainer_gbm <- explain_mlr(model_gbm, apartmentsTest, apartmentsTest$m2.price, label = "GBM")
#'
#'
#'  plot_data <- funnel_measure(explainer_lm, list(explainer_rf, explainer_gbm),
#'                           nbins = 5, measure_function = DALEX::loss_root_mean_square)
#'
#' champion_challenger(list(plot_data), dot_size = 3)
#' }
#'
#' @rdname champion_challenger
#' @export

champion_challenger <- function(sections,
                                dot_size = 4,
                                output_dir_path = getwd(),
                                output_name = "Report",
                                model_performance_table = FALSE,
                                title = "ChampionChallenger",
                                author = Sys.info()[["user"]],
                                ...) {

  output_path <-
    paste(output_dir_path,
          .Platform$file.sep,
          output_name,
          ".Rmd",
          sep = "")
  header <- c("---",
              paste("title: \"", title, "\"", sep = ""),
              paste("author: \"", author, "\"", sep = ""))
  con_introduction <- file(system.file("ChampionChallenger", "Introduction.Rmd", package = "DALEXtra"))
  introduction <- readLines(con_introduction)
  close(con_introduction)
  write(header, file = output_path)
  write(introduction, file = output_path, append = TRUE)
  models_info <- NULL

  for (i in 1:length(sections)) {
    if (is.null(models_info) & !is.null(sections[[i]]$models_info)) {
      models_info <- sections[[i]]$models_info
    }
    generate_chunk(sections[[i]], output_path, i, names(sections)[i])
  }



  end <- c("# Session Info", "", "```{r}", "sessionInfo()", "```")
  write(end, file = output_path, append = TRUE)

  rmarkdown::render(input = output_path,
                    output_file = paste(output_name, ".html", sep = ""),
                    output_dir = output_dir_path,
                    quiet = TRUE,
                    ...)
}
