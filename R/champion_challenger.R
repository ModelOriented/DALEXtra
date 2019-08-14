#' Create model comparison
#'
#' DALEX is designed to work with various black-box models like tree ensembles, linear models, neural networks etc.
#' Threfore we may want to compare our models using such a usefull tool.
#'
#' @param champion - explainer of model that is supposed to be challenged
#' @param challenger - explainer of model that is supposed to beat champion
#' @param variable - variable that is going to be used in "prediction versus varaible" comparison
#' @param type - classification or regression
#' @param control - rpart.control object that will be passed to rpart tree
#' @param ... - other arguments that will ba passed to rmarkdown::render()
#'
#' @return html raport
#'
#'
#' @rdname champion_challenger
#' @export
#' @examples
#' if(DALEXtra:::is_conda()) {
#' library("DALEXtra")
#' titanic_test <- read.csv(system.file("extdata", "titanic_test.csv", package = "DALEXtra"))
#' titanic_train <- read.csv(system.file("extdata", "titanic_train.csv", package = "DALEXtra"))
#' library("mlr")
#' task <- mlr::makeClassifTask(
#' id = "R",
#' data = titanic_train,
#' target = "survived"
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
#'
#' # Explainer build (Keep in mind that 18th column is target)
#' titanic_test <- read.csv(system.file("extdata", "titanic_test.csv", package = "DALEXtra"))
#' # Keep in mind that when pickle is being built and loaded,
#' # not only Python version but libraries versions has to match aswell
#' explainer_scikit <- explain_scikitlearn(system.file("extdata",
#'                                                     "scikitlearn.pkl",
#'                                                     package = "DALEXtra"),
#'                                         yml = system.file("extdata",
#'                                                           "scikitlearn_unix.yml",
#'                                                           package = "DALEXtra"),
#'                                         data = titanic_test[,1:17],
#'                                         y = titanic_test$survived)
#'
#' champion_challenger(explainer_mlr, explainer_scikit, type = "classification",
#'                     variable = c("fare", "age"))
#' } else {
#'   print('Conda is required.')
#' }


champion_challenger <-
  function(champion,
           challenger,
           variable = NULL,
           type,
           control = rpart::rpart.control(maxdepth = 4,
                                   minbucket = 27,
                                   cp = 0.0082212),
           ...) {
    if (control$maxdepth > 4) {
      stop("maxdepth has to be lower than 5")
    }
    if (class(champion) != "explainer" |
        class(challenger) != "explainer") {
      stop("Both, champion and challanger have to be explainer objects")
    }
    if (type == "classification") {
      rmarkdown::render(
        input = system.file("extdata", "ChampionChallenger_classif.Rmd", package = "DALEXtra"),
        output_file = "ChampionChallenger.html",
        output_dir = file.path(getwd()),
        ...
      )
    } else if (type == "regression") {
      rmarkdown::render(
        input = system.file("extdata", "ChampionChallenger_regr.Rmd", package = "DALEXtra"),
        output_file = "ChampionChallenger.html",
        output_dir = file.path(getwd()),
        ...
      )
    } else {
      stop("type argument has to be classification or regression")
    }
  }
