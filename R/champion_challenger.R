#' Create model comparison
#'
#' DALEX is designed to work with various black-box models like tree ensembles, linear models, neural networks etc.
#' Threfore we may want to compare our models using such a usefull tool.
#'
#' @param champion - explainer of model that is supposed to be challenged
#' @param challenger - explainer of model that is supposed to beat champion
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
#' explainer_mlr <- explain_mlr(gbm, titanic_test[,1:17], titanic_test[,18])-> explainermlr
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
#' champion_challenger(explainer_mlr, explainer_scikit)
#' } else {
#'   print('Conda is required.')
#' }


champion_challenger <- function(champion, challenger, ...) {
  rmarkdown::render(
    input = system.file("extdata", "ChampionChallenger.Rmd", package = "DALEXtra"),
    output_file = "ChampionChallenger.html",
    output_dir = file.path(getwd()),
    ...
  )

}
