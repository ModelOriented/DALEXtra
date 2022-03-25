#' Instance Level Surrogate Models
#'
#' Interface to different implementations of the LIME method.
#' Find information how the LIME method works here: \url{https://ema.drwhy.ai/LIME.html}.
#'
#' @param explainer a model to be explained, preprocessed by the 'explain' function
#' @param new_observation a new observation for which predictions need to be explained
#' @param ... other parameters that will be passed to
#' @param type which implementation of thee LIME method should be used. Either \code{localModel} (default), \code{lime} or \code{iml}.
#' @param n_features will be passed to the lime implementation, by default 4
#' @param k will be passed to the iml implementation, by default 4
#' @param n_permutations will be passed to the lime implementation, by default 1000
#' @param size will be passed to the localModel implementation, by default 1000
#' @param seed seed for random number generator, by default 1313
#' @param labels will be passed to the lime implementation, by default first value in the y vector
#' @param x an object to be plotted
#' @param newdata alias for new_observation
#'
#' @return Depending on the \code{type} there are different classess of the resulting object.
#'
#' @aliases predict_parts_break_down predict_parts predict_parts_ibreak_down predict_parts_shap
#' @references Explanatory Model Analysis. Explore, Explain and Examine Predictive Models. \url{https://ema.drwhy.ai/}
#'
#' @name predict_surrogate
#' @export
predict_surrogate <- function(explainer, new_observation, ..., type = "localModel") {
  switch (type,
          "localModel" = predict_surrogate_local_model(explainer, new_observation, ...),
          "lime"       = predict_surrogate_lime(explainer, new_observation, ...),
          "iml"        = predict_surrogate_iml(explainer, new_observation, ...),
          stop("The type argument shall be either 'localModel' or 'iml' or 'lime'")
  )
}

#' @name predict_surrogate
# @importFrom lime lime explain plot_features
#' @export
predict_surrogate_local_model <- function(explainer,
                                          new_observation,
                                          size = 1000,
                                          seed = 1313, ...) {
  localModel::individual_surrogate_model(explainer,
                               new_observation,
                               size = size,
                               seed = seed)
}


#' @title Predict model for lime
#' @name predict_surrogate
#' @rawNamespace export(predict_model.dalex_explainer)
#' @export
predict_model.dalex_explainer <- function(x, newdata, ...) {
  class(x) = "explainer"
  pred <- predict(x, newdata)
  return(as.data.frame(pred))
}


#' @title Model type for lime
#' @name predict_surrogate
#' @rawNamespace export(model_type.dalex_explainer)
#' @export
model_type.dalex_explainer <- function(x, ...) {
  return("regression")
}


#' @name predict_surrogate
#' @export
predict_surrogate_lime <- function(explainer, new_observation, n_features = 4, n_permutations = 1000, labels = unique(explainer$y)[1], ...) {
  class(explainer) <- "dalex_explainer"

  # https://github.com/ModelOriented/DALEXtra/issues/73
  new_observation <- new_observation[, intersect(colnames(explainer$data), colnames(new_observation))]
  
  lime_model <- lime::lime(x = explainer$data[, colnames(new_observation)],
                        model = explainer)

  lime_expl <- lime::explain(x = new_observation,
                             explainer = lime_model,
                             n_features = n_features,
                             n_permutations = n_permutations,
                             ...)
  class(lime_expl) <- c("predict_surrogate_lime", class(lime_expl))
  lime_expl
}

#' @name predict_surrogate
#' @export
plot.predict_surrogate_lime <- function(x, ...) {
  class(x) <- class(x)[-1]
  lime::plot_features(x, ...)
}


# @import iml
#' @name predict_surrogate
#' @export
predict_surrogate_iml <- function(explainer, new_observation, k = 4, ...) {
  iml_model <- iml::Predictor$new(model = explainer$model, data = explainer$data[,colnames(new_observation)])
  iml::LocalModel$new(predictor = iml_model, x.interest = new_observation, k = k)
}
