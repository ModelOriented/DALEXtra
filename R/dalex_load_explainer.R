#' DALEX load explainer
#' 
#' Load DALEX explainer created with Python library into the R environment.
#' 
#' @param path Path to the pickle file with explainer saved.
#' 
#' @details 
#' Function uses the \code{reticulate} package to load Python object saved
#' in a pickle and make it accessible within R session. It also adds explainer
#' class to the object so it can be used with DALEX R functions.
#' @export
dalex_load_explainer <- function(path) {
  dalex_load_object(path, "explainer")
}