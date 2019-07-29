# Here are functions that are not supposed to be exported

dalex_load_object <- function(path){
  tryCatch(
    model <- reticulate::py_load_object(path),

    error = function(e) {
      if (grepl("UnicodeDecodeError", e)) {
        warning(paste(e), call. = FALSE)
        warning(
          "There is a problem with encoding mismatch. You are probably using Python 3.x and are  trying to load Python 2.7 object.
              Try to use virtual environment with matching encoding.",
          call. = FALSE
        )
      }
      else if (grepl("No module", e)) {
        warning(paste(e), call. = FALSE)
        warning(
          "Your Python environment is missing some modules. Please install them using \n conda install name_of_missing_module \n or \n pip install name_of_missing_module " ,
          call. = FALSE
        )
      }
      else{
        warning(paste(e), call. = FALSE)
      }

      stop(
        "Yours environment has to match environment where pickle file was created. It also includes encoding, python version, and libraries version. Specifying .yml file or path to virtual environment may help. For more information look warnings() and then ?explain_scikitlearn",
        call. = FALSE

      )
    }

  )
  model

}
