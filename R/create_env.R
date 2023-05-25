#' Create your conda virtual env with DALEX
#'
#' Python objects may be loaded into R. However, it requires versions of the Python and libraries to match between both machines.
#' This functions allow user to create conda virtual environment based on provided .yml file.
#'
#' @usage create_env(yml, condaenv)
#' @param yml a path to the .yml file. If OS is Windows conda has to be added to the PATH first
#' @param condaenv path to main conda folder. If OS is Unix You may want to specify it. When passed with windows, param will be omitted.
#'
#' @author Szymon Maksymiuk
#'
#' @return Name of created virtual env.
#'
#'
#' @examples
#' \dontrun{
#'   create_env(system.file("extdata", "testing_environment.yml", package = "DALEXtra"))
#' }
#' @rdname create_env
#' @export
create_env <- function(yml, condaenv = NULL) {
  if (.Platform$OS.type == "unix" & is.null(condaenv)) {
    if (is_conda()) {
      condaenv = paste(sub('[/][^/]+$', '', reticulate::conda_binary()[1]),
                       "/..",
                       sep = "")
      message(paste(
        "Path to conda not specified while on unix. Default used.",
        condaenv
      ))
    } else {
       stop("Conda not found")
    }
  }

  # Extract name of the environment that is stored in .yml header
  con <- file(yml, "r")
  first_line <- readLines(con, n = 1)
  close(con)
  # Name is stored in the pattern : "name: name_of_env" so we have to split the string
  name <- strsplit(first_line, split = " ")[[1]][2]

  # Check if specified env already exists
  if (name %in% reticulate::conda_list()$name) {
    message(sprintf("There already exists environment named the same as it is specified in .yml file - %s -  loading", name))
    return(name)
  }

  # Virtual env creation

  # Windows and linux has different shells
  if (.Platform$OS.type == "windows") {
    message(
      paste(
        "Virtual environment \"" ,
        name,
        "\" is being created. It may take few minutes.",
        sep = ""
      )
    )
    tryCatch(
      expr = {
        mes <-
          shell(paste("conda env create -f", yml, sep = " "), intern = TRUE)
      },
      warning = function(w) {
        mes <-
          shell(paste("conda env create -f", yml, sep = " "), intern = TRUE)

        if (any(grepl("not recognized", mes))) {
          cat(mes)
          stop(
            "conda is not recognised by your shell. Please set system variables for conda in order to use that function",
            call. = FALSE
          )
        }
        else if (any(grepl("ResolvePackageNotFound", mes))) {
          cat(mes)
          stop("Conda cannot find specified packages at channels you have provided.\n",
               "Try to add more channels (conda repositories) to your .yml file.",
               "Additionally, packages included in your .yml file may not be available for current Python version or OS. Try to remove exact versions specifications of particular libraries.\n",
               "If nothing above works, try to use 'pip:' statement",
                call. = FALSE)
        }
        else{
          cat(mes)
          stop(
            "Unrecognized error occured when creating anaconda virtual env. Try to configure you environment manually using Anaconda prompt. For usefull commands see ?explain_scikitlearn",
            call. = FALSE
          )
        }
      }
    )
  }
  if (.Platform$OS.type == "unix") {
    message(
      paste(
        "Virtual environment \"" ,
        name,
        "\" is being created. It may take few minutes.",
        sep = ""
      )
    )
    tryCatch(
      expr = {
        mes <-
          system(paste(condaenv,  "/bin/conda ", "env create -f ", yml, sep = ""),
                 intern = TRUE)
      },
      # Unix erros not partitioned since it is impossbile to capture whole shell output
      warning = function(w) {
        stop(w, "\n",
             "Conda cannot find specified packages at channels you have provided. Try to add more channels (conda repositories) to your .yml file.\n",
             "Packages included in your .yml file may not be available for current Python version or OS. Try to remove exact versions o libraries.\n",
             "If nothing above works, some of the packages are not avialable for your conda, try to use 'pip:' statement.\n",
             "Error has occured, check warnings() for possible problems",
             call. = FALSE)
      },
      error = function(e) {
        if (any(grepl("running command", e))) {
          stop(
            "conda is not recognised by your shell. Please check you conda path is correct in order to use that function",
            call. = FALSE
          )
        } else{
          stop(
            e, "\n",
            "Unrecognized error occured when creating anaconda virtual env, use warnings() to see it. Try to configure you environment manually using Anaconda prompt. For usefull commands see ?explain_scikitlearn",
            call. = FALSE
          )
        }
      }
    )
  }
  name
}
