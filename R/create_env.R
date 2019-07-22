#' Create Your conda virtual env with DALEX
#'
#' Phython objects may be loaded into R. However, it requiers versions of the Python and libraries to match bewtween both machines.
#'


create_env <- function(yml, condaenv) {
  if(.Platform$OS.type=="unix" & is.null(condaenv)){
    stop("You have to specify condaenv at platforms with unix-like os")
  }

  con <- file(yml, "r")
  first_line <- readLines(con, n = 1)
  close(con)
  name <- strsplit(first_line, split = " ")[[1]][2]

  # Virtual env creation

  # Windows and linux has different shells
  if (.Platform$OS.type == "windows") {
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
        else if (any(grepl("already exists", mes))) {
          cat(mes)
          stop(
            "There already exists environment with a name specified by given .yml file",
            call. = FALSE
          )
        }
        else if (any(grepl("ResolvePackageNotFound", mes))) {
          cat(mes)
          warning("Conda cannot find specified packages at channels you have provided.",
                  call. = FALSE)
          warning("Try to add more channels (conda repositories) to your .yml file.",
                  call. = FALSE)
          warning(
            "Packages included in your .yml file may not be available for current Python version or OS. Try to remove exact versions o libraries",
            call. = FALSE
          )
          warning("If nothing above works, try to use 'pip:' statement",
                  call. = FALSE)
          stop(
            "Some of the packages are not avialable for your conda. See warnings() for more information",
            call. = FALSE
          )
        }
        else{
          cat(mes)
          stop(
            "Unrecognized error occured when creating anaconda virtual env. Try to configure you environment manually using Anaconda prompt. For usefull commands see ?scikitlearn_model",
            call. = FALSE
          )
        }

      }
    )
  }
  if (.Platform$OS.type == "unix") {
    tryCatch(
      expr = {
        mes <-
          system(paste(conda,  "/bin/conda ", "env create -f ", yml, sep = ""),
                 intern = TRUE)
      },
      warning = function(w) {
        warning(w, call. = FALSE)

        warning(
          "There already exists environment with a name specified by given .yml file",
          call. = FALSE
        )
        warning(
          "Conda cannot find specified packages at channels you have provided. Try to add more channels (conda repositories) to your .yml file.",
          call. = FALSE
        )
        warning(
          "Packages included in your .yml file may not be available for current Python version or OS. Try to remove exact versions o libraries",
          call. = FALSE
        )
        warning(
          "If nothing above works, some of the packages are not avialable for your conda, try to use 'pip:' statement",
          call. = FALSE
        )

        stop("Error has occured, check warnings() for possible problems",
             call. = FALSE)

      },
      error = function(e) {
        if (any(grepl("running command", e))) {
          stop(
            "conda is not recognised by your shell. Please check you conda path is correct in order to use that function",
            call. = FALSE
          )
        }
        else{
          warning(e)
          stop(
            "Unrecognized error occured when creating anaconda virtual env, use warnings() to see it. Try to configure you environment manually using Anaconda prompt. For usefull commands see ?scikitlearn_model",
            call. = FALSE
          )
        }

      }
    )
  }
  name
}
