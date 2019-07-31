# Check if conda is present. If not, warning will be rised.

.onLoad <- function(libname, pkgname) {
  is_conda <- try(reticulate::conda_binary(), silent = TRUE)
  if(class(is_conda) == "try-error") {
    warning("Anaconda not found on your computer. Conda related functionality such as create_env.R and condaenv and yml parameters from explain_scikitlearn will not be available")
  }

}
