# Check if conda is present. Of not warning will be rised.

.onLoad <- function(libname, pkgname) {
  tryCatch(reticulate::conda_binary(),
           error = function(e){
             warning("Anaconda not found on your computer. Conda related functionality such as create_env.R and condaenv and yml parameters from explain_scikitlearn will not be available")
           })
}
