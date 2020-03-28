library(DALEX)
library(reticulate)

# helper function to skip tests if we don't have the conda
skip_because_conda_conf_needed <- function() {
  if (!"CONDA_TEST" %in% names(Sys.getenv())) {
    skip("Conda test env needed for tests")
  }
}
skip_if_windows <- function() {
  if (.Platform$OS.type == "windows")
    skip("Test with unix")
}

# skip_if_no_mljar <- function() {
#   if (!"MLJAR_TOKEN" %in% names(Sys.getenv())) {
#     skip("MLJAR_TOKEN entry needed for tests")
#   }
# }

skip_if_no_java <- function() {
  if (!"JAVA" %in% names(Sys.getenv())) {
    skip("JAVA entry needed for tests")
  }
}
