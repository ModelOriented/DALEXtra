# Helper functions for ChampionChallenger. Not supposed to be exported.

generate_chunk <- function(object, output_path, iterator, name)
  UseMethod("generate_chunk")

generate_chunk.overall_comparison <- function(object, output_path, iterator, name) {
  con_overall_comparison <- file(system.file("ChampionChallenger", "overall_comparison_section.Rmd", package = "DALEXtra"))
  lines_overall_comparison <- readLines(con_overall_comparison)
  lines_overall_comparison[6] <-
    gsub(
      x = lines_overall_comparison[6],
      pattern = "overall_comparison_data",
      replacement = paste("sections[[",
                          iterator,
                          "]]")
    )
  close(con_overall_comparison)
  write(lines_overall_comparison,
        file = output_path,
        append = TRUE)
}

generate_chunk.training_test_comparison <- function(object, output_path, iterator, name) {
  con_training_test_comparison <- file(system.file("ChampionChallenger", "training_test_comparison_section.Rmd", package = "DALEXtra"))
  lines_training_test_comparison <- readLines(con_training_test_comparison)
  lines_training_test_comparison[6] <-
    gsub(
      x = lines_training_test_comparison[6],
      pattern = "training_test_comparison_data",
      replacement = paste("sections[[",
                          iterator,
                          "]]")
    )
  close(con_training_test_comparison)
  write(lines_training_test_comparison,
        file = output_path,
        append = TRUE)
}

generate_chunk.funnel_measure <- function(object, output_path, iterator, name) {
  con_funnel_measure <- file(system.file("ChampionChallenger", "funnel_measure_section.Rmd", package = "DALEXtra"))
  lines_funnel_measure <- readLines(con_funnel_measure)
  lines_funnel_measure[5] <-
    gsub(
      x = lines_funnel_measure[5],
      pattern = "funnel_measure_data",
      replacement = paste("sections[[",
                          iterator,
                          "]]")
    )
  close(con_funnel_measure)
  write(lines_funnel_measure,
        file = output_path,
        append = TRUE)

}

generate_chunk.default <- function(object, output_path, iterator, name) {
  if(is.null(name) | name == "") {
    name <- class(object)[1]
  }
  chunk <- c(
    "",
    paste("# ", name, sep = ""),
    "",
    "```{r}",
    paste("plot(sections[[",
          iterator,
          "]])", sep = ""),
    "```",
    "",
    ""
  )
  write(chunk,
        file = output_path,
        append = TRUE)

}
