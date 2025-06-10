#' Jalankan Aplikasi Shiny Sederhana
#'
#' Menjalankan contoh aplikasi Shiny dari package basictools.
#'
#' @export
run_simple_app <- function() {
  # locate all the shiny app examples that exist
  validExamples <- list.files(system.file("shiny", package = "basictools"))

  validExamplesMsg <-
    paste0(
      "Valid examples are: '",
      paste(validExamples, collapse = "', '"),
      "'")

  # if an invalid example is given, throw an error
  if (missing(example) || !nzchar(example) ||
      !example %in% validExamples) {
    stop(
      'Please run `runExample()` with a valid example app as an argument.\n',
      validExamplesMsg,
      call. = FALSE)
  }

  # find and launch the app
  appDir <- system.file("shiny", example, package = "basictools")
  shiny::runApp(appDir, display.mode = "normal")
}
