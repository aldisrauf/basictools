#' Jalankan Aplikasi Shiny Sederhana
#'
#' Menjalankan contoh aplikasi Shiny dari package basictools.
#'
#' @export
run_simple_app <- function() {
  app_dir <- system.file("shiny/simple_app", package = "basictools")
  if (app_dir == "") {
    stop("Folder aplikasi Shiny tidak ditemukan di dalam package.", call. = FALSE)
  }
  shiny::runApp(app_dir, display.mode = "normal")
}
