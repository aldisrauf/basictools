#' Jalankan Aplikasi Shiny Sederhana
#'
#' Menjalankan aplikasi Shiny contoh sederhana yang ada dalam package.
#'
#' @export
run_simple_app <- function() {
  app_dir <- system.file("shiny_examples", package = "mypackage")
  if (app_dir == "") {
    stop("Folder aplikasi Shiny tidak ditemukan di dalam package.", call. = FALSE)
  }
  shiny::runApp(app_dir, display.mode = "normal")
}
