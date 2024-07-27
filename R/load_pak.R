#' Load Daily Packages
#'
#' This function utilizes the `pacman` package to load a set of commonly used packages for daily data analysis tasks.
#' The following packages are loaded:
#'
#' \describe{
#'   \item{rio}{A package for streamlined data import and export.}
#'   \item{tidyverse}{A collection of packages for data manipulation, visualization, and more, including `dplyr`, `ggplot2`, `tidyr`, and others.}
#'   \item{DescTools}{A package providing a variety of descriptive statistics tools and tests.}
#'   \item{janitor}{A package for data cleaning and examination, offering functions like `clean_names` and `remove_empty`.}
#'   \item{flextable}{A package for creating and formatting tables for reporting.}
#'   \item{openxlsx}{A package for reading, writing, and manipulating Excel files without the need for external dependencies.}
#' }
#'
#' @return Loads the specified packages into the R environment.
#' @export
#'
#' @examples
#' load_daily_packages()
load_daily_packages <- function(){
  pacman::p_load("rio", "tidyverse", "DescTools", "janitor", "flextable", "openxlsx")
}
