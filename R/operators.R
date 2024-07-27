#' Add Corresponding Elements of Two Vectors or Lists
#'
#' This operator adds corresponding elements of two vectors or lists, removing NAs.
#'
#' @param x A numeric vector or list.
#' @param y A numeric vector or list of the same length as `x`.
#' @return A vector or list of the same length as `x` and `y`, with each element being the sum of corresponding elements of `x` and `y`.
#' @export
#'
#' @examples
#' c(1, 2, 3) %+% c(4, 5, NA)  # Returns c(5, 7, 3)
`%+%` <- function(x, y) {
  mapply(sum, x, y, MoreArgs = list(na.rm = TRUE))
}

#' Subtract Corresponding Elements of Two Vectors or Lists
#'
#' This operator subtracts corresponding elements of two vectors or lists. If both elements are `NA`, the result is `NA`, otherwise `NA` values are removed before subtraction.
#'
#' @param x A numeric vector or list.
#' @param y A numeric vector or list of the same length as `x`.
#' @return A vector or list of the same length as `x` and `y`, with each element being the difference of corresponding elements of `x` and `y`.
#' @export
#'
#' @examples
#' c(1, 2, 3) %-% c(4, 5, 6)  # Returns c(-3, -3, -3)
#' c(1, NA, 3) %-% c(NA, 2, NA)  # Returns c(1, -2, 3)
`%-%` <- function(x, y) {
  mapply(function(a, b) ifelse(is.na(a) & is.na(b), NA, sum(a, -b, na.rm = TRUE)), x, y)
}
