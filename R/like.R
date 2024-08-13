#' Pattern Matching Using SQL-Like Syntax
#'
#' This function performs pattern matching on character vectors using a syntax similar to SQL's `LIKE` operator. It supports the use of wildcards (`%`) at the beginning and/or end of the pattern to match any sequence of characters.
#'
#' @param x Character vector. The input vector to be searched.
#' @param pattern Character vector. The pattern(s) to search for. Use `%` as a wildcard at the start or end of the pattern to match any sequence of characters.
#'
#' @return A logical vector indicating whether each element of `x` matches any of the patterns.
#' @examples
#' # Check if elements contain "apple" anywhere in the string
#' c("apple", "banana", "grape") %like% "%apple%"
#'
#' # Check if elements start with "ba"
#' c("apple", "banana", "grape") %like% "ba%"
#'
#' # Check if elements end with "pe"
#' c("apple", "banana", "grape") %like% "%pe"
#' @export
`%like%` <- function (x, pattern)
{
  pattern <- sapply(pattern, function(z) {
    if (!substr(z, 1L, 1L) == "%") {
      z <- paste("^", z, sep = "")
    }
    else {
      z <- substr(z, 2L, nchar(z))
    }
    if (!substr(z, nchar(z), nchar(z)) == "%") {
      z <- paste(z, "$", sep = "")
    }
    else {
      z <- substr(z, 1L, nchar(z) - 1L)
    }
    return(z)
  })
  grepl(pattern = paste(pattern, collapse = "|"), x = x)
}
