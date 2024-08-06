#' Calculate Margin of Error for Estimating a Proportion
#'
#' This function calculates the margin of error for estimating a proportion in a finite population, given the population size, sample size, design effect, response rate, confidence level, and type of margin of error (relative or absolute).
#'
#' @param p Numeric. The estimated proportion (must be between 0 and 1).
#' @param N Numeric. The population size (must be greater than 0).
#' @param n Numeric. The sample size (must be greater than 0).
#' @param deff Numeric. The design effect, default is 1.
#' @param rr Numeric. The expected response rate, default is 0.9.
#' @param alpha Numeric. The significance level, default is 0.05.
#' @param moe_type Character. The type of margin of error, either "relative" or "absolute". Default is "relative".
#'
#' @return The calculated margin of error.
#' @examples
#' find_e(p = 0.5, N = 10000, n = 400, deff = 1, rr = 0.9, alpha = 0.05, moe_type = "relative")
#' @export
find_e <- function(p, N, n, deff = 1, rr = 0.9, alpha = 0.05, moe_type = "relative"){

  if(p <= 0 | p >= 1){
    stop("p (proportion) must be (0, 1)")
  }

  if(rr <= 0 | rr >= 1){
    stop("rr (expected response rate) must be (0, 1)")
  }

  if(N < 0){
    stop("N (number of population) must be larger than 0")
  }

  if(n <= 0){
    stop("n (number of sample) must be larger than 0")
  }

  if(!(moe_type %in% c("relative", "absolute"))){
    stop("moe_type must be either 'relative' or 'absolute'")
  }

  var = p * (1 - p)

  Z = qnorm(p = alpha / 2, lower.tail = FALSE)
  Y = deff / rr

  n2 = n / Y

  if(moe_type == "absolute"){
    term1 = Z^2 * var
    term2 = (N - n2) / (N * n2)
    term3 = sqrt(term1 * term2)
  } else {
    term1 = Z^2 * var / p^2
    term2 = (N - n2) / (N * n2)
    term3 = sqrt(term1 * term2)
  }

  e = ifelse(N == n, 0, term3)

  return(e)
}
