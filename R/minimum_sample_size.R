#' Calculate Sample Size for Proportion
#'
#' This function calculates the required sample size for estimating a proportion in a finite population, given the desired margin of error, confidence level, design effect, and expected response rate.
#'
#' @param p Numeric. The estimated proportion (must be between 0 and 1).
#' @param e Numeric. The margin of error (must be between 0 and 1).
#' @param N Numeric. The population size (must be greater than 0).
#' @param alpha Numeric. The significance level, default is 0.05.
#' @param deff Numeric. The design effect, default is 1.
#' @param rr Numeric. The expected response rate, default is 0.9.
#' @param moe_type Character. The type of margin of error, either "relative" or "absolute", default is "relative".
#'
#' @return The calculated sample size.
#' @examples
#' samplesize_prop(p = 0.5, e = 0.05, N = 10000)
#' @export
samplesize_prop <- function(p, e, N, alpha = 0.05, deff = 1, rr = 0.9, moe_type = "relative"){

  if(p <= 0 | p >= 1){
    stop("p (proportion) must be (0, 1)")
  }

  if(e <= 0 | e >= 1){
    stop("e (margin of error) must be (0, 1)")
  }

  if(rr <= 0 | rr >= 1){
    stop("rr (expected response rate) must be (0, 1)")
  }

  if(N < 0){
    stop("N (number of population) must be larger than 0")
  }

  if(!(moe_type %in% c("relative", "absolute"))){
    stop("moe_type must be either 'relative' or 'absolute'")
  }

  Z = qnorm(alpha/2, lower.tail = FALSE)
  vari = p * (1 - p)

  term1 = N * Z^2 * vari
  term2 = Z^2 * vari
  term3 = ifelse(moe_type == "relative", N * e * p, N * e)
  term4 = term1 / (term2 + term3)
  term5 = ceiling(term4 * deff / rr)

  term6 = ifelse(term5 > N, N, term5)

  return(term6)
}


#' Calculate Sample Size for Mean
#'
#' This function calculates the required sample size for estimating a mean in a finite population, given the desired margin of error, confidence level, design effect, and expected response rate.
#'
#' @param x Numeric. The estimated mean.
#' @param sd Numeric. The standard deviation of the population.
#' @param e Numeric. The margin of error (must be between 0 and 1).
#' @param N Numeric. The population size (must be greater than 0).
#' @param alpha Numeric. The significance level, default is 0.05.
#' @param deff Numeric. The design effect, default is 1.
#' @param rr Numeric. The expected response rate, default is 0.9.
#'
#' @return The calculated sample size.
#' @examples
#' samplesize_mean(x = 50, sd = 10, e = 0.05, N = 10000)
#' @export
samplesize_mean <- function(x, sd, e, N, alpha = 0.05, deff = 1, rr = 0.9){

  if(e <= 0 | e >= 1){
    stop("e (margin of error) must be (0, 1)")
  }

  if(rr <= 0 | rr >= 1){
    stop("rr (expected response rate) must be (0, 1)")
  }

  if(N < 0){
    stop("N (number of population) must be larger than 0")
  }

  Z = qnorm(alpha/2, lower.tail = FALSE)
  vari = sd^2

  term1 = N * Z^2 * vari
  term2 = Z^2 * vari
  term3 = N * e * x
  term4 = term1 / (term2 + term3)
  term5 = ceiling(term4 * deff / rr)

  term6 = ifelse(term5 > N, N, term5)

  return(term6)
}

#' Calculate Sample Size Based on Previous Survey's Relative Standard Error
#'
#' This function calculates the required sample size to achieve a target relative standard error (RSE) for a finite population, based on the RSE and sample size from a previous survey.
#'
#' @param rse_target Numeric. The target relative standard error (must be greater than 0).
#' @param rse_prev Numeric. The relative standard error from the previous survey (must be greater than 0).
#' @param n_prev Numeric. The sample size from the previous survey which generated the given RSE (must be greater than 0).
#' @param N Numeric. The population size (must be greater than 0).
#' @param rr Numeric. The expected response rate (must be between 0 and 1).
#'
#' @return The calculated sample size.
#' @examples
#' samplesize_res_prev(rse_target = 0.05, rse_prev = 0.1, n_prev = 500, N = 10000, rr = 0.9)
#' @export
samplesize_res_prev <- function(rse_target, rse_prev, n_prev, N, rr){
  if(rse_target <= 0){
    stop("rse_target must be greater than 0")
  }

  if(rse_prev <= 0){
    stop("rse_prev must be greater than 0")
  }

  if(n_prev <= 0){
    stop("n_prev (sample size in previous survey which generate RSE with rse_prev value) must be greater than 0")
  }

  if(N < 0){
    stop("N (number of population) must be larger than 0")
  }

  if(rr <= 0 | rr >= 1){
    stop("rr (expected response rate) must be (0, 1)")
  }

  term1 = (rse_prev / rse_target)^2 * n_prev
  term2 = ceiling(term1 / rr)

  term3 = ifelse(term2 > N, N, term2)

  return(term3)
}

#' Calculate Sample Size Using Slovin's Formula
#'
#' This function calculates the required sample size for a finite population using Slovin's formula, given the desired absolute margin of error, design effect, and expected response rate.
#'
#' @param d Numeric. The absolute margin of error (must be between 0 and 1).
#' @param N Numeric. The population size (must be greater than 0).
#' @param deff Numeric. The design effect, default is 1.
#' @param rr Numeric. The expected response rate, default is 0.9.
#'
#' @return The calculated sample size.
#' @examples
#' samplesize_slovin(d = 0.05, N = 10000)
#' @export
samplesize_slovin <- function(d, N, deff = 1, rr = 0.9){

  if(d <= 0 | d >= 1){
    stop("d (absolute margin of error) must be (0, 1)")
  }

  if(rr <= 0 | rr >= 1){
    stop("rr (expected response rate) must be (0, 1)")
  }

  if(N < 0){
    stop("N (number of population) must be larger than 0")
  }

  term1 = N / (1 + N * d^2)
  term2 = ceiling(term1 * deff / rr)

  term3 = ifelse(term2 > N, N, term2)

  return(term3)
}


