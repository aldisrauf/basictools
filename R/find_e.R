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
find_e <- function(p, N, n, deff = 1, rr = 0.9, alpha = 0.05, moe_type = "relative") {
  # Check length consistency
  max_len <- max(length(p), length(N), length(n), length(deff), length(rr), length(alpha))

  # Recycle scalars if needed
  p <- rep(p, length.out = max_len)
  N <- rep(N, length.out = max_len)
  n <- rep(n, length.out = max_len)
  deff <- rep(deff, length.out = max_len)
  rr <- rep(rr, length.out = max_len)
  alpha <- rep(alpha, length.out = max_len)

  # Basic input checks
  if (any(p <= 0 | p >= 1)) stop("All p (proportion) must be in (0, 1)")
  if (any(rr <= 0 | rr > 1)) stop("All rr (response rate) must be in (0, 1]")
  if (any(N < 0)) stop("All N must be >= 0")
  if (any(n <= 0)) stop("All n must be > 0")
  if (!(moe_type %in% c("relative", "absolute"))) stop("moe_type must be either 'relative' or 'absolute'")

  # Variance and constants
  var <- p * (1 - p)
  Z <- qnorm(alpha / 2, lower.tail = FALSE)
  Y <- deff / rr
  n2 <- n / Y

  # Term calculation
  if (moe_type == "absolute") {
    term1 <- Z^2 * var
    term2 <- (N - n2) / (N * n2)
    term3 <- sqrt(term1 * term2)
  } else {
    term1 <- Z^2 * var / p^2
    term2 <- (N - n2) / (N * n2)
    term3 <- sqrt(term1 * term2)
  }

  # Handle special case when N == n
  e <- ifelse(N == n, 0, term3)

  return(e)
}

#' Calculate Margin of Error for Estimating a Mean
#'
#' This function calculates the margin of error for estimating a mean in a finite population, given the population size, sample size, design effect, response rate, confidence level.
#'
#' @param x Numeric. The estimated mean.
#' @param sd Numeric. The estimated standard deviation.
#' @param N Numeric. The population size (must be greater than 0).
#' @param n Numeric. The sample size (must be greater than 0).
#' @param deff Numeric. The design effect, default is 1.
#' @param rr Numeric. The expected response rate, default is 0.9.
#' @param alpha Numeric. The significance level, default is 0.05.
#'
#' @return The calculated margin of error.
#' @examples
#' find_e(x = 20, sd = 5, N = 10000, n = 400, deff = 1, rr = 0.9, alpha = 0.05)
#' @export
find_e_mean <- function(x, sd, N, n, deff = 1, rr = 0.9, alpha = 0.05) {
  # Check length consistency
  max_len <- max(length(x), length(sd), length(N), length(n), length(deff), length(rr), length(alpha))

  # Recycle scalars if needed
  x <- rep(x, length.out = max_len)
  sd <- rep(sd, length.out = max_len)
  N <- rep(N, length.out = max_len)
  n <- rep(n, length.out = max_len)
  deff <- rep(deff, length.out = max_len)
  rr <- rep(rr, length.out = max_len)
  alpha <- rep(alpha, length.out = max_len)

  # Basic input checks
  if (any(rr <= 0 | rr > 1)) stop("All rr (response rate) must be in (0, 1]")
  if (any(N < 0)) stop("All N must be >= 0")
  if (any(n <= 0)) stop("All n must be > 0")

  # Variance and constants
  var <- sd^2
  Z <- qnorm(alpha / 2, lower.tail = FALSE)
  Y <- deff / rr
  n2 <- n / Y

  # Term calculation
  term1 <- Z^2 * var / x^2
  term2 <- (N - n2) / (N * n2)
  term3 <- sqrt(term1 * term2)

  # Handle special case when N == n
  e <- ifelse(N == n, 0, term3)

  return(e)
}

