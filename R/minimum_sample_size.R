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
samplesize_prop <- function(p, e, N,
                            alpha = 0.05,
                            deff  = 1,
                            rr    = 0.9,
                            moe_type = "relative") {
  # pastikan moe_type tunggal dan valid
  if (length(moe_type) != 1 || !(moe_type %in% c("relative", "absolute"))) {
    stop("moe_type must be either 'relative' or 'absolute'")
  }

  # hitung panjang target
  lens <- c(length(p), length(e), length(N), length(deff), length(rr))
  m    <- max(lens)
  # cek konsistensi
  if (!all(lens %in% c(1, m))) {
    stop("All of p, e, N, deff, rr must be length 1 or length ", m)
  }
  # recycle semua ke panjang m
  p     <- rep(p,     length.out = m)
  e     <- rep(e,     length.out = m)
  N     <- rep(N,     length.out = m)
  deff  <- rep(deff,  length.out = m)
  rr    <- rep(rr,    length.out = m)

  # validasi nilai
  if (any(p <= 0 | p >= 1))     stop("p (proportion) must be in (0,1)")
  if (any(e <= 0 | e >= 1))     stop("e (margin of error) must be in (0,1)")
  if (any(rr <= 0 | rr > 1))    stop("rr (response rate) must be in (0,1]")
  if (any(N < 0))               stop("N (population size) must be >= 0")

  Z    <- qnorm(alpha/2, lower.tail = FALSE)
  vari <- p * (1 - p)

  term1 <- N * Z^2 * vari
  term2 <-     Z^2 * vari
  term3 <- if (moe_type == "relative") N * (e * p)^2 else N * e^2

  raw_n <- term1 / (term2 + term3)
  adj_n <- ceiling(raw_n * deff / rr)

  # tidak boleh lebih besar dari N
  pmin(adj_n, N)
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
samplesize_mean <- function(x, sd, e, N,
                            alpha = 0.05,
                            deff  = 1,
                            rr    = 0.9) {
  # hitung panjang terpanjang
  lens <- c(length(x), length(sd), length(e), length(N), length(deff), length(rr))
  m    <- max(lens)
  # cek konsistensi: semua harus 1 atau m
  if (!all(lens %in% c(1, m))) {
    stop("All of x, sd, e, N, deff, rr must be length 1 or length ", m)
  }
  # recycle semua ke panjang m
  x    <- rep(x,    length.out = m)
  sd   <- rep(sd,   length.out = m)
  e    <- rep(e,    length.out = m)
  N    <- rep(N,    length.out = m)
  deff <- rep(deff, length.out = m)
  rr   <- rep(rr,   length.out = m)

  # validasi
  if (any(e <= 0 | e >= 1))       stop("e (margin of error) must be in (0,1)")
  if (any(rr <= 0 | rr > 1))     stop("rr (response rate) must be in (0,1]")
  if (any(N < 0))                 stop("N (population size) must be >= 0")

  Z    <- qnorm(alpha/2, lower.tail = FALSE)
  vari <- sd^2

  term1 <- N * Z^2 * vari
  term2 <-     Z^2 * vari
  term3 <- N * (e * x)^2

  raw_n <- term1 / (term2 + term3)
  adj_n <- ceiling(raw_n * deff / rr)

  # batasi maksimum N
  pmin(adj_n, N)
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
samplesize_res_prev <- function(rse_target, rse_prev, n_prev, N, rr) {
  # Hitung panjang terpanjang
  lens <- c(length(rse_target), length(rse_prev), length(n_prev), length(N), length(rr))
  m    <- max(lens)
  # Cek konsistensi: semua argumen harus length 1 atau length m
  if (!all(lens %in% c(1, m))) {
    stop("All of rse_target, rse_prev, n_prev, N, rr must be length 1 or length ", m)
  }
  # Recycle semua ke panjang m
  rse_target <- rep(rse_target, length.out = m)
  rse_prev   <- rep(rse_prev,   length.out = m)
  n_prev     <- rep(n_prev,     length.out = m)
  N          <- rep(N,          length.out = m)
  rr         <- rep(rr,         length.out = m)

  # Validasi
  if (any(rse_target <= 0)) stop("rse_target must be greater than 0")
  if (any(rse_prev   <= 0)) stop("rse_prev   must be greater than 0")
  if (any(n_prev     <= 0)) stop("n_prev     must be greater than 0")
  if (any(N         <  0)) stop("N must be >= 0")
  if (any(rr <= 0 | rr > 1)) stop("rr (response rate) must be in (0,1]")

  # Perhitungan
  term1 <- (rse_prev / rse_target)^2 * n_prev
  term2 <- ceiling(term1 / rr)

  # Batasi maksimum N
  pmin(term2, N)
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
samplesize_slovin <- function(d, N,
                              deff = 1,
                              rr   = 0.9) {
  # Hitung panjang terpanjang
  lens <- c(length(d), length(N), length(deff), length(rr))
  m    <- max(lens)
  # Cek konsistensi: semua argumen harus length 1 atau length m
  if (!all(lens %in% c(1, m))) {
    stop("All of d, N, deff, rr must be length 1 or length ", m)
  }
  # Recycle semua ke panjang m
  d     <- rep(d,     length.out = m)
  N     <- rep(N,     length.out = m)
  deff  <- rep(deff,  length.out = m)
  rr    <- rep(rr,    length.out = m)

  # Validasi
  if (any(d <= 0 | d >= 1))    stop("d (absolute margin of error) must be in (0,1)")
  if (any(rr <= 0 | rr > 1))  stop("rr (response rate) must be in (0,1]")
  if (any(N < 0))              stop("N (population size) must be >= 0")

  # Perhitungan Slovin
  term1 <- N / (1 + N * d^2)
  term2 <- ceiling(term1 * deff / rr)

  # Batasi maksimum N
  pmin(term2, N)
}
