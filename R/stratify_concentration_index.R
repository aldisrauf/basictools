#' Stratify Concentration Index
#'
#' Computes concentration indices and determines the final strata efficiently 
#' using the `data.table` package. Calculations are performed per domain (e.g., 
#' district or region), comparing individual variable values against a baseline 
#' reference value (Q1, median, mean, or a custom percentile).
#'
#' @param data A data frame or `data.table` containing the raw dataset.
#' @param id Character string; column name containing unique unit identifiers (e.g., `"bs"`).
#' @param domain Character string; column name defining grouping domains or regions (e.g., `"idkabkot"`).
#' @param varname Character vector; column names of numeric variables to index.
#' @param ref_method Character string; summary method to compute reference values per domain.
#'   Options are `"q1"`, `"median"`, `"mean"`, or `"percentile"`. Default is `"q1"`.
#' @param ref_percentile Numeric value between 0 and 1; used when `ref_method = "percentile"`. Default is `0.25`.
#'
#' @return A named list containing three components:
#' \itemize{
#'   \item \code{strata}: A \code{data.table} containing unit ID, domain, variable rankings per row (\code{r1}, \code{r2}, ...), and \code{strata_final}.
#'   \item \code{ref_values}: A \code{data.table} of calculated reference values per domain for each variable.
#'   \item \code{ref_method}: Character string indicating the reference method used.
#' }
#'
#' @details
#' The main steps executed by the function:
#' \enumerate{
#'   \item Calculate reference values (Q1, median, mean, or percentile) per domain for non-zero values.
#'   \item Merge domain reference values back into the primary dataset.
#'   \item Compute the Concentration Index (\code{CI = N / Ref}) only when \code{N > 0} and \code{N >= Ref}. Otherwise, \code{CI = 0}.
#'   \item Rank variables row-wise by highest Concentration Index (breaking ties using the smallest raw value \code{N}).
#'   \item Determine \code{strata_final}: Assigns \code{r1} (top rank) if present, otherwise assigns \code{"Non-concentrated"}.
#' }
#'
#' @examples
#' library(data.table)
#'
#' # Sample dataset
#' df <- data.frame(
#'   idkabkot = c("1101", "1101", "1102", "1102"),
#'   bs = c("BS01", "BS02", "BS03", "BS04"),
#'   JAGUNG = c(10, 0, 5, 15),
#'   KEDELAI = c(8, 2, 0, 6),
#'   UBIKAYU = c(20, 5, 0, 10)
#' )
#'
#' result <- stratify_concentration_index(
#'   data = df,
#'   id = "bs",
#'   domain = "idkabkot",
#'   varname = c("JAGUNG", "KEDELAI", "UBIKAYU"),
#'   ref_method = "q1"
#' )
#'
#' # View final stratification results
#' head(result$strata)
#'
#' # Reference values per domain
#' result$ref_values
#'
#' @export
#' @import data.table
stratify_concentration_index <- function(data, id, domain, varname,
                                ref_method = "q1",
                                ref_percentile = 0.25) {

  ref_method <- tolower(ref_method)
  if (!ref_method %in% c("q1", "median", "mean", "percentile"))
    stop('ref_method must be one of: "q1", "median", "mean", or "percentile"')

  dt <- as.data.table(data)

  # --- 1. Compute reference values per domain
  fun_ref <- switch(
    ref_method,
    "q1" = function(x) quantile(x[x > 0], 0.25, na.rm = TRUE),
    "median" = function(x) median(x[x > 0], na.rm = TRUE),
    "mean" = function(x) mean(x[x > 0], na.rm = TRUE),
    "percentile" = function(x) quantile(x[x > 0], ref_percentile, na.rm = TRUE)
  )

  ref_domain <- dt[, lapply(.SD, fun_ref), by = domain, .SDcols = varname]
  setnames(ref_domain, varname, paste0(varname, "_ref"))

  # --- 2. Merge reference values back into the main dataset
  dt <- merge(dt, ref_domain, by = domain, all.x = TRUE)

  # --- 3. Compute concentration index (N / ref) with filter N >= Ref
  #     If N == 0 or N < Ref, then CI = 0
  for (v in varname) {
    dt[[paste0(v, "_ik")]] <- ifelse(
      dt[[v]] > 0 & dt[[v]] >= dt[[paste0(v, "_ref")]],  # Rule: N > 0 AND N >= Ref
      dt[[v]] / dt[[paste0(v, "_ref")]],
      0
    )
  }

  # --- 4. Row-wise ranking based on index values
  ik_mat <- as.matrix(dt[, paste0(varname, "_ik"), with = FALSE])
  N_mat  <- as.matrix(dt[, varname, with = FALSE])

  rank_list <- lapply(seq_len(nrow(ik_mat)), function(i) {
    ik <- ik_mat[i, ]
    N <- N_mat[i, ]
    df <- data.frame(var = varname, ik = ik, N = N)
    
    # Keep only variables with CI > 0
    df <- df[df$ik > 0, ]
    
    # Jika tidak ada variabel yang memenuhi syarat, return vektor kosong
    if (nrow(df) == 0) return(character(0))
    
    # Sort by highest CI, break ties with smallest raw value N
    df <- df[order(-df$ik, df$N), ]
    
    df$var
  })

  # --- 5. Convert to data.table with dynamic padding
  max_rank <- max(sapply(rank_list, length))
  
  if (max_rank == 0) {
    # Edge case: No variable meets the concentration threshold across all rows
    rank_dt <- dt[, c(id, domain), with = FALSE]
    rank_dt[, strata_final := "Non konsentrasi"]

    for (j in seq_along(varname)) {
      rank_dt[, paste0("r", j) := NA_character_]
    }
  } else {
    # Padding setiap vektor ranking dengan NA sampai panjang max_rank
    rank_padded <- lapply(rank_list, function(x) {
      if (length(x) < max_rank) {
        c(x, rep(NA_character_, max_rank - length(x)))
      } else {
        x
      }
    })
  
    rank_vars <- do.call(rbind, rank_padded)
    colnames(rank_vars) <- paste0("r", seq_len(max_rank))
    
    rank_dt <- cbind(dt[, c(id, domain), with = FALSE], rank_vars)
    
    # --- 6. Determine strata_final
    rank_dt[, strata_final := ifelse(
      is.na(r1) | r1 == "", 
      "Non konsentrasi", 
      r1
    )]
  }

  # --- 7. Return output list
  return(list(
    strata = rank_dt,
    ref_values = ref_domain,
    ref_method = ref_method
  ))
}