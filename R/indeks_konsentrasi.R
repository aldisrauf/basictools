#' Indeks Konsentrasi Calculation
#'
#' This function calculates the concentration index for a given dataset, providing outputs for the ranking and evaluation of strata based on specified evaluation and ranking criteria.
#'
#' @param id A character vector specifying the ID variables for each observation.
#' @param varname A character vector specifying the variable names to calculate the concentration index.
#' @param domain A character vector specifying the domain or grouping variables.
#' @param data A data frame containing the input data.
#' @param eval A character string specifying the evaluation method, either "mean" or "percentile". Defaults to "mean".
#' @param percentile A numeric value between 0 and 1 specifying the percentile value for evaluation when `eval = "percentile"`.
#' @param eval_type An integer (1 to 4) specifying the evaluation type. Defaults to 1.
#' @param rank_type A character string specifying the ranking method, either "mean" or "q1". Defaults to "mean".
#' @param remove_zero_eval A logical value. If `TRUE`, zeros are excluded during evaluation. Defaults to `FALSE`.
#'
#' @return A list containing the following components:
#'  \itemize{
#'    \item \code{strata_final}: A data frame containing the final strata evaluation and calculated fields.
#'    \item \code{summarise}: A summary table grouping by domain and evaluated strata.
#'    \item \code{nonaktif}: A data frame of non-active rows (rows with zero totals across specified variables).
#'    \item \code{data}: The processed input data frame with adjustments applied.
#'  }
#' @examples
#' # Example dataset
#' df <- data.frame(
#'   id = c(1, 2, 3, 4),
#'   domain = c("A", "A", "B", "B"),
#'   var1 = c(10, 20, 0, 40),
#'   var2 = c(5, 15, 0, 35)
#' )
#'
#' # Run the function
#' result <- indeks_konsentrasi(
#'   id = "id",
#'   varname = c("var1", "var2"),
#'   domain = "domain",
#'   data = df,
#'   eval = "mean",
#'   eval_type = 1,
#'   rank_type = "mean"
#' )
#'
#' # Access results
#' result$strata_final
#' result$summarise
indeks_konsentrasi <- function(
    id, varname, domain, data, eval = "mean", percentile = NULL, eval_type = c(1, 2, 3, 4), rank_type = c("mean", "q1"), remove_zero_eval = FALSE
) {
  # Input validation
  if (!(eval %in% c("mean", "percentile"))) stop("eval hanya bernilai mean atau percentile")
  if (!(eval_type %in% c(1:4))) stop("eval_type hanya bernilai 1, 2, 3, 4")
  if (!(rank_type %in% c("mean", "q1"))) stop("rank_type hanya bernilai mean atau q1")
  if (eval == "percentile" & is.null(percentile)) stop("jika eval = percentile, maka percentile harus terisi 0 s.d. 1")
  if (eval == "percentile" && (percentile <= 0 || percentile >= 1)) stop("persentile hanya bernilai antara 0 s.d. 1")

  # Initialize output
  output_final <- list()

  # Prepare Data
  dt <- data %>% select(c(domain, id, varname))

  # Replace NA with 0
  dt <- dt %>% mutate(across(varname, ~ replace_na(.x, 0)))

  # Filter active and non-active rows
  dt <- dt %>% mutate(total = rowSums(across(-c(1:(length(id) + length(domain)))))) %>% filter(total > 0) %>% select(-total)
  dt_nonaktif <- dt %>% mutate(total = rowSums(across(-c(1:(length(id) + length(domain)))))) %>% filter(total == 0) %>% select(-total)

  # Ranking function
  function_rank <- if (rank_type == "mean") {
    function(x) mean(x[x > 0], na.rm = TRUE)
  } else {
    function(x) quantile(x[x > 0], 0.25, na.rm = TRUE)
  }

  # Calculate mean or rank values
  temp2 <- dt %>%
    group_by(across(all_of(domain))) %>%
    mutate(across(all_of(varname), list(mean = function_rank))) %>%
    ungroup() %>%
    select(ends_with("_mean"))

  # Concentration Index
  temp3 <- dt %>% select(all_of(varname)) / temp2
  temp3 <- set_names(temp3, paste0(varname, "_ik"))

  # Rank calculations
  temp4 <- temp3 %>% mutate(across(everything(), ~ rank(-., ties.method = "max")))

  # Handle ranks for zero values
  temp5 <- temp3 %>%
    bind_cols(temp4 %>%
                mutate(across(everything(), ~ ifelse(temp3[[cur_column()]] == 0, 0, .), .names = "{col}_rank2")))

  # Reshape and summarize
  temp6 <- dt %>% select(all_of(id)) %>% bind_cols(temp5 %>% select(ends_with("_rank2")))

  temp7 <- temp6 %>%
    pivot_longer(-all_of(id), names_to = "strata", values_to = "rank") %>%
    arrange(across(all_of(id))) %>%
    group_by(across(all_of(id))) %>%
    mutate(rank = glue("top{rank}")) %>%
    pivot_wider(names_from = "rank", values_from = "strata")

  # Final evaluation
  temp10 <- temp7 %>%
    mutate(
      top1 = ifelse(top1 %in% varname, top1, "NA"),
      top2 = ifelse(top2 %in% varname, top2, "NA")
    )

  # Add evaluation results
  out <- temp10 %>%
    rowwise() %>%
    mutate(
      strata_eval = top1,
      val1 = get(paste0(top1)),
      val2 = ifelse(top2 == "NA", 0, get(paste0(top2))),
      eval1 = get(paste0(top1, "_mean")),
      eval2 = ifelse(top2 == "NA", Inf, get(paste0(top2, "_mean")))
    ) %>%
    mutate(
      strata_eval = ifelse(
        top2 != "NA",
        ifelse(val1 >= eval1, top1, ifelse(val2 >= eval2, top2, "NK")),
        ifelse(val1 >= eval1, top1, "NK")
      )
    ) %>%
    ungroup()

  # Summarize results
  output_final$strata_final <- out
  output_final$summarise <- out %>% group_by(across(all_of(domain), strata_eval)) %>% summarise(n = n(), .groups = 'drop')
  output_final$nonaktif <- dt_nonaktif
  output_final$data <- dt

  return(output_final)
}
