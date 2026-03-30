#' Export Data Frame to Excel with gt-like Grouped Headers
#'
#' This function exports a data frame to an Excel worksheet with support for multi-level
#' column headers (mimicking the style of the 'gt' package). It automatically merges
#' cells horizontally and vertically based on a delimiter in the column names.
#'
#' @param df Data frame. The data to be exported. Column names should use the `delim` to indicate levels.
#' @param delim Character. The separator used in column names to define header levels. Default is "_".
#' @param sheet Character. The name of the worksheet to create. Default is "Sheet1".
#' @param start_row Numeric. The row number where the table (including headers) should start. Default is 3.
#' @param table_caption Character. An optional caption to be placed at the top-left (Row 1). Default is "".
#' @param wb An openxlsx workbook object.
#'
#' @return The modified workbook object (`wb`).
#' @examples
#' \dontrun{
#' library(openxlsx)
#' wb <- createWorkbook()
#' df <- data.frame("Group A_Sub 1" = 1:5, "Group A_Sub 2" = 6:10, "Group B_Sub 1" = 11:15)
#' wb <- export_gt_like_excel(df, wb = wb, table_caption = "Sample Table")
#' saveWorkbook(wb, "output.xlsx", overwrite = TRUE)
#' }
#' @import openxlsx
#' @export
export_gt_like_excel <- function(df,
                                 delim = "_",
                                 sheet = "Sheet1",
                                 start_row = 3,
                                 table_caption = "",
                                 wb) {

  # --- Validation ---
  if (missing(wb) || !inherits(wb, "Workbook")) {
    stop("Argument 'wb' must be a valid openxlsx Workbook object.")
  }

  if (!is.data.frame(df)) {
    stop("Argument 'df' must be a data frame.")
  }

  # --- Preprocess column names ---
  orig_names <- names(df)
  split_names <- strsplit(orig_names, delim, fixed = TRUE)
  max_levels <- max(sapply(split_names, length))
  ncol_df <- ncol(df)

  # Build header matrix
  header_matrix <- matrix("", nrow = max_levels, ncol = ncol_df)
  for (j in seq_len(ncol_df)) {
    parts <- split_names[[j]]
    header_matrix[seq_along(parts), j] <- parts
  }

  # --- Workbook and Sheet ---
  # Check if sheet already exists to avoid error
  if (!(sheet %in% names(wb))) {
    openxlsx::addWorksheet(wb, sheet)
  }

  # --- Write header rows ---
  for (r in seq_len(max_levels)) {
    openxlsx::writeData(wb, sheet, as.list(header_matrix[r, ]),
                        startCol = 1, startRow = start_row + r - 1, colNames = FALSE)
  }

  # --- Horizontal merges ---
  for (r in seq_len(max_levels)) {
    c <- 1
    while (c <= ncol_df) {
      if (header_matrix[r, c] == "") {
        c <- c + 1
        next
      }
      j <- c
      while (j + 1 <= ncol_df && header_matrix[r, j + 1] == header_matrix[r, c]) {
        j <- j + 1
      }
      if (j > c) {
        openxlsx::mergeCells(wb, sheet, cols = c:j, rows = start_row + r - 1)
      }
      c <- j + 1
    }
  }

  # --- Vertical merges ---
  for (col in seq_len(ncol_df)) {
    for (r in seq_len(max_levels)) {
      val <- header_matrix[r, col]
      if (val == "") next
      if (r < max_levels && all(header_matrix[(r+1):max_levels, col] == "")) {
        openxlsx::mergeCells(wb, sheet, cols = col,
                             rows = (start_row + r - 1):(start_row + max_levels - 1))
      }
    }
  }

  # --- Write data ---
  data_start_row <- start_row + max_levels
  openxlsx::writeData(wb, sheet, df, startCol = 1, startRow = data_start_row, colNames = FALSE)

  # --- Styles ---
  header_style <- openxlsx::createStyle(
    fontColour = "black", fgFill = "lightgrey",
    halign = "center", valign = "center",
    textDecoration = "bold", wrapText = TRUE
  )
  body_style <- openxlsx::createStyle(halign = "center", valign = "center")
  border_style <- openxlsx::createStyle(border = "TopBottomLeftRight")

  openxlsx::addStyle(wb, sheet, header_style,
                     rows = start_row:(start_row + max_levels - 1),
                     cols = 1:ncol_df, gridExpand = TRUE, stack = TRUE)
  openxlsx::addStyle(wb, sheet, body_style,
                     rows = data_start_row:(data_start_row + nrow(df) - 1),
                     cols = 1:ncol_df, gridExpand = TRUE, stack = TRUE)
  openxlsx::addStyle(wb, sheet, border_style,
                     rows = start_row:(data_start_row + nrow(df) - 1),
                     cols = 1:ncol_df, gridExpand = TRUE, stack = TRUE)

  # --- Caption at top-left ---
  if (table_caption != "") {
    openxlsx::writeData(
      wb = wb,
      sheet = sheet,
      x = table_caption,
      startCol = 1,
      startRow = 1
    )
  }

  # --- Layout ---
  openxlsx::setRowHeights(wb, sheet, rows = start_row:(start_row + max_levels - 1), heights = 20)
  openxlsx::setColWidths(wb, sheet, cols = 1:ncol_df, widths = "auto")

  # --- Return ---
  return(wb)
}
