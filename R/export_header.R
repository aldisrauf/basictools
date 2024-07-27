#' Create a Custom Header for a Data Frame
#'
#' This function generates a custom header for a data frame based on specified column names and sizes.
#'
#' @param column A character vector of column names.
#' @param size An integer vector specifying the size (number of columns) for each header.
#' @return A data frame with the specified custom header.
#' @export
#'
#' @examples
#' create_header(c("Header1", "Header2"), c(2, 3))
create_header <- function(column, size) {
  out <- c()
  for (i in 1:length(column)) {
    out <- c(out, column[i], rep(NA, size[i] - 1))
  }
  out <- data.frame(out) %>% t() %>% data.frame()
  colnames(out) <- NA
  return(out)
}

#' Merge Custom Header Cells in an Excel Sheet
#'
#' This function writes a custom header to an Excel sheet and merges the header cells based on specified sizes.
#'
#' @param wb An openxlsx workbook object.
#' @param sheet_name A character string specifying the name of the sheet.
#' @param start_row An integer specifying the starting row for the header.
#' @param column A character vector of column names.
#' @param size An integer vector specifying the size (number of columns) for each header.
#' @return None. The function modifies the Excel workbook in place.
#' @export
#'
#' @examples
#' wb <- openxlsx::createWorkbook()
#' openxlsx::addWorksheet(wb, "Sheet1")
#' create_header_merge(wb, "Sheet1", 1, c("Header1", "Header2"), c(2, 3))
create_header_merge <- function(wb, sheet_name, start_row, column, size) {
  header_temp <- create_header(column, size)
  openxlsx::writeData(wb, sheet_name, startRow = start_row, x = header_temp, colNames = FALSE, rowNames = FALSE)

  for (i in 1:length(size)) {
    if (i == 1) {
      start <- 1
    }
    mergeCells(wb, sheet_name, cols = start:sum(size[1:i]), rows = start_row)
    start <- sum(size[1:i]) + 1
  }
}

#' Export Data with Custom Headers to an Excel File
#'
#' This function exports data to an Excel file with custom headers and optional data frame headers.
#'
#' @param wb An openxlsx workbook object.
#' @param sheet_name A character string specifying the name of the sheet.
#' @param header_list A list of lists, where each sublist contains a character vector of column names and an integer vector of sizes for each header.
#' @param data A data frame to be written to the Excel sheet.
#' @param filename A character string specifying the name of the output Excel file.
#' @param print_df_header A logical value indicating whether to print the data frame headers (default is FALSE).
#' @return None. The function saves the Excel file with the specified name.
#' @export
#'
#' @examples
#' wb <- openxlsx::createWorkbook()
#' openxlsx::addWorksheet(wb, "Sheet1")
#' header_list <- list(list(c("Header1", "Header2"), c(2, 3)))
#' data <- data.frame(A = 1:3, B = 4:6, C = 7:9, D = 10:12, E = 13:15)
#' export_with_header(wb, "Sheet1", header_list, data, "output.xlsx")
export_with_header <- function(wb, sheet_name, header_list, data, filename, print_df_header = FALSE) {
  imp_list <- header_list
  for (i in 1:length(imp_list)) {
    temp_list <- imp_list[[i]]
    create_header_merge(
      wb = wb,
      sheet_name = sheet_name,
      column = temp_list[[1]],
      size = temp_list[[2]],
      start_row = i
    )
  }

  if (print_df_header) {
    writeData(wb = wb, sheet = sheet_name, x = data, startRow = (length(imp_list) + 1), rowNames = FALSE)
  } else {
    writeData(wb = wb, sheet = sheet_name, x = data, startRow = (length(imp_list) + 1), rowNames = FALSE, colNames = FALSE)
  }

  addStyle(wb, sheet = sheet_name, style = createStyle(border = "TopBottomLeftRight"), rows = 1:(length(imp_list) + nrow(data)), cols = 1:sum(temp_list[[2]]), gridExpand = TRUE)
  setColWidths(wb, sheet = sheet_name, cols = 1:ncol(data), widths = "auto")

  if (print_df_header) {
    addStyle(wb, sheet = sheet_name, style = createStyle(textDecoration = "bold", halign = "center", valign = "center", border = "TopBottomLeftRight"), rows = 1:(length(imp_list) + 1), cols = 1:sum(temp_list[[2]]), gridExpand = TRUE)
  } else {
    addStyle(wb, sheet = sheet_name, style = createStyle(textDecoration = "bold", halign = "center", valign = "center", border = "TopBottomLeftRight"), rows = 1:length(imp_list), cols = 1:sum(temp_list[[2]]), gridExpand = TRUE)
  }

  openxlsx::saveWorkbook(wb, filename, overwrite = TRUE)
}
