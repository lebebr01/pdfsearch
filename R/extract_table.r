#' Function to extract tables
#' 
#' @param text_lines The text of a PDF document read into R and split into 
#'    lines. 
#' @param delimiter A delimiter used to detect tables. The default is two 
#'   consecutive blank white spaces.
#' @param delimiter_table A delimiter used to separate table cells. The default
#'   value is two consecutive blank white spaces. 
#' @param replacement A delimiter used to separate table cells after the 
#'   replacement of white space is done. 
#' @param col_names TRUE/FALSE value passed to `readr::read_delim` 
#'   to indicate if column names should be used. Default value is FALSE which 
#'   means column names will be generic (i.e. X1, X2, etc). A value of TRUE
#'   would take the values from the first row of data extracted. 
#' @importFrom readr read_delim
#' @export
extract_tables <- function(text_lines, delimiter = "\\s{2,}",
                           delimiter_table = "\\s{2,}",
                           replacement = "\\/",
                           col_names = FALSE) {
  
  possible_table_locations <- grep(delimiter, text_lines)
  
  table_locations <- find_table_locations(possible_table_locations)
  
  table_text <- lapply(seq_along(table_locations), function(xx) 
    text_lines[table_locations[[xx]]]
  )
  
  table_text <- lapply(table_text, add_delimiter, 
                       delimiter = delimiter_table,
                       replacement = replacement)
  
  tables <- lapply(table_text, readr::read_delim, delim = "/",
                   col_names = col_names)
  
  tables
}

find_table_locations <- function(row_numbers) {
  
  length_input <- length(row_numbers) - 1
  
  diff_adjacent <- unlist(
    lapply(seq_len(length_input), function(xx) 
      row_numbers[xx + 1] - row_numbers[xx])
  )
  
  diff_adj_tf <- diff_adjacent < 5
  
  rle_df <- data.frame(
    lengths = rle(diff_adj_tf)$lengths,
    values = rle(diff_adj_tf)$values
  )
  
  rle_df$select <- ifelse(rle_df$lengths > 4 & rle_df$values, 1, 0)
  rle_df$start <- c(1, cumsum(rle_df$lengths[1:(nrow(rle_df)-1)]) + 1)
  rle_df$end <- cumsum(rle_df$lengths) + 1
  rle_df$new_values <- ifelse(rle_df$select, TRUE, FALSE)
  
  rle_true <- rle_df[rle_df$new_values == TRUE, ]
  
  convert_to_true <- lapply(1:nrow(rle_true), function(xx) 
    rle_true[xx, 'start']:rle_true[xx, 'end']
  )
  
  row_numbers_return <- lapply(seq_along(convert_to_true), function(xx) 
    row_numbers[convert_to_true[[xx]]]
    )
  
  row_numbers_return
}

add_delimiter <- function(table_lines, delimiter = "\\s{2,}", 
                          replacement = "\\/") {
  
  gsub(delimiter, replacement, table_lines)
  
} 
