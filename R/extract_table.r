#' Function to extract tables
#' 
#' @param x Either the text of the pdf read in with the pdftools package or a 
#'    path for the location of the pdf file.
#' @param path An optional path designation for the location of the pdf to be 
#'    converted to text. The pdftools package is used for this conversion.
#' @param split_pdf TRUE/FALSE indicating whether to split the pdf using white 
#'    space. This would be most useful with multicolumn pdf files. 
#'    The split_pdf function attempts to recreate the column layout of the text 
#'    into a single column starting with the left column and proceeding to the 
#'    right. 
#' @param remove_equations TRUE/FALSE indicating if equations should be removed.
#'     Default behavior is to search for the following regex:
#'     "\\([0-9]{1,}\\)$", essentially this matches a literal parenthesis,
#'     followed by at least one number followed by another parenthesis at
#'     the end of the text line. This will not detect other patterns or
#'     detect the entire equation if it is a multi-row equation.
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
extract_tables <- function(x, path = FALSE, split_pdf = FALSE,
                           remove_equations = TRUE,
                           delimiter = "\\s{2,}",
                           delimiter_table = "\\s{2,}",
                           replacement = "\\/",
                           col_names = FALSE) {
  
  if(path) {
    x <- pdftools::pdf_text(x)
  }
  line_nums <- cumsum(lapply(tokenizers::tokenize_lines(x), length))
  if(any(line_nums == 0)) {
    warning('text not recognized in pdf')
    text_out <- data.frame(keyword = NULL, 
                           page_num = NULL,
                           line_num = NULL,
                           line_text = NULL)
  } else {
    
    if(split_pdf) {
      x_list <- split_pdf(x, pattern = split_pattern)
      x_lines <- unlist(x_list)
      x_lines <- gsub("^\\s+|\\s+$", '', x_lines)
      # line_nums <- cumsum(x_list[[2]])
      # x_lines <- x_list[[1]]
    } else {
      x_lines <- unlist(stringi::stri_split_lines(x))
      x_lines <- gsub("^\\s+|\\s+$", '', x_lines)
    }
    
    if(remove_equations) {
      x_lines <- remove_equation(x_lines)
    }
  
  possible_table_locations <- grep(delimiter, x_lines)
  
  table_locations <- find_table_locations(possible_table_locations)
  
  table_text <- lapply(seq_along(table_locations), function(xx) 
    x_lines[table_locations[[xx]]]
  )
  
  table_text <- lapply(table_text, add_delimiter, 
                       delimiter = delimiter_table,
                       replacement = replacement)
  
  tables <- lapply(table_text, readr::read_delim, delim = "/",
                   col_names = col_names)
  
  tables
  }
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
