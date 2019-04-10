#' Function to extract tables
#' 
#' @param text_lines The text of a PDF document read into R and split into 
#'    lines. 
#' @param delimiter A delimiter used to detect tables
#' 
extract_tables <- function(text_lines, delimiter = "\\s{2,}") {
  
  possible_table_locations <- grep(delimiter, text_lines)
  
  
  
  
}

find_table_locations <- function(row_numbers) {
  
  length_input <- length(row_numbers) - 1
  
  diff_adjacent <- unlist(
    lapply(seq_len(length_input), function(xx) 
      row_numbers[xx + 1] - row_numbers[xx])
  )
  
  diff_adj_tf <- diff_adjacent < 5
  
  #rle(diff_adj_tf)$lengths[rle(diff_adj_tf)$values]
  
  #rle(diff_adj_tf)$lengths
  
}


