#' @importFrom stringi stri_split_lines stri_split_regex
split_pdf <- function(x, pattern = "\\p{WHITE_SPACE}{3,}") {
  
  x_lines <- stringi::stri_split_lines(x)
  
  x_lines <- lapply(x_lines, gsub,
                    pattern = "^\\s{1,20}", 
                    replacement = "")
  
  x_page <- lapply(x_lines, stringi::stri_split_regex, 
                   pattern = pattern, 
                   omit_empty = NA, simplify = TRUE)
  
  page_lines <- unlist(lapply(x_page, nrow))
  columns <- unlist(lapply(x_page, ncol))
  
  num_chars <- lapply(x_page, nchar)
  num_chars_tf <- lapply(num_chars, true_false, chars = 3)
  
  for(xx in seq_along(num_chars_tf)) {
    num_chars_tf[[xx]][is.na(num_chars_tf[[xx]])] <- FALSE
  }
  
  lapply(seq_along(x_page), function(xx) 
    x_page[[xx]][num_chars_tf[[xx]]])
  
  # columns_per_line <- lapply(seq_along(x_page), function(xx) 
  #   lapply(seq_len(page_lines[xx]), function(yy) 
  #     nchar(x_page[[xx]][yy, ])))
  # 
  # 
  # 
  # x_lines <- sapply(seq_along(x_lines), function(xx) {
  #   if(any(length_lines[[xx]] == 3)) {
  #     remove_indent(x_lines[[xx]], length_lines[[xx]])
  #   } else {
  #     x_lines[[xx]]
  #   }
  # })
  # 
  # length_lines <- sapply(seq_along(x_lines), function(xx) sapply(x_lines[[xx]], length))
  # 
  # x_lines <- lapply(seq_along(x_lines), function(xx) {
  #   collapse_columns(x_lines[[xx]], length_lines[[xx]])
  # })
  # 
  # length_lines <- sapply(x_lines, length)
  # x_lines <- do.call('c', x_lines)
  # 
  # return(list(x_lines, length_lines))
  
}

detect_num_textcolumns <- function(x, pattern = "\\p{WHITE_SPACE}{3,}") {
  
  x_lines <- stringi::stri_split_lines(x)
  
  x_lines <- lapply(x_lines, gsub,
                    pattern = "^\\s{1,20}", 
                    replacement = "")
  
  x_page <- lapply(x_lines, stringi::stri_split_regex, 
                   pattern = pattern, 
                   omit_empty = NA, simplify = TRUE)
  
  empty_cells <- lapply(seq_along(x_page), function(xx) 
    apply(x_page[[xx]], 2, stringi::stri_isempty))
  for(xx in seq_along(empty_cells)) {
    empty_cells[[xx]][is.na(empty_cells[[xx]])] <- TRUE
  }
  
  sum_columns <- unlist(lapply(seq_along(empty_cells), function(xx) 
    apply(apply(empty_cells[[xx]], 2, detect_false), 1, sum)
    )
  )
  
  most_columns <- table(sum_columns)
  
  as.numeric(attr(most_columns[order(most_columns, decreasing = TRUE)][1], "names"))
  
}

detect_false <- function(x) { x == FALSE }
