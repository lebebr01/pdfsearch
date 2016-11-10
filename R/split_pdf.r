split_pdf <- function(x, delim = ' {2,}(?=\\w)') {
  
  x_lines <- strsplit(x, split = '\r\n')
  
  x_lines <- lapply(x_lines, strsplit, split = ' {2,}(?=\\w)', perl = TRUE)
  
  length_lines <- sapply(seq_along(x_lines), function(xx) sapply(x_lines[[xx]], length))
  
  x_lines <- sapply(seq_along(x_lines), function(xx) {
    if(any(length_lines[[xx]] == 3)) {
      remove_indent(x_lines[[xx]], length_lines[[xx]])
    } else {
      x_lines[[xx]]
    }
  })
  
  length_lines <- sapply(seq_along(x_lines), function(xx) sapply(x_lines[[xx]], length))
  
  x_lines <- lapply(seq_along(x_lines), function(xx) {
    collapse_columns(x_lines[[xx]], length_lines[[xx]])
  })
  
  length_lines <- sapply(x_lines, length)
  x_lines <- do.call('c', x_lines)
  
  return(list(x_lines, length_lines))
  
}
