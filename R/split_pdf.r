#' Split multicolumn pdfs
#' 
#' The \code{\link{pdf_text}} function does not handle multicolumn pdfs well,
#' it simply returns the entire line spanning across multiple columns. This function
#' will split multiple columns and retain their place in the pdf.
#' 
#' @param x A list object already processed via pdf_text.
#' @param delim The delimiter to split multicolumns. The default is three spaces, 
#'    changing this delimiter can be useful if pdf has not been split correctly.
#' @export 
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
  
  x_lines <- do.call('c', 
                     lapply(seq_along(x_lines), function(xx) {
                       collapse_columns(x_lines[[xx]], length_lines[[xx]])
                     })
  )
  
  return(x_lines)
  
}
