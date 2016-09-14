#' Split multicolumn pdfs
#' 
#' The \code{\link{pdf_text}} function does not handle multicolumn pdfs well,
#' it simply returns the entire line spanning across multiple columns. This function
#' will split multiple columns and retain their place in the pdf.
#' 
#' @param x A list object already processed via pdf_text.
#' @param numcols Number of columns in the pdf, default is 2.
#' @param delim The delimiter to split multicolumns. The default is three spaces, 
#'    changing this delimiter can be useful if pdf has not been split correctly.
split_pdf <- function(x, numcols = 2, delim = ' {2,}') {
  
  # x <- gsub('^\\s+|\\s+$', '', x)
  x_lines <- strsplit(x, split = '\r\n')
  
  x_lines <- lapply(x_lines, strsplit, split = ' {2,}')
  
  # x_lines_collate <- 
  
}
