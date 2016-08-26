#' Function to search pdf for keywords
#' 
#' This function is able to return keyword locations indicating the line of the text
#' as well as the page number that the keyword is found in.
#' 
#' @param x Either the text of the pdf read in with the pdftools package or a path
#'    for the location of the pdf file.
#' @param keyword The keyword(s) to be used to search in the text. Multiple 
#'    keywords can be specified with a character vector.
#' @param path An optional path designation for the location of the pdf to be converted 
#'    to text. The pdftools package is used for this conversion.
#' @param surround_lines numeric/FALSE indicating whether the output should extract the 
#'    surrouding lines of text in addition to the matching line. Default is FALSE, if 
#'    not false, include a numeric number that indicates the additional number of 
#'    surrounding lines that will be extracted.
#' @param ignore.case TRUE/FALSE indicating whether the case of the keyword matters. 
#'    Default is FALSE meaning that case of the keyword is literal.
#' @importFrom pdftools pdf_text
#' @importFrom tibble tibble
#' @export
keyword_search <- function(x, keyword, path = FALSE,
                           surround_lines = FALSE, ignore.case = FALSE) {
  if(path) {
    x <- pdftools::pdf_text(x)
  }
  line_nums <- cumsum(sapply(strsplit(x, split = '\r\n'), length))
  if(any(line_nums == 0)) {
    warning('text not recognized in pdf')
    text_out <- data.frame(keyword = NULL, 
                               page_num = NULL,
                               line_num = NULL,
                               line_text = NULL)
  } else {
    
    x_lines <- unlist(strsplit(x, split = '\r\n'))
    x_lines <- gsub("^\\s+|\\s+$", '', x_lines)
    
    keyword_line_loc <- lapply(seq_along(keyword), function(xx) 
      grep(keyword[xx], x_lines, ignore.case))
    keyword_line <- unlist(keyword_line_loc)
    
    if(surround_lines != FALSE) {
      if(!is.numeric(surround_lines)) stop('surround_lines must be FALSE or numeric')
      srd_line_loc <- t(sapply(keyword_line, function(xx) xx + c(-1, 1) * surround_lines))
      srd_line_loc <- sapply(1:nrow(srd_line_loc), function(xx) paste(srd_line_loc[xx, ], 
                                                                      collapse = ":"))
      lines_sel <- lapply(seq_along(srd_line_loc), function(xx) 
        x_lines[eval(parse(text = srd_line_loc[xx]))])
    } else {
      lines_sel <- lapply(seq_along(keyword_line), function(xx)
        x_lines[keyword_line[xx]])
    }
    
    pages <- findInterval(keyword_line, c(1, line_nums))
    
    text_out <- tibble::tibble(keyword = rep(keyword, sapply(keyword_line_loc, length)), 
                               page_num = pages,
                               line_num = keyword_line,
                               line_text = lines_sel)
  }
  
  return(text_out)
}