#' Format PDF input text
#' 
#' Performs some formatting of pdf text upon import.
#' 
#' @param pdf_text A list of text from PDF import, most likely from 
#'    `pdftools::pdf_text()`. Each element of the list is a unique page of 
#'    text from the PDF. 
#' @param split_pdf TRUE/FALSE indicating whether to split the pdf using white 
#'    space. This would be most useful with multicolumn pdf files. 
#'    The split_pdf function attempts to recreate the column layout of the text 
#'    into a single column starting with the left column and proceeding to the 
#'    right.
#' @param blank_lines TRUE/FALSE indicating whether blank text lines should
#'    be removed. Default is TRUE.
#' @param remove_hyphen TRUE/FALSE indicating whether hyphenated words should
#'    be adjusted to combine onto a single line. Default is TRUE.
#' @param convert_sentence TRUE/FALSE indicating if individual lines of PDF file
#'     should be collapsed into a single large paragraph to perform keyword 
#'     searching. Default is TRUE
#' @param remove_equations TRUE/FALSE indicating if equations should be removed.
#'     Default behavior is to search for a literal parenthesis,
#'     followed by at least one number followed by another parenthesis at
#'     the end of the text line. This will not detect other patterns or
#'     detect the entire equation if it is a multi-row equation.
#' @param split_pattern Regular expression pattern used to split multicolumn 
#'     PDF files using \code{stringi::stri_split_regex}. 
#'     Default pattern is to 
#'     split based on three or more consecutive white space characters. 
#' @param ... Additional arguments, currently not used.
#' 
#' @importFrom stringi stri_split_lines stri_split_boundaries
#' 
#' @export
format_text <- function(pdf_text, split_pdf = FALSE,
                        blank_lines = TRUE,
                        remove_hyphen = TRUE,
                        convert_sentence = TRUE, 
                        remove_equations = FALSE,
                        split_pattern = "\\p{WHITE_SPACE}{3,}",
                        ...) {
  if(split_pdf) {
    x_list <- split_pdf(pdf_text, pattern = split_pattern)
    x_lines_list <- x_list
  } else {
    x_lines_list <- stringi::stri_split_lines(pdf_text)
  }
  
  x_lines_list <- lapply(seq_along(x_lines_list), function(xx) gsub("^\\s+|\\s+$", '', 
                                                                    x = x_lines_list[[xx]]))
  
  if(blank_lines) {
    x_lines_list <- lapply(x_lines_list, remove_blank_lines)
  }
  
  if(remove_hyphen) {
    x_lines_list <- lapply(x_lines_list, remove_hyphen)
  }
  
  if(remove_equations) {
    x_lines_list <- lapply(x_lines_list, remove_equation)
  }
  
  # collapse into a single paragraph
  if(convert_sentence) {
    x_lines_list <- lapply(seq_along(x_lines_list), function(xx) paste(x_lines_list[[xx]], collapse = ' ')
    )
    x_lines_list <- lapply(seq_along(x_lines_list), function(xx) unlist(stringi::stri_split_boundaries(x_lines_list[[xx]], 
                                                                                                       type = "sentence"))
    )
  }
  
  x_lines_list
}