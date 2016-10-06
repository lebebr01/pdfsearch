#' Function to locate sections of pdf
#' 
#' The ability to extract the location of the text and separate by sections. 
#' The function will return the headings with their location in the pdf.
#' 
#' @param x Either the text of the pdf read in with the pdftools package or a path
#'    for the location of the pdf file.
#' @param headings A character vector representing the headings to search for.
#'    Can be NULL if pdf_toc = TRUE.
#' @param path An optional path designation for the location of the pdf to be converted 
#'    to text. The pdftools package is used for this conversion.
#' @param pdf_toc TRUE/FALSE whether the pdf_toc function should be used. This
#'    is most useful if the pdf has the table of contents embedded within the pdf.
#'    Must specify path = TRUE if pdf_toc = TRUE.
#' @param full_line TRUE/FALSE indicating whether the headings should reside on
#'    their own line. This can create problems with multiple column pdfs.
#' @param ignore_case TRUE/FALSE/vector of TRUE/FALSE, indicating whether the case of the keyword matters. 
#'    Default is FALSE meaning that case of the headings keywords are literal. 
#'    If a vector, must be same length as the headings vector.
#' @param split_pdf TRUE/FALSE indicating whether 
#' @importFrom pdftools pdf_toc
#' @export 
heading_search <- function(x, headings, path = FALSE, pdf_toc = FALSE,
                           full_line = FALSE, ignore_case = FALSE, 
                           split_pdf = FALSE) {
  
  if(pdf_toc & !path) stop('If pdf_toc = TRUE, then path must be specified')
  if(pdf_toc & path) {
    toc <- pdftools::pdf_toc(x)
    headings <- unlist(lapply(seq_along(toc$children[[1]]$children), function(xx) 
      toc$children[[1]]$children[[xx]]$title))
  }
  if(full_line) {
    headings <- paste0('^', headings, '$')
  }
  
  keyword_search(x, keyword = headings, path = path, 
                 surround_lines = FALSE, ignore_case = ignore_case)
  
}
