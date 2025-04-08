#' Function to locate sections of pdf
#' 
#' The ability to extract the location of the text and separate by sections. 
#' The function will return the headings with their location in the pdf.
#' 
#' @param x Either the text of the pdf read in with the pdftools package or a 
#'    path for the location of the pdf file.
#' @param headings A character vector representing the headings to search for.
#'    Can be NULL if pdf_toc = TRUE.
#' @param path An optional path designation for the location of the pdf to be 
#'    converted to text. The pdftools package is used for this conversion.
#' @param pdf_toc TRUE/FALSE whether the pdf_toc function should be used from
#'    the pdftools package. This is most useful if the pdf has 
#'    the table of contents embedded within the pdf.
#'    Must specify path = TRUE if pdf_toc = TRUE.
#' @param full_line TRUE/FALSE indicating whether the headings should reside on
#'    their own line. This can create problems with multiple column pdfs.
#' @param ignore_case TRUE/FALSE/vector of TRUE/FALSE, indicating whether the 
#'    case of the keyword matters. 
#'    Default is FALSE meaning that case of the headings keywords are literal. 
#'    If a vector, must be same length as the headings vector.
#' @param split_pdf TRUE/FALSE indicating whether to split the pdf using white 
#'    space. This would be most useful with multicolumn pdf files. 
#'    The split_pdf function attempts to recreate the column layout of the text 
#'    into a single column starting with the left column and proceeding to the 
#'    right.
#' @param convert_sentence TRUE/FALSE indicating if individual lines of PDF file
#'     should be collapsed into a single large paragraph to perform keyword 
#'     searching. Default is FALSE
#' @importFrom pdftools pdf_toc
#' 
#' @examples 
#' file <- system.file('pdf', '1501.00450.pdf', package = 'pdfsearch')
#' 
#' heading_search(file, headings = c('abstract', 'introduction'),
#'   path = TRUE)
#' 
#' @export
heading_search <- function(x, headings, path = FALSE, pdf_toc = FALSE,
                           full_line = FALSE, ignore_case = FALSE, 
                           split_pdf = FALSE, convert_sentence = FALSE) {
  
  if(pdf_toc & !path) stop('If pdf_toc = TRUE, then path must be specified')
  if(pdf_toc & path) {
    toc <- pdftools::pdf_toc(x)
    headings <- unlist(lapply(seq_along(toc$children[[1]]$children), 
                              function(xx) 
      toc$children[[1]]$children[[xx]]$title))
  }
  if(full_line) {
    headings <- paste0('^', headings, '$')
  }
  
  keyword_search(x, keyword = headings, path = path, 
                 surround_lines = FALSE, ignore_case = ignore_case, 
                 split_pdf = split_pdf, convert_sentence = convert_sentence)
  
}
