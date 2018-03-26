#' Ability to tokenize words.
#' 
#' @param x The text of the pdf file. This can be specified directly 
#'   or the pdftools package is used to read the pdf file from a file path. 
#'   To use the pdftools, the path argument must be set to TRUE.
#' @param path An optional path designation for the location of the pdf to be 
#'    converted to text. The pdftools package is used for this conversion.
#' @param token_function This is a function from the tokenizers package. Default
#'   is the tokenize_words function.
#'   
#' @return A list of character vectors containing the tokens. More detail can 
#'   be found looking at the documentation of the tokenizers package.
#' 
#' @importFrom tokenizers tokenize_words
#' 
#' @examples
#'  file <- system.file('pdf', '1610.00147.pdf', package = 'pdfsearch')
#'  convert_tokens(file, path = TRUE) 
#' 
#' @export
convert_tokens <- function(x, path = FALSE, 
                           token_function = tokenizers::tokenize_words) {
  
  if(path) {
    x <- pdftools::pdf_text(x)
  }
  
  lapply(x, token_function)
  
}
