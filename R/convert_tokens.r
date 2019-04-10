#' Ability to tokenize words.
#' 
#' @param x The text of the pdf file. This can be specified directly 
#'   or the pdftools package is used to read the pdf file from a file path. 
#'   To use the pdftools, the path argument must be set to TRUE.
#' @param path An optional path designation for the location of the pdf to be 
#'    converted to text. The pdftools package is used for this conversion.
#' @param split_pdf TRUE/FALSE indicating whether to split the pdf using white 
#'    space. This would be most useful with multicolumn pdf files. 
#'    The split_pdf function attempts to recreate the column layout of the text 
#'    into a single column starting with the left column and proceeding to the 
#'    right.
#' @param remove_hyphen TRUE/FALSE indicating whether hyphenated words should
#'    be adjusted to combine onto a single line. Default is TRUE.
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
convert_tokens <- function(x, path = FALSE, split_pdf = FALSE, 
                           remove_hyphen = TRUE,
                           token_function = NULL) {
  
  if(path) {
    x <- pdftools::pdf_text(x)
  }
  
  if(split_pdf) {
    x_list <- split_pdf(x)
    line_nums <- cumsum(x_list[[2]])
    x_lines <- x_list[[1]]
  } else {
    x_lines <- unlist(stringi::stri_split_lines(x))
    x_lines <- gsub("^\\s+|\\s+$", '', x_lines)
  }
  
  if(remove_hyphen) {
    x_lines <- remove_hyphen(x_lines)
  }
  
  if(is.null(token_function)) {
    token_function <- tokenizers::tokenize_words
  }
  
  lapply(x, token_function)
  
}

convert_tokens_keyword <- function(x, token_function = NULL) {
  
  if(is.null(token_function)) {
    token_function <- tokenizers::tokenize_words
  }
  
  lapply(x, token_function)
  
}
