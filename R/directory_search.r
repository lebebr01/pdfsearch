#' Wrapper for keyword search function
#' 
#' This will use the keyword_search function to loop over all pdf files in a 
#' directory. Includes the ability to include subdirectories as well.
#'  
#' @param directory The directory to perform the search for pdf files to search.
#' @param keyword The keyword(s) to be used to search in the text. Multiple 
#'    keywords can be specified with a character vector.
#' @param split_pdf TRUE/FALSE indicating whether to split the pdf using white 
#'    space. This would be most useful with multicolumn pdf files. 
#'    The split_pdf function attempts to recreate the column layout of the text 
#'    into a single column starting with the left column and proceeding to the 
#'    right.
#' @param surround_lines numeric/FALSE indicating whether the output should 
#'    extract the surrouding lines of text in addition to the matching line. 
#'    Default is FALSE, if not false, include a numeric number that indicates 
#'    the additional number of surrounding lines that will be extracted.
#' @param ignore_case TRUE/FALSE/vector of TRUE/FALSE, indicating whether the 
#'    case of the keyword matters. 
#'    Default is FALSE meaning that case of the keyword is literal. If a vector, 
#'    must be same length as the keyword vector.
#' @param remove_hyphen TRUE/FALSE indicating whether hyphenated words should
#'    be adjusted to combine onto a single line. Default is TRUE.
#' @param token_results TRUE/FALSE indicating whether the results text returned
#'    should be split into tokens. See the tokenizers package and 
#'    \code{\link{convert_tokens}} for more details. Defaults to TRUE.
#' @param convert_sentence TRUE/FALSE indicating if individual lines of PDF file
#'     should be collapsed into a single large paragraph to perform keyword 
#'     searching. Default is TRUE.
#' @param remove_equations TRUE/FALSE indicating if equations should be removed.
#'     Default behavior is to search for the following regex:
#'     "\\([0-9]{1,}\\)$", essentially this matches a literal parenthesis,
#'     followed by at least one number followed by another parenthesis at
#'     the end of the text line. This will not detect other patterns or
#'     detect the entire equation if it is a multi-row equation.
#' @param split_pattern Regular expression pattern used to split multicolumn 
#'     PDF files using \code{stringi::stri_split_regex}. 
#'     Default pattern is "\\p{WHITE_SPACE}{3,}" which can be interpreted as: 
#'     split based on three or more consecutive white space characters. 
#' @param full_names TRUE/FALSE indicating if the full file path should be used.
#'    Default is TRUE, see \code{\link{list.files}} for more details.
#' @param file_pattern An optional regular expression to select specific file
#'    names. Only files that match the regular expression will be searched. 
#'    Defaults to all pdfs, i.e. \code{".pdf"}. See \code{\link{list.files}} 
#'    for more details.
#' @param recursive TRUE/FALSE indicating if subdirectories should be searched 
#'    as well.
#'    Default is FALSE, see \code{\link{list.files}} for more details.
#' @param max_search An optional numeric vector indicating the maximum number
#'    of pdfs to search. Will only search the first n cases.
#' @param ... token_function to pass to \code{\link{convert_tokens}} 
#'   function. 
#'   
#' @return A tibble data frame that contains the keyword, location of match, 
#'   the line of text match, and optionally the tokens associated with the line
#'   of text match. The output is combined (row binded) for all pdf input files.
#'    
#' @examples 
#' # find directory
#' directory <- system.file('pdf', package = 'pdfsearch')
#' 
#' # do search over two files
#' keyword_directory(directory, 
#'        keyword = c('repeated measures', 'measurement error'),
#'        surround_lines = 1, full_names = TRUE)
#'        
#' # can also split pdfs
#' keyword_directory(directory, 
#'        keyword = c('repeated measures', 'measurement error'),
#'        split_pdf = TRUE, remove_hyphen = FALSE,
#'        surround_lines = 1, full_names = TRUE)
#' 
#' 
#' @export
keyword_directory <- function(directory, keyword, 
                              split_pdf = FALSE, surround_lines = FALSE,
                              ignore_case = FALSE, remove_hyphen = TRUE,
                              token_results = TRUE, convert_sentence = TRUE, 
                              remove_equations = TRUE,
                              split_pattern = "\\p{WHITE_SPACE}{3,}",
                              full_names = TRUE, file_pattern = ".pdf",
                              recursive = FALSE, max_search = NULL, ...) {
  
  files_dir <- list.files(path = directory, pattern = file_pattern, 
                          full.names = full_names, recursive = recursive)
  file_name <- list.files(path = directory, pattern = file_pattern, 
                          full.names = FALSE, recursive = recursive)
  
  if(is.null(max_search)) {
    extract_table <- lapply(seq_along(files_dir), function(xx) 
      keyword_search(files_dir[xx], keyword = keyword, path = TRUE,
                     split_pdf = split_pdf, surround_lines = surround_lines, 
                     ignore_case = ignore_case, remove_hyphen = remove_hyphen,
                     token_results = token_results, 
                     convert_sentence = convert_sentence, 
                     split_pattern = split_pattern, ...))
  } else {
    files_dir <- files_dir[1:max_search]
    file_name <- file_name[1:max_search]
    extract_table <- lapply(seq_along(files_dir), function(xx) 
      keyword_search(files_dir[xx], keyword = keyword, path = TRUE,
                     split_pdf = split_pdf, surround_lines = surround_lines, 
                     ignore_case = ignore_case, remove_hyphen = remove_hyphen,
                     token_results = token_results, 
                     convert_sentence = convert_sentence, 
                     split_pattern = split_pattern, ...))
  }
  
  num_rows <- unlist(lapply(extract_table, nrow))
  
  result_out <- do.call("rbind", extract_table)
  
  ids <- data.frame(ID = rep(seq_along(files_dir), num_rows),
                    pdf_name = rep(file_name, num_rows))
  result_out <- cbind(ids, result_out)
  return(result_out)
}
