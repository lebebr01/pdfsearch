#' Search a pdf file for keywords
#' 
#' This uses the pdf_text from the pdftools package to perform keyword searches. 
#' Keyword locations indicating the line of the text as well as the page number 
#' that the keyword is found are returned.
#' 
#' @param x Either the text of the pdf read in with the pdftools package or a 
#'    path for the location of the pdf file.
#' @param keyword The keyword(s) to be used to search in the text. Multiple 
#'    keywords can be specified with a character vector.
#' @param path An optional path designation for the location of the pdf to be 
#'    converted to text. The pdftools package is used for this conversion.
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
#'    case of the keyword matters. Default is FALSE meaning that case of the 
#'    keyword is literal. If a vector, must be same length as the keyword 
#'    vector.
#' @param remove_hyphen TRUE/FALSE indicating whether hyphenated words should
#'    be adjusted to combine onto a single line. Default is TRUE.
#' @param token_results TRUE/FALSE indicating whether the results text returned
#'    should be split into tokens. See the tokenizers package and 
#'    \code{\link{convert_tokens}} for more details. Defaults to TRUE.
#' @param heading_search TRUE/FALSE indicating whether to search for headings 
#'    in the pdf.
#' @param heading_args A list of arguments to pass on to the 
#'    \code{\link{heading_search}} function. See \code{\link{heading_search}} 
#'     for more details on arguments needed.
#' @param collapse TRUE/FALSE indicating if individual lines of PDF file should
#'     be collapsed into a single large paragraph to perform keyword searching.
#' @param ... token_function to pass to \code{\link{convert_tokens}} 
#'   function. 
#'   
#' @return A tibble data frame that contains the keyword, location of match, 
#'   the line of text match, and optionally the tokens associated with the line
#'   of text match. 
#'   
#' @importFrom pdftools pdf_text
#' @importFrom tibble tibble
#' @importFrom tokenizers tokenize_lines
#' @examples 
#' file <- system.file('pdf', '1501.00450.pdf', package = 'pdfsearch')
#' 
#' keyword_search(file, keyword = c('repeated measures', 'mixed effects'),
#'   path = TRUE)
#'   
#' # Add surrounding text
#' keyword_search(file, keyword = c('variance', 'mixed effects'),
#'   path = TRUE, surround_lines = 1)
#'   
#' # split pdf
#' keyword_search(file, keyword = c('repeated measures', 'mixed effects'),
#'   path = TRUE, split_pdf = TRUE, remove_hyphen = FALSE)
#' 
#' @export
keyword_search <- function(x, keyword, path = FALSE, split_pdf = FALSE,
                           surround_lines = FALSE, ignore_case = FALSE,
                           remove_hyphen = TRUE, token_results = TRUE,
                           heading_search = FALSE, heading_args = NULL,
                           collapse = TRUE, ...) {
  if(path) {
    x <- pdftools::pdf_text(x)
  }
  line_nums <- cumsum(lapply(tokenizers::tokenize_lines(x), length))
  if(any(line_nums == 0)) {
    warning('text not recognized in pdf')
    text_out <- data.frame(keyword = NULL, 
                               page_num = NULL,
                               line_num = NULL,
                               line_text = NULL)
  } else {
    
    if(split_pdf) {
      x_list <- split_pdf(x)
      line_nums <- cumsum(x_list[[2]])
      x_lines <- x_list[[1]]
    } else {
      x_lines <- unlist(tokenizers::tokenize_lines(x))
      x_lines <- gsub("^\\s+|\\s+$", '', x_lines)
    }
    
    if(remove_hyphen) {
      x_lines <- remove_hyphen(x_lines)
    }
    
    # collapse into a single paragraph
    if(collapse) {
      x_lines <- paste(x_lines, collapse = ' ')
    }
    
    if(length(ignore_case) > 1) {
      if(length(keyword) != length(ignore_case)) {
          stop('keyword and ignore.case must be same length')
      }
      keyword_line_loc <- lapply(seq_along(keyword), function(xx) 
        grep(keyword[xx], x_lines, ignore_case[xx]))
    } else {
      keyword_line_loc <- lapply(seq_along(keyword), function(xx) 
        grep(keyword[xx], x_lines, ignore_case))
    }
    keyword_line <- unlist(keyword_line_loc)
    
    if(surround_lines != FALSE) {
      if(!is.numeric(surround_lines)) {
        stop('surround_lines must be FALSE or numeric')
      }
      srd_line_loc <- t(sapply(keyword_line, function(xx) 
        xx + c(-1, 1) * surround_lines))
      srd_line_loc <- sapply(1:nrow(srd_line_loc), function(xx) 
        paste(srd_line_loc[xx, ], collapse = ":"))
      lines_sel <- lapply(seq_along(srd_line_loc), function(xx) 
        x_lines[eval(parse(text = srd_line_loc[xx]))])
    } else {
      lines_sel <- lapply(seq_along(keyword_line), function(xx)
        x_lines[keyword_line[xx]])
    }
    
    if(token_results) {
      token_results_text <- convert_tokens_keyword(lines_sel, ...)
    } else {
      token_results_text <- NULL
    }
    
    pages <- findInterval(keyword_line, c(1, line_nums))
    
    text_out <- tibble::tibble(keyword = rep(keyword, 
                                             sapply(keyword_line_loc, length)), 
                               page_num = pages,
                               line_num = keyword_line,
                               line_text = lines_sel,
                               token_text = token_results_text
    )
    
    if(heading_search) {
      head_res <- do.call('heading_search', heading_args)
      
      row_nums <- findInterval(text_out$line_num, head_res$line_num)
      col <- data.frame(do.call('rbind', lapply(seq_along(row_nums), 
                  function(xx) head_res[row_nums[xx], 'keyword'])))
      if(any(row_nums == 0)) {
        col <- data.frame(c(rep('NA', table(row_nums)[1]), col$keyword))
      }
      names(col) <- 'heading'
      text_out <- cbind(text_out, col)
    }
  }
  
  return(text_out)
}
