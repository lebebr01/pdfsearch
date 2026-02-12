#' Wrapper for keyword search function
#' 
#' This will use the keyword_search function to loop over all pdf files in a 
#' directory. Includes the ability to include subdirectories as well.
#'  
#' @param directory The directory to perform the search for pdf files to search.
#' @param keyword The keyword(s) to be used to search in the text. Multiple 
#'    keywords can be specified with a character vector.
#' @param surround_lines numeric/FALSE indicating whether the output should 
#'    extract the surrouding lines of text in addition to the matching line. 
#'    Default is FALSE, if not false, include a numeric number that indicates 
#'    the additional number of surrounding lines that will be extracted.
#' @param ignore_case TRUE/FALSE/vector of TRUE/FALSE, indicating whether the 
#'    case of the keyword matters. 
#'    Default is FALSE meaning that case of the keyword is literal. If a vector, 
#'    must be same length as the keyword vector.
#' @param token_results TRUE/FALSE indicating whether the results text returned
#'    should be split into tokens. See the tokenizers package and 
#'    \code{\link{convert_tokens}} for more details. Defaults to TRUE.
#' @param split_pdf TRUE/FALSE indicating whether to split the pdf using white 
#'    space. This would be most useful with multicolumn pdf files. 
#'    The split_pdf function attempts to recreate the column layout of the text 
#'    into a single column starting with the left column and proceeding to the 
#'    right.
#' @param remove_hyphen TRUE/FALSE indicating whether hyphenated words should
#'    be adjusted to combine onto a single line. Default is TRUE.
#' @param convert_sentence TRUE/FALSE indicating if individual lines of PDF file
#'     should be collapsed into a single large paragraph to perform keyword 
#'     searching. Default is TRUE.
#' @param remove_equations TRUE/FALSE indicating if equations should be removed.
#'     Default behavior is to search for a literal parenthesis,
#'     followed by at least one number followed by another parenthesis at
#'     the end of the text line. This will not detect other patterns or
#'     detect the entire equation if it is a multi-row equation.
#' @param split_pattern Regular expression pattern used to split multicolumn 
#'     PDF files using \code{stringi::stri_split_regex}. 
#'     Default pattern is to 
#'     split based on three or more consecutive white space characters. 
#' @param split_method Method used for splitting multicolumn PDF text.
#'     Defaults to "regex". Use "coordinates" to split with
#'     \code{pdftools::pdf_data()} token coordinates.
#' @param column_count Expected number of columns for coordinate splitting.
#'     Options are "auto", "1", or "2". Used when
#'     \code{split_method = "coordinates"}.
#' @param mask_nonprose TRUE/FALSE indicating if non-prose lines (likely
#'     equations, tables, figure/table captions) should be removed when
#'     using coordinate splitting.
#' @param nonprose_digit_ratio Numeric threshold for classifying a line as
#'     non-prose based on digit character ratio.
#' @param nonprose_symbol_ratio Numeric threshold for classifying a line as
#'     non-prose based on math-symbol character ratio.
#' @param nonprose_short_token_max Maximum token count for short symbolic
#'     lines to classify as non-prose.
#' @param remove_section_headers TRUE/FALSE indicating if section-header-like
#'     lines should be removed when using coordinate splitting.
#' @param remove_page_headers TRUE/FALSE indicating if page-header furniture
#'     (e.g., arXiv identifiers, emails, URLs) should be removed when using
#'     coordinate splitting.
#' @param remove_page_footers TRUE/FALSE indicating if page-footer furniture
#'     (e.g., page numbers, copyright markers) should be removed when using
#'     coordinate splitting.
#' @param page_margin_ratio Numeric ratio used to define top and bottom page
#'     bands for header/footer removal.
#' @param remove_repeated_furniture TRUE/FALSE indicating if repeated text
#'     found in the first/last lines across many pages should be removed.
#' @param repeated_edge_n Number of lines from top and bottom of each page to
#'     consider for repeated edge-line detection.
#' @param repeated_edge_min_pages Minimum number of pages an edge line must
#'     appear on before being removed.
#' @param remove_captions TRUE/FALSE indicating if figure/table caption lines
#'     should be removed.
#' @param caption_continuation_max Number of additional lines after a caption
#'     start line to remove when they appear to be caption continuations.
#' @param table_mode How to handle detected table blocks. "keep" keeps all
#'     lines, "remove" excludes table blocks, and "only" keeps only table
#'     blocks.
#' @param table_min_numeric_tokens Minimum numeric tokens used to classify a
#'     line as table-like.
#' @param table_min_digit_ratio Minimum digit-character ratio used to classify
#'     a line as table-like.
#' @param table_min_block_lines Minimum number of adjacent table-like lines for
#'     a block to be treated as a table block.
#' @param table_block_max_gap Maximum gap (in lines) allowed between
#'     table-like lines inside one table block.
#' @param table_include_headers TRUE/FALSE indicating if table header lines
#'     adjacent to detected table blocks should be included in table blocks.
#' @param table_header_lookback Number of lines above a detected table block to
#'     inspect for header rows.
#' @param table_include_notes TRUE/FALSE indicating if trailing note/source
#'     lines should be included with detected table blocks.
#' @param table_note_lookahead Number of lines after a detected table block to
#'     inspect for note/source rows.
#' @param concatenate_pages TRUE/FALSE indicating if page text should be
#'     concatenated after column rectification and cleaning, before sentence
#'     conversion. This is only used when \code{convert_sentence = TRUE}.
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
                              surround_lines = FALSE,
                              ignore_case = FALSE, 
                              token_results = TRUE, 
                              split_pdf = FALSE,
                              remove_hyphen = TRUE,
                              convert_sentence = TRUE, 
                              remove_equations = TRUE,
                              split_pattern = "\\p{WHITE_SPACE}{3,}",
                              split_method = c("regex", "coordinates"),
                              column_count = c("auto", "1", "2"),
                              mask_nonprose = FALSE,
                              nonprose_digit_ratio = 0.35,
                              nonprose_symbol_ratio = 0.15,
                              nonprose_short_token_max = 3,
                              remove_section_headers = FALSE,
                              remove_page_headers = FALSE,
                              remove_page_footers = FALSE,
                              page_margin_ratio = 0.08,
                              remove_repeated_furniture = FALSE,
                              repeated_edge_n = 3,
                              repeated_edge_min_pages = 4,
                              remove_captions = FALSE,
                              caption_continuation_max = 2,
                              table_mode = c("keep", "remove", "only"),
                              table_min_numeric_tokens = 3,
                              table_min_digit_ratio = 0.18,
                              table_min_block_lines = 2,
                              table_block_max_gap = 3,
                              table_include_headers = TRUE,
                              table_header_lookback = 3,
                              table_include_notes = FALSE,
                              table_note_lookahead = 2,
                              concatenate_pages = FALSE,
                              full_names = TRUE, 
                              file_pattern = ".pdf",
                              recursive = FALSE, 
                              max_search = NULL, 
                              ...) {
  split_method <- match.arg(split_method)
  column_count <- match.arg(column_count)
  table_mode <- match.arg(table_mode)
  
  files_dir <- list.files(path = directory, pattern = file_pattern, 
                          full.names = full_names, recursive = recursive)
  file_name <- list.files(path = directory, pattern = file_pattern, 
                          full.names = FALSE, recursive = recursive)
  
  if(is.null(max_search)) {
    extract_table <- lapply(seq_along(files_dir), function(xx) 
      keyword_search(files_dir[xx], keyword = keyword, path = TRUE,
                     surround_lines = surround_lines, 
                     ignore_case = ignore_case, 
                     token_results = token_results, 
                     split_pdf = split_pdf, 
                     remove_hyphen = remove_hyphen,
                     convert_sentence = convert_sentence, 
                     split_pattern = split_pattern,
                     split_method = split_method,
                     column_count = column_count,
                     mask_nonprose = mask_nonprose,
                     nonprose_digit_ratio = nonprose_digit_ratio,
                     nonprose_symbol_ratio = nonprose_symbol_ratio,
                     nonprose_short_token_max = nonprose_short_token_max,
                     remove_section_headers = remove_section_headers,
                     remove_page_headers = remove_page_headers,
                     remove_page_footers = remove_page_footers,
                     page_margin_ratio = page_margin_ratio,
                     remove_repeated_furniture = remove_repeated_furniture,
                     repeated_edge_n = repeated_edge_n,
                     repeated_edge_min_pages = repeated_edge_min_pages,
                     remove_captions = remove_captions,
                     caption_continuation_max = caption_continuation_max,
                     table_mode = table_mode,
                     table_min_numeric_tokens = table_min_numeric_tokens,
                     table_min_digit_ratio = table_min_digit_ratio,
                     table_min_block_lines = table_min_block_lines,
                     table_block_max_gap = table_block_max_gap,
                     table_include_headers = table_include_headers,
                     table_header_lookback = table_header_lookback,
                     table_include_notes = table_include_notes,
                     table_note_lookahead = table_note_lookahead,
                     concatenate_pages = concatenate_pages,
                     ...))
  } else {
    files_dir <- files_dir[1:max_search]
    file_name <- file_name[1:max_search]
    extract_table <- lapply(seq_along(files_dir), function(xx) 
      keyword_search(files_dir[xx], keyword = keyword, path = TRUE,
                     surround_lines = surround_lines, 
                     ignore_case = ignore_case, 
                     token_results = token_results, 
                     split_pdf = split_pdf, 
                     remove_hyphen = remove_hyphen,
                     convert_sentence = convert_sentence, 
                     split_pattern = split_pattern,
                     split_method = split_method,
                     column_count = column_count,
                     mask_nonprose = mask_nonprose,
                     nonprose_digit_ratio = nonprose_digit_ratio,
                     nonprose_symbol_ratio = nonprose_symbol_ratio,
                     nonprose_short_token_max = nonprose_short_token_max,
                     remove_section_headers = remove_section_headers,
                     remove_page_headers = remove_page_headers,
                     remove_page_footers = remove_page_footers,
                     page_margin_ratio = page_margin_ratio,
                     remove_repeated_furniture = remove_repeated_furniture,
                     repeated_edge_n = repeated_edge_n,
                     repeated_edge_min_pages = repeated_edge_min_pages,
                     remove_captions = remove_captions,
                     caption_continuation_max = caption_continuation_max,
                     table_mode = table_mode,
                     table_min_numeric_tokens = table_min_numeric_tokens,
                     table_min_digit_ratio = table_min_digit_ratio,
                     table_min_block_lines = table_min_block_lines,
                     table_block_max_gap = table_block_max_gap,
                     table_include_headers = table_include_headers,
                     table_header_lookback = table_header_lookback,
                     table_include_notes = table_include_notes,
                     table_note_lookahead = table_note_lookahead,
                     concatenate_pages = concatenate_pages,
                     ...))
  }
  
  num_rows <- unlist(lapply(extract_table, nrow))
  
  result_out <- do.call("rbind", extract_table)
  
  ids <- data.frame(ID = rep(seq_along(files_dir), num_rows),
                    pdf_name = rep(file_name, num_rows))
  result_out <- cbind(ids, result_out)
  return(result_out)
}
