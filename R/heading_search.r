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
                           split_pdf = FALSE,
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
                           convert_sentence = FALSE) {
  split_method <- match.arg(split_method)
  column_count <- match.arg(column_count)
  table_mode <- match.arg(table_mode)
  
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
                 split_pdf = split_pdf,
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
                 convert_sentence = convert_sentence)
  
}
