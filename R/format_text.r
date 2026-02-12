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
#' @param split_method Method used for splitting multicolumn PDF text.
#'     Defaults to "regex". Use "coordinates" to split with
#'     \code{pdftools::pdf_data()} token coordinates.
#' @param pdf_data Optional token-level PDF data from \code{pdftools::pdf_data()}.
#'     Used when \code{split_method = "coordinates"}.
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
#'     concatenated before sentence conversion. This is only used when
#'     \code{convert_sentence = TRUE}.
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
                        split_method = c("regex", "coordinates"),
                        pdf_data = NULL,
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
                        ...) {
  split_method <- match.arg(split_method)
  column_count <- match.arg(column_count)
  table_mode <- match.arg(table_mode)
  
  if(split_pdf) {
    if(split_method == "coordinates" && !is.null(pdf_data)) {
      x_list <- split_pdf_coordinates(
        pdf_data,
        column_count = column_count,
        mask_nonprose = mask_nonprose,
        nonprose_digit_ratio = nonprose_digit_ratio,
        nonprose_symbol_ratio = nonprose_symbol_ratio,
        nonprose_short_token_max = nonprose_short_token_max,
        remove_section_headers = remove_section_headers,
        remove_page_headers = remove_page_headers,
        remove_page_footers = remove_page_footers,
        page_margin_ratio = page_margin_ratio
      )
    } else {
      if(split_method == "coordinates" && is.null(pdf_data)) {
        warning("split_method = 'coordinates' requested but pdf_data is missing; using regex split")
      }
      x_list <- split_pdf(pdf_text, pattern = split_pattern)
    }
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
 
  if(remove_section_headers) {
    x_lines_list <- lapply(x_lines_list, function(page_lines) {
      is_section <- vapply(page_lines, detect_section_header_line, logical(1))
      is_table_header <- vapply(page_lines, detect_caption_line, logical(1))
      keep <- !is_section | is_table_header
      page_lines[keep]
    })
  }
  
  if(remove_repeated_furniture) {
    x_lines_list <- remove_repeated_edge_lines(
      x_lines_list,
      edge_n = repeated_edge_n,
      min_pages = repeated_edge_min_pages
    )
  }
  
  if(remove_captions) {
    remove_table_captions <- !(table_mode == "only" && table_include_headers)
    x_lines_list <- lapply(x_lines_list, remove_caption_lines,
                           continuation_max = caption_continuation_max,
                           remove_table_captions = remove_table_captions)
  }
  
  if(table_mode != "keep") {
    x_lines_list <- lapply(x_lines_list, function(page_lines) {
      is_table <- table_block_selector(
        page_lines,
        min_numeric_tokens = table_min_numeric_tokens,
        min_digit_ratio = table_min_digit_ratio,
        max_gap = table_block_max_gap,
        min_block_lines = table_min_block_lines,
        include_headers = table_include_headers,
        header_lookback = table_header_lookback,
        include_notes = table_include_notes,
        note_lookahead = table_note_lookahead
      )
      if(table_mode == "remove") {
        page_lines[!is_table]
      } else {
        page_lines[is_table]
      }
    })
  }
  
  # collapse into a single paragraph
  if(convert_sentence) {
    if(concatenate_pages) {
      sent <- collapse_pages_to_sentences(x_lines_list)
      x_lines_list <- list(sent$sentences)
    } else {
      x_lines_list <- lapply(seq_along(x_lines_list), function(xx) paste(x_lines_list[[xx]], collapse = ' ')
      )
      x_lines_list <- lapply(seq_along(x_lines_list), function(xx) unlist(stringi::stri_split_boundaries(x_lines_list[[xx]], 
                                                                                                         type = "sentence"))
      )
    }
  }
  
  x_lines_list
}
