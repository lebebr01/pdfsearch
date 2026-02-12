#' Function to extract tables
#'
#' @param x Either the text of the pdf read in with the pdftools package or a
#'    path for the location of the pdf file.
#' @param path An optional path designation for the location of the pdf to be
#'    converted to text. The pdftools package is used for this conversion.
#' @param split_pdf TRUE/FALSE indicating whether to split the pdf using white
#'    space. This would be most useful with multicolumn pdf files.
#'    The split_pdf function attempts to recreate the column layout of the text
#'    into a single column starting with the left column and proceeding to the
#'    right.
#' @param remove_equations TRUE/FALSE indicating if equations should be removed.
#'     Default behavior is to search for a literal parenthesis,
#'     followed by at least one number followed by another parenthesis at
#'     the end of the text line. This will not detect other patterns or
#'     detect the entire equation if it is a multi-row equation.
#' @param delimiter A delimiter used to detect tables. The default is two
#'   consecutive blank white spaces.
#' @param delimiter_table A delimiter used to separate table cells. The default
#'   value is two consecutive blank white spaces.
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
#' @param remove_section_headers TRUE/FALSE indicating if section-header-like
#'     lines should be removed prior to table extraction.
#' @param remove_page_headers TRUE/FALSE indicating if page-header furniture
#'     should be removed prior to table extraction.
#' @param remove_page_footers TRUE/FALSE indicating if page-footer furniture
#'     should be removed prior to table extraction.
#' @param remove_repeated_furniture TRUE/FALSE indicating if repeated text
#'     found in page edges should be removed prior to table extraction.
#' @param table_min_numeric_tokens Minimum numeric tokens used to classify a
#'     line as table-like.
#' @param table_min_digit_ratio Minimum digit-character ratio used to classify
#'     a line as table-like.
#' @param table_min_block_lines Minimum number of adjacent table-like lines for
#'     a block to be treated as a table block.
#' @param table_block_max_gap Maximum gap (in lines) allowed between
#'     table-like lines inside one table block.
#' @param table_include_headers TRUE/FALSE indicating if table header lines
#'     adjacent to detected table blocks should be included in output blocks.
#' @param table_header_lookback Number of lines above a detected table block to
#'     inspect for header rows.
#' @param table_include_notes TRUE/FALSE indicating if note/source lines after
#'     table blocks should be included in output blocks.
#' @param table_note_lookahead Number of lines after a detected table block to
#'     inspect for note/source rows.
#' @param remove_captions TRUE/FALSE indicating if figure/table caption lines
#'     should be removed before table-block detection.
#' @param caption_continuation_max Number of additional lines after a caption
#'     start line to remove when they appear to be caption continuations.
#' @param replacement A delimiter used to separate table cells after the
#'   replacement of white space is done.
#' @param col_names TRUE/FALSE value passed to `readr::read_delim`
#'   to indicate if column names should be used. Default value is FALSE which
#'   means column names will be generic (i.e. X1, X2, etc). A value of TRUE
#'   would take the values from the first row of data extracted.
#' @param output Output mode: "parsed" returns list of parsed data frames,
#'   "blocks" returns detected table blocks with metadata, and "both" returns
#'   a list with both representations.
#' @param merge_across_pages TRUE/FALSE indicating if adjacent blocks on
#'   consecutive pages should be merged when they appear to be table
#'   continuations.
#' @importFrom readr read_delim
#' @export
extract_tables <- function(
  x,
  path = FALSE,
  split_pdf = FALSE,
  remove_equations = TRUE,
  delimiter = "\\s{2,}",
  delimiter_table = "\\s{2,}",
  split_pattern = "\\p{WHITE_SPACE}{3,}",
  split_method = c("regex", "coordinates"),
  column_count = c("auto", "1", "2"),
  remove_section_headers = FALSE,
  remove_page_headers = FALSE,
  remove_page_footers = FALSE,
  remove_repeated_furniture = FALSE,
  table_min_numeric_tokens = 3,
  table_min_digit_ratio = 0.18,
  table_min_block_lines = 2,
  table_block_max_gap = 3,
  table_include_headers = TRUE,
  table_header_lookback = 3,
  table_include_notes = FALSE,
  table_note_lookahead = 2,
  remove_captions = TRUE,
  caption_continuation_max = 2,
  replacement = "\\/",
  col_names = FALSE,
  output = c("parsed", "blocks", "both"),
  merge_across_pages = TRUE
) {
  split_method <- match.arg(split_method)
  column_count <- match.arg(column_count)
  output <- match.arg(output)
  pdf_data <- NULL

  if (path) {
    if (split_pdf && split_method == "coordinates") {
      pdf_data <- pdftools::pdf_data(x)
    }
    x <- pdftools::pdf_text(x)
  }
  line_nums <- cumsum(lapply(tokenizers::tokenize_lines(x), length))
  if (any(line_nums == 0)) {
    warning('text not recognized in pdf')
    return(list())
  } else {
    x_lines_list <- format_text(
      pdf_text = x,
      split_pdf = split_pdf,
      blank_lines = TRUE,
      remove_hyphen = FALSE,
      convert_sentence = FALSE,
      remove_equations = remove_equations,
      split_pattern = split_pattern,
      split_method = split_method,
      pdf_data = pdf_data,
      column_count = column_count,
      remove_section_headers = remove_section_headers,
      remove_page_headers = remove_page_headers,
      remove_page_footers = remove_page_footers,
      remove_repeated_furniture = remove_repeated_furniture,
      remove_captions = remove_captions,
      caption_continuation_max = caption_continuation_max,
      table_mode = "only",
      table_min_numeric_tokens = table_min_numeric_tokens,
      table_min_digit_ratio = table_min_digit_ratio,
      table_min_block_lines = table_min_block_lines,
      table_block_max_gap = table_block_max_gap,
      table_include_headers = table_include_headers,
      table_header_lookback = table_header_lookback,
      table_include_notes = table_include_notes,
      table_note_lookahead = table_note_lookahead
    )

    blocks <- collect_table_blocks(
      x_lines_list,
      delimiter = delimiter,
      min_numeric_tokens = table_min_numeric_tokens,
      min_digit_ratio = table_min_digit_ratio,
      min_block_lines = table_min_block_lines,
      max_gap = table_block_max_gap,
      include_headers = table_include_headers,
      header_lookback = table_header_lookback,
      include_notes = table_include_notes,
      note_lookahead = table_note_lookahead
    )

    if (merge_across_pages) {
      blocks <- merge_table_blocks(blocks)
    }

    parsed_tables <- lapply(seq_len(nrow(blocks)), function(ii) {
      block_lines <- blocks$line_text[[ii]]
      block_lines <- add_delimiter(
        block_lines,
        delimiter = delimiter_table,
        replacement = replacement
      )
      parsed <- tryCatch(
        withCallingHandlers(
          readr::read_delim(
            I(block_lines),
            delim = "/",
            col_names = col_names,
            show_col_types = FALSE,
            progress = FALSE
          ),
          warning = function(w) {
            invokeRestart("muffleWarning")
          }
        ),
        error = function(e) NULL
      )
      if (is.null(parsed)) {
        tibble::tibble(raw_table_line = blocks$line_text[[ii]])
      } else {
        parsed
      }
    })

    if (output == "parsed") {
      return(parsed_tables)
    }
    if (output == "blocks") {
      return(blocks)
    }
    list(parsed = parsed_tables, blocks = blocks)
  }
}

#' @importFrom stats ave
merge_table_blocks <- function(blocks) {
  if (nrow(blocks) <= 1) {
    return(blocks)
  }

  blocks <- blocks[order(blocks$page_num, blocks$line_start), ]
  merged <- list()

  cur <- blocks[1, ]
  cur$page_end <- cur$page_num

  for (ii in 2:nrow(blocks)) {
    nxt <- blocks[ii, ]
    nxt_head <- trimws(nxt$line_text[[1]][1])
    nxt_has_table_title <- grepl(
      "^(table)\\s*[0-9a-zivx]+[\\.:]?",
      tolower(nxt_head),
      perl = TRUE
    )

    cur_has_table_title <- any(
      grepl(
        "^(table)\\s*[0-9a-zivx]+[\\.:]?",
        tolower(trimws(cur$line_text[[1]])),
        perl = TRUE
      )
    )

    can_merge <- (nxt$page_num - cur$page_end) == 1 &&
      cur_has_table_title &&
      !nxt_has_table_title

    if (can_merge) {
      cur$line_text[[1]] <- c(cur$line_text[[1]], nxt$line_text[[1]])
      cur$line_end <- nxt$line_end
      cur$page_end <- nxt$page_num
    } else {
      merged[[length(merged) + 1]] <- cur
      cur <- nxt
      cur$page_end <- cur$page_num
    }
  }

  merged[[length(merged) + 1]] <- cur
  out <- do.call("rbind", merged)
  rownames(out) <- NULL

  out$block_id <- as.integer(ave(out$page_num, out$page_num, FUN = seq_along))
  out
}

collect_table_blocks <- function(
  x_lines_list,
  delimiter = "\\s{2,}",
  min_numeric_tokens = 3,
  min_digit_ratio = 0.18,
  min_block_lines = 2,
  max_gap = 1,
  include_headers = TRUE,
  header_lookback = 2,
  include_notes = FALSE,
  note_lookahead = 2
) {
  out <- list()
  row_id <- 1

  for (page_id in seq_along(x_lines_list)) {
    page_lines <- x_lines_list[[page_id]]
    if (length(page_lines) == 0) {
      next
    }

    is_table <- table_block_selector(
      page_lines,
      min_numeric_tokens = min_numeric_tokens,
      min_digit_ratio = min_digit_ratio,
      delimiter = delimiter,
      max_gap = max_gap,
      min_block_lines = min_block_lines,
      include_headers = include_headers,
      header_lookback = header_lookback,
      include_notes = include_notes,
      note_lookahead = note_lookahead
    )

    if (!any(is_table)) {
      next
    }

    r <- rle(is_table)
    ends <- cumsum(r$lengths)
    starts <- c(1, head(ends, -1) + 1)
    true_runs <- which(r$values)

    for (block_id in seq_along(true_runs)) {
      rr <- true_runs[block_id]
      s <- starts[rr]
      e <- ends[rr]
      out[[row_id]] <- list(
        page_num = page_id,
        block_id = block_id,
        line_start = s,
        line_end = e,
        line_text = page_lines[s:e]
      )
      row_id <- row_id + 1
    }
  }

  if (length(out) == 0) {
    return(tibble::tibble(
      page_num = integer(0),
      block_id = integer(0),
      line_start = integer(0),
      line_end = integer(0),
      line_text = list()
    ))
  }

  tibble::tibble(
    page_num = unlist(lapply(out, function(x) x$page_num)),
    block_id = unlist(lapply(out, function(x) x$block_id)),
    line_start = unlist(lapply(out, function(x) x$line_start)),
    line_end = unlist(lapply(out, function(x) x$line_end)),
    line_text = lapply(out, function(x) x$line_text)
  )
}

find_table_locations <- function(row_numbers) {
  length_input <- length(row_numbers) - 1

  diff_adjacent <- unlist(
    lapply(seq_len(length_input), function(xx) {
      row_numbers[xx + 1] - row_numbers[xx]
    })
  )

  diff_adj_tf <- diff_adjacent < 5

  rle_df <- data.frame(
    lengths = rle(diff_adj_tf)$lengths,
    values = rle(diff_adj_tf)$values
  )

  rle_df$select <- ifelse(rle_df$lengths > 4 & rle_df$values, 1, 0)
  rle_df$start <- c(1, cumsum(rle_df$lengths[1:(nrow(rle_df) - 1)]) + 1)
  rle_df$end <- cumsum(rle_df$lengths) + 1
  rle_df$new_values <- ifelse(rle_df$select, TRUE, FALSE)

  rle_true <- rle_df[rle_df$new_values == TRUE, ]

  convert_to_true <- lapply(1:nrow(rle_true), function(xx) {
    rle_true[xx, 'start']:rle_true[xx, 'end']
  })

  row_numbers_return <- lapply(seq_along(convert_to_true), function(xx) {
    row_numbers[convert_to_true[[xx]]]
  })

  row_numbers_return
}

add_delimiter <- function(
  table_lines,
  delimiter = "\\s{2,}",
  replacement = "\\/"
) {
  gsub(delimiter, replacement, table_lines)
}
