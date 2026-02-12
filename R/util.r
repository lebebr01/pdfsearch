remove_indent <- function(x, sel_element) {
  x_tmp <- x[sel_element == 3]
  blank_first <- sapply(seq_along(x_tmp), function(tt) x_tmp[[tt]][1] == '')
  x_tmp2 <- x_tmp[blank_first]
  x_tmp2 <- lapply(seq_along(x_tmp2), function(tt) x_tmp2[[tt]][-1])
  x_tmp[blank_first] <- x_tmp2
  x[sel_element == 3] <- x_tmp
  return(x)
}

collapse_columns <- function(x_list, lengths) {
  do.call(
    'c',
    do.call(
      'c',
      lapply(1:max(lengths), function(ii) {
        lapply(seq_along(x_list), function(xx) {
          x_list[[xx]][ii]
        })
      })
    )
  )
}

remove_blank_lines <- function(text_lines) {
  text_lines[nchar(text_lines) != 0]
}

remove_hyphen <- function(text_lines) {
  hyphen_location <- grep("-$", text_lines)

  hyphen_wrap_text <- text_lines[hyphen_location + 1]
  hyphen_wrap_text <- strsplit(hyphen_wrap_text, split = ' ')
  hyphen_wrap_text <- unlist(lapply(seq_along(hyphen_wrap_text), function(xx) {
    hyphen_wrap_text[[xx]][1]
  }))

  for (xx in seq_along(hyphen_location)) {
    text_lines[hyphen_location[xx]] <- gsub(
      "-$",
      hyphen_wrap_text[xx],
      text_lines[hyphen_location[xx]]
    )
    text_lines[hyphen_location[xx] + 1] <- gsub(
      hyphen_wrap_text[xx],
      "",
      text_lines[hyphen_location[xx] + 1],
      fixed = TRUE
    )
  }
  text_lines <- gsub("^\\s+|\\s+$", '', text_lines)
  text_lines
}

true_false <- function(x, chars) {
  x > chars
}

remove_equation <- function(text_lines) {
  idx <- grep("\\([0-9]{1,}\\)$", text_lines)
  if (length(idx) == 0) {
    return(text_lines)
  }
  text_lines[-idx]
}

detect_nonprose_line <- function(
  text_line,
  digit_ratio = 0.35,
  symbol_ratio = 0.15,
  short_token_max = 3
) {
  txt <- trimws(text_line)
  if (nchar(txt) == 0) {
    return(TRUE)
  }

  lower_txt <- tolower(txt)
  if (grepl("^(table|figure|fig\\.)\\s*[0-9a-zivx]+", lower_txt, perl = TRUE)) {
    return(TRUE)
  }

  chars <- unlist(strsplit(txt, split = ""))
  total_chars <- length(chars)
  if (total_chars == 0) {
    return(TRUE)
  }

  symbol_set <- c(
    "=",
    "+",
    "-",
    "*",
    "/",
    "^",
    "_",
    "{",
    "}",
    "<",
    ">",
    "\\",
    "|",
    "~"
  )
  digit_chars <- sum(grepl("[0-9]", chars))
  symbol_chars <- sum(chars %in% symbol_set)

  d_ratio <- digit_chars / total_chars
  s_ratio <- symbol_chars / total_chars

  tokens <- unlist(strsplit(txt, split = "\\s+"))
  tokens <- tokens[nchar(tokens) > 0]

  short_symbolic <- length(tokens) <= short_token_max &&
    (d_ratio >= 0.2 || s_ratio >= 0.1) &&
    any(
      chars %in% c("=", "+", "-", "*", "/", "^", "_", "{", "}", "<", ">", "\\")
    )

  d_ratio >= digit_ratio || s_ratio >= symbol_ratio || short_symbolic
}

detect_section_header_line <- function(text_line, max_words = 12) {
  txt <- trimws(text_line)
  if (nchar(txt) == 0) {
    return(FALSE)
  }

  lower_txt <- tolower(txt)
  if (
    lower_txt %in%
      c(
        "abstract",
        "introduction",
        "background",
        "methods",
        "method",
        "results",
        "discussion",
        "conclusion",
        "conclusions",
        "limitations",
        "implications",
        "instrument",
        "participants",
        "references",
        "acknowledgments",
        "acknowledgements",
        "general terms",
        "categories and subject descriptors",
        "keywords",
        "model fit and reliability"
      )
  ) {
    return(TRUE)
  }

  words <- unlist(strsplit(txt, "\\s+"))
  words <- words[nchar(words) > 0]
  if (length(words) > max_words) {
    return(FALSE)
  }

  numbered_heading <- grepl(
    "^\\s*(\\d+(\\.\\d+)*|[IVXLCM]+)\\.?\\s+[A-Z][A-Za-z][A-Za-z\\s\\-,:/&]{1,100}$",
    txt,
    perl = TRUE
  )

  upper_heading <- grepl("^[A-Z][A-Z\\s\\-,:/&]{3,}$", txt, perl = TRUE) &&
    !grepl("\\.$", txt)

  title_case_heading <- length(words) <= 10 &&
    grepl("^[A-Z][A-Za-z0-9\\s\\-,:/&]{2,80}$", txt, perl = TRUE) &&
    !grepl("[\\.!\\?]$", txt) &&
    sum(grepl("^[A-Z]", words)) >= max(1, ceiling(length(words) * 0.6))

  numbered_heading || upper_heading || title_case_heading
}

detect_page_furniture_line <- function(text_line) {
  txt <- trimws(text_line)
  if (nchar(txt) == 0) {
    return(FALSE)
  }

  lower_txt <- tolower(txt)

  if (grepl("^(page\\s*)?[0-9]+$", lower_txt, perl = TRUE)) {
    return(TRUE)
  }

  if (
    grepl("arxiv:|doi:|copyright|all rights reserved", lower_txt, perl = TRUE)
  ) {
    return(TRUE)
  }

  if (grepl("https?://|www\\.|@", lower_txt, perl = TRUE)) {
    return(TRUE)
  }

  short_nonnarrative <- nchar(txt) <= 25 &&
    !grepl("[a-z]{3,}", lower_txt, perl = TRUE)
  short_nonnarrative
}

detect_strong_page_furniture_line <- function(text_line) {
  txt <- trimws(text_line)
  if (nchar(txt) == 0) {
    return(FALSE)
  }

  lower_txt <- tolower(txt)
  grepl(
    "arxiv:|doi:|copyright|all rights reserved|https?://|www\\.|@",
    lower_txt,
    perl = TRUE
  )
}

collapse_pages_to_sentences <- function(x_lines_list) {
  page_text <- vapply(
    x_lines_list,
    function(x) paste(x, collapse = " "),
    character(1)
  )
  page_text <- trimws(page_text)

  keep_page <- nchar(page_text) > 0
  page_text <- page_text[keep_page]
  page_ids <- seq_along(x_lines_list)[keep_page]

  if (length(page_text) == 0) {
    return(list(sentences = character(0), page_map = integer(0)))
  }

  page_start <- cumsum(c(1, nchar(page_text[-length(page_text)]) + 1))
  full_text <- paste(page_text, collapse = " ")

  sent_locs <- stringi::stri_locate_all_boundaries(
    full_text,
    type = "sentence"
  )[[1]]
  if (is.null(sent_locs) || nrow(sent_locs) == 0) {
    return(list(sentences = character(0), page_map = integer(0)))
  }

  sentences <- stringi::stri_sub(full_text, sent_locs[, 1], sent_locs[, 2])
  sentences <- trimws(sentences)
  keep_sent <- nchar(sentences) > 0

  sentences <- sentences[keep_sent]
  sent_start <- sent_locs[keep_sent, 1]

  if (length(sentences) == 0) {
    return(list(sentences = character(0), page_map = integer(0)))
  }

  page_map_idx <- findInterval(sent_start, page_start)
  page_map <- page_ids[page_map_idx]

  list(sentences = sentences, page_map = page_map)
}

#' @importFrom utils head tail
remove_repeated_edge_lines <- function(
  x_lines_list,
  edge_n = 3,
  min_pages = 4,
  max_chars = 90
) {
  if (length(x_lines_list) < min_pages) {
    return(x_lines_list)
  }

  top_lines <- lapply(x_lines_list, function(x) trimws(head(x, edge_n)))
  bottom_lines <- lapply(x_lines_list, function(x) trimws(tail(x, edge_n)))

  edge_lines <- unlist(c(top_lines, bottom_lines), use.names = FALSE)
  edge_lines <- edge_lines[
    nchar(edge_lines) > 0 & nchar(edge_lines) <= max_chars
  ]
  if (length(edge_lines) == 0) {
    return(x_lines_list)
  }

  edge_counts <- table(edge_lines)
  repeated <- names(edge_counts[edge_counts >= min_pages])
  if (length(repeated) == 0) {
    return(x_lines_list)
  }

  lapply(x_lines_list, function(page_lines) {
    keep <- !(trimws(page_lines) %in% repeated)
    page_lines[keep]
  })
}

detect_caption_line <- function(text_line) {
  txt <- trimws(text_line)
  if (nchar(txt) == 0) {
    return(FALSE)
  }

  lower_txt <- tolower(txt)

  if (
    grepl(
      "^(figure|fig\\.?|table)\\s*[0-9a-zivx]+[\\.:]",
      lower_txt,
      perl = TRUE
    )
  ) {
    return(TRUE)
  }

  if (grepl("^note\\.", lower_txt, perl = TRUE)) {
    return(TRUE)
  }

  FALSE
}

detect_table_caption_line <- function(text_line) {
  txt <- trimws(text_line)
  if (nchar(txt) == 0) {
    return(FALSE)
  }
  lower_txt <- tolower(txt)
  grepl("^(table)\\s*[0-9a-zivx]+[\\.:]", lower_txt, perl = TRUE)
}

remove_caption_lines <- function(
  page_lines,
  continuation_max = 2,
  remove_table_captions = TRUE
) {
  n <- length(page_lines)
  if (n == 0) {
    return(page_lines)
  }

  keep <- rep(TRUE, n)
  idx <- 1
  while (idx <= n) {
    if (detect_caption_line(page_lines[idx])) {
      if (!remove_table_captions && detect_table_caption_line(page_lines[idx])) {
        idx <- idx + 1
        next
      }
      keep[idx] <- FALSE

      # Drop a few likely caption continuation rows after the caption start.
      if (continuation_max > 0 && idx < n) {
        end_idx <- min(n, idx + continuation_max)
        for (j in (idx + 1):end_idx) {
          line <- trimws(page_lines[j])
          if (nchar(line) == 0) {
            keep[j] <- FALSE
            next
          }

          low <- tolower(line)
          looks_caption_cont <- grepl(
            "^note\\.|^source\\.",
            low,
            perl = TRUE
          ) ||
            nchar(line) <= 120 ||
            grepl(
              "\\b(left|right)\\s+figure\\b|\\bconfidence\\s+interval\\b|\\bpercentile\\b",
              low,
              perl = TRUE
            )

          if (looks_caption_cont) {
            keep[j] <- FALSE
          } else {
            break
          }
        }
      }
    }
    idx <- idx + 1
  }

  page_lines[keep]
}

detect_table_like_line <- function(
  text_line,
  min_numeric_tokens = 3,
  min_digit_ratio = 0.18
) {
  txt <- trimws(text_line)
  if (nchar(txt) == 0) {
    return(FALSE)
  }

  low <- tolower(txt)
  if (grepl("^(table|fig\\.?|figure)\\s*[0-9a-zivx]+", low, perl = TRUE)) {
    return(TRUE)
  }

  tokens <- unlist(strsplit(txt, "\\s+"))
  tokens <- tokens[nchar(tokens) > 0]
  if (length(tokens) == 0) {
    return(FALSE)
  }

  numeric_tokens <- grepl(
    "^[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?[%)]?$",
    tokens,
    perl = TRUE
  )
  n_numeric <- sum(numeric_tokens)

  chars <- unlist(strsplit(txt, split = ""))
  digit_ratio <- sum(grepl("[0-9]", chars)) / max(1, length(chars))

  low_alpha <- sum(grepl("[A-Za-z]", chars)) / max(1, length(chars)) < 0.45

  (n_numeric >= min_numeric_tokens && digit_ratio >= min_digit_ratio) ||
    (n_numeric >= (min_numeric_tokens + 1) && low_alpha)
}

detect_table_header_candidate <- function(text_line, delimiter = "\\s{2,}") {
  txt <- trimws(text_line)
  if (nchar(txt) == 0) {
    return(FALSE)
  }

  low <- tolower(txt)
  if (grepl("^(table|fig\\.?|figure)\\s*[0-9a-zivx]+", low, perl = TRUE)) {
    return(TRUE)
  }

  if (!grepl(delimiter, txt, perl = TRUE)) {
    return(FALSE)
  }

  chars <- unlist(strsplit(txt, split = ""))
  digit_ratio <- sum(grepl("[0-9]", chars)) / max(1, length(chars))
  alpha_ratio <- sum(grepl("[A-Za-z]", chars)) / max(1, length(chars))

  alpha_ratio >= 0.35 && digit_ratio <= 0.35 && !grepl("[.!?]$", txt)
}

table_block_selector <- function(
  page_lines,
  min_numeric_tokens = 3,
  min_digit_ratio = 0.18,
  delimiter = "\\s{2,}",
  max_gap = 1,
  min_block_lines = 2,
  include_headers = FALSE,
  header_lookback = 2,
  include_notes = FALSE,
  note_lookahead = 2
) {
  if (length(page_lines) == 0) {
    return(logical(0))
  }

  table_like <- vapply(
    page_lines,
    detect_table_like_line,
    logical(1),
    min_numeric_tokens = min_numeric_tokens,
    min_digit_ratio = min_digit_ratio
  )
  has_delim <- grepl(delimiter, page_lines, perl = TRUE)
  table_like <- table_like | has_delim

  idx <- which(table_like)
  keep <- rep(FALSE, length(page_lines))
  if (length(idx) == 0) {
    return(keep)
  }

  groups <- split(idx, cumsum(c(1, diff(idx) > (max_gap + 1))))
  for (g in groups) {
    block_start <- min(g)
    block_end <- max(g)
    block_len <- block_end - block_start + 1
    if (length(g) >= min_block_lines || block_len >= min_block_lines) {
      keep[block_start:block_end] <- TRUE

      if (include_headers && header_lookback > 0) {
        look_start <- max(1, block_start - header_lookback)
        if ((block_start - 1) >= look_start) {
          for (hh in seq(block_start - 1, look_start, by = -1)) {
            line <- trimws(page_lines[hh])
            if (nchar(line) == 0) {
              break
            }
            if (
              detect_caption_line(line) ||
                detect_table_header_candidate(line, delimiter = delimiter)
            ) {
              keep[hh] <- TRUE
            } else {
              break
            }
          }
        }
      }

      if (include_notes && note_lookahead > 0) {
        look_end <- min(length(page_lines), block_end + note_lookahead)
        if (block_end < look_end) {
          for (nn in seq(block_end + 1, look_end)) {
            low <- tolower(trimws(page_lines[nn]))
            if (grepl("^note\\.|^source\\.", low, perl = TRUE)) {
              keep[nn] <- TRUE
            } else if (nchar(trimws(page_lines[nn])) == 0) {
              keep[nn] <- TRUE
            } else {
              break
            }
          }
        }
      }
    }
  }

  keep
}
