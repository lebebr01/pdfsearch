#' @importFrom stringi stri_split_lines stri_split_regex
split_pdf <- function(x, pattern = "\\p{WHITE_SPACE}{3,}") {
  
  x_lines <- stringi::stri_split_lines(x)
  
  x_lines <- lapply(x_lines, gsub,
                    pattern = "^\\s{1,20}", 
                    replacement = "")
  
  x_page <- lapply(x_lines, stringi::stri_split_regex, 
                   pattern = pattern, 
                   omit_empty = NA, simplify = TRUE)
  
  page_lines <- unlist(lapply(x_page, nrow))
  columns <- unlist(lapply(x_page, ncol))
  
  num_chars <- lapply(x_page, nchar)
  num_chars_tf <- lapply(num_chars, true_false, chars = 3)
  
  for(xx in seq_along(num_chars_tf)) {
    num_chars_tf[[xx]][is.na(num_chars_tf[[xx]])] <- FALSE
  }
  
  lapply(seq_along(x_page), function(xx) 
    x_page[[xx]][num_chars_tf[[xx]]])
  
  # columns_per_line <- lapply(seq_along(x_page), function(xx) 
  #   lapply(seq_len(page_lines[xx]), function(yy) 
  #     nchar(x_page[[xx]][yy, ])))
  # 
  # 
  # 
  # x_lines <- sapply(seq_along(x_lines), function(xx) {
  #   if(any(length_lines[[xx]] == 3)) {
  #     remove_indent(x_lines[[xx]], length_lines[[xx]])
  #   } else {
  #     x_lines[[xx]]
  #   }
  # })
  # 
  # length_lines <- sapply(seq_along(x_lines), function(xx) sapply(x_lines[[xx]], length))
  # 
  # x_lines <- lapply(seq_along(x_lines), function(xx) {
  #   collapse_columns(x_lines[[xx]], length_lines[[xx]])
  # })
  # 
  # length_lines <- sapply(x_lines, length)
  # x_lines <- do.call('c', x_lines)
  # 
  # return(list(x_lines, length_lines))
  
}

#' @importFrom stringi stri_isempty
detect_num_textcolumns <- function(x, pattern = "\\p{WHITE_SPACE}{3,}") {
  
  x_lines <- stringi::stri_split_lines(x)
  
  x_lines <- lapply(x_lines, gsub,
                    pattern = "^\\s{1,20}", 
                    replacement = "")
  
  x_page <- lapply(x_lines, stringi::stri_split_regex, 
                   pattern = pattern, 
                   omit_empty = NA, simplify = TRUE)
  
  empty_cells <- lapply(seq_along(x_page), function(xx) 
    apply(x_page[[xx]], 2, stringi::stri_isempty))
  for(xx in seq_along(empty_cells)) {
    empty_cells[[xx]][is.na(empty_cells[[xx]])] <- TRUE
  }
  
  sum_columns <- unlist(lapply(seq_along(empty_cells), function(xx) 
    apply(apply(empty_cells[[xx]], 2, detect_false), 1, sum)
    )
  )
  
  most_columns <- table(sum_columns)
  
  as.numeric(attr(most_columns[order(most_columns, decreasing = TRUE)][1], "names"))
  
}

detect_false <- function(x) { x == FALSE }

split_pdf_coordinates <- function(pdf_data, y_tol = NULL, min_column_gap = 40,
                                  column_count = c("auto", "1", "2"),
                                  mask_nonprose = FALSE,
                                  nonprose_digit_ratio = 0.35,
                                  nonprose_symbol_ratio = 0.15,
                                  nonprose_short_token_max = 3,
                                  remove_section_headers = FALSE,
                                  remove_page_headers = FALSE,
                                  remove_page_footers = FALSE,
                                  page_margin_ratio = 0.08) {
  column_count <- match.arg(column_count)
  
  if(!is.list(pdf_data)) {
    stop("pdf_data must be a list, typically from pdftools::pdf_data()")
  }
  
  lapply(seq_along(pdf_data), function(page_id) {
    page <- pdf_data[[page_id]]
    
    if(!all(c("x", "y", "text") %in% names(page))) {
      return(character(0))
    }
    
    keep <- !is.na(page$text) & nchar(trimws(page$text)) > 0
    page <- page[keep, , drop = FALSE]
    
    if(nrow(page) == 0) {
      return(character(0))
    }
    
    if(is.null(y_tol)) {
      if("height" %in% names(page)) {
        page_y_tol <- max(1, round(stats::median(page$height, na.rm = TRUE) * 0.6))
      } else {
        page_y_tol <- 2
      }
    } else {
      page_y_tol <- y_tol
    }
    
    page$column_id <- 1
    use_two_columns <- FALSE
    
    if(column_count == "2") {
      use_two_columns <- TRUE
    } else if(column_count == "auto" && stats::IQR(page$x) >= min_column_gap) {
      use_two_columns <- TRUE
    }
    
    if(use_two_columns) {
      km <- tryCatch(
        stats::kmeans(page$x, centers = 2, iter.max = 30),
        error = function(e) NULL
      )
      
      if(!is.null(km)) {
        centers <- as.numeric(km$centers)
        center_gap <- diff(sort(centers))
        
        if(column_count == "2" || (length(center_gap) == 1 && !is.na(center_gap) && center_gap >= min_column_gap)) {
          left_cluster <- which.min(centers)
          page$column_id <- ifelse(km$cluster == left_cluster, 1, 2)
        }
      } else if(column_count == "2") {
        x_mid <- stats::median(page$x, na.rm = TRUE)
        page$column_id <- ifelse(page$x <= x_mid, 1, 2)
      }
    }
    
    build_lines <- function(col_page, col_id, y_tolerance) {
      col_page <- col_page[order(col_page$y, col_page$x), , drop = FALSE]
      
      line_id <- integer(nrow(col_page))
      line_id[1] <- 1
      current_line <- 1
      current_y <- col_page$y[1]
      
      if(nrow(col_page) > 1) {
        for(ii in 2:nrow(col_page)) {
          if(abs(col_page$y[ii] - current_y) <= y_tolerance) {
            line_id[ii] <- current_line
          } else {
            current_line <- current_line + 1
            current_y <- col_page$y[ii]
            line_id[ii] <- current_line
          }
        }
      }
      
      col_lines <- lapply(split(seq_len(nrow(col_page)), line_id), function(idx) {
        row <- col_page[idx[order(col_page$x[idx])], , drop = FALSE]
        data.frame(
          column_id = col_id,
          y = min(row$y, na.rm = TRUE),
          min_x = min(row$x, na.rm = TRUE),
          text = paste(row$text, collapse = " "),
          stringsAsFactors = FALSE
        )
      })
      
      do.call("rbind", col_lines)
    }
    
    col_ids <- sort(unique(page$column_id))
    lines <- lapply(col_ids, function(col_id) {
      col_page <- page[page$column_id == col_id, , drop = FALSE]
      if(nrow(col_page) == 0) {
        return(NULL)
      }
      build_lines(col_page, col_id, page_y_tol)
    })
    lines <- lines[!vapply(lines, is.null, logical(1))]
    
    if(length(lines) == 0) {
      return(character(0))
    }
    
    lines <- do.call("rbind", lines)
    
    if(remove_page_headers || remove_page_footers) {
      header_cutoff <- stats::quantile(lines$y, probs = page_margin_ratio, na.rm = TRUE)
      footer_cutoff <- stats::quantile(lines$y, probs = 1 - page_margin_ratio, na.rm = TRUE)
      
      furniture <- vapply(lines$text, detect_page_furniture_line, logical(1))
      strong_furniture <- vapply(lines$text, detect_strong_page_furniture_line, logical(1))
      
      if(remove_page_headers) {
        keep_headers <- !((lines$y <= header_cutoff & furniture) | strong_furniture)
        lines <- lines[keep_headers, , drop = FALSE]
      }
      
      if(remove_page_footers && nrow(lines) > 0) {
        footer_cutoff <- stats::quantile(lines$y, probs = 1 - page_margin_ratio, na.rm = TRUE)
        furniture <- vapply(lines$text, detect_page_furniture_line, logical(1))
        keep_footers <- !(lines$y >= footer_cutoff & furniture)
        lines <- lines[keep_footers, , drop = FALSE]
      }
    }
    
    if(remove_section_headers && nrow(lines) > 0) {
      keep_sections <- !vapply(lines$text, detect_section_header_line, logical(1))
      lines <- lines[keep_sections, , drop = FALSE]
    }
    
    if(mask_nonprose) {
      keep_lines <- !vapply(
        lines$text,
        detect_nonprose_line,
        logical(1),
        digit_ratio = nonprose_digit_ratio,
        symbol_ratio = nonprose_symbol_ratio,
        short_token_max = nonprose_short_token_max
      )
      lines <- lines[keep_lines, , drop = FALSE]
    }
    
    if(nrow(lines) <= 1) {
      return(lines$text)
    }
    
    line_order <- order(lines$column_id, lines$y, lines$min_x)
    
    lines$text[line_order]
  })
}
