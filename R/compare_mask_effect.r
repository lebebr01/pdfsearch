#' Compare keyword results with and without coordinate masking
#'
#' Runs \code{\link{keyword_search}} twice with coordinate splitting:
#' once with \code{mask_nonprose = FALSE} and once with
#' \code{mask_nonprose = TRUE}. Returns a compact summary for A/B checks.
#'
#' @param x Either the text of the pdf read in with the pdftools package or a
#'    path for the location of the pdf file.
#' @param keyword The keyword(s) to be used to search in the text. Multiple
#'    keywords can be specified with a character vector.
#' @param path An optional path designation for the location of the pdf to be
#'    converted to text. The pdftools package is used for this conversion.
#'    Must be TRUE for coordinate splitting.
#' @param column_count Expected number of columns for coordinate splitting.
#'    Options are "auto", "1", or "2".
#' @param nonprose_digit_ratio Numeric threshold for classifying a line as
#'    non-prose based on digit character ratio.
#' @param nonprose_symbol_ratio Numeric threshold for classifying a line as
#'    non-prose based on math-symbol character ratio.
#' @param nonprose_short_token_max Maximum token count for short symbolic
#'    lines to classify as non-prose.
#' @param ... Additional arguments passed to \code{\link{keyword_search}}.
#'
#' @return A tibble data frame with one row per mode ("unmasked", "masked")
#'   and the number of matches.
#'
#' @examples
#' file <- system.file('pdf', '1501.00450.pdf', package = 'pdfsearch')
#' compare_mask_effect(file, keyword = "error", path = TRUE, column_count = "2")
#'
#' @export
compare_mask_effect <- function(x, keyword, path = FALSE,
                                column_count = c("auto", "1", "2"),
                                nonprose_digit_ratio = 0.35,
                                nonprose_symbol_ratio = 0.15,
                                nonprose_short_token_max = 3,
                                ...) {
  column_count <- match.arg(column_count)
  
  if(!path) {
    stop("compare_mask_effect requires path = TRUE")
  }
  
  unmasked <- keyword_search(
    x = x,
    keyword = keyword,
    path = path,
    split_pdf = TRUE,
    split_method = "coordinates",
    column_count = column_count,
    mask_nonprose = FALSE,
    ...
  )
  
  masked <- keyword_search(
    x = x,
    keyword = keyword,
    path = path,
    split_pdf = TRUE,
    split_method = "coordinates",
    column_count = column_count,
    mask_nonprose = TRUE,
    nonprose_digit_ratio = nonprose_digit_ratio,
    nonprose_symbol_ratio = nonprose_symbol_ratio,
    nonprose_short_token_max = nonprose_short_token_max,
    ...
  )
  
  tibble::tibble(
    mode = c("unmasked", "masked"),
    num_matches = c(nrow(unmasked), nrow(masked))
  )
}
