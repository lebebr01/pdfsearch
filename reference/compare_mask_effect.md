# Compare keyword results with and without coordinate masking

Runs
[`keyword_search`](https://pdfsearch.brandonlebeau.org/reference/keyword_search.md)
twice with coordinate splitting: once with `mask_nonprose = FALSE` and
once with `mask_nonprose = TRUE`. Returns a compact summary for A/B
checks.

## Usage

``` r
compare_mask_effect(
  x,
  keyword,
  path = FALSE,
  column_count = c("auto", "1", "2"),
  nonprose_digit_ratio = 0.35,
  nonprose_symbol_ratio = 0.15,
  nonprose_short_token_max = 3,
  ...
)
```

## Arguments

- x:

  Either the text of the pdf read in with the pdftools package or a path
  for the location of the pdf file.

- keyword:

  The keyword(s) to be used to search in the text. Multiple keywords can
  be specified with a character vector.

- path:

  An optional path designation for the location of the pdf to be
  converted to text. The pdftools package is used for this conversion.
  Must be TRUE for coordinate splitting.

- column_count:

  Expected number of columns for coordinate splitting. Options are
  "auto", "1", or "2".

- nonprose_digit_ratio:

  Numeric threshold for classifying a line as non-prose based on digit
  character ratio.

- nonprose_symbol_ratio:

  Numeric threshold for classifying a line as non-prose based on
  math-symbol character ratio.

- nonprose_short_token_max:

  Maximum token count for short symbolic lines to classify as non-prose.

- ...:

  Additional arguments passed to
  [`keyword_search`](https://pdfsearch.brandonlebeau.org/reference/keyword_search.md).

## Value

A tibble data frame with one row per mode ("unmasked", "masked") and the
number of matches.

## Examples

``` r
file <- system.file('pdf', '1501.00450.pdf', package = 'pdfsearch')
compare_mask_effect(file, keyword = "error", path = TRUE, column_count = "2")
#> # A tibble: 2 × 2
#>   mode     num_matches
#>   <chr>          <int>
#> 1 unmasked           2
#> 2 masked             2
```
