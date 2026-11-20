# Search a pdf file for keywords

This uses the pdf_text from the pdftools package to perform keyword
searches. Keyword locations indicating the line of the text as well as
the page number that the keyword is found are returned.

## Usage

``` r
keyword_search(
  x,
  keyword,
  path = FALSE,
  surround_lines = FALSE,
  ignore_case = FALSE,
  token_results = TRUE,
  heading_search = FALSE,
  heading_args = NULL,
  split_pdf = FALSE,
  blank_lines = TRUE,
  remove_hyphen = TRUE,
  convert_sentence = TRUE,
  remove_equations = FALSE,
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

- surround_lines:

  numeric/FALSE indicating whether the output should extract the
  surrounding lines of text in addition to the matching line. Default is
  FALSE, if not false, include a numeric number that indicates the
  additional number of surrounding lines that will be extracted.

- ignore_case:

  TRUE/FALSE/vector of TRUE/FALSE, indicating whether the case of the
  keyword matters. Default is FALSE meaning that case of the keyword is
  literal. If a vector, must be same length as the keyword vector.

- token_results:

  TRUE/FALSE indicating whether the results text returned should be
  split into tokens. See the tokenizers package and
  [`convert_tokens`](https://pdfsearch.brandonlebeau.org/reference/convert_tokens.md)
  for more details. Defaults to TRUE.

- heading_search:

  TRUE/FALSE indicating whether to search for headings in the pdf.

- heading_args:

  A list of arguments to pass on to the
  [`heading_search`](https://pdfsearch.brandonlebeau.org/reference/heading_search.md)
  function. See
  [`heading_search`](https://pdfsearch.brandonlebeau.org/reference/heading_search.md)
  for more details on arguments needed.

- split_pdf:

  TRUE/FALSE indicating whether to split the pdf using white space. This
  would be most useful with multicolumn pdf files. The split_pdf
  function attempts to recreate the column layout of the text into a
  single column starting with the left column and proceeding to the
  right.

- blank_lines:

  TRUE/FALSE indicating whether blank text lines should be removed.
  Default is TRUE.

- remove_hyphen:

  TRUE/FALSE indicating whether hyphenated words should be adjusted to
  combine onto a single line. Default is TRUE.

- convert_sentence:

  TRUE/FALSE indicating if individual lines of PDF file should be
  collapsed into a single large paragraph to perform keyword searching.
  Default is TRUE

- remove_equations:

  TRUE/FALSE indicating if equations should be removed. Default behavior
  is to search for a literal parenthesis, followed by at least one
  number followed by another parenthesis at the end of the text line.
  This will not detect other patterns or detect the entire equation if
  it is a multi-row equation.

- split_pattern:

  Regular expression pattern used to split multicolumn PDF files using
  [`stringi::stri_split_regex`](https://rdrr.io/pkg/stringi/man/stri_split.html).
  Default pattern is to split based on three or more consecutive white
  space characters.

- split_method:

  Method used for splitting multicolumn PDF text. Defaults to "regex".
  Use "coordinates" to split with
  [`pdftools::pdf_data()`](https://docs.ropensci.org/pdftools/reference/pdftools.html)
  token coordinates.

- column_count:

  Expected number of columns for coordinate splitting. Options are
  "auto", "1", or "2". Used when `split_method = "coordinates"`.

- mask_nonprose:

  TRUE/FALSE indicating if non-prose lines (likely equations, tables,
  figure/table captions) should be removed when using coordinate
  splitting.

- nonprose_digit_ratio:

  Numeric threshold for classifying a line as non-prose based on digit
  character ratio.

- nonprose_symbol_ratio:

  Numeric threshold for classifying a line as non-prose based on
  math-symbol character ratio.

- nonprose_short_token_max:

  Maximum token count for short symbolic lines to classify as non-prose.

- remove_section_headers:

  TRUE/FALSE indicating if section-header-like lines should be removed
  when using coordinate splitting.

- remove_page_headers:

  TRUE/FALSE indicating if page-header furniture (e.g., arXiv
  identifiers, emails, URLs) should be removed when using coordinate
  splitting.

- remove_page_footers:

  TRUE/FALSE indicating if page-footer furniture (e.g., page numbers,
  copyright markers) should be removed when using coordinate splitting.

- page_margin_ratio:

  Numeric ratio used to define top and bottom page bands for
  header/footer removal.

- remove_repeated_furniture:

  TRUE/FALSE indicating if repeated text found in the first/last lines
  across many pages should be removed.

- repeated_edge_n:

  Number of lines from top and bottom of each page to consider for
  repeated edge-line detection.

- repeated_edge_min_pages:

  Minimum number of pages an edge line must appear on before being
  removed.

- remove_captions:

  TRUE/FALSE indicating if figure/table caption lines should be removed.

- caption_continuation_max:

  Number of additional lines after a caption start line to remove when
  they appear to be caption continuations.

- table_mode:

  How to handle detected table blocks. "keep" keeps all lines, "remove"
  excludes table blocks, and "only" keeps only table blocks.

- table_min_numeric_tokens:

  Minimum numeric tokens used to classify a line as table-like.

- table_min_digit_ratio:

  Minimum digit-character ratio used to classify a line as table-like.

- table_min_block_lines:

  Minimum number of adjacent table-like lines for a block to be treated
  as a table block.

- table_block_max_gap:

  Maximum gap (in lines) allowed between table-like lines inside one
  table block.

- table_include_headers:

  TRUE/FALSE indicating if table header lines adjacent to detected table
  blocks should be included in table blocks.

- table_header_lookback:

  Number of lines above a detected table block to inspect for header
  rows.

- table_include_notes:

  TRUE/FALSE indicating if trailing note/source lines should be included
  with detected table blocks.

- table_note_lookahead:

  Number of lines after a detected table block to inspect for
  note/source rows.

- concatenate_pages:

  TRUE/FALSE indicating if page text should be concatenated after column
  rectification and cleaning, before sentence conversion. This is only
  used when `convert_sentence = TRUE`.

- ...:

  token_function to pass to
  [`convert_tokens`](https://pdfsearch.brandonlebeau.org/reference/convert_tokens.md)
  function.

## Value

A tibble data frame that contains the keyword, location of match, the
line of text match, and optionally the tokens associated with the line
of text match.

## Examples

``` r
file <- system.file('pdf', '1501.00450.pdf', package = 'pdfsearch')

keyword_search(file, keyword = c('repeated measures', 'mixed effects'),
  path = TRUE)
#> # A tibble: 9 × 5
#>   keyword           page_num line_num line_text token_text
#>   <chr>                <int>    <int> <list>    <list>    
#> 1 repeated measures        1        9 <chr [1]> <list [1]>
#> 2 repeated measures        2       31 <chr [1]> <list [1]>
#> 3 repeated measures        2       58 <chr [1]> <list [1]>
#> 4 repeated measures        2       60 <chr [1]> <list [1]>
#> 5 repeated measures        2       70 <chr [1]> <list [1]>
#> 6 repeated measures        6      168 <chr [1]> <list [1]>
#> 7 repeated measures        6      179 <chr [1]> <list [1]>
#> 8 repeated measures        6      184 <chr [1]> <list [1]>
#> 9 repeated measures        9      314 <chr [1]> <list [1]>
  
# Add surrounding text
keyword_search(file, keyword = c('variance', 'mixed effects'),
  path = TRUE, surround_lines = 1)
#> # A tibble: 65 × 5
#>    keyword  page_num line_num line_text token_text
#>    <chr>       <int>    <int> <list>    <list>    
#>  1 variance        1        4 <chr [3]> <list [3]>
#>  2 variance        1       10 <chr [3]> <list [3]>
#>  3 variance        1       21 <chr [3]> <list [3]>
#>  4 variance        2       32 <chr [3]> <list [3]>
#>  5 variance        2       34 <chr [3]> <list [3]>
#>  6 variance        2       39 <chr [3]> <list [3]>
#>  7 variance        2       41 <chr [3]> <list [3]>
#>  8 variance        3       73 <chr [3]> <list [3]>
#>  9 variance        3       74 <chr [3]> <list [3]>
#> 10 variance        3       76 <chr [3]> <list [3]>
#> # ℹ 55 more rows
  
# split pdf
keyword_search(file, keyword = c('repeated measures', 'mixed effects'),
  path = TRUE, split_pdf = TRUE, remove_hyphen = FALSE)
#> # A tibble: 9 × 5
#>   keyword           page_num line_num line_text token_text
#>   <chr>                <int>    <int> <list>    <list>    
#> 1 repeated measures        1        5 <chr [1]> <list [1]>
#> 2 repeated measures        2       42 <chr [1]> <list [1]>
#> 3 repeated measures        2       43 <chr [1]> <list [1]>
#> 4 repeated measures        2       50 <chr [1]> <list [1]>
#> 5 repeated measures        2       51 <chr [1]> <list [1]>
#> 6 repeated measures        6      211 <chr [1]> <list [1]>
#> 7 repeated measures        6      219 <chr [1]> <list [1]>
#> 8 repeated measures        6      225 <chr [1]> <list [1]>
#> 9 repeated measures        9      353 <chr [1]> <list [1]>
```
