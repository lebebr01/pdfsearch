# Robust Multi-Column and Table Workflows

This vignette demonstrates newer `pdfsearch` workflows for:

1.  Multi-column reconstruction with coordinate-aware ordering.
2.  Cleaning recurring headers, section headings, captions, and other
    non-body text.
3.  Controlling table behavior in
    [`keyword_search()`](https://pdfsearch.brandonlebeau.org/reference/keyword_search.md).
4.  Extracting tables with richer metadata from
    [`extract_tables()`](https://pdfsearch.brandonlebeau.org/reference/extract_tables.md).

## Data

``` r

library(pdfsearch)

file <- system.file("pdf", "LeBeauetal2020-gcq.pdf", package = "pdfsearch")
```

## Coordinate-Based Column Rectification

The `split_method = "coordinates"` option uses token coordinates from
[`pdftools::pdf_data()`](https://docs.ropensci.org/pdftools/reference/pdftools.html)
and can be more robust than whitespace-only splitting.

Use `column_count` to control how column order is handled:

- `"auto"`: infer number of columns.
- `"1"`: force single-column reading order.
- `"2"`: force left-column then right-column order.

``` r

res_coord <- keyword_search(
  file,
  keyword = c("test theory", "above-level"),
  path = TRUE,
  split_pdf = TRUE,
  split_method = "coordinates",
  column_count = "2",
  remove_hyphen = TRUE
)

head(res_coord)
#> # A tibble: 6 × 5
#>   keyword     page_num line_num line_text token_text
#>   <chr>          <int>    <int> <list>    <list>    
#> 1 test theory        1        2 <chr [1]> <list [1]>
#> 2 test theory        3       53 <chr [1]> <list [1]>
#> 3 test theory        9      247 <chr [1]> <list [1]>
#> 4 test theory       14      353 <chr [1]> <list [1]>
#> 5 test theory       17      502 <chr [1]> <list [1]>
#> 6 above-level        1       12 <chr [1]> <list [1]>
```

## Cleaning Page Artifacts and Section Headings

Several options are available to reduce non-body text before keyword
searching. This includes page headers, footers, section headings,
repeated furniture, and captions. These can be particularly helpful for
multi-column documents where such elements may be more prevalent. The
goal of removing these is to better align column text and keep the
sentence structure and keyword proximity intact.

``` r

res_clean <- keyword_search(
  file,
  keyword = "variance",
  path = TRUE,
  split_pdf = TRUE,
  split_method = "coordinates",
  column_count = "2",
  remove_section_headers = TRUE,
  remove_page_headers = TRUE,
  remove_page_footers = TRUE,
  remove_repeated_furniture = TRUE,
  repeated_edge_n = 2,
  repeated_edge_min_pages = 4,
  remove_captions = TRUE,
  caption_continuation_max = 2
)

head(res_clean)
#> # A tibble: 6 × 5
#>   keyword  page_num line_num line_text token_text
#>   <chr>       <int>    <int> <list>    <list>    
#> 1 variance        4       85 <chr [1]> <list [1]>
#> 2 variance        4       87 <chr [1]> <list [1]>
#> 3 variance        4       89 <chr [1]> <list [1]>
#> 4 variance        7      185 <chr [1]> <list [1]>
#> 5 variance       17      467 <chr [1]> <list [1]>
#> 6 variance       18      618 <chr [1]> <list [1]>
```

## Table Control in `keyword_search()`

Use `table_mode` to choose whether table-like blocks are searched:

- `"keep"`: include all text (default).
- `"remove"`: exclude table-like blocks from search.
- `"only"`: search only table-like blocks.

Additional options can improve table-only extraction:

- `table_include_headers`: include nearby table header rows (default
  `TRUE`).
- `table_header_lookback`: number of lines above detected table blocks
  to inspect for header rows (default `3`).
- `table_include_notes`: include trailing note/source rows.
- `table_note_lookahead`: number of lines after detected blocks to
  inspect for notes.
- `table_block_max_gap`: maximum number of non-table lines allowed
  before a block is split. Increase this when tables are fragmented.

When specifying `table_mode = 'remove'`, the same cleaning options above
are applied to table blocks as well, which can help ensure that only
body text is retained for keyword searching. When using
`table_mode = 'only'`, the cleaning options are not applied since the
focus is on analyzing tables specifically.

``` r

res_keep <- keyword_search(
  file,
  keyword = "0.83",
  path = TRUE,
  split_pdf = TRUE,
  split_method = "coordinates",
  table_mode = "keep",
  convert_sentence = FALSE
)

res_remove <- keyword_search(
  file,
  keyword = "0.83",
  path = TRUE,
  split_pdf = TRUE,
  split_method = "coordinates",
  table_mode = "remove",
  convert_sentence = FALSE
)

res_only <- keyword_search(
  file,
  keyword = "0.83",
  path = TRUE,
  split_pdf = TRUE,
  split_method = "coordinates",
  table_mode = "only",
  table_include_headers = TRUE,
  table_header_lookback = 3,
  table_block_max_gap = 3,
  table_include_notes = FALSE,
  table_note_lookahead = 2,
  convert_sentence = FALSE
)

c(
  keep = nrow(res_keep),
  remove = nrow(res_remove),
  only = nrow(res_only)
)
#>   keep remove   only 
#>      4      2      2
```

## Enhanced `extract_tables()`

[`extract_tables()`](https://pdfsearch.brandonlebeau.org/reference/extract_tables.md)
now supports coordinate splitting and output modes:

- `"parsed"`: list of parsed table data frames.
- `"blocks"`: metadata plus raw block lines.
- `"both"`: both parsed tables and block metadata.

It also supports table-block tuning options:

- `table_include_headers`, `table_header_lookback`
- `table_include_notes`, `table_note_lookahead`
- `table_min_numeric_tokens`, `table_min_digit_ratio`,
  `table_min_block_lines`, and `table_block_max_gap`
- `merge_across_pages` for continuation tables that span adjacent pages

``` r

tab_blocks <- extract_tables(
  file,
  path = TRUE,
  split_pdf = TRUE,
  split_method = "coordinates",
  column_count = "2",
  remove_section_headers = TRUE,
  remove_page_headers = TRUE,
  remove_page_footers = TRUE,
  remove_repeated_furniture = TRUE,
  remove_captions = TRUE,
  table_include_headers = TRUE,
  table_header_lookback = 3,
  table_block_max_gap = 3,
  table_include_notes = FALSE,
  table_note_lookahead = 2,
  merge_across_pages = TRUE,
  output = "blocks"
)

head(tab_blocks)
#> # A tibble: 3 × 6
#>   page_num block_id line_start line_end line_text  page_end
#>      <int>    <int>      <dbl>    <int> <list>        <int>
#> 1        8        1          1       10 <chr [10]>        8
#> 2        9        1          1       16 <chr [35]>       10
#> 3       14        1          1       29 <chr [29]>       14
```

``` r

tab_parsed <- extract_tables(
  file,
  path = TRUE,
  split_pdf = TRUE,
  split_method = "coordinates",
  column_count = "1",
  remove_section_headers = TRUE,
  remove_page_headers = TRUE,
  remove_page_footers = TRUE,
  remove_repeated_furniture = TRUE,
  remove_captions = TRUE,
  table_include_headers = TRUE,
  table_header_lookback = 3,
  table_block_max_gap = 3,
  table_include_notes = FALSE,
  table_note_lookahead = 3,
  merge_across_pages = TRUE,
  output = "parsed"
)

length(tab_parsed)
#> [1] 3
if (length(tab_parsed) > 0) {
  head(tab_parsed[[1]])
}
#> # A tibble: 6 × 1
#>   X1                                                                            
#>   <chr>                                                                         
#> 1 Table 1. Fit Statistics for Two-Parameter IRT Multigroup Models by Subject.   
#> 2 Subject M 2 RMSEA [CI] CFI M 2 RMSEA [CI] CFI                                 
#> 3 English 3930 (2340), p < .01 0.019 [0.018, 0.020] 0.893 2320 (740), p < .01 0…
#> 4 Math 1630 (1283), p < .01 0.012 [0.010, 0.014] 0.949 805 (405), p < .01 0.023…
#> 5 Reading 1895 (1307), p < .01 0.015 [0.014, 0.017] 0.965 902 (405), p < .01 0.…
#> 6 Science 1818 (1146), p < .01 0.018 [0.016, 0.019] 0.936 948 (350), p < .01 0.…
```

### Table-Block Tuning Reference

One primary element to test is the number of columns from the PDF. If
the tables span multiple columns, but the text is in multiple columns
you would want to ensure `column_count = 1` is specified when extracting
the tables. This will ensure the table is not truncated to only include
half of the table.

The table detector is controlled by several additional options that can
be tuned for better performance on specific documents. The key
parameters are:

- `table_min_numeric_tokens`: minimum number of numeric-looking tokens
  required for a line to be considered table-like. Larger values are
  stricter.
- `table_min_digit_ratio`: minimum proportion of digit characters in a
  line for table-like classification. Larger values reduce prose false
  positives.
- `table_min_block_lines`: minimum number of adjacent table-like lines
  needed to keep a block.
- `table_block_max_gap`: maximum number of non-table lines allowed
  between table-like lines when merging a block. Increase this when
  tables are split.
- `table_include_headers`: include nearby table headers and column-label
  rows.
- `table_header_lookback`: number of lines above a detected block to
  inspect for headers.
- `table_include_notes`: include trailing `Note.` or `Source.` rows.
- `table_note_lookahead`: number of lines after a block to inspect for
  note lines.
- `merge_across_pages`: if `TRUE`, continuation blocks across adjacent
  pages are merged when they appear to be one table.

A practical tuning workflow:

1.  If table blocks are fragmented, increase `table_block_max_gap`.
2.  If prose is incorrectly classified as table text, increase
    `table_min_numeric_tokens` and/or `table_min_digit_ratio`.
3.  If table headers are missing, keep `table_include_headers = TRUE`
    and increase `table_header_lookback`.
4.  If the same table is split across pages, set
    `merge_across_pages = TRUE`.

## Cross-Page Sentence Conversion (Optional)

If desired, sentence conversion can be done after pages are
concatenated. This has the benefit of allowing the sentence conversion
to work across pages and ensuring proper context and allowing for better
keyword proximity when sentences are split across page breaks.

``` r

res_cross_page <- keyword_search(
  file,
  keyword = "fixed effects",
  path = TRUE,
  split_pdf = TRUE,
  split_method = "coordinates",
  column_count = "2",
  remove_section_headers = TRUE,
  remove_page_headers = TRUE,
  remove_page_footers = TRUE,
  convert_sentence = TRUE,
  concatenate_pages = TRUE
)

head(res_cross_page)
#> # A tibble: 0 × 5
#> # ℹ 5 variables: keyword <chr>, page_num <int>, line_num <int>,
#> #   line_text <list>, token_text <list>
```

## Summary

For dense multi-column journal articles, a practical default is:

1.  `split_method = "coordinates"`
2.  `column_count = "2"`
3.  `remove_section_headers = TRUE`
4.  `remove_page_headers = TRUE`
5.  `remove_page_footers = TRUE`
6.  `remove_repeated_furniture = TRUE`
7.  `remove_captions = TRUE`
8.  `table_mode = "remove"` for prose-focused keyword search

Use `table_mode = "only"` or `extract_tables(..., output = "blocks")`
when the goal is specifically to analyze tables. If table headers are
being missed, set `table_include_headers = TRUE` and increase
`table_header_lookback`. If the table continues across pages, use
`merge_across_pages = TRUE`.
