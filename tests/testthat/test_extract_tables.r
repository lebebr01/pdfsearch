context("extract tables")

test_that("extract_tables supports parsed and block outputs", {
  txt <- c(
    "Intro line\nMath   0.63   0.42   0.35\nReading   0.82   0.65   0.72\nOutro line"
  )
  
  parsed <- extract_tables(
    txt,
    path = FALSE,
    split_pdf = FALSE,
    output = "parsed"
  )
  
  blocks <- extract_tables(
    txt,
    path = FALSE,
    split_pdf = FALSE,
    output = "blocks"
  )
  
  both <- extract_tables(
    txt,
    path = FALSE,
    split_pdf = FALSE,
    output = "both"
  )
  
  expect_true(is.list(parsed))
  expect_gt(length(parsed), 0)
  expect_s3_class(blocks, "tbl_df")
  expect_gt(nrow(blocks), 0)
  expect_true(is.list(both))
  expect_true(all(c("parsed", "blocks") %in% names(both)))
})

test_that("extract_tables coordinate mode works on package sample pdf", {
  path <- system.file("pdf", "1501.00450.pdf", package = "pdfsearch")
  
  blocks <- extract_tables(
    path,
    path = TRUE,
    split_pdf = TRUE,
    split_method = "coordinates",
    column_count = "2",
    output = "blocks"
  )
  
  expect_s3_class(blocks, "tbl_df")
})

test_that("extract_tables can merge continuation blocks across pages", {
  txt <- c(
    "Table 1. Example\nA  1.0  2.0\nB  1.1  2.1",
    "C  1.2  2.2\nD  1.3  2.3\nText"
  )
  
  no_merge <- extract_tables(
    txt,
    path = FALSE,
    split_pdf = FALSE,
    output = "blocks",
    merge_across_pages = FALSE
  )
  
  merged <- extract_tables(
    txt,
    path = FALSE,
    split_pdf = FALSE,
    output = "blocks",
    merge_across_pages = TRUE
  )
  
  expect_gt(nrow(no_merge), nrow(merged))
  expect_equal(nrow(merged), 1)
  expect_true(any(grepl("^Table 1\\.", merged$line_text[[1]])))
  expect_true(any(grepl("^D  1\\.3  2\\.3$", merged$line_text[[1]])))
})
