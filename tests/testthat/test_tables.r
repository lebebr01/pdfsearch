context("table workflows")

test_that("keyword_search table_mode remove drops table-only keywords", {
  txt <- c(
    "Prose intro line\nMath 0.63 0.42 0.35 0.46 0.67\nReading 0.82 0.65 0.72 0.74 0.81\nClosing prose line"
  )
  
  res_remove <- keyword_search(
    txt,
    keyword = "0\\.63",
    path = FALSE,
    split_pdf = FALSE,
    convert_sentence = FALSE,
    table_mode = "remove",
    token_results = FALSE
  )
  
  res_keep <- keyword_search(
    txt,
    keyword = "0\\.63",
    path = FALSE,
    split_pdf = FALSE,
    convert_sentence = FALSE,
    table_mode = "keep",
    token_results = FALSE
  )
  
  expect_equal(nrow(res_remove), 0)
  expect_gt(nrow(res_keep), 0)
})

test_that("keyword_search table_mode only keeps table rows", {
  txt <- c(
    "Prose intro line\nTable 1. Sample scores\nMath 0.63 0.42 0.35 0.46 0.67\nReading 0.82 0.65 0.72 0.74 0.81\nClosing prose line"
  )
  
  prose_in_only <- keyword_search(
    txt,
    keyword = "Prose intro line",
    path = FALSE,
    split_pdf = FALSE,
    convert_sentence = FALSE,
    table_mode = "only",
    token_results = FALSE
  )
  
  table_in_only <- keyword_search(
    txt,
    keyword = "0\\.82",
    path = FALSE,
    split_pdf = FALSE,
    convert_sentence = FALSE,
    table_mode = "only",
    token_results = FALSE
  )
  
  expect_equal(nrow(prose_in_only), 0)
  expect_gt(nrow(table_in_only), 0)
})

test_that("table_mode only includes adjacent table headers by default", {
  txt <- c(
    "Intro\nTable 2. Reliability by Grade\nGrade 4 0.83 0.79 0.79\nGrade 5 0.77 0.77 0.75\nOutro"
  )
  
  header_in_only <- keyword_search(
    txt,
    keyword = "Table 2\\. Reliability",
    path = FALSE,
    split_pdf = FALSE,
    convert_sentence = FALSE,
    table_mode = "only",
    token_results = FALSE
  )
  
  expect_gt(nrow(header_in_only), 0)
})

test_that("extract_tables output both returns parsed and blocks", {
  txt <- c(
    "Intro line\nMath   0.63   0.42   0.35\nReading   0.82   0.65   0.72\nOutro line"
  )
  
  out <- extract_tables(
    txt,
    path = FALSE,
    split_pdf = FALSE,
    output = "both"
  )
  
  expect_true(is.list(out))
  expect_true(all(c("parsed", "blocks") %in% names(out)))
  expect_true(is.list(out$parsed))
  expect_s3_class(out$blocks, "tbl_df")
})

test_that("extract_tables remove_captions strips caption-start lines", {
  txt <- c(
    "Figure 1. Distribution of scores\nMath 0.63 0.42 0.35 0.46 0.67\nReading 0.82 0.65 0.72 0.74 0.81"
  )
  
  blocks <- extract_tables(
    txt,
    path = FALSE,
    split_pdf = FALSE,
    remove_captions = TRUE,
    output = "blocks"
  )
  
  has_figure <- any(vapply(blocks$line_text, function(x) any(grepl("^Figure 1\\.", x)), logical(1)))
  expect_false(has_figure)
})

test_that("extract_tables can merge fragmented table blocks", {
  txt <- c(
    "Table 1. Example\nA  1.2  0.3\ncontinued\nB  1.4  0.4\nText paragraph"
  )
  
  blocks <- extract_tables(
    txt,
    path = FALSE,
    split_pdf = FALSE,
    output = "blocks"
  )
  
  expect_equal(nrow(blocks), 1)
  expect_true(any(grepl("^Table 1\\.", blocks$line_text[[1]])))
  expect_true(any(grepl("^B  1\\.4  0\\.4$", blocks$line_text[[1]])))
})

test_that("remove_equation keeps lines when no equation matches", {
  lines <- c("This is prose.", "Another prose line.")
  out <- remove_equation(lines)
  expect_equal(out, lines)
})
