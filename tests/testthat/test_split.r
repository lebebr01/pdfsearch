context('two column pdf')

test_that('spliting of columns', {
  path <- system.file('pdf', '1501.00450.pdf', package = 'pdfsearch')
  
  tmp <- keyword_search(x = path, keyword = 'error', 
                        path = TRUE, split_pdf = TRUE,
                        remove_hyphen = FALSE)
  tmp2 <- keyword_search(x = path, keyword = 'error', 
                         path = TRUE, remove_hyphen = FALSE)
  
  expect_false(isTRUE(all.equal(tmp$line_num, tmp2$line_num)))
})

test_that('coordinate split method returns matches', {
  path <- system.file('pdf', '1501.00450.pdf', package = 'pdfsearch')
  
  tmp <- keyword_search(x = path, keyword = 'error',
                        path = TRUE, split_pdf = TRUE,
                        split_method = 'coordinates',
                        remove_hyphen = FALSE)
  
  expect_s3_class(tmp, "data.frame")
  expect_gt(nrow(tmp), 0)
})

test_that('coordinate split supports explicit column_count', {
  path <- system.file('pdf', '1501.00450.pdf', package = 'pdfsearch')
  
  one_col <- keyword_search(x = path, keyword = 'error',
                            path = TRUE, split_pdf = TRUE,
                            split_method = 'coordinates',
                            column_count = '1',
                            remove_hyphen = FALSE)
  
  two_col <- keyword_search(x = path, keyword = 'error',
                            path = TRUE, split_pdf = TRUE,
                            split_method = 'coordinates',
                            column_count = '2',
                            remove_hyphen = FALSE)
  
  expect_s3_class(one_col, "data.frame")
  expect_s3_class(two_col, "data.frame")
  expect_gt(nrow(one_col), 0)
  expect_gt(nrow(two_col), 0)
})

test_that('coordinate mask and compare helper run', {
  path <- system.file('pdf', '1501.00450.pdf', package = 'pdfsearch')
  
  masked <- keyword_search(x = path, keyword = 'error',
                           path = TRUE, split_pdf = TRUE,
                           split_method = 'coordinates',
                           column_count = '2',
                           mask_nonprose = TRUE,
                           remove_hyphen = FALSE)
  
  cmp <- compare_mask_effect(path, keyword = 'error',
                             path = TRUE, column_count = '2',
                             remove_hyphen = FALSE)
  
  expect_s3_class(masked, "data.frame")
  expect_s3_class(cmp, "tbl_df")
  expect_equal(nrow(cmp), 2)
})

test_that('coordinate split builds lines within columns first', {
  path <- system.file('pdf', '1501.00450.pdf', package = 'pdfsearch')
  txt <- pdftools::pdf_text(path)
  dat <- pdftools::pdf_data(path)
  
  out <- format_text(
    txt,
    split_pdf = TRUE,
    split_method = "coordinates",
    pdf_data = dat,
    column_count = "2",
    convert_sentence = FALSE,
    remove_hyphen = FALSE
  )
  
  expect_false(any(grepl("ABSTRACT 1. INTRODUCTION", out[[1]], fixed = TRUE)))
})

test_that('coordinate split can remove section headers and page headers', {
  path <- system.file('pdf', '1501.00450.pdf', package = 'pdfsearch')
  txt <- pdftools::pdf_text(path)
  dat <- pdftools::pdf_data(path)
  
  out <- format_text(
    txt,
    split_pdf = TRUE,
    split_method = "coordinates",
    pdf_data = dat,
    column_count = "2",
    convert_sentence = FALSE,
    remove_hyphen = FALSE,
    remove_section_headers = TRUE,
    remove_page_headers = TRUE
  )
  
  expect_false(any(grepl("^ABSTRACT$", out[[1]])))
  expect_false(any(grepl("arXiv:1501.00450v1", out[[1]], fixed = TRUE)))
})

test_that('concatenate_pages can reconstruct cross-page sentences', {
  txt <- c("This sentence starts on page one and",
           "ends on page two.")
  
  out <- format_text(
    txt,
    split_pdf = FALSE,
    convert_sentence = TRUE,
    concatenate_pages = TRUE,
    remove_hyphen = FALSE
  )
  
  expect_true(any(grepl("This sentence starts on page one and ends on page two\\.", out[[1]])))
})

test_that('keyword_search concatenate_pages preserves page mapping', {
  path <- system.file('pdf', '1501.00450.pdf', package = 'pdfsearch')
  
  out <- keyword_search(
    path,
    keyword = "fixed effects in the model in this section",
    path = TRUE,
    split_pdf = TRUE,
    split_method = "coordinates",
    column_count = "2",
    remove_section_headers = TRUE,
    remove_page_headers = TRUE,
    remove_page_footers = TRUE,
    convert_sentence = TRUE,
    concatenate_pages = TRUE,
    remove_hyphen = TRUE,
    token_results = FALSE
  )
  
  expect_gt(nrow(out), 0)
  expect_true(any(out$page_num == 2))
})

test_that('remove_repeated_furniture removes repeated edge lines', {
  txt <- c(
    "Running Header\nBody line one",
    "Running Header\nBody line two",
    "Running Header\nBody line three",
    "Running Header\nBody line four"
  )
  
  out <- format_text(
    txt,
    split_pdf = FALSE,
    convert_sentence = FALSE,
    remove_repeated_furniture = TRUE,
    repeated_edge_n = 1,
    repeated_edge_min_pages = 4
  )
  
  expect_false(any(vapply(out, function(x) any(grepl("Running Header", x, fixed = TRUE)), logical(1))))
})

test_that('remove_section_headers removes canonical section headings', {
  txt <- c("Results\nThis is a finding.")
  
  out <- format_text(
    txt,
    split_pdf = TRUE,
    split_method = "regex",
    convert_sentence = FALSE,
    remove_section_headers = TRUE
  )
  
  expect_false(any(grepl("^Results$", out[[1]])))
})

test_that('remove_captions removes figure and table caption lines', {
  txt <- c(
    "Body paragraph line\nFigure 1. Distribution of scores\nNote. Example caption note.\nNext body paragraph",
    "Table 2. Summary statistics\nSource. Simulated data\nAnother body line"
  )
  
  out <- format_text(
    txt,
    split_pdf = FALSE,
    convert_sentence = FALSE,
    remove_captions = TRUE,
    caption_continuation_max = 2
  )
  
  has_figure <- any(vapply(out, function(x) any(grepl("^Figure 1\\.", x)), logical(1)))
  has_table <- any(vapply(out, function(x) any(grepl("^Table 2\\.", x)), logical(1)))
  has_note <- any(vapply(out, function(x) any(grepl("^Note\\.", x)), logical(1)))
  
  expect_false(has_figure)
  expect_false(has_table)
  expect_false(has_note)
})

test_that('table_mode can keep only tables or remove tables', {
  txt <- c(
    "Prose intro line\nMath 0.63 0.42 0.35 0.46 0.67\nReading 0.82 0.65 0.72 0.74 0.81\nClosing prose line"
  )
  
  only_tables <- format_text(
    txt,
    split_pdf = FALSE,
    convert_sentence = FALSE,
    table_mode = "only"
  )
  
  no_tables <- format_text(
    txt,
    split_pdf = FALSE,
    convert_sentence = FALSE,
    table_mode = "remove"
  )
  
  expect_true(any(grepl("^Math 0.63", only_tables[[1]])))
  expect_false(any(grepl("^Prose intro line$", only_tables[[1]])))
  expect_false(any(grepl("^Math 0.63", no_tables[[1]])))
  expect_true(any(grepl("^Prose intro line$", no_tables[[1]])))
})
