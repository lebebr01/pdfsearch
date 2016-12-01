context("Test Structure")

test_that("correct structure", {
  path <- system.file('pdf', '1610.00147.pdf', package = 'pdfsearch')
  expect_true(is.tibble(keyword_search(x = path, keyword = 'measurement error', 
                                       path = TRUE)))
})

test_that('surround_lines returns multiple lines', {
  path <- system.file('pdf', '1610.00147.pdf', package = 'pdfsearch')
  expect_true(is.list(keyword_search(x = path, keyword = 'measurement', 
                                     path = TRUE,
                            ignore_case = TRUE, surround_lines = 1)$line_text))
})

test_that('directory search', {
  path <- system.file('pdf', '1610.00147.pdf', package = 'pdfsearch')
  path <- gsub('/1610.00147.pdf', '', path)
  expect_equal(length(table(keyword_directory(directory = path, 
                        keyword = 'error', full_names = TRUE)$ID)), 2)
})

test_that('directory search max_search', {
  path <- system.file('pdf', '1610.00147.pdf', package = 'pdfsearch')
  path <- gsub('/1610.00147.pdf', '', path)
  expect_equal(length(table(keyword_directory(directory = path, 
                      keyword = 'error', full_names = TRUE,
                    max_search = 1)$ID)), 1)
})

test_that("heading search", {
  file <- system.file('pdf', '1501.00450.pdf', package = 'pdfsearch')
  
  expect_true(is.tibble(heading_search(file, 
                                  headings = c('abstract', 'introduction'),
                 path = TRUE)))
})

test_that("heading search within keyword search", {
  file <- system.file('pdf', '1501.00450.pdf', package = 'pdfsearch')
  head_args <- list(x = file, 
                    headings = c('INTRODUCTION', 'Motivation', 'RESULTS'),
                    path = TRUE)
  
  key_res <- keyword_search(file, 
                            keyword = c('repeated measures', 'mixed effects'),
                            path = TRUE, heading_search = TRUE, 
                            heading_args = head_args)
  expect_true('heading' %in% names(key_res))
})
