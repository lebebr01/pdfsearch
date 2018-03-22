context("Test Errors")

test_that('error for multiple keywords ignore case', {
  path <- system.file('pdf', '1610.00147.pdf', package = 'pdfsearch')
  expect_error(keyword_search(x = path, 
                  keyword = c('measurement error', 'package', ' R '), 
                  path = TRUE,
                  ignore.case = c(TRUE, FALSE)))
})

test_that('error for surround_lines character', {
  path <- system.file('pdf', '1610.00147.pdf', package = 'pdfsearch')
  expect_error(keyword_search(x = path, 
                              keyword = c('measurement error', 'package', ' R '), 
                              path = TRUE, surround_lines = '1'))
})

test_that('error for keyword and ignore.case mismatch', {
  path <- system.file('pdf', '1610.00147.pdf', package = 'pdfsearch')
  expect_error(keyword_search(x = path, 
                              keyword = c('measurement error', 'package', ' R '),
                              ignore_case = c(TRUE, FALSE),
                              path = TRUE))
})
