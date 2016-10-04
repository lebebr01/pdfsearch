context("Test Structure")

test_that("correct structure", {
  path <- system.file('pdf', '1610.00147.pdf', package = 'pdfsearch')
  expect_true(is.tibble(keyword_search(x = path, keyword = 'measurement error', path = TRUE)))
})
