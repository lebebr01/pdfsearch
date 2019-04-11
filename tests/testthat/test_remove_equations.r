context("Remove Equations")

test_that('remove equations', {
  path <- system.file('pdf', '1610.00147.pdf', package = 'pdfsearch')
  no_equations <- keyword_search(path, keyword = c('1'),
                                 path = TRUE)
  with_equations <- keyword_search(path, keyword = c('1'),
                                   path = TRUE,
                                   remove_equations = FALSE)
  expect_lt(nrow(no_equations), nrow(with_equations))
})