context('two column pdf')

test_that('spliting of columns', {
  path <- system.file('pdf', '1501.00450.pdf', package = 'pdfsearch')
  tmp <- keyword_search(x = path, keyword = 'error', path = TRUE, split = TRUE)
  tmp2 <- keyword_search(x = path, keyword = 'error', path = TRUE)
  expect_equal(tmp$page_num, tmp2$page_num)
  expect_false(isTRUE(all.equal(tmp$line_num, tmp2$line_num)))
})
