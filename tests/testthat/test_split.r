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
