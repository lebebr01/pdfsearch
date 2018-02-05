context('remove hyphens')

test_that('remove hyphens', {
  path <- system.file('pdf', '1610.00147.pdf', package = 'pdfsearch')
  
  hyphen_removed <- keyword_search(x = path, keyword = 'inferences', path = TRUE)
  keep_hyphen <- keyword_search(x = path, keyword = 'inferences', path = TRUE,
                         remove_hyphen = FALSE)
  # expect_equal(tmp$page_num, tmp2$page_num)
  expect_false(isTRUE(all.equal(nrow(hyphen_removed), 
                                nrow(keep_hyphen))))
})
