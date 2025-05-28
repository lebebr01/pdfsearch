context('correct cardinal page number')

test_that('correct page', {
  path <- system.file('pdf', '1610.00147.pdf', package = 'pdfsearch')

  do_not_convert_sentence <- keyword_search(x = path, keyword = 'nontrivial', path = TRUE,
                         convert_sentence = FALSE)
  do_convert_sentence <- keyword_search(x = path, keyword = 'nontrivial', path = TRUE,
                                        convert_sentence = TRUE)

  # expect_false(isTRUE(all.equal(do_convert_sentence[['page_num']],
  #                     do_not_convert_sentence[['page_num']])))
  expect_equal(do_convert_sentence[['page_num']], 6)
  
  path <- system.file('pdf', '1501.00450.pdf', package = 'pdfsearch')
  
  expect_equal(keyword_search(x = path, keyword = 'limitation', path = TRUE,
                 convert_sentence = TRUE)[['page_num']], c(2, 10))
})

test_that('correct page manual', {
  expect_equal(keyword_search(
    system.file('pdf', 'simglm.pdf', package = 'pdfsearch'), 
    keyword = 'parse', 
    path = TRUE, 
    ignore_case = TRUE
  )[['page_num']][1], 2)
})

