context("Test token conversion")

test_that('error for multiple keywords ignore case', {
  path <- system.file('pdf', '1610.00147.pdf', package = 'pdfsearch')
  expect_output(str(convert_tokens(x = path, 
                              path = TRUE)), "List of 31"
  )
  
  page_one_words <- length(convert_tokens(x = path, path = TRUE)[[1]][[1]])
  
  expect_lt(length(convert_tokens(x = path, path = TRUE, token_function = tokenizers::tokenize_lines)),
            page_one_words)
  
})
