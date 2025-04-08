remove_indent <- function(x, sel_element) {
  x_tmp <- x[sel_element == 3]
  blank_first <- sapply(seq_along(x_tmp), function(tt) x_tmp[[tt]][1] == '')
  x_tmp2 <- x_tmp[blank_first]
  x_tmp2 <- lapply(seq_along(x_tmp2), function(tt) x_tmp2[[tt]][-1])
  x_tmp[blank_first] <- x_tmp2
  x[sel_element == 3] <- x_tmp
  return(x)
}

collapse_columns <- function(x_list, lengths) {
  do.call('c', 
          do.call('c',
                  lapply(1:max(lengths), function(ii)
                    lapply(seq_along(x_list), function(xx) 
                      x_list[[xx]][ii]
                    ))
          )
  )
}

remove_blank_lines <- function(text_lines) {
  text_lines[nchar(text_lines) != 0]
}

remove_hyphen <- function(text_lines) {
  
  hyphen_location <- grep("-$", text_lines)
  
  hyphen_wrap_text <- text_lines[hyphen_location + 1]
  hyphen_wrap_text <- strsplit(hyphen_wrap_text, split = ' ')
  hyphen_wrap_text <- unlist(lapply(seq_along(hyphen_wrap_text), function(xx) 
    hyphen_wrap_text[[xx]][1]))
  
  for(xx in seq_along(hyphen_location)) {
    text_lines[hyphen_location[xx]] <- gsub("-$", hyphen_wrap_text[xx], 
                                            text_lines[hyphen_location[xx]])
    text_lines[hyphen_location[xx] + 1] <- gsub(hyphen_wrap_text[xx], "", 
                                            text_lines[hyphen_location[xx] + 1],
                                            fixed = TRUE)
  }
  text_lines <- gsub("^\\s+|\\s+$", '', text_lines)
  text_lines
}

true_false <- function(x, chars) {
  x > chars
}

remove_equation <- function(text_lines) {
  
  text_lines[-grep("\\([0-9]{1,}\\)$", text_lines)]
  
}

