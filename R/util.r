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
