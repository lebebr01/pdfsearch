remove_ws <- function(x) {
  sapply(x,
         FUN = function(s) { trimws(gsub("\\s+", " ", s)) },
         USE.NAMES  =F
         )
}

#' @importFrom  tokenizers tokenize_sentences
#' @importFrom stringi stri_length stri_flatten stri_join
#' @importFrom httr progress status_code upload_file GET POST headers content_type add_headers content stop_for_status
do_ocr_pdf <- function(path,params,md5) {
  print("will OCR document")
  upload <- upload_file(path)

  post_response <-  POST("https://westeurope.api.cognitive.microsoft.com/vision/v3.0/read/analyze",
                         add_headers(`Ocp-Apim-Subscription-Key` = params$api_token),
                         content_type("application/octet-stream"),
                                        #                       httr::verbose(),
                         progress("up"),
                         body=upload)
                                        #print(post_response)
  stop_for_status(post_response,paste0("OCR: ",content(post_response,as = "text")))
  response_headers <- headers(post_response)
                                        #print(response_headers)

  results_location <- response_headers$`operation-location`
  #print(results_location)

  repeat {
    results_response <-
      GET(results_location,
          add_headers(`Ocp-Apim-Subscription-Key` = params$api_token))
    stop_for_status(results_response)
    if (content(results_response)$status =="succeeded") {
      break
    }
    print("sleep 5 seconds")
    flush.console()
    Sys.sleep(5)

  }


  ocr_result <- content(results_response)

  num_results <- length(ocr_result$analyzeResult$readResults)
  lines <- c()
  counter <- 1
#  browser()
  for (i in 1:num_results) {

    num_lines = length(ocr_result$analyzeResult$readResults[[i]]$lines)
    if (num_lines > 0) {
      for (j in 1:num_lines) {
#        print(paste(i, " - ", j , " -> ", counter))
        lines[counter]=trimws(gsub("\\s+", " ", ocr_result$analyzeResult$readResults[[i]]$lines[[j]]$text))
        counter <- counter + 1
      }
    }
  }
 #  browser()
  output_dir_name <- paste0(dirname(path),"/outputs/")
  dir.create(output_dir_name,showWarnings = F)
  writeLines(lines,file(paste0(output_dir_name,basename(path),".ocr.txt")))

  return(lines)

}


cache_folder <- paste0(Sys.getenv("HOME"),"/.pdfsearchCache")
dir.create(cache_folder,showWarnings = F)
fc <- memoise::cache_filesystem(cache_folder)


mem_do_ocr_pdf = memoise::memoise(do_ocr_pdf,cache=fc)

ocr_pdf <- function(path,params) {
  md5 <- tools::md5sum(path)
  mem_do_ocr_pdf(path,params,md5)
}


 chunk_list <- function(d,n) {
 
  split(d, ceiling(seq_along(d)/n))
 }

translate_chunk <- function(x,translation_target_language,params) {
  texts_to_translate <- lapply(x, function(s) {list(text=s)})

  print("translate chunk")
  x <- remove_ws(x)
  print(paste("#lines: ", length(x)));
  body <- jsonlite::toJSON(texts_to_translate,auto_unbox = T)
  print(paste("body size: " , stri_length(body) ))
 #browser()
  
  
  result <- POST("https://api.cognitive.microsofttranslator.com/translate?api-version=3.0&to=en",
                 add_headers(`Ocp-Apim-Subscription-Key` = params$api_token,
                             `Ocp-Apim-Subscription-Region` = "westeurope"),
                 encode = "json",
                 content_type("application/json"),
                 #verbose(),
                 body = body
                 )
  stop_for_status(result,paste0("tranlate: ",content(result,as="text")))
  print(result)
  if (status_code(result)!=200) {
    print(content(result))
    stop("non 200 status code")
    
  }

  translation <- sapply(content(result),function(res) {res$translations[[1]]$text})

  return(translation)

}

calc_size <- function(sentence) {
  body <- jsonlite::toJSON(list(text=sentence),auto_unbox = T)
  return(stri_length(body)+50)
  
}

cumSumReset <- function(x, thresh = 10000) {
  ans    <- numeric()
  i      <- 0
  
  while(length(x) > 0) {
    cs_over <- cumsum(x)
    ntimes <- sum( cs_over <= thresh )
    x      <- x[-(1:ntimes)]
    ans <- c(ans, rep(i, ntimes))
    i   <- i + 1
  }
  return(ans)
}




#' @import purrr
#' @importFrom dplyr %>% pull mutate

split_into_chunks <- function(sentences) {
  lengths <- purrr::map_dbl(sentences,~ calc_size(.x))
  tib <- tibble(
    lengths = lengths,
    cumsum = cumsum(lengths))
  chunk_indices <- groups <- tib %>% 
    mutate(g = cumSumReset(lengths, 10000)) %>%     
    pull(g)
  split_sentences <-split(sentences,chunk_indices)
  split_sentences <- purrr::flatten(purrr::map(split_sentences,~ chunk_list(.x,100)))
  print(paste("chunk str-sizes: ",  paste(split_sentences %>% purrr::map_dbl(~ sum(calc_size(.x))),collapse = " ")))
  print(paste("chunk lengths: ",  paste(split_sentences %>% purrr::map_int(~ length(.x)),collapse = " ")))
  return(split_sentences)
}  


do_translate <- function(x,path,translation_target_language,params) {

  print("do translation")
  x <- remove_ws(x)
  print(paste("#lines: ", length(x)));
  text_length <- sum(stri_length(stri_flatten(x)))
  print(paste("text length: " ,text_length))

  
  chunks <- split_into_chunks(tokenize_sentences( stri_flatten(x))[[1]])

  results = list()
  for (chunk in chunks) {
    results <- append(results,translate_chunk(chunk,translation_target_language ,params))
  }
  
  results <- unlist(results)
  output_dir_name <- paste0(dirname(path),"/outputs/")
  dir.create(output_dir_name,showWarnings = F)
  writeLines(results,file(paste0(output_dir_name,
                               basename(path),".",
                               translation_target_language, ".txt")))
  return(results)
}


mem_translate <- memoise::memoise(do_translate,cache=fc)

translate <- function(x,path,translation_target_language,params) {
  mem_translate(x,path,translation_target_language,params)
}

