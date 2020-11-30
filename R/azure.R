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
  for (i in 1:num_results) {

    num_lines = length(ocr_result$analyzeResult$readResults[[i]]$lines)
    for (j in 1:num_lines) {

      lines[counter]=trimws(gsub("\\s+", " ", ocr_result$analyzeResult$readResults[[i]]$lines[[j]]$text))
      counter <- counter +1
    }
  }
  output_dir_name <- paste0(dirname(path),"/outputs/")
  dir.create(output_dir_name,showWarnings = F)
  writeLines(lines,file(paste0(output_dir_name,basename(path),".ocr.txt")))

  return(lines)

}

fc <- memoise::cache_filesystem(".cache")


mem_do_ocr_pdf = memoise::memoise(do_ocr_pdf,cache=fc)

ocr_pdf <- function(path,params) {
  md5 <- tools::md5sum(path)
  mem_do_ocr_pdf(path,params,md5)
}

split_text <- function(x) {
  chunks <- c()
  current_chunk = c()
  chunk_counter <- 1
  print(paste("length x : ", length(x)))
  for (i in 1:length(x)) {
    #print(x[i])
    next_chunk <- append(current_chunk,x[[i]])
    #print(length(next_chunk))
    if ((length(next_chunk) > 99) ||
        (stri_length(stri_join(next_chunk,collapse="")) > 10000)) {
      #browser()
      chunks[[chunk_counter]] <- current_chunk
      next_chunk  = c()
      current_chunk = x[[i]]
      chunk_counter <- chunk_counter +1
    } else {
      current_chunk <- next_chunk
    }
  }
  if (length(current_chunk)!=0) {
      chunks[[chunk_counter]] <- current_chunk
  }
  print(paste("result chunks: " , length(chunks)))
  print(paste("#chunks ",sapply(chunks, length),collapse = " "))
  return(chunks)  
  
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
do_translate <- function(x,path,translation_target_language,params) {

  print("do translation")
  x <- remove_ws(x)
  print(paste("#lines: ", length(x)));
  text_length <- sum(stri_length(stri_flatten(x)))
  print(paste("text length: " ,text_length))

  
  chunks <- chunk_list(tokenize_sentences( stri_flatten(x))[[1]],30)

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




