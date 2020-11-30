#' @importFrom  tokenizers tokenize_sentences
#' @importFrom stringi stri_length stri_flatten stri_join
#' @importFrom httr progress status_code upload_file GET POST headers content_type add_headers content
do_ocr_pdf <- function(path,params,md5) {
  upload <- upload_file(path)

  post_response <-  POST("https://westeurope.api.cognitive.microsoft.com/vision/v3.0/read/analyze",
                         add_headers(`Ocp-Apim-Subscription-Key` = params$api_token),
                         content_type("application/octet-stream"),
                                        #                       httr::verbose(),
                         progress("up"),
                         body=upload)
  #print(post_response)
  response_headers <- headers(post_response)
                                        #print(response_headers)

  results_location <- response_headers$`operation-location`
  #print(results_location)

  repeat {
    results_response <-
      GET(results_location,
          add_headers(`Ocp-Apim-Subscription-Key` = params$api_token))
    if (content(results_response)$status =="succeeded") {
      break
    }
    print("sleep 5 seconds")
    flush.console()
    Sys.sleep(5)

  }


  ocr_result <- content(results_response)


  num_lines = length(ocr_result$analyzeResult$readResults[[1]]$lines)

  lines = c()

  for (i in 1:num_lines) {
    lines[i]=ocr_result$analyzeResult$readResults[[1]]$lines[[i]]$text
  }

  dir.create("outputs",showWarnings = F)
  writeLines(lines,file(paste0("outputs/",basename(path),".")))
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

split_string_max_10000 <- function(s) {
  if (length(s) > 1) {
    stop("length > 1")
  }
  result = list()
  current_string = ""
  chunk_counter <- 1
  words <- stringi::stri_split_fixed(s," ")
  for (word in words[[1]]) {
    if (stri_length(paste(current_string, word)) > 8000) {
      
      result[chunk_counter] <- current_string
      current_string <- ""    
      chunk_counter <- chunk_counter + 1
    }
    current_string <- paste(current_string,word)
  }
  result[chunk_counter] <- current_string
  return(result)
}

translate_chunk <- function(x,translation_target_language,params) {
  texts_to_translate <- lapply(x, function(s) {list(text=s)})
  
 #browser()
  
  
  result <- POST("https://api.cognitive.microsofttranslator.com/translate?api-version=3.0&to=en",
                 add_headers(`Ocp-Apim-Subscription-Key` = params$api_token,
                             `Ocp-Apim-Subscription-Region` = "westeurope"),
                 encode = "json",
                 content_type("application/json"),
                 #verbose(),
                 body = jsonlite::toJSON(texts_to_translate,auto_unbox = T)
  )
  print(result)
  if (status_code(result)!=200) {
    print(content(result))
    stop("non 200 status code")
    
  }
  translation <- sapply(content(result),function(res) {res$translations[[1]]$text})
  return(translation)
  
  
}
do_translate <- function(x,translation_target_language,params) {


 print("do translation")
  print(paste("#lines: ", length(x)));
  text_length <- sum(stri_length(stri_flatten(x)))
  print(paste("text length: " ,text_length))

  
  chunks <- tokenize_sentences( stri_flatten(x))
  

  results = list()
  for (chunk in chunks) {
    results <- append(results,translate_chunk(chunk,translation_target_language ,params))
  }
  
  # if (text_length>10000) {
  #   print("NOT translated , text too long")
  #   warning("more then 10000 character , not traslated")
  #   return(x)
  # }
  # print(paste("# texts to translate: ",length(texts_to_translate)))
  # if (length(texts_to_translate) > 100) {
  #   print("NOT translated , to may texts")
  #   warning("more then 100 texts , not tranlated")
  #   return(x)
  # }

}


mem_translate <- memoise::memoise(do_translate,cache=fc)

translate <- function(x,translation_target_language,params) {
  mem_translate(x,translation_target_language,params)
}




