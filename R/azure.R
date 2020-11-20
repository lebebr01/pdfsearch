#' @importFrom httr upload_file GET POST headers content_type add_headers content
do_ocr_pdf <- function(path,params,md5) {
  upload <- upload_file(path)

  post_response <-  POST("https://westeurope.api.cognitive.microsoft.com/vision/v3.0/read/analyze",
                         add_headers(`Ocp-Apim-Subscription-Key` = params$api_token),
                         content_type("application/octet-stream"),
                                        #                       httr::verbose(),
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
    Sys.sleep(5)

  }


  ocr_result <- content(results_response)


  num_lines = length(ocr_result$analyzeResult$readResults[[1]]$lines)

  lines = c()

  for (i in 1:num_lines) {
    lines[i]=ocr_result$analyzeResult$readResults[[1]]$lines[[i]]$text
  }

  return(lines)

}

fc <- memoise::cache_filesystem(".cache")


mem_do_ocr_pdf = memoise::memoise(do_ocr_pdf,cache=fc)

ocr_pdf <- function(path,params) {
  md5 <- tools::md5sum(path)
  mem_do_ocr_pdf(path,params,md5)
}

do_translate <- function(x,translation_target_language,params) {


  print("do translation")

  texts_to_translate=lapply(x,function(s) {list(text=s)})

  result <- POST("https://api.cognitive.microsofttranslator.com/translate?api-version=3.0&to=en",
                 add_headers(`Ocp-Apim-Subscription-Key` = params$api_token,
                             `Ocp-Apim-Subscription-Region` = "westeurope"),
                 encode = "json",
                 content_type("application/json"),
                                        #verbose(),
                 body = jsonlite::toJSON(texts_to_translate,auto_unbox = T)
                 )

  translation <- sapply(content(result),function(res) {res$translations[[1]]$text})
  return(translation)
}


mem_translate <- memoise::memoise(do_translate,cache=fc)

translate <- function(x,translation_target_language,params) {
  mem_translate(x,translation_target_language,params)
}
