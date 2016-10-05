library(shiny)
library(shinydashboard)
library(pdfsearch)

server <- function(input, output, session) {
  
  output$key_vals <- renderUI({
    num_keys <- input$num_key
    lapply(1:num_keys, function(i)
      div(style = 'display:inline-block',
          textInput(paste0('key', i), label = paste0('Keyword', i), 
                    value = '', width = '75px'))
    )
  })
  
  output$vec_ignore <- renderUI({
    num_keys <- input$num_key
    lapply(1:num_keys, function(i)
      div(style = 'display:inline-block',
          radioButtons(paste0('ig_case', i), label = paste0('Ignore Case', i), 
                    choices = c('No' = FALSE, 'Yes' = TRUE),
                    selected = 1))
    )
  })
  
  keyword_result <- eventReactive(input$run_search, {
    num_files <- nrow(input$path)
    num_keys <- input$num_key
    keywords <- do.call('c', lapply(1:num_keys, function(xx) 
      eval(parse(text = paste0('input$key', xx)))))
    if(input$surround == 1) {
      srd <- FALSE
    } else {
      srd <- input$num_surround
    }
    if(input$ignore_case == 3) {
      ign_cs <- do.call('c', lapply(1:input$num_key, function(xx) 
        eval(parse(text = paste0('ig_case', xx)))))
    } else {
      ign_cs <- input$ignore_case
    }
    do.call('rbind', lapply(seq_along(num_files), function(xx) 
      keyword_search(input$path[[xx, 'datapath']], 
                     keyword = keywords,
                     path = TRUE,
                     surround_lines = srd,
                     ignore.case = ign_cs))
    )
  })
  
  output$search_results <- renderDataTable({
    keyword_result()
  })
  
  keyword_flatten <- reactive({
    text_lines <- data.frame(do.call('rbind', keyword_result()[['line_text']]))
    if(input$surround == 1) {
      srd <- 0
    } else {
      srd <- input$num_surround
    }
    names(text_lines) <- paste0('line_text_', 1:ncol(text_lines))
    key_flat <- keyword_result()[c('keyword', 'page_num', 'line_num')]
    cbind(key_flat, text_lines)
  })
  
  output$down_results <- downloadHandler(
    filename = function() { 
      paste0('results', '.csv')
    },
    content = function(file) {
      write.csv(keyword_flatten(), file)
    }
  )
  
}
