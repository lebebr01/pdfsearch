library(shiny)
library(shinydashboard)
library(pdfsearch)

server <- function(input, output, session) {
  
  output$key_vals <- renderUI({
    num_keys <- input$num_key
    lapply(1:num_keys, function(i)
      div(style = 'display:inline-block',
          textInput(paste0('key', i), label = paste0('Keyword', i), 
                    value = 'Keyword', width = '75px'))
    )
  })
  
  output$vec_ignore <- renderUI({
    num_keys <- input$num_key
    lapply(1:num_keys, function(i)
      div(style = 'display:inline-block',
          radioButtons(paste0('ig_case', i), label = paste0('Ignore Case', i), 
                    choices = c('No' = 1, 'Yes' = 2),
                    selected = 1))
    )
  })
  
}
