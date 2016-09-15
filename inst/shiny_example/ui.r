library(shinydashboard)
library(DT)

ui <- dashboardPage(skin = "green",
      dashboardHeader(title = "pdfsearch Demo"),
        dashboardSidebar(
          sidebarMenu(
            menuItem('Search Options', tabName = 'search', 
                     icon = icon('info-circle')),
            menuItem('Results', tabName = 'results',
                     icon = icon('table'))
          )
        ),
        dashboardBody(
          tabItems(
            tabItem(tabName = 'search',
              h2('Enter pdf search parameters'),
              h3('Input basic pdf search parameters to include in pdf search'),
              fluidRow(
                box(title = 'Select pdf file to search',
                    status = 'primary', width = 4,
                  fileInput('path', 'Choose pdf file(s)',
                            multiple = TRUE,
                            accept = '.pdf'),
                  p('You may select more than one pdf file'),
                  hr(),
                  radioButtons('surround', 'Extract surrounding lines?',
                               choices = c('No' = 1, 'Yes' = 2),
                               selected = 1),
                  conditionalPanel(
                    condition = 'input.surround == 2',
                    numericInput('num_surround', 'Number of Surrounding lines to extract',
                                 val = 1, min = 1, max = NA)
                  )
                  ),
                box(title = 'Keywords',
                    status = 'primary', width = 6,
                    numericInput('num_key', 'Number of Keywords', 
                                 value = 1, min = 1, max = NA),
                    uiOutput('key_vals'),
                    hr(),
                    radioButtons('ignore_case', 'Ignore Case in Search?',
                                 choices = c('No' = FALSE, 'Yes' = TRUE, 'Vector' = 3),
                                 selected = 1),
                    conditionalPanel(
                      condition = 'input.ignore_case == 3',
                      uiOutput('vec_ignore')
                    )
                )
                )
              )
         )
        )
)
