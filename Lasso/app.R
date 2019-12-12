library(shiny)
library(shinyjs)
library(tidyverse)
library(ggplot2)
library(glmnet)



ui <- navbarPage(theme = shinythemes::shinytheme('cosmo'),
                 title = 'HHHHH',
                 
                 ###EDA###
                 tabPanel(title = 'EDA',
                          icon = icon('stats', lib = 'glyphicon'),
                          tabsetPanel(
                              id = 'tabset_eda',
                              
                              ##EDA - Parameter Summary##
                              tabPanel(
                                  title = 'Parameters Summary',
                                  
                                  fluidRow(
                                      column(
                                          width = 3,
                                          selectizeInput(
                                              inputId = 'summary_in',
                                              label = h4('Select parameter'),
                                              choices = 1,
                                              multiple = FALSE
                                          )
                                      ),
                                      column(
                                          width = 7, 
                                          offset = 1, 
                                          DT::dataTableOutput('numeric_out')
                                      )
                                  ),
                                  
                                  navlistPanel(
                                      well = FALSE,
                                      widths = c(3, 9),
                                      
                                      tabPanel(
                                          title = 'Histogram',
                                          plotOutput('hist_plot_out', height = '250px')
                                      ),
                                      
                                      tabPanel(
                                          title = 'Density',
                                          plotOutput('density_plot_out', height = '250px')
                                      )
                                  )
                              ),
                              
                              ##EDA - Scatter plots##
                              tabPanel(
                                  title = 'Scatter plots',
                                  
                                  navlistPanel(
                                      well = FALSE,
                                      widths = c(3, 9),
                                      
                                      tabPanel(
                                          title = 'Bivariate',
                                          fluidRow(
                                              column(
                                                  width = 3,
                                                  selectizeInput(
                                                      "bivariate_x",
                                                      label = "x-axis",
                                                      choices = 1,
                                                      multiple = FALSE
                                                  )
                                              ),
                                              column(
                                                  width = 3,
                                                  selectizeInput(
                                                      "bivariate_y",
                                                      label = "y-axis",
                                                      choices = 1,
                                                      multiple = FALSE
                                                  )
                                              ),
                                          ),
                                          br(),
                                          plotOutput('bi_plot_out', height = '350px')
                                      ),
                                      
                                      tabPanel(
                                          title = 'Trivariate',
                                          fluidRow(
                                              column(
                                                  width = 3,
                                                  selectizeInput(
                                                      "trivariat_x",
                                                      label = "x-axis",
                                                      choices = 1,
                                                      multiple = FALSE
                                                  )
                                              ),
                                              column(
                                                  width = 3,
                                                  selectizeInput(
                                                      "trivariate_y",
                                                      label = "y-axis",
                                                      choices = 1,
                                                      multiple = FALSE
                                                  )
                                              ),
                                              column(
                                                  width = 3,
                                                  selectizeInput(
                                                      "trivariate_z",
                                                      label = "z-axis",
                                                      choices = 1,
                                                      multiple = FALSE
                                                  )
                                              )
                                          ),
                                          br(),
                                          threejs::scatterplotThreeOutput("trivariate_plot_out", height = "400px"),
                                          helpText(
                                              style = "font-size: 12px;",
                                              "Use your mouse and trackpad to rotate the plot and zoom in or out."
                                          )
                                      ),
                                      
                                      tabPanel(
                                          title = 'Multivariate',
                                          
                                          sidebarLayout(
                                              sidebarPanel(
                                                  checkboxGroupInput(inputId = 'mul_in', label = 'Parameters to plot:',
                                                                     choices = 1,
                                                  div(align = 'right',actionButton('mul_button','Plot'))
                                                  )
                                              ),
                                              mainPanel(
                                                  plotOutput('mul_plot_out', height = '400px')
                                              )
                                          )
                                      )
                                  )
                              ),
                              
                              ##EDA - Parameters Transformation##
                              tabPanel(
                                  title = 'Parameters Transformation',
                                  sidebarLayout(
                                      sidebarPanel(
                                          selectizeInput(inputId = 'trans_in', 
                                                         label = 'Parameters (include response) to transform:', 
                                                         choices = 1),
                                          div(align = 'right',actionButton('trans_button','Transform'))
                                      ),
                                      mainPanel(
                                          DT::dataTableOutput('trans_result')
                                      )
                                  )
                              )
                          )
                          
                 ),
                 
                 ###Visualization###
                 tabPanel(title = 'Visualization',
                          icon = icon('eye-open', lib = 'glyphicon')
                          
                 ),
                 
                 ###Diagnostic###     keep it or not?
                 tabPanel(title = 'Diagnostic',
                          icon = icon('medkit')
                 ),
                 
                 navbarMenu('More',
                            tabPanel('Glossary',
                            ),
                            tabPanel('About',
                                     fluidRow(
                                         column(9,
                                                htmlOutput('intro'))
                                     )
                            ),
                            tabPanel("dataset management",
                                     basicPage(
                                         actionButton("new_data", "Upload a new dataset"))
                            )
                 ) # end navbar menu
)



server <- function(input, output, session) {
    
    vals <- reactiveValues()
    
    dataModal <- function(failed = FALSE) {
        modalDialog(
            fileInput("dataset_attempt", "Choose a dataset in CSV format",
                      multiple = FALSE,
                      accept = c('text/csv',
                                 'text/comma-separated-values,text/plain',
                                 '.csv')),
            span('Please tidy your dataset to a data frame object in R'),
            if (failed) div(tags$b('Invalid dataset/path', style = 'color: red;')),
            footer = tagList(actionButton('ok', 'OK'))
        )
    }
    showModal(dataModal())
    
    
    
    observeEvent(input$ok, {
        t <- try(read.csv(input$dataset_attempt$datapath))
        if ("try-error" %in% class(t)) {
            showModal(dataModal(failed = TRUE))
        } else {
            vals$dataset <- read.csv(input$dataset_attempt$datapath)
            removeModal()
        }
    })
    
    dataset <- reactive({vals$dataset})
    
    updateSelectizeInput(session, inputId = 'summary_in',
                         label = h4('Select parameter'),
                         choices = names(dataset),
                         selected = names(dataset)[1],server = TRUE)
    updateSelectizeInput(session,  "bivariate_x",
                         label = "x-axis",
                         choices = names(dataset),
                         selected = names(dataset)[1], server = TRUE)
    updateSelectizeInput(session, "bivariate_y",
                         label = "y-axis",
                         choices = names(dataset),
                         selected = names(dataset)[1], server = TRUE)
    updateSelectizeInput(session, "trivariat_x",
                         label = "x-axis",
                         choices = names(dataset),
                         selected = names(dataset)[1], server = TRUE)
    updateSelectizeInput(session, "trivariate_y",
                         label = "y-axis",
                         choices = names(dataset), server = TRUE)
    updateSelectizeInput(session, "trivariate_z",
                         label = "z-axis",
                         choices = names(dataset), server = TRUE)
    updateCheckboxGroupInput(session, inputId = 'mul_in', 
                             label = 'Parameters to plot:',
                             choices = names(dataset), 
                             selected = names(dataset)[1:2])
    updateSelectizeInput(session, inputId = 'trans_in', 
                         label = 'Parameters (include response) to transform:', 
                         choices = names(dataset), server = TRUE)
    
    
}


# Run the application 
shinyApp(ui = ui, server = server)