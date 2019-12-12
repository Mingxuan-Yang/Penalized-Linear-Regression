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
                                          plotOutput('bi_plot_out', height = '250px')
                                      ),
                                      
                                      tabPanel(
                                          title = 'Trivariate',
                                          fluidRow(
                                              column(
                                                  width = 3,
                                                  selectizeInput(
                                                      "trivariate_x",
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
                                          threejs::scatterplotThreeOutput("tri_plot_out", height = "400px"),
                                          helpText(
                                              style = "font-size: 12px;",
                                              "Use your mouse and trackpad to rotate the plot and zoom in or out."
                                          )
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
            updateSelectizeInput(session, inputId = 'summary_in',
                               
                                choices = colnames(vals$dataset),
                                selected = colnames(vals$dataset)[1],server = TRUE)
            updateSelectizeInput(session,  "bivariate_x",
                                 label = "x-axis",
                                 choices = colnames(vals$dataset),
                                 selected = colnames(vals$dataset)[1], server = TRUE)
            updateSelectizeInput(session, "bivariate_y",
                                 label = "y-axis",
                                 choices = colnames(vals$dataset),
                                 selected = colnames(vals$dataset)[1], server = TRUE)
            updateSelectizeInput(session, "trivariate_x",
                                 label = "x-axis",
                                 choices = colnames(vals$dataset),
                                 selected = colnames(vals$dataset)[1], server = TRUE)
            updateSelectizeInput(session, "trivariate_y",
                                 label = "y-axis",
                                 choices = colnames(vals$dataset), server = TRUE)
            updateSelectizeInput(session, "trivariate_z",
                                 label = "z-axis",
                                 choices = colnames(vals$dataset), server = TRUE)
            removeModal()
        }
    })
    
    
    
    eda_summary <- function(par) {
        if (is.numeric(par)) {
            data.frame('Min.' = min(par, na.rm = T),
                       'Mean' = mean(par, na.rm = T),
                       'Max.' = max(par, na.rm = T),
                       '1st Qu' = quantile(par, 0.25, na.rm = T),
                       'Median' = median(par, na.rm = T),
                       '3rd Qu' = quantile(par, 0.75, na.rm = T),
                       check.names = F)
        } else if (is.factor(par)) {
            data.frame('Level' = names(summary(par)),
                       'Freq' = summary(par))
        } else {
            data.frame('Sorry' = 'No summary for this type of variable')
        }
        
    }
    
    
    output$numeric_out <- DT::renderDataTable({
        DT::datatable({
            eda_summary(vals$dataset[,input$summary_in])
        },
        rownames = FALSE,
        options = list(
            paging = FALSE, 
            searching = FALSE, 
            info = FALSE, 
            ordering = FALSE,
            autoWidth = TRUE
            )
        )
    })
    
    output$hist_plot_out <- renderPlot({
        ggplot(data = vals$dataset) + 
            geom_histogram(aes(vals$dataset[,input$summary_in]),
                           fill = 'deepskyblue', color = 'purple') + 
            labs(x = input$summary_in)
    })
    
    output$density_plot_out <- renderPlot({
        ggplot(data = vals$dataset, aes(x = vals$dataset[,input$summary_in])) +
            geom_density(fill = 'deepskyblue') + 
            labs(x = input$summary_in)
    })
    
    output$bi_plot_out <- renderPlot({
        ggplot(data = vals$dataset, aes(x = vals$dataset[,input$bivariate_x],
                                        y = vals$dataset[,input$bivariate_y])) +
            geom_point(color = 'deepskyblue', alpha = 0.3) + 
            labs(x = input$bivariate_x, y = input$bivariate_y)
    })
    
    
    output$tri_plot_out <- threejs::renderScatterplotThree({
        dat <- cbind(vals$dataset[,input$trivariate_x],
                     vals$dataset[,input$trivariate_y],
                     vals$dataset[,input$trivariate_z])
        threejs::scatterplot3js(dat, size = 0.5, color = rainbow(dim(dat)[1]),
                                axisLabels=c(input$trivariate_x,
                                             input$trivariate_y,
                                             input$trivariate_z))
    })
  
}


# Run the application 
shinyApp(ui = ui, server = server)