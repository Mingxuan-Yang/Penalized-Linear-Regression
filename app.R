library(shiny)
library(shinyjs)
library(shinyBS)
library(shinyWidgets)
library(tidyverse)
library(ggplot2)
library(highcharter)
library(glmnet)
library(readr)

ui <- navbarPage(theme = shinythemes::shinytheme('cosmo'),
                 title = 'Penalized Linear Regression',
                 
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
                                  fluidRow(
                                    column(
                                      width = 6,
                                      offset = 3,
                                      plotOutput('hist_plot_out', height = '500px', width = '500px')
                                    )
                                  )
                                ),
                                
                                tabPanel(
                                  title = 'Density',
                                  fluidRow(
                                    column(
                                      width = 6,
                                      offset = 3,
                                      plotOutput('density_plot_out', height = '500px', width = '500px')
                                    )
                                  )
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
                                    )
                                  ),
                                  br(),
                                  fluidRow(
                                    column(
                                      width = 6,
                                      offset = 3,
                                      plotOutput('bi_plot_out', height = '500px' ,width = '500px')
                                    )
                                  )
                                  
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
                 
                 ###Regression###
                 tabPanel(title = 'Regression',
                          icon = icon('eye-open', lib = 'glyphicon'),
                          
                          ##Regression - Plot##
                          tabsetPanel(
                            id = 'tabset_eda',
                            tabPanel(title = 'Regression Plot',
                                     
                                     sidebarLayout(
                                       sidebarPanel(
                                         selectInput('type',
                                                     'Input type:',
                                                     choices = c('Formula', 'Components'),
                                                     selected = 'Formula'),
                                         conditionalPanel(
                                           condition = "input.type == 'Formula'",
                                           textInput("formula", 
                                                     "Regression Formula:",
                                                     placeholder = 'Y ~ a + b + c'),
                                           bsTooltip(id = "formula", 
                                                     title = "Do remember to delete the formula if you want to switch to components input <br> Or the result of components input will be overwritten by formula input",
                                                     placement = "bottom", 
                                                     trigger = "hover", 
                                                     options = list(placement = 'bottom'))
                                         ),
                                         conditionalPanel(condition = "input.type == 'Components'",
                                                          selectInput('response', 'Response:', 
                                                                      choices = 1),
                                                          pickerInput('predictor', 'Predictors:', 
                                                                      choices = 1, options = list(`actions-box` = TRUE),
                                                                      multiple = T),
                                                          numericInput('interaction', 'Interactions:(the level of multiway interactions)',
                                                                       min = 1, max = 10, step = 1, value = 1)
                                         ),
                                         selectInput('model', 'Model type:',
                                                     choices = c('Ridge', 'Lasso'),
                                                     selected = 'Ridge'
                                         ),
                                         sliderInput('loglambda', 'Shrinkage Parameter (lambda) Range on log scale :',
                                                     min = -10, max = 10, value = c(-5, 5)
                                         ),
                                         conditionalPanel(
                                           condition = "input.power == 'Input'",
                                           textInput('power_input', 'Input Power Level:',
                                                     placeholder = 'e.g.: 1 or 2 or 3'
                                           )
                                         ),
                                         selectInput('x_axis', 'X axis type',
                                                     choices =  c("log-lambda", "prop"),
                                                     selected = 'log-lambda'
                                         ),
                                         bsTooltip(id = "x_axis", 
                                                   title = "'log-lambda': the logarithmic form of shirnkage parameter <br>'prop' = |coefficients|/sum(|coefficients|) for Lasso<br>'prop' = coefficient^2/coefficient_OLS^2 for Ridge",
                                                   placement = "bottom", 
                                                   trigger = "hover", 
                                                   options = list(placement = 'bottom')
                                         ),
                                         div(align = 'right',actionButton('reg','Regression'))
                                       ),
                                       mainPanel(
                                         fluidRow(
                                           column(width = 8,
                                                  offset = 2,
                                                  highchartOutput('coef_plot', height = '500px', width = '700px')
                                                  )
                                         ),
                                         fluidRow(
                                           dropdownButton(
                                             tags$h3("List of Input"),
                                             selectInput('x_elli', 'x_axis', choices = 1),
                                             selectInput('y_elli', 'y_axis', choices = 1),
                                             sliderInput('lambda_elli', 'log-lambda to be used',
                                                         min = -10, max = 10, value = 0),
                                             div(align = 'right', actionButton('elli','Visualize model fitting')),
                                             circle = TRUE, status = "secondary", icon = icon("gear"), width = "200px",
                                             tooltip = tooltipOptions(title = "Click to see inputs !")
                                           )
                                         ),
                                         fluidRow(
                                           column(8,
                                                  offset = 2,
                                                  plotOutput('elli_plot', height = '500px', width = '700px')
                                                  )
                                         )
                                         
                                       )
                                     )
                            ),
                            
                            ##Regression - Summary##
                            tabPanel(title = "Regression Summary",
                                     fluidRow(
                                       column(
                                         width = 3,
                                         offset = 1,
                                         numericInput('nshow_su', 'No. of regression summaries displayed:',
                                                     min = 1, max = 300, value = 5)
                                       )
                                     ),
                                     br(),
                                     DT::dataTableOutput("df_su")
                            )
                          )
                 ),
                
                 navbarMenu('More',
                            tabPanel('Glossary',
                                     fluidRow(
                                       column(width = 9, 
                                              includeMarkdown('AppFiles/glossary.md'))
                                     )
                            ),
                            tabPanel('About',
                                     fluidRow(
                                       column(9,
                                              includeMarkdown('AppFiles/about.md'))
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
      fileInput("dataset_attempt", "Please upload a dataset in CSV format",
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
  observeEvent(input$new_data, {
    showModal(dataModal())
  })
  
  observeEvent(input$ok, {
    t <- try(read.csv(input$dataset_attempt$datapath))
    if ("try-error" %in% class(t)) {
      showModal(dataModal(failed = TRUE))
    } else {
      vals$dataset <- read.csv(input$dataset_attempt$datapath)
      updateSelectizeInput(session, 'summary_in',
                           choices = colnames(vals$dataset),
                           selected = colnames(vals$dataset)[1],server = TRUE)
      updateSelectizeInput(session, "bivariate_x",
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
                           choices = colnames(vals$dataset),
                           selected = colnames(vals$dataset)[1], server = TRUE)
      updateSelectizeInput(session, "trivariate_z",
                           label = "z-axis",
                           choices = colnames(vals$dataset),
                           selected = colnames(vals$dataset)[1], server = TRUE)
      updateSelectInput(session, 'response', 
                        choices = colnames(vals$dataset),
                        selected = colnames(vals$dataset)[1])
      updatePickerInput(session, 'predictor',
                        choices = colnames(vals$dataset),
                        selected = colnames(vals$dataset)[-1])
      updateNumericInput(session, 'interaction',
                         min = 1, max = ncol(vals$dataset))
      removeModal()
    }
  })
  
  eda_summary <- function(par) {
    if (is.numeric(par)) {
      data.frame('Min.' = round(min(par, na.rm = T), digits = 3),
                 'Mean' = round(mean(par, na.rm = T),digits = 3),
                 'Max.' = round(max(par, na.rm = T),digits = 3),
                 '1st Qu' = round(quantile(par, 0.25, na.rm = T),digits = 3),
                 'Median' = round(median(par, na.rm = T),digits = 3),
                 '3rd Qu' = round(quantile(par, 0.75, na.rm = T),digits = 3),
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
      searching = FALSE, 
      info = FALSE, 
      ordering = FALSE,
      autoWidth = TRUE
    )
    )
  })
  
  output$hist_plot_out <- renderPlot({
    stat_type = ifelse(is.numeric(vals$dataset[,input$summary_in]), 'bin', 'count')
    ggplot(data = vals$dataset) + 
      geom_histogram(aes(vals$dataset[,input$summary_in]), stat = stat_type, 
                     fill = "#C0C0C0", color = "black") + 
      labs(x = input$summary_in) +
      theme_bw(base_size = 15) + 
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank())
  })
  
  output$density_plot_out <- renderPlot({
    ggplot(data = vals$dataset, aes(x = vals$dataset[,input$summary_in])) +
      geom_density(fill = "#C0C0C0") + 
      labs(x = input$summary_in) +
      theme_bw(base_size = 15) + 
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank())
  })
  
  output$bi_plot_out <- renderPlot({
    ggplot(data = vals$dataset, aes(x = vals$dataset[,input$bivariate_x],
                                    y = vals$dataset[,input$bivariate_y])) +
      geom_point(color = "black") + 
      labs(x = input$bivariate_x, 
           y = input$bivariate_y) +
      theme_bw(base_size = 15)
  })
  
  output$tri_plot_out <- threejs::renderScatterplotThree({
    dat <- cbind(vals$dataset[,input$trivariate_x],
                 vals$dataset[,input$trivariate_y],
                 vals$dataset[,input$trivariate_z])
    threejs::scatterplot3js(dat, size = 0.2, color = "#C0C0C0",
                            flip.y = FALSE, brush = TRUE,
                            axisLabels=c(input$trivariate_x,
                                         input$trivariate_y,
                                         input$trivariate_z))
  })
  
  source('reg.R', local = TRUE)
  
  form <- function(x) {
    if (x == '') {
      res <- NULL
    } else res <- as.formula(x)
  }
  reg_result <-eventReactive(input$reg,{
    reg(df = vals$dataset, model = input$model, formula = form(input$formula),
        response = which(colnames(vals$dataset)== input$response), 
        predictors = which(colnames(vals$dataset)%in% input$predictor), 
        interactions = input$interaction,
        lambda0 = exp(seq(input$loglambda[1], 
                          input$loglambda[2], 
                          length.out = 300)))
  })
  
  observeEvent(input$reg,{
    output$coef_plot <- renderHighchart({
      plot(reg_result(), x_axis = input$x_axis, plot = F)
    })
    output$df_su <- DT::renderDataTable({
      DT::datatable(round(summary(reg_result(), nShow = input$nshow_su),3),
                    options = list(searchHighlight = TRUE))
    })
    updateSelectInput(session, 'x_elli', 
                      choices = colnames(reg_result()$X.scale),
                      selected = colnames(reg_result()$X.scale)[1])
    updateSelectInput(session, 'y_elli', 
                      choices = colnames(reg_result()$X.scale),
                      selected = colnames(reg_result()$X.scale)[2])
    updateSliderInput(session, 'lambda_elli',
                      min = min(log(reg_result()$lambda)),
                      max = max(log(reg_result()$lambda)),step = 0.2)
  })
    
  observeEvent(input$elli,{
    f = reg_result()$info(i = which(colnames(reg_result()$X.scale) == input$x_elli),
                          j = which(colnames(reg_result()$X.scale) == input$y_elli))
    output$elli_plot = renderPlot({
      plot(f(exp(input$lambda_elli)), xlabel = input$x_elli, ylabel = input$y_elli)
    })
  })
 
}


# Run the application 
portnum <- read_file("port.txt") %>% as.numeric()
shinyApp(ui = ui, server = server, options = list(port = portnum))