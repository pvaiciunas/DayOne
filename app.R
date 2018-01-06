library(shiny)
library(rhandsontable)

source("helpers.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
  rHandsontableOutput('table'),
  textOutput('result'),
  plotOutput('plot')
  radioButtons('radio_buttons', 
               label = "CMA or Historical Volatility?",
               choices = c("CMAs" = "cma",
                           "Historical" = "historical"))
)


# Define server logic required to draw a histogram
server <- function(input,output,session)({

  values <- reactiveValues(data=wgt_deltas) # datatable found in helpers2  
  
    
  weightInput <- reactive({
  
  })
  
  # Choose the right covariance matrix
  portfolio_vols <- reactive({
    cov_mat <- ifelse(input$radio_buttons == "CMAs", cma_covmat, hist_covmat)
  })
  
  
  ####
  # Do you choose the 
  
  
  observe({
    if(!is.null(input$table))
      values$data <- hot_to_r(input$table)
  })
  
  
  output$table <- renderRHandsontable({
    rhandsontable(values$data)
  })
  
  
  output$result <- renderText({ 
    sum(values$data)
  })
  
  output$plot <- renderPlot({
    new_wgts <- starting_wgts + wgt_deltas
    
    ggplot()
      
  })
}) 

# Run the application 
shinyApp(ui = ui, server = server)