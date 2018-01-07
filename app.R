library(shiny)
library(rhandsontable)
library(tidyr)
library(ggplot2)

source("helpers.R")
source("helpers2.R")
# Define UI for application that draws a histogram
ui <- fluidPage(
  
  titlePanel("Day One Risk Model"),
  
  sidebarLayout(
    sidebarPanel("Variables",
                 actionButton("calculate", label = "Calculate"),
                 radioButtons('radio_buttons', 
                              label = "CMA or Historical Volatility?",
                              choices = c("CMA!!!" = "cma",
                                          "Historical" = "historical")),
                 numericInput('cash_rate', label = "Cash Rate:", value = 0),
                 img(src = "QMA_logo.jpg", height = 140, width = 400)),
    mainPanel("Graphing",
              rHandsontableOutput('table'),
              plotOutput('plot'),
              tableOutput('rbind_table')
              )
  )
)


# Define server logic required to draw a histogram
server <- function(input,output,session)({

  # The table up top
  values <- reactiveValues(data = wgt_delta/100) # datatable found in helpers2  
  
  observe({
    if(!is.null(input$table))
      values$data <- hot_to_r(input$table)
  })
  
  output$table <- renderRHandsontable({
    rhandsontable(values$data, rowHeaderWidth = 200) %>%
      hot_col(col = column_names, format = "0.0")
  })
  
  # Choose the right covariance matrix
  covmat_final <- reactive({
    if(input$radio_buttons == "cma") {
      covmat_cma 
    } else if (input$radio_buttons == "historical") {
      covmat_hist 
    }
  })
  
  # CAsh Rate
  cash_rate <- reactive({
    as.numeric(input$cash_rate)
  })
  
  ############################
  # Base Portfolio Statistics
  ############################
  # base Port Returns
  bport_return <- reactive({ 
    colSums(mapply("*",(wgt_base),returns_cma))
  })
  # base Port Vol
  bport_vol <- reactive({
    calcPortfolioVols2(covmat_final(), wgt_base)
  })
  # base Port Sharpe
  bport_sharpe <- reactive({
    (bport_return() - cash_rate()) / bport_vol()
  })
  
  
  ############################
  # Delta portfolio statistics
  ############################
  # delta Port Returns
  dport_return <- reactive({ 
    colSums(mapply("*",(values$data + wgt_base),returns_cma))
  })
  # delta Port Vol
  dport_vol <- reactive({
    calcPortfolioVols2(covmat_final(), (values$data + wgt_base))
  })
  # delta Port Sharpe
  dport_sharpe <- reactive({
    (dport_return() - cash_rate()) / dport_vol()
  })
  
  
  # plotting dataframes
  dp_data <- reactive({
    rbind(dport_return(), dport_sharpe(), dport_vol()) %>%
      as.data.frame() %>%
      mutate(measure = c("return", "sharpe", "volatility"),
             port_type = "delta") %>%
      gather(portfolio, value, -port_type, -measure)
  })
  
  bp_data <- reactive({
    rbind(bport_return(), bport_sharpe(), bport_vol()) %>%
      as.data.frame() %>%
      mutate(measure = c("return", "sharpe", "volatility"),
             port_type = "base") %>%
      gather(portfolio, value, -port_type, -measure)
  })
  

  # The plot
  output$plot <- renderPlot({
    ggplot(rbind(bp_data(), dp_data()), aes(x = portfolio, y = value, colour = port_type)) +
      geom_point() +
      facet_wrap(~measure)
  })
  
  
  # Just a test to see what's driving the plotting
  output$rbind_table <- renderTable ({
    rbind(bp_data(), dp_data()) %>%
      as.data.frame()
    
  })
  
  
  
}) 





# Run the application 
shinyApp(ui = ui, server = server)