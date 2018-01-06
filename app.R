library(shiny)
library(rhandsontable)
library(tidyr)
library(ggplot2)

source("helpers.R")
source("helpers2.R")
# Define UI for application that draws a histogram
ui <- fluidPage(
  rHandsontableOutput('table'),
  textOutput('result'),
  tableOutput('covmat'),
  plotOutput('plot'),
  tableOutput('rbind_table'),
  
  radioButtons('radio_buttons', 
               label = "CMA or Historical Volatility?",
               choices = c("CMA!!!" = "cma",
                           "Historical" = "historical")),
  numericInput('cash_rate', label = "Cash Rate:", value = 0)
  
)


# Define server logic required to draw a histogram
server <- function(input,output,session)({

  # The table up top
  values <- reactiveValues(data = wgt_delta) # datatable found in helpers2  
  
  observe({
    if(!is.null(input$table))
      values$data <- hot_to_r(input$table)
  })
  
  output$table <- renderRHandsontable({
    rhandsontable(values$data)
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
  
  # Port Returns
  port_return <- reactive({ 
    as.double(colSums(mapply("*",(values$data + wgt_base),returns_cma)))
  })
  
  # Port Vol
  port_vol <- reactive({
    calcPortfolioVols2(covmat_final(), (values$data + wgt_base))
  })
  
  # Port Sharpe
  port_sharpe <- reactive({
    as.double((port_return() - cash_rate()) / port_vol())
  })
  
 
  output$covmat <- renderTable ({
    covmat_final()
  })
  
  output$rbind_table <- renderTable ({
    p_data <- rbind(as.double(port_return()), as.double(port_sharpe()), as.double(port_vol())) %>%
      as.data.frame() %>%
      mutate(measure = c("return", "sharpe", "volatility")) %>%
      gather(key = measure)
    
    colnames(p_data) <- c("measure","portfolio","value")
    
    p_data
  })

  output$plot <- renderPlot({
    p_data <- rbind(as.double(port_return()), as.double(port_sharpe()), as.double(port_vol())) %>%
    as.data.frame() %>%
    mutate(measure = c("return", "sharpe", "volatility")) %>%
    gather(key = measure)

    colnames(p_data) <- c("measure","portfolio","value")
  
    ggplot(p_data, aes(x = portfolio, y = value)) +
      geom_line() +
      facet_wrap(~measure)
  })
}) 

# Run the application 
shinyApp(ui = ui, server = server)