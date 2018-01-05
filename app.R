library(shiny)
library(rhandsontable)

# Define UI for application that draws a histogram
ui <- fluidPage(
  rHandsontableOutput('table'),
  textOutput('result')
)


# Define server logic required to draw a histogram
server <- function(input,output,session)({
  values <- reactiveValues(data=as.data.frame(runif(2)))
  
  
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
}) 

# Run the application 
shinyApp(ui = ui, server = server)