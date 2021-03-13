# Applied Statistics 2014 conference workshop

# load the library for developing web apps with R (http://shiny.rstudio.com/)
library(shiny)

# define the server-side logic of the Shiny application
shinyServer(function(input, output) {
  
  data <- reactive({
    read.csv2(input$uploadedFile$datapath)
    })
  
  output$data <- renderDataTable({
    data()
    })
  
})