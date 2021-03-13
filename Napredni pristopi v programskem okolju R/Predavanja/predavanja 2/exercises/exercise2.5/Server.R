# Applied Statistics 2014 conference workshop

# load the library for developing web apps with R (http://shiny.rstudio.com/)
library(shiny)

# define the server-side logic of the Shiny application
shinyServer(function(input, output) {
  
  data <- reactive({
    if(is.null(input$uploadedFile)) {return()}
    read.csv2(input$uploadedFile$datapath)
    })
  
  output$data <- renderDataTable({
    data()
    })
  
  output$barPlot <- renderPlot({
    barplot(height=data()[,input$selectedVariable], names.arg=data()$Company )    
  })
  
  variableNames <- reactive({
    colnames(data())
    })
  
  output$variableSelection <- renderUI({
    selectInput(inputId = "selectedVariable", label = "Select variable to display",
                choices=variableNames())
  })
  
  output$downloadData <- downloadHandler(
    filename="data.csv",
    content= function (file) {
      write.csv2(data(), file=file)
    }
    )
  
  output$downloadPlot <- downloadHandler(
    filename="plot.png",
    content= function (file) {
      png(filename=file)
      barplot(height=data()[,input$selectedVariable], names.arg=data()$Company)
      dev.off()
      
    }
  )
  
})