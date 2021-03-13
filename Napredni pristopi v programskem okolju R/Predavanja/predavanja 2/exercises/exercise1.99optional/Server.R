# Applied Statistics 2014 conference workshop

# define the server-side logic of the Shiny application
shinyServer(function(input, output) {
  
  
  #defining a new set of reactive values, 
  
  #mean and SD get updated always
  my.mean=reactive(input$meanValue)
  my.sd=reactive(input$sdValue)
  
  
  #all the other reactive values are updated only when the goButton is pressed
  my.color=reactive({
    input$goButton 
    isolate(input$curveColor)
  })
  
  my.plotTitle=reactive({
    input$goButton 
    isolate(input$plotTitle)
  })
  
  my.verticalLabel=reactive({
    input$goButton 
    isolate(input$verticalLabel)
  })
  
  my.plotMean=reactive({
    input$goButton 
    isolate(input$plotMean)
  })
  
  
  
  #within the renderPlot function we use the reactive values  defined above
  output$plotDensity <- renderPlot({
    
    
    curve(dnorm(x, mean=my.mean(), sd=my.sd()), from=-4, to=4, col=my.color(),
          main=my.plotTitle(), ylab=my.verticalLabel())
    if(my.plotMean()==TRUE) {abline(v=my.mean())} 
  })
  
  
  output$tableDensity <- renderTable({
    x <- seq(from = -4, to = 4, by = 0.1)
    table <- cbind("Value" = x, "Probability density" = dnorm(x, mean=input$meanValue, sd=input$sdValue))
    return(table)
  }, digits=4)
  
})