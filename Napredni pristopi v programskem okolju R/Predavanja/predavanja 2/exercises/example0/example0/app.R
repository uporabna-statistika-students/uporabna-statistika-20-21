#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that selects a distribution, some additional parameters, and draws a histogram
ui <- shinyUI(
  #pageWithSidebar(
  fluidPage(
    #title of the App
    headerPanel("My first app"),
    
    #what appears on the side - reads the inputs
    sidebarPanel(
      
      radioButtons("distribution", "Distribution type:",
                   list("Normal" = "norm",
                        "Uniform" = "unif")),
      
      sliderInput("obs", "Number of observations",
                  min=0, max=100, value=50)
      
    ),
    
    #what appears in the main panel, displays the outputs
    mainPanel(plotOutput("hist1"),
              textOutput("text1")
    )
  ))





# Define server logic required to draw a histogram
server <- function(input, output) {
   
  #simulate data
  my.x=reactive({
    if(input$distribution=="norm") {x=rnorm(input$obs)}
    if(input$distribution=="unif") x=runif(input$obs)
    x
  })
  
  
  #calculate the average and save it in a reactive object
  my.av.x=reactive(round(mean(my.x()),2))
  
  #save the outputs
  #histogram
  output$hist1=renderPlot(hist(my.x(), ylab="Frequency", xlab="Data", main="Histogram" ))
  #string  
  output$text1=renderText(paste("The mean value is: ", 
                                my.av.x()))
  
}	





# Run the application 
shinyApp(ui = ui, server = server)

