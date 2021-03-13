# Applied Statistics 2014 conference workshop

# define the user interface
shinyUI(
  # define type of page layout
  pageWithSidebar(
    
    # define content of page header ####
    headerPanel("Statistical distributions"),
    
    # define content of left side of the page ####
    sidebarPanel(
      numericInput(inputId = "meanValue", label = "Mean", value = 0, min = -2, max = 2, step = 0.1),
	  sliderInput(inputId = "sdValue", label = "Standard deviation", min = 0, max = 2, value = 1, step = 0.1, animate=FALSE)
       ),
    
    # define content of the main part of the page ####   
    mainPanel(
      plotOutput("plotDensity")
      )
  )
)