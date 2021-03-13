# Applied Statistics 2014 conference workshop

# define the user interface
shinyUI(
  # define type of page layout
  pageWithSidebar(
    
    # define content of page header ####
    headerPanel("Statistical distributions"),
    
    # define content of left side of the page ####
    sidebarPanel(
      img(src="logo_ibmi.png", height = 80, width = 80),
      numericInput(inputId = "meanValue", label = "Mean", value = 0, min = -2, max = 2, step = 0.1),
      hr(),
      sliderInput(inputId = "sdValue",
                  label = "Standard deviation",
                  min = 0, max = 2, value = 1, step = 0.1),
      hr(),
      checkboxInput(inputId = "plotMean", label = "Plot vertical line at the mean?", value = FALSE),
      hr(),
      selectInput(inputId = "curveColor",
                  label = "Select curve color",
                  choices = c("Black"="black",
                              "Red"="red",
                              "Blue"="Blue"),
                  selected="black"),
      hr(),
      textInput(inputId = "plotTitle", label = "Plot title", value = "A plot"),
      hr(),
      textInput(inputId = "verticalLabel", label = "Vertical axis label", value = "probability density"),
      hr(),
      hr(),
      #### adds an action button that will be used to update the outputs
      actionButton(inputId="goButton", "Update the results", icon = NULL)
      ),
    
    # define content of the main part of the page ####   
    mainPanel(
      tabsetPanel(
        tabPanel(title="Plot",      
                 plotOutput("plotDensity")
        ),
        tabPanel(title="Table",
      tableOutput("tableDensity")
        )
      )
      )
  )
)