# Applied Statistics 2014 conference workshop

# load the library for developing web apps with R (http://shiny.rstudio.com/)
library(shiny)

# define the user interface
shinyUI(
  # define type of page layout
  pageWithSidebar(
    
    # define content of page header ####
    headerPanel("Statistical distributions"),
    
    # define content of left side of the page ####
    sidebarPanel(
      # select which distrubution will be used
      radioButtons("distribution", "Distribution type:",
                   list("Normal" = "norm",
                        "Chi-squared" = "chisq",
                        "Binomial" = "binom",
                        "t" = "t"), selected="norm"),
      
      uiOutput("chooseParameters")
      ),
    
    # define content of the main part of the page ####   
    mainPanel(
      # define the tabs on the page
      tabsetPanel(
        # define contents of 1st tab
        tabPanel(title="Plot of the distribution",
                 plotOutput("plotDensity"), 
                 tableOutput("tabularQuantiles")),
        # define contents of 2nd tab        
        tabPanel(title="Plots and probabilities for the chosen value", 
                 uiOutput("chooseValue"), 
                 plotOutput("plotValue"),
                 tableOutput("tabularValue")
                 ),
        # define contents of 3rd tab
        tabPanel("Table", tableOutput("tabularProbabilities")),
        # define contents of 4th tab
        tabPanel("Links", tableOutput("wikipediaLinks"))
        )
      )
    )
  )