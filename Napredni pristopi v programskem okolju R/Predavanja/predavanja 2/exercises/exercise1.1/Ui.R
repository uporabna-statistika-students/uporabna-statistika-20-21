# Applied Statistics 2014 conference workshop

# define the user interface
shinyUI(
  # define type of page layout
  pageWithSidebar(

    # define content of page header ####
    headerPanel("Statistical distributions"),

    # define content of left side of the page ####
    sidebarPanel(
        sliderInput("asdf", "testni slider, ki ne naredi nič za deploy test",
                             min=0, max=100, value=50)),

    # define content of the main part of the page ####
    mainPanel(
      plotOutput("plotDensity")
      )
  )
)
