
shinyUI(
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




