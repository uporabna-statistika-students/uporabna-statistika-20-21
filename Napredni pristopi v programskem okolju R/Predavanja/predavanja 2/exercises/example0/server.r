library(shiny)

shinyServer(function(input, output) {

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

})
