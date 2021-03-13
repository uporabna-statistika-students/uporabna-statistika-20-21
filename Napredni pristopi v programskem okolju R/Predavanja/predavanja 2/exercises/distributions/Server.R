# Applied Statistics 2014 conference workshop

# load the library for developing web apps with R (http://shiny.rstudio.com/)
library(shiny)

# define the server-side logic of the Shiny application
shinyServer(function(input, output) {
  
  # output$chooseParameters : build the menus for parameter selection based on the chosen distribution ####
  output$chooseParameters <- renderUI({
    # return() if no distribution is selected
    if(is.null(input$distribution)) { return() }
    
    # menus for parameter selection for normal distribution
    if(input$distribution=="norm") {
      out=tagList(  
        numericInput("my.mean", "Mean:", value=0), 
        numericInput("my.sd", "Standard deviation", value=1, min=0.00001) 
        )
    }
    
    # menus for parameter selection for binomial distribution
    if(input$distribution=="binom") {
      out=tagList( 
        numericInput("n", "Number of trials:", value=10, min=1), 
        numericInput("my.p", "Probability of success:", 0.5, min=0, max=1, step=0.05) 
        )
    }
    
    # menus for parameter selection for t distribution
    if(input$distribution=="t") {
      out=tagList( 
        numericInput("df", "Degrees of freedom:", value=10, min=1)
        )
    }
    
    # menus for parameter selection for chi-square distribution
    if(input$distribution=="chisq") {
      out=tagList( 
        numericInput("df", "Degrees of freedom:", value=1, min=1)
        )
    }
    
    # return the code for the menus for parameter selection
    return(out)
    })
  
  # define number of digits to print
  my.digits=4
  
  # output$chooseValue : define the menus to use with each distribution on the tab ####
  output$chooseValue=renderUI({
    # define initial values
    if(input$distribution=="norm")  my.value.init=round(qnorm(.975, input$my.mean, input$my.sd),2)
    if(input$distribution=="t")  my.value.init=round(qt(.975, input$df),2)
    if(input$distribution=="chisq")  my.value.init=round(qchisq(.95, input$df), 2)
    if(input$distribution=="binom")  my.value.init=qbinom(.90, input$n, input$my.p)
    # define menu for entering values
    out=tagList( 
      numericInput("my.value", "Choose a value: ", value=my.value.init)
      )
    # define an additional menu for binomial distribution
    if(input$distribution=="binom")
      out=tagList( 
        numericInput("my.value", "Choose a value: ", value=my.value.init), 
        checkboxInput("my.gl", "Display P(X>=x) and P(X<x)", value=FALSE) )
    # return the code for the menus
    return(out)
  }
  )
  
  # output$wikipediaLinks : define table of links on Wikipedia ####
  output$wikipediaLinks=renderTable({
    
    WIKIPEDIA=c("http://en.wikipedia.org/wiki/Normal_distribution", "http://en.wikipedia.org/wiki/Chi-squared_distribution", "http://en.wikipedia.org/wiki/Binomial_distribution", "http://en.wikipedia.org/wiki/Student%27s_t-distribution" )
    WIKIPEDIA=paste0("<a href='",  WIKIPEDIA, c("' target='_blank'>Normal distribution</a>", "' target='_blank'>Chi-squared distribution</a>", "' target='_blank'>Binomial distribution</a>", "' target='_blank'>t distribution</a>"))
    
    data.frame(WIKIPEDIA)
  }, sanitize.text.function = function(x) x)
  
  
  # output$tabularProbabilities : define table of probabilities ####
  output$tabularProbabilities=renderTable({
    # return() if no distribution is chosen
    if(is.null(input$distribution)) { return() }
    
    # table of probabilities for normal distribution
    if(input$distribution=="norm"){
      
      x=seq(input$my.mean-4*input$my.sd, input$my.mean+4*input$my.sd, length.out=100)
      my.table=cbind(x, pnorm(x, input$my.mean, input$my.sd ), 1-pnorm(x, input$my.mean, input$my.sd)  )
      
      dimnames(my.table)[[2]]=c("x", "P(X<x)", "P(X>x)")
    }
    
    # table of probabilities for t distribution
    if(input$distribution=="t"){
      
      x=seq(-5, 5, length.out=1000)
      my.table=cbind(x, pt(x, input$df ), 1-pt(x,input$df)  )
      
      dimnames(my.table)[[2]]=c("x", "P(X<x)", "P(X>x)")
    }
    
    # table of probabilities for chi-square distribution
    if(input$distribution=="chisq"){
      
      x=seq(qchisq(1-.99, input$df),  qchisq(.99, input$df), length.out=1000)
      my.table=cbind(x, pchisq(x, input$df ), 1-pchisq(x,input$df)  )
      
      dimnames(my.table)[[2]]=c("x", "P(X<x)", "P(X>x)")
    }
    
    # table of probabilities for binomial distribution
    if(input$distribution=="binom"){
      my.table=cbind(0:input$n, dbinom(0:input$n, size=input$n, prob=input$my.p),pbinom(0:input$n, size=input$n, prob=input$my.p) )
      dimnames(my.table)[[2]]=c("k", "P(K=k)", "P(K<=k)")
    }
    
    # return the table of probabilities
    return(my.table)
    
    # pass additional parameters for table output formating to renderTable()
  }, digits=c(my.digits, my.digits, 4, 4), include.rownames=FALSE)  
  
  # output$tabularQuantiles : define table of quantiles ####
  output$tabularQuantiles=renderTable({
    # return() if no distribution is selected
    if(is.null(input$distribution)) { return() }
    
    # table of quantiles for normal distribution
    if(input$distribution=="norm") {
      my.sign=c(0.05, 0.01, 0.001)
      my.val1.q=qnorm(1-my.sign/2, input$my.mean, input$my.sd)
      my.val2.q=qnorm(my.sign/2, input$my.mean, input$my.sd)
      my.table.2=as.table(cbind(1-my.sign, my.val2.q, my.val1.q))
      dimnames(my.table.2)[[2]]=c("Proportion of observations in the interval", "Lower value", "Upper value")
    }
    
    # table of quantiles for t distribution
    if(input$distribution=="t") {
      my.sign=c(0.05, 0.01, 0.001)
      my.val1=qt(1-my.sign/2, input$df)
      my.val2=qt(my.sign/2, input$df)      
      my.table.2=as.table(cbind(1-my.sign, my.val2, my.val1))
      dimnames(my.table.2)[[2]]=c("Proportion of observations in the interval", "Lower value", "Upper value")
    }
    
    # table of quantiles for chi-sq distribution
    if(input$distribution=="chisq"){
      my.sign=c(0.05, 0.01, 0.001)      
      my.val1=qchisq(1-my.sign, input$df)
      my.val2=qchisq(0, input$df)
      my.table.2=as.table(cbind(1-my.sign, my.val2,  my.val1))
      dimnames(my.table.2)[[2]]=c("Proportion of observations in the interval", "Lower value", "Upper value")
    }
    
    # table of quantiles for binomial distribution
    if(input$distribution=="binom") {
      my.sign=c(0.05, 0.01, 0.001)
      my.val1=qbinom(1-my.sign/2, input$n, input$my.p)      
      my.val2=qbinom(my.sign/2, input$n, input$my.p)      
      my.val3=numeric(length(my.sign))
      for(i in 1:length(my.sign))
        my.val3[i]=sum(dbinom(my.val2[i]:my.val1[i], input$n, input$my.p))
      my.table.2=as.table(cbind(1-my.sign, my.val2,  my.val1, my.val3))
      dimnames(my.table.2)[[2]]=c("Approximate proportion of observations in the interval", "Lower value", "Upper value", "Exact proportion of observations in the interval")
    }
    
    # return the table of quantiles
    return(my.table.2)
  }, digits=4 )
  
  # output$barplot.d : WHAT IS THIS PLOT - IS IT EVEN PLOTTED? ####
  output$barplot.d=renderPlot({
    
    if(is.null(input$distribution))
      return()
    
    par(mfrow=c(1,2))
    
    my.dbinom.res=dbinom(0:input$n, size=input$n, prob=input$my.p)
    
    barplot(my.dbinom.res, ylab="", xlab="", main="")
    grid()
    
    my.col=rep("gray", input$n+1)
    
    barplot(dbinom(0:input$n, size=input$n, prob=input$my.p), ylab="Probability", xlab="Outcome", 
            main=paste("Binomial distribution \n Bin (", input$n, ", ", input$my.p, ")", sep="" ), 
            add=TRUE, names=c(0:input$n),
            col=my.col, cex.main=1, cex.lab=1.5, cex.axis=2       
    )
    
    barplot(pbinom(0:input$n, size=input$n, prob=input$my.p), ylab="", xlab="", main="", cex.main=2, cex.lab=2, cex.axis=2)
    grid()
    
    my.col=rep("gray", input$n+1)
    
    barplot(pbinom(0:input$n, size=input$n, prob=input$my.p), ylab="Probability", xlab="Outcome", 
            main=paste("Cumulative \n binomial distribution \n Bin (", input$n, ", ", input$my.p, ")", sep=""   ), 
            add=TRUE, names=c(0:input$n),
            col=my.col,  cex.main=1, cex.lab=1.5, cex.axis=2
    )
    
  })# end my.table.value
  
  
  # output$tabularValue : define table of tail probabilities ####
  output$tabularValue=renderTable({
    # return() if no distribution is selected
    if(is.null(input$distribution)) { return() }
    
    # table for normal distribution
    if(input$distribution=="norm") {
      
      my.val1=pnorm(input$my.value, input$my.mean, input$my.sd)
      my.val1.ok=ifelse(my.val1<0.0001, "<0.0001", round(my.val1,4))    
      if(my.val1>0.9999) my.val1.ok=">0.9999"
      
      my.val2=1-pnorm(input$my.value, input$my.mean, input$my.sd)
      my.val2.ok=ifelse(my.val2<0.0001, "<0.0001", round(my.val2,4))    
      if(my.val2>0.9999) my.val2.ok=">0.9999"
      
      my.table.2=as.table(c(input$my.value, my.val1, my.val2))
      dimnames(my.table.2)[[1]]=c("Value", paste("P(X<", input$my.value, ")", sep=""),  paste("P(X>", input$my.value, ")", sep=""))
    }
    
    # table for t distribution
    if(input$distribution=="t") {
      
      my.val1=pt(input$my.value, input$df)
      my.val1.ok=ifelse(my.val1<0.0001, "<0.0001", round(my.val1,4))    
      if(my.val1>0.9999) my.val1.ok=">0.9999"
      
      my.val2=1-pt(input$my.value, input$df)
      my.val2.ok=ifelse(my.val2<0.0001, "<0.0001", round(my.val2,4))    
      if(my.val2>0.9999) my.val2.ok=">0.9999"
      
      my.table.2=as.table(c(input$my.value, my.val1, my.val2))
      dimnames(my.table.2)[[1]]=c("Value", paste("P(X<", input$my.value, ")", sep=""),  paste("P(X>", input$my.value, ")", sep=""))      
    }
    
    # table for chi-square distribution
    if(input$distribution=="chisq") {
      
      my.val1=pchisq(input$my.value, input$df)
      my.val1.ok=ifelse(my.val1<0.0001, "<0.0001", round(my.val1,4))    
      if(my.val1>0.9999) my.val1.ok=">0.9999"
      
      my.val2=1-pchisq(input$my.value, input$df)
      my.val2.ok=ifelse(my.val2<0.0001, "<0.0001", round(my.val2,4))    
      if(my.val2>0.9999) my.val2.ok=">0.9999"
      
      my.table.2=as.table(c(input$my.value, my.val1, my.val2))
      dimnames(my.table.2)[[1]]=c("Value", paste("P(X<", input$my.value, ")", sep=""),  paste("P(X>", input$my.value, ")", sep=""))
    }
    
    # table for binomial distribution
    if(input$distribution=="binom") {
      
      my.val1=pbinom(input$my.value, input$n, input$my.p)
      my.val1.ok=ifelse(my.val1<0.0001, "<0.0001", round(my.val1,4))    
      if(my.val1>0.9999) my.val1.ok=">0.9999"
      
      my.val2=1-pbinom(input$my.value, input$n, input$my.p)
      my.val2.ok=ifelse(my.val2<0.0001, "<0.0001", round(my.val2,4))    
      if(my.val2>0.9999) my.val2.ok=">0.9999"
      
      #probability of the selected value
      my.val3=dbinom(input$my.value, input$n, input$my.p)
      
      my.table.2=as.table(c(input$my.value, my.val1, my.val2, my.val3))
      dimnames(my.table.2)[[1]]=c("Value", paste("P(K<=", input$my.value, ")", sep=""),  paste("P(K>", input$my.value, ")", sep=""), paste("P(K=", input$my.value, ")", sep=""))
    }
    
    # return the table of tail probabilities
    return(my.table.2)
    
  }, digits=4 )
  
  # output$plotDensity : define the distribution density plots ####
  output$plotDensity=renderPlot({
    # return() of no distribution is selected
    if(is.null(input$distribution)) { return() }
    
    # plots for the binomial distribution
    if(input$distribution=="binom") {
      par(mfrow=c(1,2))
      
      #save computational time
      my.dbinom.res=dbinom(0:input$n, size=input$n, prob=input$my.p)
      
      #added to limit the xlim of the plots with too large n
      if(input$n>50) which.pos=which(my.dbinom.res>1e-20) else which.pos=c(0:input$n)+1
      
      barplot(my.dbinom.res[which.pos], ylab="", xlab="", main="", cex.main=2, cex.lab=1.5, cex.axis=2)
      
      grid()
      
      my.col=rep("gray", input$n+1)
      
      barplot(my.dbinom.res[which.pos], ylab="Probability", xlab="Outcome",
              main=paste("Binomial distribution \n Bin (", input$n, ", ", input$my.p, ")", sep="" ), 
              add=TRUE, names=c(0:input$n)[which.pos],
              col=my.col, cex.main=1.5, cex.lab=1.5, cex.axis=2)
      
      
      ################## cumulative distribution
      barplot(pbinom(0:input$n, size=input$n, prob=input$my.p), ylab="", xlab="", main="",cex.main=2, cex.lab=1.5, cex.axis=2)
      grid()
      
      my.col=rep("gray", input$n+1)
      
      barplot(pbinom(0:input$n, size=input$n, prob=input$my.p), ylab="Probability", xlab="Outcome", 
              main=paste("Cumulative \n binomial distribution \n Bin (", input$n, ", ", input$my.p, ")", sep="" ), 
              add=TRUE, names=c(0:input$n),
              col=my.col, cex.main=1.5, cex.lab=1.5, cex.axis=2)
      
    }# if distr=binom
    
    # plots for the normal distribution
    if(input$distribution=="norm") {
      
      x=seq(input$my.mean-4*input$my.sd, input$my.mean+4*input$my.sd, length.out=1000)
      
      plot(x, dnorm(x, input$my.mean, input$my.sd), type="l", main=paste("Normal density : N(" , round(input$my.mean,2), "," , round(input$my.sd,2), ")",  sep=""), , ylab="Density", xlab="Value",
           cex.main=2, cex.lab=1.5, cex.axis=2, axes=FALSE)
      
      axis(1, at=c(input$my.mean-3*input$my.sd, input$my.mean-2*input$my.sd, input$my.mean-1*input$my.sd, input$my.mean, input$my.mean+input$my.sd, 
                   input$my.mean+2*input$my.sd, input$my.mean+3*input$my.sd), cex.axis=2)
      axis(2, cex.axis=2)
      
      box()
      
    }# end if distr = normal
    
    # plots for the t distribution
    if(input$distribution=="t") {
      
      x=seq(-5, 5, length.out=1000)
      plot(x, dt(x, input$df), type="l", main=paste("t density : t(" , input$df, ")",  sep=""), , ylab="Density", xlab="Value", 
           cex.main=2, cex.lab=1.5, cex.axis=2)   
      
    }# end if distr = normal
    
    # plots for the chi-square distribution
    if(input$distribution=="chisq") {
      
      x=seq(qchisq(1-.999, input$df), qchisq(.99, input$df), length.out=1000)
      if(input$df==1) x=seq(qchisq(1-.6, input$df), qchisq(.99, input$df), length.out=1000)
      plot(x, dchisq(x, input$df), type="l", main=paste("chi-square density : chi(" , input$df, ")",  sep=""), , ylab="Density", xlab="Value",
           cex.main=2, cex.lab=1.5, cex.axis=2)   
      
    }# end if distr = chisq
    
  })#end reactive barplot
  
  
  # output$plotValue : define distribution density where the focus is given to a specific value ####
  output$plotValue=renderPlot({
    # return() if no distribution is selected
    if(is.null(input$distribution)) { return() }
    
    # plots for the binomial distribution
    if(input$distribution=="binom") {
      
      par(mfrow=c(1,2))
      
      if(input$my.gl==FALSE) {
        
        barplot(dbinom(0:input$n, size=input$n, prob=input$my.p), ylab="", xlab="", main="", cex.main=2, cex.lab=1.5, cex.axis=2)
        grid()
        
        my.col=rep("gray", input$n+1)
        my.col[which(c(0:input$n)>input$my.value)]="red"
        
        barplot(dbinom(0:input$n, size=input$n, prob=input$my.p), ylab="Probability", xlab="Outcome", 
                main=paste("Binomial distribution \n Bin (", input$n, ", ", input$my.p, ")", sep="" ), 
                names=c(0:input$n),
                col=my.col, cex.main=2, cex.lab=1.5, cex.axis=2, add=TRUE)
        
        my.p0=1-pbinom(input$my.value, input$n, input$my.p)
        my.p=ifelse(my.p0<0.0001, "<0.0001", paste("=", round(my.p0,4) )) 
        if(my.p0>0.9999) my.p=">0.9999"
        
        my.adj=0
        
        legend("topright", legend=paste("P(X>", input$my.value, ") ", "\n", my.p, sep=""), fill="red", bty="n") 
        
        barplot(dbinom(0:input$n, size=input$n, prob=input$my.p), ylab="", xlab="", main="", cex.main=2, cex.lab=1.5, cex.axis=2)
        grid()
        
        my.col=rep("gray", input$n+1)
        
        my.col[which(c(0:input$n)<=input$my.value)]="blue"
        
        barplot(dbinom(0:input$n, size=input$n, prob=input$my.p), ylab="Probability", xlab="Outcome", 
                main=paste("Binomial distribution \n Bin (", input$n, ", ", input$my.p, ")", sep="" ), 
                names=c(0:input$n),
                col=my.col, cex.main=2, cex.lab=1.5, cex.axis=2, add=TRUE)
        
        my.p0=pbinom(input$my.value, input$n, input$my.p)
        my.p=ifelse(my.p0<0.0001, "<0.0001", paste("=", round(my.p0,4) )) 
        if(my.p0>0.9999) my.p=">0.9999"
        
        
        legend("topleft", legend=paste("P(X<=", input$my.value, ") ", "\n", my.p, sep=""), fill="blue", bty="n")
      }
      
      if(input$my.gl==TRUE) {
        
        barplot(dbinom(0:input$n, size=input$n, prob=input$my.p), ylab="", xlab="", main="", cex.main=2, cex.lab=1.5, cex.axis=2)
        grid()
        
        my.col=rep("gray", input$n+1)
        my.col[which(c(0:input$n)>=input$my.value)]="red"
        
        barplot(dbinom(0:input$n, size=input$n, prob=input$my.p), ylab="Probability", xlab="Outcome", 
                main=paste("Binomial distribution \n Bin (", input$n, ", ", input$my.p, ")", sep="" ), 
                names=c(0:input$n),
                col=my.col, cex.main=2, cex.lab=1.5, cex.axis=2, add=TRUE)
        
        my.p0=1-pbinom(input$my.value-1, input$n, input$my.p)
        my.p=ifelse(my.p0<0.0001, "<0.0001", paste("=", round(my.p0,4) )) 
        if(my.p0>0.9999) my.p=">0.9999"
        
        my.adj=0
        
        legend("topright", legend=paste("P(X>=", input$my.value, ") ", "\n", my.p, sep=""), fill="red", bty="n") 
        
        barplot(dbinom(0:input$n, size=input$n, prob=input$my.p), ylab="", xlab="", main="", cex.main=2, cex.lab=1.5, cex.axis=2)
        grid()
        
        my.col=rep("gray", input$n+1)
        
        my.col[which(c(0:input$n)<input$my.value)]="blue"
        
        barplot(dbinom(0:input$n, size=input$n, prob=input$my.p), ylab="Probability", xlab="Outcome", 
                main=paste("Binomial distribution \n Bin (", input$n, ", ", input$my.p, ")", sep="" ), 
                names=c(0:input$n),
                col=my.col, cex.main=2, cex.lab=1.5, cex.axis=2, add=TRUE)
        
        my.p0=pbinom(input$my.value-1, input$n, input$my.p)
        my.p=ifelse(my.p0<0.0001, "<0.0001", paste("=", round(my.p0,4) )) 
        if(my.p0>0.9999) my.p=">0.9999"
        
        legend("topleft", legend=paste("P(X<", input$my.value, ") ", "\n", my.p, sep=""), fill="blue", bty="n")
      }
      
    }# if distr=binom
    
    # plots for the normal distribution
    if(input$distribution=="norm") {
      
      par(mfrow=c(1,2))
      
      my.max=max(input$my.mean+4*input$my.sd, abs(input$my.value)+ input$my.sd )
      #centering around the mean
      x=seq(2*input$my.mean-my.max, my.max, length.out=10000)
      plot(x, dnorm(x, input$my.mean, input$my.sd), type="l", 
           main=paste("Normal density : N(" , round(input$my.mean,2), "," , round(input$my.sd,2), ")",  sep=""), ylab="Density", xlab="Value", 
           cex.main=2, cex.lab=1.5, cex.axis=2, axes=FALSE)
      
      axis(1, at=c(input$my.mean-3*input$my.sd, input$my.mean-2*input$my.sd, input$my.mean-1*input$my.sd, input$my.mean, input$my.mean+input$my.sd, 
                   input$my.mean+2*input$my.sd, input$my.mean+3*input$my.sd, input$my.value), cex.axis=1.5)
      axis(2, cex.axis=1.5)
      
      box()
      
      segments(  x[x>input$my.value],  rep(0, length(x[x>input$my.value])),   x[x>input$my.value], dnorm(x[x>input$my.value], input$my.mean, input$my.sd), col="red")
      my.p0=1-pnorm(input$my.value, input$my.mean, input$my.sd)
      my.p=ifelse(my.p0<0.0001, "<0.0001", paste("=", round(my.p0,4) )) 
      if(my.p0>0.9999) my.p=">0.9999"
      
      #left-align for values below the mean, right align for values above the mean
      my.adj=ifelse(input$my.value<input$my.mean, 0, 1)
      
      legend("topright", legend=paste("P(X>", input$my.value, ") ", "\n", my.p, sep=""), fill="red", bty="n")
      
      plot(x, dnorm(x, input$my.mean, input$my.sd), type="l", main=paste("Normal density : N(" , round(input$my.mean,2), "," , round(input$my.sd,2), ")",  sep=""), ylab="Density", xlab="Value",
           cex.main=2, cex.lab=1.5, cex.axis=2, axes=FALSE
      )   
      segments(  x[x<input$my.value],  rep(0, length(x[x<input$my.value])),   x[x<input$my.value], dnorm(x[x<input$my.value], input$my.mean, input$my.sd), col="blue", ylab="Density")
      
      axis(1, at=c(input$my.mean-3*input$my.sd, input$my.mean-2*input$my.sd, input$my.mean-1*input$my.sd, input$my.mean, input$my.mean+input$my.sd, 
                   input$my.mean+2*input$my.sd, input$my.mean+3*input$my.sd, input$my.value), cex.axis=2)
      axis(2, cex.axis=2)
      
      my.p0=pnorm(input$my.value, input$my.mean, input$my.sd)
      my.p=ifelse(my.p0<0.0001, "<0.0001", paste("=", round(my.p0,4) )) 
      if(my.p0>0.9999) my.p=">0.9999"
      
      legend("topleft", legend=paste("P(X<", input$my.value, ") ", "\n", my.p, sep=""), fill="blue", bty="n")
      
    }# end if distr = normal
    
    # plots for the t distribution
    if(input$distribution=="t") {
      
      par(mfrow=c(1,3))
      
      #left-align for values below the mean, right align for values above the mean
      my.adj=ifelse(input$my.value<0, 0, 1)
      
      my.max=max(5, abs(input$my.value)+ 0.5 )
      #centering around the mean
      x=seq(-my.max, my.max, length.out=10000)
      plot(x, dt(x, input$df), type="l",main=paste("t density : t(" , input$df, ")",  sep=""),  ylab="Density", xlab="Value",
           cex.main=2, cex.lab=1.5, cex.axis=2)   
      
      segments(  x[x>input$my.value],  rep(0, length(x[x>input$my.value])),   x[x>input$my.value], dt(x[x>input$my.value], input$df), col="red")
      
      my.p0=1-pt(input$my.value, input$df)
      my.p=ifelse(my.p0<0.0001, "<0.0001", paste("=", round(my.p0,4) )) 
      if(my.p0>0.9999) my.p=">0.9999"
      
      
      legend("topright", legend=paste("P(X>", input$my.value, ") ", "\n", my.p, sep=""), fill="red", bty="n")
      
      plot(x, dt(x, input$df), type="l", main=paste("t density : t(" , input$df, ")",  sep=""), ylab="Density", xlab="Value", 
           cex.main=2, cex.lab=1.5, cex.axis=2)   
      segments(  x[x<input$my.value],  rep(0, length(x[x<input$my.value])),   x[x<input$my.value], dt(x[x<input$my.value], input$df), col="blue", ylab="Density",
                 cex.main=2, cex.lab=2, cex.axis=2)
      
      my.p0=pt(input$my.value, input$df)
      my.p=ifelse(my.p0<0.0001, "<0.0001", paste("=", round(my.p0,4) )) 
      if(my.p0>0.9999) my.p=">0.9999"
      
      legend("topleft", legend=paste("P(X<", input$my.value, ") ", "\n", my.p, sep=""), fill="blue", bty="n") 
      
      ############# two-tailed
      
      plot(x, dt(x, input$df), type="l",main=paste("t density : t(" , input$df, ")",  sep=""),  ylab="Density", xlab="Value",
           cex.main=2, cex.lab=1.5, cex.axis=2)   
      
      segments(  x[x>abs(input$my.value)],  rep(0, length(x[x>abs(input$my.value)])),   x[x>abs(input$my.value)], dt(x[x>abs(input$my.value)], input$df), col="red")
      
      my.p0=1-pt(abs(input$my.value), input$df)
      my.p=ifelse(my.p0<0.0001, "<0.0001", paste("=", round(my.p0,4) )) 
      if(my.p0>0.9999) my.p=">0.9999"
      
      #two-tailed probability
      my.p2=ifelse(my.p0*2<0.0001, "<0.0001", paste("=", round(my.p0*2,4) )) 
      if(my.p0*2>0.9999) my.p2=">0.9999"
      
      segments(  x[x<(-abs(input$my.value))],  rep(0, length(x[x<(-abs(input$my.value))])),   x[x<(-abs(input$my.value))], dt(x[x<(-abs(input$my.value))], input$df),
                 col="red")
      
      legend("topleft", legend=paste("P(X<-", abs(input$my.value), " or X>", abs(input$my.value), ") ", "\n", my.p2, sep=""), fill="red", bty="n")   
      
    }# end if distr = t
    
    # plots for the chi-square distribution
    if(input$distribution=="chisq") {
      
      par(mfrow=c(1,2))
      
      my.max=max(qchisq(.999, input$df), input$my.value+ 0.5 )
      #centering around the mean
      
      x=seq(qchisq(1- .999, input$df), my.max, length.out=10000)    
      if(input$df==1)     x=seq(min(input$my.value, qchisq(1- .6, input$df)), my.max, length.out=10000)    
      
      #left-align for values below the mean, right align for values above the mean
      my.adj=ifelse(input$my.value<median(x), 0, 1)
      
      plot(x, dchisq(x, input$df), type="l",main=paste("chi-squared density: \n chi(" , input$df, ")",  sep=""),  ylab="Density", xlab="Value",
           cex.main=1.5, cex.lab=1.5, cex.axis=2)   
      
      segments(  x[x>input$my.value],  rep(0, length(x[x>input$my.value])),   x[x>input$my.value], dchisq(x[x>input$my.value], input$df), col="red")
      
      my.p0=1-pchisq(input$my.value, input$df)
      my.p=ifelse(my.p0<0.0001, "<0.0001", paste("=", round(my.p0,4) )) 
      if(my.p0>0.9999) my.p=">0.9999"
      
      legend("topright", legend=paste("P(X>", input$my.value, ") ", "\n", my.p, sep=""), fill="red", bty="n")
      
      plot(x, dchisq(x, input$df), type="l", main=paste("chi-squared density: \n chi(" , input$df, ")",  sep=""), ylab="Density", xlab="Value", 
           cex.main=1.5, cex.lab=1.5, cex.axis=2)   
      segments(  x[x<input$my.value],  rep(0, length(x[x<input$my.value])),   x[x<input$my.value], dchisq(x[x<input$my.value], input$df), col="blue", ylab="Density",
                 cex.main=2, cex.lab=2, cex.axis=2)
      
      my.p0=pchisq(input$my.value, input$df)
      my.p=ifelse(my.p0<0.0001, "<0.0001", paste("=", round(my.p0,4) )) 
      if(my.p0>0.9999) my.p=">0.9999"
      
      legend("topleft", legend=paste("P(X<", input$my.value, ") ", "\n", my.p, sep=""), fill="blue", bty="n") 
      
    }# end if distr = chisq
    
  })#end reactive barplot
})