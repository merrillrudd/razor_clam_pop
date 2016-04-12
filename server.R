
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyServer(function(input, output) {
  
  get_transitions <- function(input){
    
    stages <- c("L", "S", "J", "A")
    tmat <- matrix(0, nrow=length(stages), ncol=length(stages))
    rownames(tmat) <- colnames(tmat) <- stages
    
    #### ----------- from larva ---------------------------
    tmat["L","L"] <- input$LL * input$survL * input$capacity * input$quality
    
    ## larva to set = probability of transitioning * larva survival * beach capacity metric * beach quality metric
    tmat["S","L"] <- input$LS * input$survL * input$capacity * input$quality
    
    #### ----------- from set -----------------------------
    tmat["S","S"] <- input$SS * input$survS * input$quality
    
    ## set to juvenile = probability of transitioning * set survival * beach quality metric
    tmat["J","S"] <- input$SJ * input$survS * input$quality
    
    #### ----------- from juvenile ------------------------
    ## juvenile to juvenile = probability of staying juvenile * juvenile survival * beach quality metric * probability of not being harvested
    tmat["J","J"] <- input$JJ * input$survJ * input$quality * (1 - input$hrJ)
    
    ## juvenile to adult = probability of transitioning * juvenile survival * beach quality metric * probability of not being harvested
    tmat["A","J"] <- input$JA * input$survJ * input$quality * (1 - input$hrJ)
    
    #### ----------- from adult ---------------------------
    ## adult to adult = probability of staying adult * adult survival * beach quality metric * probability of not being harvested
    AA <- 1
    tmat["A","A"] <- AA * input$survA * input$quality * (1 - input$hrA)
    
    ## eggs to larva = probability of an adult reproducing * fecundity * egg survival
    tmat["L","A"] <- input$AL * input$fec * input$survE
    
    return(tmat)
    
  }
  
  get_abundances <- function(input, tmat, nyears, stochastic=FALSE, sd=0.6){
    
    ## initial population size by stage in numbers
    initial <- c("L"=input$fec*input$survE, "S"=input$fec*input$survE*input$survL, "J"=input$fec*input$survE*input$survL*input$survS, "A"=input$fec*input$survE*input$survL*input$survJ)
    
    ## population matrix
    stages <- c("L", "S", "J", "A")
    pmat <- matrix(NA, nrow=nyears, ncol=(length(stages)+1))
    colnames(pmat) <- c(stages, "lamda")
    
    ## start population at initial values in first year
    pmat[1,] <- c(initial, NA)
    
    dev <- rnorm(nyears,0,sd)
    
    ## loop over years, starting with year 2
    for(y in 2:nyears){
      
      ## larva = number of adults last year * number of surviving eggs
      if(stochastic==TRUE) pmat[y, "L"] <- (pmat[y-1, "A"] * tmat["L", "A"] + pmat[y-1, "L"] * tmat["L", "L"])*exp(dev[y])
      if(stochastic==FALSE) pmat[y, "L"] <- (pmat[y-1, "A"] * tmat["L", "A"])
      
      ## sets = number of larva last year * joint probability of being a set this year
      pmat[y, "S"] <- (pmat[y-1, "L"] * tmat["S", "L"]) + (pmat[y-1, "S"] * tmat["S", "S"])
      
      ## juveniles = (number of sets last year * joint probability of being a juv this year if you were a set last year) + (number of juveniles last year * joint probability of being a juvenile this year if you were a juv last year)
      pmat[y, "J"] <- (pmat[y-1, "S"] * tmat["J","S"]) + (pmat[y-1, "J"] * tmat["J", "J"])
      
      ## adults = (number of juveniles last year * joint probability of being an adult this year if you were a juv last year) + (number of adults last year * joint probability of being an adult this year if you were an adult last year)
      pmat[y, "A"] <-  (pmat[y-1, "J"] * tmat["A", "J"]) + (pmat[y-1, "A"] * tmat["A", "A"])
      
      ## population growth rate
      pmat[y, "lamda"] <- sum(pmat[y-1,1:length(stages)])/sum(pmat[y,1:length(stages)])
      
    }
    
    return(pmat)
  }

  output$GrowthRate <- renderPlot({
    tmat <- get_transitions(input=input)
    pmat <- get_abundances(input=input, tmat=tmat, nyears=300, stochastic=FALSE)
    plot(pmat[,"lamda"], type="l", lty=2, xlab="Year", ylab="Population growth rate", lwd=2)
    abline(h=1, col="red", lwd=3)
  })
  
  output$Stochastic <- renderPlot({
    tmat <- get_transitions(input=input)
    pmat <- get_abundances(input=input, tmat=tmat, nyears=1000, stochastic=TRUE, sd=input$sd)
    plot(pmat[701:1000,"lamda"], type="l", lty=2, xlab="Year", ylab="Population growth rate", ylim=c(0,2))
    abline(h=1, col="red", lwd=2)
  })
  

  
})