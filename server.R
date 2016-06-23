
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyServer(function(input, output) {
  ################ FUNCTIONS ###############################
    
  ## create lefkovitch matrix based on inputs
  create_lefko <- function(M_eggs, M_prerecruits, M_recruits, F, fecundity, plus_yrs, prop_spawners){
    stages <- c("P", "R")
    lefko <- matrix(0, nrow=length(stages), ncol=length(stages))
    rownames(lefko) <- colnames(lefko) <- stages
    
    tmat <- build_transitions()
    lefko["R","P"] <- tmat["R","P"] * exp(-M_prerecruits)
    lefko["P","P"] <- tmat["P","P"] * exp(-M_prerecruits)
    lefko["R","R"] <- tmat["R","R"] * exp(-M_recruits - F) * (1-exp(-M_recruits - F)^(plus_yrs - 1))/(1-(exp(-M_recruits - F)^plus_yrs))
    lefko["P","R"] <- fecundity * exp(-M_eggs) * prop_spawners
    
    return(lefko)
  }
  
  ## harvest based on inputs
  catch_fn <- function(abundance, F, M){
    catch <- abundance * F * (1 - exp(-M-F) )/(M + F)
    return(catch)
  }
    
  ## project population forward in time based on inputs
  project_fn <- function(nyears, F_t, M_eggs, M_prerecruits, M_recruits, fecundity, plus_yrs, prop_spawners){
    stages <- c("P", "R")
    pmat <- matrix(NA, nrow=nyears, ncol=4)
    colnames(pmat) <- c(stages, "lambda", "catch")
    
    lefko1 <- create_lefko(M_eggs=M_eggs, M_prerecruits=M_prerecruits, M_recruits=M_recruits, F=F_t[1], fecundity=fecundity, plus_yrs=plus_yrs, prop_spawners=prop_spawners)
    e1 <- eigen(lefko1)
    pmat[1,stages] <- abs(e1$vectors[,1])
    pmat[1,"lambda"] <- e1$value[1]
    pmat[1,"catch"] <- catch_fn(abundance=pmat[1,"R"], F=F_t[1], M=M_recruits)
    
    for(i in 2:nyears){	
      
      lefko <- create_lefko(M_eggs=M_eggs, M_prerecruits=M_prerecruits, M_recruits=M_recruits, F=F_t[i], fecundity=fecundity, plus_yrs=plus_yrs, prop_spawners=prop_spawners)
      
      pmat[i,stages] <- pmat[(i-1),stages] %*% t(lefko)
      pmat[i,"lambda"] <- sum(pmat[i,stages])/sum(pmat[(i-1),stages])
      pmat[i,"catch"] <- catch_fn(abundance=pmat[i,"R"], F=F_t[i], M=M_recruits)
      
      rm(lefko)
    }	
    
    catch <- pmat[,"catch"]	
    
    Outs <- NULL
    Outs$pmat <- pmat
    Outs$meancatch <- mean(catch)
    Outs$totalcatch <- sum(catch)
    return(Outs)
  }
    
  ## calculate relative values
  relative <- function(x){
    x2 <- x/max(x)
    return(x2)
  }
  
  ## build transition matrix - fix values, but could add them to user interface in the future
  build_transitions <- function(PP=0.05, PR=1-PP, RR=1){
    stages <- c("P", "R")
    tmat <- matrix(0, nrow=length(stages), ncol=length(stages))
    rownames(tmat) <- colnames(tmat) <- stages
    
    tmat["P","P"] <- PP
    tmat["R","P"] <- PR
    tmat["R","R"] <- RR
    if(all(colSums(tmat)!=1)) stop("Transitions must sum to 1")
    
    return(tmat)
  }
  
  build_results <- function(nyears=20, inc_waves, HABs, pollution, NIX, hypoxia, dec_habitat, M_eggs, M_prerecruits, M_recruits){
    scenarios <- c("inc_waves", "HABs", "pollution", "NIX", "hypoxia", "dec_habitat")
    choose_scenarios <- c(inc_waves, HABs, pollution, NIX, hypoxia, dec_habitat)
    
    F_med <- rep(0.7, nyears)
    
    if(inc_waves==TRUE) M_prerecruits <- M_prerecruits*1.3
    out <- project_fn(nyears=nyears, F_t=F_med, M_eggs=M_eggs, M_prerecruits=M_prerecruits, M_recruits=M_recruits)
    return(out$pmat)
  }
  
  output$CatchOverTime <- renderPlot({
    
    res <- build_results(input$inc_waves, input$HABs, input$pollution, input$NIX, input$hypoxia, input$dec_habitat, input$M_eggs, input$M_prerecruits, input$M_recruits)
    plot(res$catch, type-"l", col=1, lwd=2, ylim=c(-0.02, 0.5), xaxs="i", yaxs="i", main="Expected catch over time", xlab="Years into future", ylab="Catch")
    
  })
  

  
  
  
})