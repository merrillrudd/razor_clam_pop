
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(png)

shinyServer(function(input, output) {

################ FUNCTIONS ###############################
##### --------- Transition probabilities ------------  

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

  tmat <- build_transitions()

##### --------- Lefkovitch matrix ------------  

## create lefkovitch matrix based on inputs
create_lefko <- function(S_eggs, S_prerecruits, S_recruits, u, fecundity, plus_yrs, prop_spawners, dd, abund, capacity){
  stages <- c("P", "R")
  lefko <- matrix(0, nrow=length(stages), ncol=length(stages))
  rownames(lefko) <- colnames(lefko) <- stages
  
  tmat <- build_transitions()
  
  lefko["R","P"] <- tmat["R","P"] * S_prerecruits
  lefko["P","P"] <- tmat["P","P"] * S_prerecruits
  lefko["R","R"] <- tmat["R","R"] * ((S_recruits*(1-u))^(plus_yrs-1))/(1-S_recruits*(1-u)) * exp(dd*abund/capacity)
  lefko["P","R"] <- prop_spawners * fecundity * S_eggs
  
  return(lefko)
}

  # lefko1 <- create_lefko(capacity=capacity,dd=0,abund=0,S_eggs=0.0000556, S_prerecruits=0.11, S_recruits=0.75, u=0.3, fecundity=300000, plus_yrs=3, prop_spawners=0.5)
  # eigen(lefko1)
  # 
  # lefko0 <- create_lefko(capacity=capacity,dd=0,abund=0,S_eggs=0.0000556, S_prerecruits=0.11, S_recruits=0.75, u=0, fecundity=300000, plus_yrs=3, prop_spawners=0.5)
  # eigen(lefko0)

##### --------- Project model ------------  

  ## project population forward in time based on inputs
  project_fn <- function(nyears, u_t, S_eggs, S_prerecruits, S_recruits, fecundity, plus_yrs, prop_spawners, dd, capacity){
    stages <- c("P", "R")
    nyears <- 20
    pmat <- matrix(NA, nrow=nyears, ncol=5)
    colnames(pmat) <- c(stages, "lambda", "catch", "exploit")
    
    lefko0 <- create_lefko(capacity=capacity,dd=dd,abund=1,S_eggs=S_eggs[1], S_prerecruits=S_prerecruits[1], S_recruits=S_recruits[1], u=0, fecundity=fecundity, plus_yrs=plus_yrs, prop_spawners=prop_spawners)
    e0 <- eigen(lefko0)
    unfishedR <- abs(e0$vectors[,1])[2]
    
    lefko1 <- create_lefko(capacity=capacity,dd=dd,abund=1,S_eggs=S_eggs[1], S_prerecruits=S_prerecruits[1], S_recruits=S_recruits[1], u=u_t[1], fecundity=fecundity, plus_yrs=plus_yrs, prop_spawners=prop_spawners)
    e1 <- eigen(lefko1)
    pmat[1,stages] <- abs(e1$vectors[,1])
    pmat[1,"lambda"] <- e1$value[1]
    pmat[1,"catch"] <- pmat[1,"R"]*u_t[1]
    pmat[1,"exploit"] <- u_t[1]
    
    for(i in 2:nyears){ 
    
      lefko <- create_lefko(capacity=capacity,dd=dd,abund=sum(pmat[i-1,c("P","R")]),S_eggs=S_eggs[i], S_prerecruits=S_prerecruits[i], S_recruits=S_recruits[i], u=u_t[i], fecundity=fecundity, plus_yrs=plus_yrs, prop_spawners=prop_spawners)
      
      pmat[i,stages] <- pmat[(i-1),stages] %*% t(lefko)
      pmat[i,"lambda"] <- sum(pmat[i,stages])/sum(pmat[(i-1),stages])
      if(pmat[i,"R"]<unfishedR*0.1) u_t[i] <- 0
      pmat[i,"exploit"] <- u_t[i]
      pmat[i,"catch"] <- pmat[i,"R"]*u_t[i]
      
      rm(lefko)
    } 

    return(pmat)
  }

##### --------- function to calculate relative value ------------  

  ## calculate relative values
  relative <- function(x, max){
    x2 <- x/max
    return(x2)
  }
  
  output$PopFlowchart <- renderPlot({
    if(input$u==0.3) ima <- readPNG(file.path("www", "equil_recruits_flow.png"))
    if(input$u<0.3) ima <- readPNG(file.path("www", "fewer_recruits_flow.png"))
    if(input$u>0.3) ima <- readPNG(file.path("www", "more_recruits_flow.png"))
    plot(x=1,y=1,type="n",axes=F,ann=F)
    lim <- par()
    rasterImage(ima, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])
  })
  
  output$CompareConstantHarvest <- renderPlot({
    nyears <- 20
    ut <- rep(input$u, nyears)
    dd <- -1.19
    capacity <- 1
    if(any(input$yct>0)) ut[input$yct[1]:input$yct[2]] <- 0
    base <- project_fn(capacity=capacity, dd=dd,nyears=nyears, u_t=rep(0.3,nyears), S_eggs=rep(0.0000556,nyears), S_prerecruits=rep(0.11,nyears), S_recruits=rep(0.7,nyears), fecundity=300000, plus_yrs=3, prop_spawners=0.5)
    alt <- project_fn(capacity=capacity, dd=dd,nyears=nyears, u_t=ut, S_eggs=rep(0.0000556,nyears), S_prerecruits=rep(0.11,nyears), S_recruits=rep(0.7,nyears), fecundity=300000, plus_yrs=3, prop_spawners=0.5)
    close <- project_fn(capacity=capacity, dd=dd,nyears=nyears, u_t=rep(0,nyears), S_eggs=rep(0.0000556,nyears), S_prerecruits=rep(0.11,nyears), S_recruits=rep(0.7,nyears), fecundity=300000, plus_yrs=3, prop_spawners=0.5)
    
    par(mfrow=c(1,4))
    plot(base[,"exploit"], ylim=c(0,1), type="l", lwd=4, xaxt="n", yaxt="n", xaxs="i", yaxs="i", xpd=NA, xlab="", ylab="", col="gray")
    lines(alt[,"exploit"], col="red", lwd=4)
    lines(close[,"exploit"], col="blue", lwd=4)
    axis(2, cex.axis=2, las=2, at=seq(0,1,by=0.25))
    mtext(side=3, "Harvest rate", font=2, cex=1.5, line=-2)
    axis(1, cex.axis=2)
    
    plot(relative(base[,"catch"], max(base[,"catch"])), ylim=c(0,3), type="l", lwd=4, xaxt="n", yaxt="n", xaxs="i", yaxs="i", xpd=NA, xlab="", ylab="", col="gray")
    lines(relative(alt[,"catch"], max(base[,"catch"])), lwd=4, col="red")
    lines(relative(close[,"catch"], max(base[,"catch"])), lwd=4, col="blue")
    axis(2, cex.axis=2, las=2, at=seq(0,3,by=1))
    mtext(side=3, "Catch", font=2, cex=1.5, line=-2)
    axis(1, cex.axis=2)
    
    plot(relative(base[,"R"], max(base[,"R"])), ylim=c(0,2), type="l", lwd=4, xaxt="n", yaxt="n", xaxs="i", yaxs="i", xpd=NA, xlab="", ylab="", col="gray")
    lines(relative(alt[,"R"], max(base[,"R"])), lwd=4, col="red")
    lines(relative(close[,"R"], max(base[,"R"])), lwd=4, col="blue")
    axis(2, cex.axis=2, las=2, at=seq(0,2,by=0.5))
    mtext(side=3, "Recruits", font=2, cex=1.5, line=-2)
    axis(1, cex.axis=2)
    
    plot(x=1,y=1,type="n",axes=F,ann=F)
    legend("topleft", cex=2, lwd=4, box.lwd=0, box.col="white", legend=c("Equilibrium", "No fishing", "With fishing"), col=c("gray", "blue", "red"))
    
    
  })

  output$ScenarioOutput <- renderPlot({
    colors <- brewer.pal(4, "Set1")[c(2,3,1,4)]
     scenarios=list("inc_waves"=input$inc_waves, "HABs"=input$HABs, "pollution"=input$pollution, "dec_habitat"=input$dec_habitat)
     nyears <- 20
     ut <- rep(input$u2, nyears)
     if(scenarios$HABs==TRUE) ut[seq(from=input$hab_freq,by=input$hab_freq,length=input$hab_nyears)] <- 0
     
     dd <- -1.19
     if(scenarios$dec_habitat==TRUE) capacity <- input$capacity
     if(scenarios$dec_habitat==FALSE) capacity <- 1
     
     if(scenarios$inc_waves==TRUE | scenarios$pollution==TRUE) S_prerecruits <- seq(0.11, input$min_Spre, length=nyears)
     if(scenarios$inc_waves==FALSE & scenarios$pollution==FALSE) S_prerecruits <- rep(0.11, nyears)
     
     if(scenarios$pollution==TRUE) S_recruits <- seq(0.7, input$min_Srec, length=nyears)
     if(scenarios$pollution==FALSE) S_recruits <- rep(0.7, nyears)
     
     base <- project_fn(capacity=1, dd=dd,nyears=nyears, u_t=rep(0.3,nyears), S_eggs=rep(0.0000556,nyears), S_prerecruits=rep(0.11,nyears), S_recruits=rep(0.7,nyears), fecundity=300000, plus_yrs=3, prop_spawners=0.5)
     alt <- project_fn(capacity=capacity, dd=dd,nyears=nyears, u_t=ut, S_eggs=rep(0.0000556,nyears), S_prerecruits=S_prerecruits, S_recruits=S_recruits, fecundity=300000, plus_yrs=3, prop_spawners=0.5)
     
     par(mfrow=c(2,3))
     
     plot(base[,"exploit"], ylim=c(0,1), type="l", lwd=4, xaxt="n", yaxt="n", xaxs="i", yaxs="i", xpd=NA, xlab="", ylab="", col="gray")
     if(length(which(scenarios==TRUE))>0) col <- "black"
     if(scenarios$HABs==TRUE) col <- colors[2]
     if(length(which(scenarios==TRUE))>0) lines(alt[,"exploit"], col=col, lwd=4)
     axis(2, cex.axis=2, las=2, at=seq(0,1,by=0.5))
     mtext(side=1, "Year", line=3)
     mtext(side=3, "Harvest rate", font=2, cex=1.5, line=-2)
     axis(1, cex.axis=2)
     
     plot(relative(base[,"catch"], max(base[,"catch"])), ylim=c(0,3), type="l", lwd=4, xaxt="n", yaxt="n", xaxs="i", yaxs="i", xpd=NA, xlab="", ylab="", col="gray")
     if(length(which(scenarios==TRUE))>0){
       col <- "black"
       lty <- 1
     }
     if(scenarios$HABs==TRUE){
       col <- colors[2]
       lty <- 1
     }
     if(length(which(scenarios==TRUE))>0) lines(relative(alt[,"catch"], max(base[,"catch"])), col=col, lwd=4, lty=lty)

     axis(2, cex.axis=2, las=2, at=seq(0,3,by=1))
     mtext(side=1, "Year", line=3)
     mtext(side=3, "Catch", font=2, cex=1.5, line=-2)
     axis(1, cex.axis=2)
     
     plot(relative(base[,"R"], max(base[,"R"])), ylim=c(0,2), type="l", lwd=4, xaxt="n", yaxt="n", xaxs="i", yaxs="i", xpd=NA, xlab="", ylab="", col="gray")
     if(length(which(scenarios==TRUE))>0){
       col <- "black"
       lty=1
     }
     if(scenarios$inc_waves==TRUE & scenarios$pollution==FALSE & scenarios$dec_habitat==FALSE){
       col <- colors[1]
       lty=1
     }
     if(scenarios$inc_waves==FALSE & scenarios$pollution==TRUE & scenarios$dec_habitat==FALSE){
       col <- colors[3]
       lty=1
     }
     if(scenarios$inc_waves==TRUE & scenarios$pollution==TRUE & scenarios$dec_habitat==FALSE){
       col <- c(colors[1], colors[3])
       lty <- c(1,2)
     }
     if(scenarios$inc_waves==FALSE & scenarios$pollution==FALSE & scenarios$dec_habitat==TRUE){
       col <- colors[4]
       lty <- 1
     }
     if(scenarios$inc_waves==TRUE & scenarios$pollution==FALSE & scenarios$dec_habitat==TRUE){
       col <- c(colors[1], colors[4])
       lty <- c(1,2)
     }
     if(scenarios$inc_waves==FALSE & scenarios$pollution==TRUE & scenarios$dec_habitat==TRUE){
       col <- c(colors[3], colors[4])
       lty <- c(1,2)
     }
     if(scenarios$inc_waves==TRUE & scenarios$pollution==TRUE & scenarios$dec_habitat==TRUE){
       col <- c(colors[1], colors[3], colors[4])
       lty <- c(1,2,3)
     }
     if(length(which(scenarios==TRUE))>0){
       for(i in 1:length(col)){
         lines(relative(alt[,"R"], max(base[,"R"])), col=col[i], lty=lty[i], lwd=4)
       }
     }
     axis(2, cex.axis=2, las=2, at=seq(0,2,by=0.5))
     mtext(side=1, "Year", line=3)
     mtext(side=3, "Recruits", font=2, cex=1.5, line=-2)
     axis(1, cex.axis=2)
  
     

     if(scenarios$inc_waves==TRUE|scenarios$pollution==TRUE){
        plot(rep(0.11,nyears), col="gray", lwd=4, xlim=c(1,nyears), ylim=c(0,0.2), xlab="", ylab="", type="l", xaxs="i", yaxs="i", xaxt="n", yaxt="n")
        if(scenarios$inc_waves==TRUE & scenarios$pollution==FALSE){
          col <- colors[1]
          lty=1
        }
        if(scenarios$inc_waves==FALSE & scenarios$pollution==TRUE){
          col <- colors[3]
          lty=1
        }
        if(scenarios$inc_waves==TRUE & scenarios$pollution==TRUE){
          col <- c(colors[1], colors[3])
          lty <- c(1,2)
        }
        for(i in 1:length(col)){
          lines(S_prerecruits, lwd=4, col=col[i], lty=lty[i])
        }
        axis(2, cex.axis=2, las=2, at=seq(0,0.2,by=0.1))
        mtext(side=1, "Year", line=3)
        mtext(side=3, "Pre-recruit survival", font=2, cex=1.5, line=-2)
        axis(1, cex.axis=2)
     }
     
     if(scenarios$pollution==TRUE){
       plot(rep(0.7,nyears), col="gray", lwd=4, xlim=c(1,nyears), ylim=c(0,1), xlab="", ylab="", type="l", xaxs="i", yaxs="i", xaxt="n", yaxt="n")
       lines(S_recruits, lwd=4, col=colors[3])
       axis(2, cex.axis=2, las=2, at=seq(0,1,by=0.25))
       mtext(side=1, "Year", line=3)
       mtext(side=3, "Recruit survival", font=2, cex=1.5, line=-2)
       axis(1, cex.axis=2)
     }
     
   })

  
  
})