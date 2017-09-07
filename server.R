
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(png)

### some way to show average catch
### 5 year moving average of catch

shinyServer(function(input, output) {

################ FUNCTIONS ###############################
### ----- introduction page - goals and approaches ---##
  output$IntroTable <- renderImage({
    list(src="www/goals_table.png", width=750, height=400)
  }, deleteFile=FALSE)
  
  output$Glossary <- renderImage({
    list(src="www/glossary.png", width=500, height=260)
  }, deleteFile=FALSE)
  
  output$GlossarySmall <- renderImage({
    list(src="www/glossary.png", width=270, height=140)
  }, deleteFile=FALSE)
  
  output$IPOC <- renderImage({
    list(src="www/ipoc.png", width=400, height=160)
  }, deleteFile=FALSE)
  
  output$RazorClamPopDD <- renderImage({
    list(src="www/razor_clam_pop_DD.png", width=750, height=480)
  }, deleteFile=FALSE)
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
create_lefko <- function(S_eggs, S_prerecruits, S_recruits, u, fecundity, plus_yrs, prop_spawners, dd, abund, capacity, dev){
  stages <- c("P", "R")
  lefko <- matrix(0, nrow=length(stages), ncol=length(stages))
  rownames(lefko) <- colnames(lefko) <- stages
  
  tmat <- build_transitions()
  
  lefko["R","P"] <- tmat["R","P"] * S_prerecruits
  lefko["P","P"] <- tmat["P","P"] * S_prerecruits
  lefko["R","R"] <- tmat["R","R"] * ((S_recruits*(1-u))^(plus_yrs-1))/(1-S_recruits*(1-u)) * exp(dd*abund/capacity)
  lefko["P","R"] <- prop_spawners * fecundity * S_eggs * exp(dev)
  
  return(lefko)
}

  # lefko1 <- create_lefko(capacity=capacity,dd=0,abund=0,S_eggs=2.865e-6, S_prerecruits=0.0888, S_recruits=0.45, u=0.3, fecundity=8e6, plus_yrs=3, prop_spawners=0.5)
  # eigen(lefko1)
  # 
  # lefko0 <- create_lefko(capacity=capacity,dd=0,abund=0,S_eggs=2.865e-6, S_prerecruits=0.0888, S_recruits=0.45, u=0, fecundity=8e6, plus_yrs=3, prop_spawners=0.5)
  # eigen(lefko0)

##### --------- Project model ------------  

  ## project population forward in time based on inputs
  project_fn <- function(nyears=20, u_t, S_eggs, S_prerecruits, S_recruits, fecundity, plus_yrs, prop_spawners, dd, capacity, sigma=0){
    stages <- c("P", "R")
    pmat <- matrix(NA, nrow=nyears, ncol=5)
    colnames(pmat) <- c(stages, "lambda", "catch", "exploit")
    
    lefko0 <- create_lefko(capacity=capacity,dd=dd,abund=1,S_eggs=S_eggs[1], S_prerecruits=S_prerecruits[1], S_recruits=S_recruits[1], u=0, fecundity=fecundity, plus_yrs=plus_yrs, prop_spawners=prop_spawners, dev=0)
    e0 <- eigen(lefko0)
    unfishedR <- abs(e0$vectors[,1])[2]
    
    lefko1 <- create_lefko(capacity=capacity,dd=dd,abund=1,S_eggs=S_eggs[1], S_prerecruits=S_prerecruits[1], S_recruits=S_recruits[1], u=u_t[1], fecundity=fecundity, plus_yrs=plus_yrs, prop_spawners=prop_spawners, dev=(rnorm(1,0,sigma)-(sigma^2)/2))
    e1 <- eigen(lefko1)
    pmat[1,stages] <- abs(e1$vectors[,1])
    pmat[1,"lambda"] <- e1$value[1]
    pmat[1,"catch"] <- pmat[1,"R"]*u_t[1]
    pmat[1,"exploit"] <- u_t[1]
    
    for(i in 2:nyears){ 
    
      lefko <- create_lefko(capacity=capacity,dd=dd,abund=sum(pmat[i-1,c("P","R")]),S_eggs=S_eggs[i], S_prerecruits=S_prerecruits[i], S_recruits=S_recruits[i], u=u_t[i], fecundity=fecundity, plus_yrs=plus_yrs, prop_spawners=prop_spawners, dev=(rnorm(1,0,sigma)-(sigma^2)/2))
      
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
    
  #  test <- project_fn(capacity=1, dd=-1.19,nyears=20, u_t=rep(0.3,nyears), S_eggs=rep(2.865e-6,nyears), S_prerecruits=rep(0.0888,nyears), S_recruits=rep(0.4,nyears), fecundity=8e6, plus_yrs=3, prop_spawners=0.5)
    
    base <- project_fn(capacity=capacity, dd=dd,nyears=nyears, u_t=rep(0.3,nyears), S_eggs=rep(2.865e-6,nyears), S_prerecruits=rep(0.0888,nyears), S_recruits=rep(0.4,nyears), fecundity=8e6, plus_yrs=3, prop_spawners=0.5)
    alt <- project_fn(capacity=capacity, dd=dd,nyears=nyears, u_t=ut, S_eggs=rep(2.865e-6,nyears), S_prerecruits=rep(0.0888,nyears), S_recruits=rep(0.4,nyears), fecundity=8e6, plus_yrs=3, prop_spawners=0.5)
    close <- project_fn(capacity=capacity, dd=dd,nyears=nyears, u_t=rep(0,nyears), S_eggs=rep(2.865e-6,nyears), S_prerecruits=rep(0.0888,nyears), S_recruits=rep(0.4,nyears), fecundity=8e6, plus_yrs=3, prop_spawners=0.5)
    
    par(mfrow=c(2,2), mar=c(3,6,4,0), omi=c(0.2,0.2,0.2,1))
    plot(base[,"exploit"], ylim=c(0,1), type="l", lwd=4, xaxt="n", yaxt="n", xaxs="i", yaxs="i", xpd=NA, xlab="", ylab="", col="gray")
    lines(close[,"exploit"], col="cyan4", lwd=6)
    lines(alt[,"exploit"], col="orangered2", lwd=6, lty=2)
    axis(2, cex.axis=2, las=2, at=seq(0,1,by=0.25))
    mtext(side=3, "Harvest rate", font=2, cex=1.5, line=0.5)
    mtext(side=2, "Harvest rate", cex=1.5, line=4.5)
    mtext(side=1, "Year", cex=1.5, line=3)
    axis(1, cex.axis=2)
    
    plot(relative(base[,"catch"], max(base[,"catch"])), ylim=c(0,3), type="l", lwd=6, xaxt="n", yaxt="n", xaxs="i", yaxs="i", xpd=NA, xlab="", ylab="", col="gray")
    lines(relative(close[,"catch"], max(base[,"catch"])), lwd=6, col="cyan4")
    lines(relative(alt[,"catch"], max(base[,"catch"])), lwd=6, col="orangered2", lty=2)
    axis(2, cex.axis=2, las=2, at=seq(0,3,by=1))
    mtext(side=3, "Catch", font=2, cex=1.5, line=0.5)
    mtext(side=2, "Relative catch", cex=1.5, line=2.5)
    mtext(side=1, "Year", cex=1.5, line=3)
    axis(1, cex.axis=2)
    
    plot(relative(base[,"R"], max(base[,"R"])), ylim=c(0,1.5), type="l", lwd=6, xaxt="n", yaxt="n", xaxs="i", yaxs="i", xpd=NA, xlab="", ylab="", col="gray")
    lines(relative(close[,"R"], max(base[,"R"])), lwd=6, col="cyan4")
    lines(relative(alt[,"R"], max(base[,"R"])), lwd=6, col="orangered2", lty=2)
    axis(2, cex.axis=2, las=2, at=seq(0,2,by=0.5))
    mtext(side=3, "Recruits", font=2, cex=1.5, line=0.5)
    mtext(side=2, "Relative recruits", cex=1.5, line=4)
    mtext(side=1, "Year", cex=1.5, line=3)
    axis(1, cex.axis=2)
    
    plot(x=1,y=1,type="n",axes=F,ann=F)
    legend("topleft", xpd=NA, cex=2, lwd=6, box.lwd=0, box.col="white", lty=c(1,2,1), legend=c("Equilibrium status quo", "With fishing", "No fishing (constant, for comparison)"), col=c("gray", "orangered2", "cyan4"))
    
    
  }, height=800, width=1200)

  output$ScenarioOutput <- renderPlot({
    colors <- brewer.pal(4, "Set1")[c(2,3,1,4)]
     scenarios=list("inc_waves"=input$inc_waves, "HABs"=input$HABs, "pollution"=input$pollution, "dec_habitat"=input$dec_habitat)
     nyears <- 20
     ut <- rep(input$u2, nyears)
     if(scenarios$HABs==TRUE) ut[seq(from=input$hab_freq,by=input$hab_freq,to=nyears)] <- 0
     
     dd <- -1.19
     if(scenarios$dec_habitat==TRUE) capacity <- input$capacity
     if(scenarios$dec_habitat==FALSE) capacity <- 1
     
     if(scenarios$inc_waves==TRUE | scenarios$pollution==TRUE) S_prerecruits <- seq(0.0888, input$min_Spre, length=nyears)
     if(scenarios$inc_waves==FALSE & scenarios$pollution==FALSE) S_prerecruits <- rep(0.0888, nyears)
     
     if(scenarios$pollution==TRUE) S_recruits <- seq(0.4, input$min_Srec, length=nyears)
     if(scenarios$pollution==FALSE) S_recruits <- rep(0.4, nyears)
     
     base <- project_fn(capacity=1, dd=dd,nyears=nyears, u_t=rep(0.3,nyears), S_eggs=rep(2.865e-6,nyears), S_prerecruits=rep(0.0888,nyears), S_recruits=rep(0.4,nyears), fecundity=8e6, plus_yrs=3, prop_spawners=0.5)
     alt <- project_fn(capacity=capacity, dd=dd,nyears=nyears, u_t=ut, S_eggs=rep(2.865e-6,nyears), S_prerecruits=S_prerecruits, S_recruits=S_recruits, fecundity=8e6, plus_yrs=3, prop_spawners=0.5)
     
     par(mfrow=c(3,3), mar=c(5,6,3,1))
     
     plot(base[,"exploit"], ylim=c(0,1), type="l", lwd=4, xaxt="n", yaxt="n", xaxs="i", yaxs="i", xpd=NA, xlab="", ylab="", col="gray")
     if(length(which(scenarios==TRUE))>0) col <- "black"
     if(scenarios$HABs==TRUE) col <- colors[2]
     if(length(which(scenarios==TRUE))>0) lines(alt[,"exploit"], col=col, lwd=4)
     axis(2, cex.axis=2, las=2, at=seq(0,1,by=0.5))
     mtext(side=1, "Year", line=3, cex=1.5)
     mtext(side=3, "Harvest rate", font=2, cex=1.5, line=0.5)
     mtext(side=2, "Harvest rate", cex=1.5, line=4)
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
     mtext(side=1, "Year", line=3, cex=1.5)
     mtext(side=3, "Catch", font=2, cex=1.5, line=0.5)
     mtext(side=2, "Relative catch", cex=1.5, line=3)
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
     mtext(side=1, "Year", line=3, cex=1.5)
     mtext(side=3, "Recruits", font=2, cex=1.5, line=0.5)
     mtext(side=2, "Relative recruits", cex=1.5, line=4)
     axis(1, cex.axis=2)
  
     

     if(scenarios$inc_waves==TRUE|scenarios$pollution==TRUE){
        plot(rep(0.0888,nyears), col="gray", lwd=4, xlim=c(1,nyears), ylim=c(0,0.2), xlab="", ylab="", type="l", xaxs="i", yaxs="i", xaxt="n", yaxt="n")
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
        mtext(side=1, "Year", line=3, cex=1.5)
        mtext(side=3, "Pre-recruit survival", font=2, cex=1.5, line=0.5)
        mtext(side=2, "Pre-recruit survival", cex=1.5, line=4)
        axis(1, cex.axis=2)
     }
     
     if(scenarios$pollution==TRUE){
       plot(rep(0.4,nyears), col="gray", lwd=4, xlim=c(1,nyears), ylim=c(0,1), xlab="", ylab="", type="l", xaxs="i", yaxs="i", xaxt="n", yaxt="n")
       lines(S_recruits, lwd=4, col=colors[3])
       axis(2, cex.axis=2, las=2, at=seq(0,1,by=0.25))
       mtext(side=1, "Year", line=3, cex=1.5)
       mtext(side=3, "Recruit survival", font=2, cex=1.5, line=0.5)
       mtext(side=2, "Recruit survival", cex=1.5, line=4)
       axis(1, cex.axis=2)
     }
     
   }, height=800, width=1000)

  
  output$SimulationOutput <- renderPlot({
    colors <- brewer.pal(4, "Set1")[c(2,3,1,4)]
    scenarios=list("inc_waves"=input$inc_waves, "HABs"=input$HABs, "pollution"=input$pollution, "dec_habitat"=input$dec_habitat)
    nyears <- input$simyears
    ut <- rep(input$u2, nyears)
    if(scenarios$HABs==TRUE) ut[seq(from=input$hab_freq,by=input$hab_freq,to=nyears)] <- 0
    
    dd <- -1.19
    if(scenarios$dec_habitat==TRUE) capacity <- input$capacity
    if(scenarios$dec_habitat==FALSE) capacity <- 1
    
    if(scenarios$inc_waves==TRUE | scenarios$pollution==TRUE) S_prerecruits <- seq(0.0888, input$min_Spre, length=nyears)
    if(scenarios$inc_waves==FALSE & scenarios$pollution==FALSE) S_prerecruits <- rep(0.0888, nyears)
    
    if(scenarios$pollution==TRUE) S_recruits <- seq(0.4, input$min_Srec, length=nyears)
    if(scenarios$pollution==FALSE) S_recruits <- rep(0.4, nyears)
    
    base <- project_fn(capacity=1, dd=dd,nyears=nyears, u_t=rep(0.3,nyears), S_eggs=rep(2.865e-6,nyears), S_prerecruits=rep(0.0888,nyears), S_recruits=rep(0.4,nyears), fecundity=8e6, plus_yrs=3, prop_spawners=0.5)
    alt_det <- project_fn(capacity=capacity, dd=dd,nyears=nyears, u_t=ut, S_eggs=rep(2.865e-6,nyears), S_prerecruits=S_prerecruits, S_recruits=S_recruits, fecundity=8e6, plus_yrs=3, prop_spawners=0.5)
    
    alt <- base_var <- list()
    for(i in 1:input$nsims){
      set.seed(i*100)
      alt[[i]] <- project_fn(capacity=capacity, dd=dd,nyears=nyears, u_t=ut, S_eggs=rep(2.865e-6,nyears), S_prerecruits=S_prerecruits, S_recruits=S_recruits, fecundity=8e6, plus_yrs=3, prop_spawners=0.5, sigma=input$recvar)
      
      set.seed(i*100)
      base_var[[i]] <- project_fn(capacity=1, dd=dd,nyears=nyears, u_t=rep(0.3,nyears), S_eggs=rep(2.865e-6,nyears), S_prerecruits=rep(0.0888,nyears), S_recruits=rep(0.4,nyears), fecundity=8e6, plus_yrs=3, prop_spawners=0.5, sigma=input$recvar)    
    }
    
    par(mfrow=c(3,3), mar=c(5,6,3,1))
    
    plot(base[,"exploit"], ylim=c(0,1), type="l", lwd=4, xaxt="n", yaxt="n", xaxs="i", yaxs="i", xpd=NA, xlab="", ylab="", col="gray")
    if(length(which(scenarios==TRUE))>0) col <- "black"
    if(scenarios$HABs==TRUE) col <- colors[2]
    if(length(which(scenarios==TRUE))>0){
      alt_new <- matrix(NA, nrow=nrow(alt[[1]]), ncol=length(alt))
      for(i in 1:input$nsims){
        alt_new[,i] <- alt[[i]][,"exploit"]
      }
      up <- sapply(1:nrow(alt_new), function(x) quantile(alt_new[x,], 0.9))
      med <- sapply(1:nrow(alt_new), function(x) quantile(alt_new[x,], 0.5))
      low <- sapply(1:nrow(alt_new), function(x) quantile(alt_new[x,], 0.1))
      polygon(x=c(1:length(base[,"exploit"]), length(base[,"exploit"]):1), y=c(up, rev(low)), col="#AA000050", border=NA)
      lines(x=1:length(base[,"exploit"]), y=med, col="#AA0000", lwd=4)
    } 
    axis(2, cex.axis=2, las=2, at=seq(0,1,by=0.5))
    mtext(side=1, "Year", line=3, cex=1.5)
    mtext(side=3, "Harvest rate", font=2, cex=1.5, line=1)
    mtext(side=2, "Harvest rate", cex=1.5, line=4)
    axis(1, cex.axis=2)
    
    plot(1:length(base[,"catch"]), relative(base[,"catch"], max(base[,"catch"])), ylim=c(0,3), type="l", lwd=4, xaxt="n", yaxt="n", xaxs="i", yaxs="i", xpd=NA, xlab="", ylab="", col="gray")
    if(length(which(scenarios==TRUE))>0){
      alt_new <- matrix(NA, nrow=nrow(alt[[1]]), ncol=length(alt))
      for(i in 1:input$nsims){
        alt_new[,i] <- relative(alt[[i]][,"catch"], max(base[,"catch"]))
      }
      up <- sapply(1:nrow(alt_new), function(x) quantile(alt_new[x,], 0.9))
      med <- sapply(1:nrow(alt_new), function(x) quantile(alt_new[x,], 0.5))
      low <- sapply(1:nrow(alt_new), function(x) quantile(alt_new[x,], 0.1))
      polygon(x=c(1:length(base[,"catch"]), length(base[,"catch"]):1), y=c(up, rev(low)), col="#AA000050", border=NA)
      lines(x=1:length(base[,"catch"]), y=med, col="#AA0000", lwd=4)
    } 
    
    axis(2, cex.axis=2, las=2, at=seq(0,3,by=1))
    mtext(side=1, "Year", line=3, cex=1.5)
    mtext(side=3, "Catch", font=2, cex=1.5, line=1)
    mtext(side=2, "Relative catch", cex=1.5, line=4)
    axis(1, cex.axis=2)
    
    plot(relative(base[,"R"], max(base[,"R"])), ylim=c(0,2), type="l", lwd=4, xaxt="n", yaxt="n", xaxs="i", yaxs="i", xpd=NA, xlab="", ylab="", col="gray")
    if(length(which(scenarios==TRUE))>0){
      alt_new <- matrix(NA, nrow=nrow(alt[[1]]), ncol=length(alt))
      for(i in 1:input$nsims){
        alt_new[,i] <- relative(alt[[i]][,"R"], max(base[,"R"]))
      }
      up <- sapply(1:nrow(alt_new), function(x) quantile(alt_new[x,], 0.9))
      med <- sapply(1:nrow(alt_new), function(x) quantile(alt_new[x,], 0.5))
      low <- sapply(1:nrow(alt_new), function(x) quantile(alt_new[x,], 0.1))
      polygon(x=c(1:length(base[,"R"]), length(base[,"R"]):1), y=c(up, rev(low)), col="#AA000050", border=NA)
      lines(x=1:length(base[,"R"]), y=med, col="#AA0000", lwd=4)
    } 
    axis(2, cex.axis=2, las=2, at=seq(0,2,by=0.5))
    mtext(side=1, "Year", line=3, cex=1.5)
    mtext(side=3, "Recruits", font=2, cex=1.5, line=1)
    mtext(side=2, "Relative recruits", cex=1.5, line=4)
    axis(1, cex.axis=2)
    
    plot(x=1,y=1, type="n", axes=F, ann=F)
    plot(x=1,y=1, type="n", axes=F, ann=F)
    mtext(side=3, "Average number of years with no harvest", cex=2, font=2, line=-2)
    zeros <- sapply(1:length(alt), function(x) length(which(alt[[i]][,"exploit"]==0)))
    mtext(side=3, paste0(round(mean(zeros),2), " out of ", nyears, " years"), cex=2, line=-5)
    mtext(side=3, paste0("Due to HAB closures: ", length(which(ut==0)), "/", nyears), cex=2, line=-9)
    zeros2 <- zeros - length(which(ut==0))
    mtext(side=3, paste0("Due to lack of clams on beach: ", round(mean(zeros2),2), "/", nyears), cex=2, line=-12)
    plot(x=1,y=1,type="n", axes=F, ann=F)
    
    plot(x=1,y=1, type="n", axes=F, ann=F)
    plot(x=1,y=1, type="n", axes=F, ann=F)
    mtext(side=3, "Average % change in catch from status quo", cex=2, font=2, line=2)
    perc_total <- mean(sapply(1:length(alt), function(x) round(((sum(alt[[x]][,"catch"]) - sum(base[,"catch"]))/sum(base[,"catch"])*100))))
    perc_var <- mean(sapply(1:length(base_var), function(x) round(((sum(base_var[[x]][,"catch"]) - sum(base[,"catch"]))/sum(base[,"catch"])*100))))
    perc_impact <- round(((sum(alt_det[,"catch"]) - sum(base[,"catch"]))/sum(base[,"catch"])*100))
    mtext(side=3, paste0("Total over ", nyears, " years: ", ifelse(perc_total>0,"+","-"),abs(perc_total), "%"), cex=2, line=-1)
    mtext(side=3, paste0("Variability only: ", ifelse(perc_var>0,"+","-"), abs(perc_var), "%"), cex=2, line=-5)
    mtext(side=3, paste0("Impacts from risks: ", ifelse(perc_impact>0, "+","-"), abs(perc_impact), "%"), cex=2, line=-8)
    
    # pcatch <- rep(NA, length(alt))
    # for(i in 1:length(alt)){
    #   pcatch[i] <- 100*(sum(alt[[i]][,"catch"]) - sum(base[,"catch"]))/sum(base[,"catch"])
    # }
    # avg_pcatch <- round(mean(pcatch),2)
    # 
    # 
    # mtext(side=3, paste0("Average proportion of years with no harvest: ", avg_zero), font=2, xpd=NA, line=-1)
    # mtext(side=3, paste0("Average percent change in catch: ", avg_pcatch,"%"), font=2, xpd=NA, line=-3)
    
  }, height=800, width=1000)
  
  
  
})