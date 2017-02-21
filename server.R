
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
create_lefko <- function(S_eggs, S_prerecruits, S_recruits, u, fecundity, plus_yrs, prop_spawners, dd, abund, capacity=1){
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

  # lefko1 <- create_lefko(dd=0,abund=0,S_eggs=0.0000556, S_prerecruits=input$S_prerecruits, S_recruits=0.75, u=0.3, fecundity=300000, plus_yrs=3, prop_spawners=0.5)
  # eigen(lefko1)
  # 
  # lefko0 <- create_lefko(dd=0,abund=0,S_eggs=0.0000556, S_prerecruits=input$S_prerecruits, S_recruits=0.75, u=0, fecundity=300000, plus_yrs=3, prop_spawners=0.5)
  # eigen(lefko0)

##### --------- Project model ------------  

  ## project population forward in time based on inputs
  project_fn <- function(nyears, u_t, S_eggs, S_prerecruits, S_recruits, fecundity, plus_yrs, prop_spawners, rate_Mpre=FALSE, rate_Mrec=FALSE, dd){
    stages <- c("P", "R")
    nyears <- 20
    pmat <- matrix(NA, nrow=nyears, ncol=5)
    colnames(pmat) <- c(stages, "lambda", "catch", "exploit")
    
    if(rate_Mpre==FALSE & rate_Mrec==FALSE) lefko0 <- create_lefko(dd=dd,abund=1,S_eggs=S_eggs, S_prerecruits=S_prerecruits, S_recruits=S_recruits, u=0, fecundity=fecundity, plus_yrs=plus_yrs, prop_spawners=prop_spawners)
    if(rate_Mpre==TRUE & rate_Mrec==FALSE) lefko0 <- create_lefko(dd=dd,abund=1,S_eggs=S_eggs, S_prerecruits=S_prerecruits[1], S_recruits=S_recruits, u=0, fecundity=fecundity, plus_yrs=plus_yrs, prop_spawners=prop_spawners)
    if(rate_Mpre==FALSE & rate_Mrec==TRUE) lefko0 <- create_lefko(dd=dd,abund=1,S_eggs=S_eggs, S_prerecruits=S_prerecruits, S_recruits=S_recruits[1], u=0, fecundity=fecundity, plus_yrs=plus_yrs, prop_spawners=prop_spawners)
    if(rate_Mpre==TRUE & rate_Mrec==TRUE) lefko0 <- create_lefko(dd=dd,abund=1,S_eggs=S_eggs, S_prerecruits=S_prerecruits[1], S_recruits=S_recruits[1], u=0, fecundity=fecundity, plus_yrs=plus_yrs, prop_spawners=prop_spawners)
    e0 <- eigen(lefko0)
    unfishedR <- abs(e0$vectors[,1])[2]
    
    if(rate_Mpre==FALSE & rate_Mrec==FALSE) lefko1 <- create_lefko(dd=dd,abund=1,S_eggs=S_eggs, S_prerecruits=S_prerecruits, S_recruits=S_recruits, u=u_t[1], fecundity=fecundity, plus_yrs=plus_yrs, prop_spawners=prop_spawners)
    if(rate_Mpre==TRUE & rate_Mrec==FALSE) lefko1 <- create_lefko(dd=dd,abund=1,S_eggs=S_eggs, S_prerecruits=S_prerecruits[1], S_recruits=S_recruits, u=u_t[1], fecundity=fecundity, plus_yrs=plus_yrs, prop_spawners=prop_spawners)
    if(rate_Mpre==FALSE & rate_Mrec==TRUE) lefko1 <- create_lefko(dd=dd,abund=1,S_eggs=S_eggs, S_prerecruits=S_prerecruits, S_recruits=S_recruits[1], u=u_t[1], fecundity=fecundity, plus_yrs=plus_yrs, prop_spawners=prop_spawners)
    if(rate_Mpre==TRUE & rate_Mrec==TRUE) lefko1 <- create_lefko(dd=dd,abund=1,S_eggs=S_eggs, S_prerecruits=S_prerecruits[1], S_recruits=S_recruits[1], u=u_t[1], fecundity=fecundity, plus_yrs=plus_yrs, prop_spawners=prop_spawners)
    e1 <- eigen(lefko1)
    pmat[1,stages] <- abs(e1$vectors[,1])
    pmat[1,"lambda"] <- e1$value[1]
    pmat[1,"catch"] <- pmat[1,"R"]*u_t[1]
    pmat[1,"exploit"] <- u_t[1]
    
    for(i in 2:nyears){ 
    
      if(rate_Mpre==FALSE & rate_Mrec==FALSE) lefko <- create_lefko(dd=dd,abund=sum(pmat[i-1,c("P","R")]),S_eggs=S_eggs, S_prerecruits=S_prerecruits, S_recruits=S_recruits, u=u_t[i], fecundity=fecundity, plus_yrs=plus_yrs, prop_spawners=prop_spawners)
      if(rate_Mpre==TRUE & rate_Mrec==FALSE) lefko <- create_lefko(dd=dd,abund=sum(pmat[i-1,c("P","R")]),S_eggs=S_eggs, S_prerecruits=S_prerecruits[i], S_recruits=S_recruits, u=u_t[i], fecundity=fecundity, plus_yrs=plus_yrs, prop_spawners=prop_spawners)
      if(rate_Mpre==FALSE & rate_Mrec==TRUE) lefko <- create_lefko(dd=dd,abund=sum(pmat[i-1,c("P","R")]),S_eggs=S_eggs, S_prerecruits=S_prerecruits, S_recruits=S_recruits[i], u=u_t[i], fecundity=fecundity, plus_yrs=plus_yrs, prop_spawners=prop_spawners)
      if(rate_Mpre==TRUE & rate_Mrec==TRUE) lefko <- create_lefko(dd=dd,abund=sum(pmat[i-1,c("P","R")]),S_eggs=S_eggs, S_prerecruits=S_prerecruits[i], S_recruits=S_recruits[i], u=u_t[i], fecundity=fecundity, plus_yrs=plus_yrs, prop_spawners=prop_spawners)
      
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
    dd <- input$dd
    if(any(input$yct>0)) ut[input$yct[1]:input$yct[2]] <- 0
    base <- project_fn(dd=dd,nyears=nyears, u_t=rep(0.3,nyears), S_eggs=0.0000556, S_prerecruits=input$S_prerecruits, S_recruits=input$S_recruits, fecundity=300000, plus_yrs=3, prop_spawners=0.5, rate_Mpre=FALSE, rate_Mrec=FALSE)
    alt <- project_fn(dd=dd,nyears=nyears, u_t=ut, S_eggs=0.0000556, S_prerecruits=input$S_prerecruits, S_recruits=input$S_recruits, fecundity=300000, plus_yrs=3, prop_spawners=0.5, rate_Mpre=FALSE, rate_Mrec=FALSE)
    close <- project_fn(dd=dd,nyears=nyears, u_t=rep(0,nyears), S_eggs=0.0000556, S_prerecruits=input$S_prerecruits, S_recruits=input$S_recruits, fecundity=300000, plus_yrs=3, prop_spawners=0.5, rate_Mpre=FALSE, rate_Mrec=FALSE)
    
    par(mfrow=c(1,4))
    plot(base[,"exploit"], ylim=c(0,1), type="l", lwd=4, xaxt="n", yaxt="n", xaxs="i", yaxs="i", xpd=NA, xlab="", ylab="", col="gray")
    lines(alt[,"exploit"], col="red", lwd=4)
    lines(close[,"exploit"], col="blue", lwd=4)
    axis(2, cex.axis=2, las=2, at=seq(0,0.9,by=0.3))
    mtext(side=3, "Harvest rate", font=2, cex=1.5, line=-2)
    axis(1, cex.axis=2)
    
    plot(relative(base[,"catch"], max(base[,"catch"])), ylim=c(0,3), type="l", lwd=4, xaxt="n", yaxt="n", xaxs="i", yaxs="i", xpd=NA, xlab="", ylab="", col="gray")
    lines(relative(alt[,"catch"], max(base[,"catch"])), lwd=4, col="red")
    lines(relative(close[,"catch"], max(base[,"catch"])), lwd=4, col="blue")
    axis(2, cex.axis=2, las=2, at=seq(0,2.5,by=0.5))
    mtext(side=3, "Catch", font=2, cex=1.5, line=-2)
    axis(1, cex.axis=2)
    
    plot(relative(base[,"R"], max(base[,"R"])), ylim=c(0,2), type="l", lwd=4, xaxt="n", yaxt="n", xaxs="i", yaxs="i", xpd=NA, xlab="", ylab="", col="gray")
    lines(relative(alt[,"R"], max(base[,"R"])), lwd=4, col="red")
    lines(relative(close[,"R"], max(base[,"R"])), lwd=4, col="blue")
    axis(2, cex.axis=2, las=2, at=seq(0,20,by=1))
    mtext(side=3, "Recruits", font=2, cex=1.5, line=-2)
    axis(1, cex.axis=2)
    
    plot(x=1,y=1,type="n",axes=F,ann=F)
    legend("topleft", cex=2, lwd=4, box.lwd=0, box.col="white", legend=c("Equilibrium", "No fishing", "With fishing"), col=c("gray", "blue", "red"))
    
    
  })



  # output$CatchOverTime <- renderPlot({
    
  #   scenarios=list("inc_waves"=input$inc_waves, "HABs"=input$HABs, "pollution"=input$pollution, "dec_habitat"=input$dec_habitat)
  #   res <- build_results(scenarios=scenarios)
  #   plot(relative(res$SQ[,"catch"], max(res$SQ[,"catch"], na.rm=TRUE)), type="l", col=gray(0.5), lwd=2, ylim=c(0, 10), xaxs="i", yaxs="i", main="Expected catch over time", xlab="Years into future", ylab="Relative Catch")
  #   if(any(scenarios==TRUE)) points(relative(res$total[,"catch"], max(res$SQ[,"catch"], na.rm=TRUE)), pch=19, col=gray(0.2), cex=2, xpd=NA)
  # })
  
  # output$RecruitsOverTime <- renderPlot({
    
  #   scenarios=list("inc_waves"=input$inc_waves, "HABs"=input$HABs, "pollution"=input$pollution, "dec_habitat"=input$dec_habitat)
  #   res <- build_results(scenarios=scenarios)
  #   plot(res$SQ[,"R"], type="l", col=gray(0.5), lwd=2, ylim=c(0, 10), xaxs="i", yaxs="i", main="Expected number of recruits over time", xlab="Years into future", ylab="Relative number of recruits")
  #   if(any(scenarios==TRUE)) points(res$total[,"R"], pch=19, col=gray(0.2), cex=2, xpd=NA)
  # })

  # output$MeanCatch <- renderPlot({
  #   colors <- brewer.pal(4, "Set1")
  #   scenarios=list("inc_waves"=input$inc_waves, "HABs"=input$HABs, "pollution"=input$pollution, "dec_habitat"=input$dec_habitat)
  #   res <- build_results(scenarios=scenarios)
  #   mean_catch <- relative(sapply(1:length(res), function(x) mean(res[[x]][,"catch"])), max=mean(res$SQ[,"catch"]))
  #   res_names <- sapply(1:length(res), function(x) names(res[x]))
  #   res_colors <- list("SQ"=gray(0.5))
  #   scen_colors <- colors[which(names(scenarios) %in% res_names)]
  #   names(scen_colors) <- names(scenarios)[which(names(scenarios)%in% res_names)]
  #   res_colors <- c(res_colors, scen_colors, "total"=gray(0.2))
    
    
  #   plot(mean_catch, type="h", lwd=10, col=unlist(res_colors), xlim=c(0, length(scenarios)+3), ylim=c(0,10), xaxt="n", xlab="Scenario", ylab="Mean Catch", xaxs="i", yaxs="i")
  #   axis(1, at=1:length(mean_catch), labels=res_names)
  # })
  
  # output$TotalCatch <- renderPlot({
  #   colors <- brewer.pal(4, "Set1")
  #   scenarios=list("inc_waves"=input$inc_waves, "HABs"=input$HABs, "pollution"=input$pollution, "dec_habitat"=input$dec_habitat)
  #   res <- build_results(scenarios=scenarios)
  #   sum_catch <- relative(sapply(1:length(res), function(x) sum(res[[x]][,"catch"])), max=sum(res$SQ[,"catch"]))
  #   res_names <- sapply(1:length(res), function(x) names(res[x]))
  #   res_colors <- list("SQ"=gray(0.5))
  #   scen_colors <- colors[which(names(scenarios) %in% res_names)]
  #   names(scen_colors) <- names(scenarios)[which(names(scenarios)%in% res_names)]
  #   res_colors <- c(res_colors, scen_colors, "total"=gray(0.2))
    
    
  #   plot(sum_catch, type="h", lwd=10, col=unlist(res_colors), xlim=c(0, length(scenarios)+3), xpd=NA, ylim=c(0,10), main="Total catch over time", xaxt="n", xlab="Scenario", ylab="Total Catch", xaxs="i", yaxs="i")
  #   axis(1, at=1:length(sum_catch), labels=res_names)
  # })
  
  # output$NoCatch <- renderPlot({
  #   colors <- brewer.pal(4, "Set1")
  #   scenarios=list("inc_waves"=input$inc_waves, "HABs"=input$HABs, "pollution"=input$pollution, "dec_habitat"=input$dec_habitat)
  #   res <- build_results(scenarios=scenarios)
  #   no_catch <- sapply(1:length(res), function(x) length(which(res[[x]][,"exploit"]==0)))
  #   res_names <- sapply(1:length(res), function(x) names(res[x]))
  #   res_colors <- list("SQ"=gray(0.5))
  #   scen_colors <- colors[which(names(scenarios) %in% res_names)]
  #   names(scen_colors) <- names(scenarios)[which(names(scenarios)%in% res_names)]
  #   res_colors <- c(res_colors, scen_colors, "total"=gray(0.2))
    
    
  #   plot(no_catch, type="h", lwd=10, col=unlist(res_colors), xlim=c(0, length(scenarios)+3), xpd=NA, ylim=c(0,10), xaxt="n", xlab="Scenario", ylab="Number of years", xaxs="i", yaxs="i")
  #   axis(1, at=1:length(no_catch), labels=res_names)
  # })
  
  # output$NoCatchByMeanCatch <- renderPlot({
  #   colors <- brewer.pal(4, "Set1")
  #   scenarios=list("inc_waves"=input$inc_waves, "HABs"=input$HABs, "pollution"=input$pollution, "dec_habitat"=input$dec_habitat)
  #   res <- build_results(scenarios=scenarios)
  #   no_catch <- sapply(1:length(res), function(x) length(which(res[[x]][,"exploit"]==0)))
  #   mean_catch <- relative(sapply(1:length(res), function(x) mean(res[[x]][,"catch"])), max=mean(res$SQ[,"catch"]))
  #   res_names <- sapply(1:length(res), function(x) names(res[x]))
  #   res_colors <- list("SQ"=gray(0.5))
  #   scen_colors <- colors[which(names(scenarios) %in% res_names)]
  #   names(scen_colors) <- names(scenarios)[which(names(scenarios)%in% res_names)]
  #   res_colors <- c(res_colors, scen_colors, "total"=gray(0.2))
  #   mean_catch[which(mean_catch > 3)] <- 3
    
  #   plot(mean_catch, no_catch, pch=19, cex=2, col=unlist(res_colors), xlim=c(0, 3), ylim=c(0, 20), xaxt="n", xpd=NA, xlab="Mean Catch", ylab="Number of years without catch", main="Tradeoff in Mean Catch and Number of Years Without Harvest", xaxs="i", yaxs="i")
  #   axis(1, at=seq(0,3,by=0.5), labels=c(seq(0,2.5,by=0.5),"3+"))  
  # })
  
  # output$NoCatchByTotalCatch <- renderPlot({
  #   colors <- brewer.pal(4, "Set1")
  #   scenarios=list("inc_waves"=input$inc_waves, "HABs"=input$HABs, "pollution"=input$pollution, "dec_habitat"=input$dec_habitat)
  #   res <- build_results(scenarios=scenarios)
  #   no_catch <- sapply(1:length(res), function(x) length(which(res[[x]][,"exploit"]==0)))
  #   total_catch <- relative(sapply(1:length(res), function(x) sum(res[[x]][,"catch"])), max=sum(res$SQ[,"catch"]))
  #   res_names <- sapply(1:length(res), function(x) names(res[x]))
  #   res_colors <- list("SQ"=gray(0.5))
  #   scen_colors <- colors[which(names(scenarios) %in% res_names)]
  #   names(scen_colors) <- names(scenarios)[which(names(scenarios)%in% res_names)]
  #   res_colors <- c(res_colors, scen_colors, "total"=gray(0.2))
  #   total_catch[which(total_catch > 10)] <- 10
    
  #   plot(total_catch, no_catch, pch=19, cex=2, col=unlist(res_colors), xlim=c(0, 10), ylim=c(0, 20), xpd=NA, xaxt="n", xlab="Total Catch", ylab="Number of years without catch", main="Tradeoff in Total Catch and Number of Years Without Harvest", xaxs="i", yaxs="i")
  #   axis(1, at=0:10, labels=c(0:9, "10+"))  
  # })

  

  
  
  
})