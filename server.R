
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyServer(function(input, output) {
  ################ FUNCTIONS ###############################
    
  ## create lefkovitch matrix based on inputs
  create_lefko <- function(M_eggs, M_prerecruits, M_recruits, u, fecundity, plus_yrs, prop_spawners){
    stages <- c("P", "R")
    lefko <- matrix(0, nrow=length(stages), ncol=length(stages))
    rownames(lefko) <- colnames(lefko) <- stages
    
    tmat <- build_transitions()
    
    lefko["R","P"] <- tmat["R","P"] * exp(-M_prerecruits)
    lefko["P","P"] <- tmat["P","P"] * exp(-M_prerecruits)
    lefko["R","R"] <- tmat["R","R"] * ((exp(-M_recruits)*(1-u))^(plus_yrs-1))/(1-(exp(-M_recruits)*(1-u)))
    lefko["P","R"] <- 1#fecundity #* exp(-M_eggs)# * prop_spawners
    
    return(lefko)
  }
    
  ## project population forward in time based on inputs
  project_fn <- function(nyears, u_t, M_eggs, M_prerecruits, M_recruits, fecundity, plus_yrs, prop_spawners, rate_Mpre=FALSE, rate_Mrec=FALSE){
    stages <- c("P", "R")
    pmat <- matrix(NA, nrow=nyears, ncol=5)
    colnames(pmat) <- c(stages, "lambda", "catch", "exploit")
    
    if(rate_Mpre==FALSE & rate_Mrec==FALSE) lefko0 <- create_lefko(M_eggs=M_eggs, M_prerecruits=M_prerecruits, M_recruits=M_recruits, u=0, fecundity=fecundity, plus_yrs=plus_yrs, prop_spawners=prop_spawners)
    if(rate_Mpre==TRUE & rate_Mrec==FALSE) lefko0 <- create_lefko(M_eggs=M_eggs, M_prerecruits=M_prerecruits[1], M_recruits=M_recruits, u=0, fecundity=fecundity, plus_yrs=plus_yrs, prop_spawners=prop_spawners)
    if(rate_Mpre==FALSE & rate_Mrec==TRUE) lefko0 <- create_lefko(M_eggs=M_eggs, M_prerecruits=M_prerecruits, M_recruits=M_recruits[1], u=0, fecundity=fecundity, plus_yrs=plus_yrs, prop_spawners=prop_spawners)
    if(rate_Mpre==TRUE & rate_Mrec==TRUE) lefko0 <- create_lefko(M_eggs=M_eggs, M_prerecruits=M_prerecruits[1], M_recruits=M_recruits[1], u=0, fecundity=fecundity, plus_yrs=plus_yrs, prop_spawners=prop_spawners)
    e0 <- eigen(lefko0)
    unfishedR <- abs(e0$vectors[,1])[2]
    
    if(rate_Mpre==FALSE & rate_Mrec==FALSE) lefko1 <- create_lefko(M_eggs=M_eggs, M_prerecruits=M_prerecruits, M_recruits=M_recruits, u=u_t[1], fecundity=fecundity, plus_yrs=plus_yrs, prop_spawners=prop_spawners)
    if(rate_Mpre==TRUE & rate_Mrec==FALSE) lefko1 <- create_lefko(M_eggs=M_eggs, M_prerecruits=M_prerecruits[1], M_recruits=M_recruits, u=u_t[1], fecundity=fecundity, plus_yrs=plus_yrs, prop_spawners=prop_spawners)
    if(rate_Mpre==FALSE & rate_Mrec==TRUE) lefko1 <- create_lefko(M_eggs=M_eggs, M_prerecruits=M_prerecruits, M_recruits=M_recruits[1], u=u_t[1], fecundity=fecundity, plus_yrs=plus_yrs, prop_spawners=prop_spawners)
    if(rate_Mpre==TRUE & rate_Mrec==TRUE) lefko1 <- create_lefko(M_eggs=M_eggs, M_prerecruits=M_prerecruits[1], M_recruits=M_recruits[1], u=u_t[1], fecundity=fecundity, plus_yrs=plus_yrs, prop_spawners=prop_spawners)
    e1 <- eigen(lefko1)
    pmat[1,stages] <- abs(e1$vectors[,1])
    pmat[1,"lambda"] <- e1$value[1]
    pmat[1,"catch"] <- pmat[1,"R"]*u_t[1]
    pmat[1,"exploit"] <- u_t[1]
    
    for(i in 2:nyears){	
    
      if(rate_Mpre==FALSE & rate_Mrec==FALSE) lefko <- create_lefko(M_eggs=M_eggs, M_prerecruits=M_prerecruits, M_recruits=M_recruits, u=u_t[i], fecundity=fecundity, plus_yrs=plus_yrs, prop_spawners=prop_spawners)
      if(rate_Mpre==TRUE & rate_Mrec==FALSE) lefko <- create_lefko(M_eggs=M_eggs, M_prerecruits=M_prerecruits[i], M_recruits=M_recruits, u=u_t[i], fecundity=fecundity, plus_yrs=plus_yrs, prop_spawners=prop_spawners)
      if(rate_Mpre==FALSE & rate_Mrec==TRUE) lefko <- create_lefko(M_eggs=M_eggs, M_prerecruits=M_prerecruits, M_recruits=M_recruits[i], u=u_t[i], fecundity=fecundity, plus_yrs=plus_yrs, prop_spawners=prop_spawners)
      if(rate_Mpre==TRUE & rate_Mrec==TRUE) lefko <- create_lefko(M_eggs=M_eggs, M_prerecruits=M_prerecruits[i], M_recruits=M_recruits[i], u=u_t[i], fecundity=fecundity, plus_yrs=plus_yrs, prop_spawners=prop_spawners)
      
      pmat[i,stages] <- pmat[(i-1),stages] %*% t(lefko)
      pmat[i,"lambda"] <- sum(pmat[i,stages])/sum(pmat[(i-1),stages])
      if(pmat[i,"R"]<unfishedR*0.1) u_t[i] <- 0
      pmat[i,"exploit"] <- u_t[i]
      pmat[i,"catch"] <- pmat[i,"R"]*u_t[i]
      
      rm(lefko)
    }	

    return(pmat)
  }
    
  ## calculate relative values
  relative <- function(x, max){
    x2 <- x/max
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
  
  build_results <- function(scenarios){
    
    Outs <- list()
    u_t <- rep(input$u_equil, input$nyears)
    Outs[["SQ"]] <- project_fn(nyears=input$nyears, u_t=u_t, M_eggs=input$M_eggs, M_prerecruits=input$M_prerecruits, M_recruits=input$M_recruits, fecundity=input$fecundity, plus_yrs=input$plus_yrs, prop_spawners=input$prop_spawners)
    
    scenarios_on <- scenarios[which(scenarios==TRUE)]
    if(length(scenarios_on>1)){
      
      M_prerecruits_val_vec <- M_recruits_val_vec <- NULL
      u_t_val_mat <- NULL
      
      for(i in 1:length(scenarios)){
        if(names(scenarios[i]) %in% names(scenarios_on) == FALSE) next
        if(names(scenarios[i])=="inc_waves" & scenarios[[i]]==TRUE){
          ## scenario-specific rates
          u_t_new=rep(input$u_equil, input$nyears)
          M_eggs_new=input$M_eggs
          M_prerecruits_new=input$M_prerecruits
          M_recruits_new=input$M_recruits
          fecundity_new=input$fecundity
          plus_yrs_new=input$plus_yrs
          prop_spawners_new=input$prop_spawners
          
          val <- ifelse(input$inc_waves_strength=="Low", 1.10, ifelse(input$inc_waves_strength=="High", 1.30, stop("invalid input for change in wave frequency strength")))
          rates <- seq(1, val, length=input$nyears)
          M_prerecruits_new <- M_prerecruits_new*rates
          M_prerecruits_val_vec <- c(M_prerecruits_val_vec, mean(rates))
          Outs[["inc_waves"]] <- project_fn(nyears=input$nyears, u_t=u_t_new, M_eggs=M_eggs_new, M_prerecruits=M_prerecruits_new, M_recruits=M_recruits_new, fecundity=fecundity_new, plus_yrs=plus_yrs_new, prop_spawners=prop_spawners_new, rate_Mpre=TRUE)
        }
        if(names(scenarios[i])=="pollution" & scenarios[[i]]==TRUE){
          ## scenario-specific rates
          u_t_new=rep(input$u_equil, input$nyears)
          M_eggs_new=input$M_eggs
          M_prerecruits_new=input$M_prerecruits
          M_recruits_new=input$M_recruits
          fecundity_new=input$fecundity
          plus_yrs_new=input$plus_yrs
          prop_spawners_new=input$prop_spawners
          
          val <- ifelse(input$pollution_strength=="Low", 1.10, ifelse(input$pollution_strength=="High", 1.30, stop("invalid input for change in wave frequency strength")))
          M_prerecruits_new <- M_prerecruits_new*val
          M_recruits_new <- M_recruits_new*val
          M_prerecruits_val_vec <- c(M_prerecruits_val_vec, val)
          M_recruits_val_vec <- c(M_recruits_val_vec, val)
          Outs[["pollution"]] <- project_fn(nyears=input$nyears, u_t=u_t_new, M_eggs=M_eggs_new, M_prerecruits=M_prerecruits_new, M_recruits=M_recruits_new, fecundity=fecundity_new, plus_yrs=plus_yrs_new, prop_spawners=prop_spawners_new)
        }
        if(names(scenarios[i])=="dec_habitat" & scenarios[[i]]==TRUE){
          ## scenario-specific rates
          u_t_new=rep(input$u_equil, input$nyears)
          M_eggs_new=input$M_eggs
          M_prerecruits_new=input$M_prerecruits
          M_recruits_new=input$M_recruits
          fecundity_new=input$fecundity
          plus_yrs_new=input$plus_yrs
          prop_spawners_new=input$prop_spawners
          
          val <- ifelse(input$dec_habitat_strength=="Low", 1.05, ifelse(input$dec_habitat_strength=="High", 1.20, stop("invalid input for change in wave frequency strength")))
          rates <- seq(1, val, length=input$nyears)
          M_prerecruits_new <- M_prerecruits_new*rates
          M_recruits_new <- M_recruits_new*rates
          M_prerecruits_val_vec <- c(M_prerecruits_val_vec, mean(rates))
          M_recruits_val_vec <- c(M_recruits_val_vec, mean(rates))
          Outs[["dec_habitat"]] <- project_fn(nyears=input$nyears, u_t=u_t_new, M_eggs=M_eggs_new, M_prerecruits=M_prerecruits_new, M_recruits=M_recruits_new, fecundity=fecundity_new, plus_yrs=plus_yrs_new, prop_spawners=prop_spawners_new, rate_Mpre=TRUE, rate_Mrec=TRUE)
        }
        if(names(scenarios[i])=="HABs" & scenarios[[i]]==TRUE){
          ## scenario-specific rates
          u_t_new=rep(input$u_equil, input$nyears)
          M_eggs_new=input$M_eggs
          M_prerecruits_new=input$M_prerecruits
          M_recruits_new=input$M_recruits
          fecundity_new=input$fecundity
          plus_yrs_new=input$plus_yrs
          prop_spawners_new=input$prop_spawners
          
          val <- ifelse(input$HABs_strength=="Lower", input$u_equil/2, ifelse(input$HABs_strength=="Status quo", input$u_equil, ifelse(input$HABs_strength=="Higher", input$u_equil*1.5, stop("invalid input for change in HABs strength"))))
          freq <- ifelse(input$HABs_freq=="High", 2, ifelse(input$HABs_freq=="Low", 5, stop("Invalid input for frequency of HABs closures")))
          index_off <- seq(freq, input$nyears, by=freq)
          index_on <- which(1:input$nyears %in% index_off==FALSE)
          u_t_new[index_off] <- 0
          u_t_new[index_on] <- val
          u_t_val_mat <- rbind(u_t_val_mat, u_t_new)
          Outs[["HABs"]] <- project_fn(nyears=input$nyears, u_t=u_t_new, M_eggs=M_eggs_new, M_prerecruits=M_prerecruits_new, M_recruits=M_recruits_new, fecundity=fecundity_new, plus_yrs=plus_yrs_new, prop_spawners=prop_spawners_new)
        }
      }
      ## combined rates
      if(all(is.null(u_t_val_mat))) u_t_all <- u_t
      if(all(is.null(u_t_val_mat))==FALSE){
        u_t_meds <- apply(u_t_val_mat, 2, function(x) median(x))
        u_t_all <- u_t
        index_min <- which(u_t_meds < u_t)
        index_max <- which(u_t_meds > u_t)
        if(length(index_min)>0) u_t_all[index_min] <- sapply(1:length(index_min), function(x) min(u_t_meds[index_min[x]], u_t[index_min[x]]))
        if(length(index_max)>0) u_t_all[index_max] <- sapply(1:length(index_max), function(x) max(u_t_meds[index_max[x]], u_t[index_max[x]]))
      }
      
      M_eggs_all=input$M_eggs
      if(length(M_prerecruits_val_vec)==0) M_prerecruits_all <- input$M_prerecruits
      if(length(M_prerecruits_val_vec)>0){ 
        for(i in 1:length(M_prerecruits_val_vec)){
          if(i==1) M_prerecruits_all=input$M_prerecruits*M_prerecruits_val_vec[i]
          if(i>1) M_prerecruits_all <- M_prerecruits_all*M_prerecruits_val_vec[i]
        }
      }
      if(length(M_recruits_val_vec)==0) M_recruits_all <- input$M_recruits
      if(length(M_recruits_val_vec)>0){
        for(i in 1:length(M_recruits_val_vec)){
          if(i==1) M_recruits_all=input$M_recruits*M_recruits_val_vec[i]
          if(i>1) M_recruits_all <- M_recruits_all*M_recruits_val_vec[i]
        }
      }
      fecundity_all=input$fecundity
      plus_yrs_all=input$plus_yrs
      prop_spawners_all=input$prop_spawners
      
      Outs[["total"]] <- project_fn(nyears=input$nyears, u_t=u_t_all, M_eggs=M_eggs_all, M_prerecruits=M_prerecruits_all, M_recruits=M_recruits_all, fecundity=fecundity_all, plus_yrs=plus_yrs_all, prop_spawners=prop_spawners_all)
    }
    return(Outs)
  }
  
  output$CatchOverTime <- renderPlot({
    
    scenarios=list("inc_waves"=input$inc_waves, "HABs"=input$HABs, "pollution"=input$pollution, "dec_habitat"=input$dec_habitat)
    res <- build_results(scenarios=scenarios)
    plot(relative(res$SQ[,"catch"], max(res$SQ[,"catch"], na.rm=TRUE)), type="l", col=gray(0.5), lwd=2, ylim=c(0, 10), xaxs="i", yaxs="i", main="Expected catch over time", xlab="Years into future", ylab="Relative Catch")
    if(any(scenarios==TRUE)) points(relative(res$total[,"catch"], max(res$SQ[,"catch"], na.rm=TRUE)), pch=19, col=gray(0.2), cex=2, xpd=NA)
  })
  
  output$RecruitsOverTime <- renderPlot({
    
    scenarios=list("inc_waves"=input$inc_waves, "HABs"=input$HABs, "pollution"=input$pollution, "dec_habitat"=input$dec_habitat)
    res <- build_results(scenarios=scenarios)
    plot(res$SQ[,"R"], type="l", col=gray(0.5), lwd=2, ylim=c(0, 10), xaxs="i", yaxs="i", main="Expected number of recruits over time", xlab="Years into future", ylab="Relative number of recruits")
    if(any(scenarios==TRUE)) points(res$total[,"R"], pch=19, col=gray(0.2), cex=2, xpd=NA)
  })

  output$MeanCatch <- renderPlot({
    colors <- brewer.pal(4, "Set1")
    scenarios=list("inc_waves"=input$inc_waves, "HABs"=input$HABs, "pollution"=input$pollution, "dec_habitat"=input$dec_habitat)
    res <- build_results(scenarios=scenarios)
    mean_catch <- relative(sapply(1:length(res), function(x) mean(res[[x]][,"catch"])), max=mean(res$SQ[,"catch"]))
    res_names <- sapply(1:length(res), function(x) names(res[x]))
    res_colors <- list("SQ"=gray(0.5))
    scen_colors <- colors[which(names(scenarios) %in% res_names)]
    names(scen_colors) <- names(scenarios)[which(names(scenarios)%in% res_names)]
    res_colors <- c(res_colors, scen_colors, "total"=gray(0.2))
    
    
    plot(mean_catch, type="h", lwd=10, col=unlist(res_colors), xlim=c(0, length(scenarios)+3), ylim=c(0,10), xaxt="n", xlab="Scenario", ylab="Mean Catch", xaxs="i", yaxs="i")
    axis(1, at=1:length(mean_catch), labels=res_names)
  })
  
  output$TotalCatch <- renderPlot({
    colors <- brewer.pal(4, "Set1")
    scenarios=list("inc_waves"=input$inc_waves, "HABs"=input$HABs, "pollution"=input$pollution, "dec_habitat"=input$dec_habitat)
    res <- build_results(scenarios=scenarios)
    sum_catch <- relative(sapply(1:length(res), function(x) sum(res[[x]][,"catch"])), max=sum(res$SQ[,"catch"]))
    res_names <- sapply(1:length(res), function(x) names(res[x]))
    res_colors <- list("SQ"=gray(0.5))
    scen_colors <- colors[which(names(scenarios) %in% res_names)]
    names(scen_colors) <- names(scenarios)[which(names(scenarios)%in% res_names)]
    res_colors <- c(res_colors, scen_colors, "total"=gray(0.2))
    
    
    plot(sum_catch, type="h", lwd=10, col=unlist(res_colors), xlim=c(0, length(scenarios)+3), xpd=NA, ylim=c(0,10), main="Total catch over time", xaxt="n", xlab="Scenario", ylab="Total Catch", xaxs="i", yaxs="i")
    axis(1, at=1:length(sum_catch), labels=res_names)
  })
  
  output$NoCatch <- renderPlot({
    colors <- brewer.pal(4, "Set1")
    scenarios=list("inc_waves"=input$inc_waves, "HABs"=input$HABs, "pollution"=input$pollution, "dec_habitat"=input$dec_habitat)
    res <- build_results(scenarios=scenarios)
    no_catch <- sapply(1:length(res), function(x) length(which(res[[x]][,"exploit"]==0)))
    res_names <- sapply(1:length(res), function(x) names(res[x]))
    res_colors <- list("SQ"=gray(0.5))
    scen_colors <- colors[which(names(scenarios) %in% res_names)]
    names(scen_colors) <- names(scenarios)[which(names(scenarios)%in% res_names)]
    res_colors <- c(res_colors, scen_colors, "total"=gray(0.2))
    
    
    plot(no_catch, type="h", lwd=10, col=unlist(res_colors), xlim=c(0, length(scenarios)+3), xpd=NA, ylim=c(0,10), xaxt="n", xlab="Scenario", ylab="Number of years", xaxs="i", yaxs="i")
    axis(1, at=1:length(no_catch), labels=res_names)
  })
  
  output$NoCatchByMeanCatch <- renderPlot({
    colors <- brewer.pal(4, "Set1")
    scenarios=list("inc_waves"=input$inc_waves, "HABs"=input$HABs, "pollution"=input$pollution, "dec_habitat"=input$dec_habitat)
    res <- build_results(scenarios=scenarios)
    no_catch <- sapply(1:length(res), function(x) length(which(res[[x]][,"exploit"]==0)))
    mean_catch <- relative(sapply(1:length(res), function(x) mean(res[[x]][,"catch"])), max=mean(res$SQ[,"catch"]))
    res_names <- sapply(1:length(res), function(x) names(res[x]))
    res_colors <- list("SQ"=gray(0.5))
    scen_colors <- colors[which(names(scenarios) %in% res_names)]
    names(scen_colors) <- names(scenarios)[which(names(scenarios)%in% res_names)]
    res_colors <- c(res_colors, scen_colors, "total"=gray(0.2))
    mean_catch[which(mean_catch > 3)] <- 3
    
    plot(mean_catch, no_catch, pch=19, cex=2, col=unlist(res_colors), xlim=c(0, 3), ylim=c(0, 20), xaxt="n", xpd=NA, xlab="Mean Catch", ylab="Number of years without catch", main="Tradeoff in Mean Catch and Number of Years Without Harvest", xaxs="i", yaxs="i")
    axis(1, at=seq(0,3,by=0.5), labels=c(seq(0,2.5,by=0.5),"3+"))  
  })
  
  output$NoCatchByTotalCatch <- renderPlot({
    colors <- brewer.pal(4, "Set1")
    scenarios=list("inc_waves"=input$inc_waves, "HABs"=input$HABs, "pollution"=input$pollution, "dec_habitat"=input$dec_habitat)
    res <- build_results(scenarios=scenarios)
    no_catch <- sapply(1:length(res), function(x) length(which(res[[x]][,"exploit"]==0)))
    total_catch <- relative(sapply(1:length(res), function(x) sum(res[[x]][,"catch"])), max=sum(res$SQ[,"catch"]))
    res_names <- sapply(1:length(res), function(x) names(res[x]))
    res_colors <- list("SQ"=gray(0.5))
    scen_colors <- colors[which(names(scenarios) %in% res_names)]
    names(scen_colors) <- names(scenarios)[which(names(scenarios)%in% res_names)]
    res_colors <- c(res_colors, scen_colors, "total"=gray(0.2))
    total_catch[which(total_catch > 10)] <- 10
    
    plot(total_catch, no_catch, pch=19, cex=2, col=unlist(res_colors), xlim=c(0, 10), ylim=c(0, 20), xpd=NA, xaxt="n", xlab="Total Catch", ylab="Number of years without catch", main="Tradeoff in Total Catch and Number of Years Without Harvest", xaxs="i", yaxs="i")
    axis(1, at=0:10, labels=c(0:9, "10+"))  
  })

  

  
  
  
})