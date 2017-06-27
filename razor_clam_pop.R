#########################################################
## razor clam stage-based population model
#########################################################

#########   DIRECTORIES AND PACKAGES  ###################

rm(list=ls()) ## clear environment
setwd("C:\\Git_Projects\\razor_clam_pop") ## set working directory

################ LIFE HISTORY PARAMETERS #################

clam_input <- list()

clam_input$stages <- c("P","R")

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


## create lefkovitch matrix based on inputs
create_lefko <- function(S_eggs, S_prerecruits, S_recruits, u, fecundity, plus_yrs, prop_spawners){
  stages <- c("P", "R")
  lefko <- matrix(0, nrow=length(stages), ncol=length(stages))
  rownames(lefko) <- colnames(lefko) <- stages
  
  tmat <- build_transitions()
  
  lefko["R","P"] <- tmat["R","P"] * S_prerecruits
  lefko["P","P"] <- tmat["P","P"] * S_recruits
  lefko["R","R"] <- tmat["R","R"] * ((S_recruits*(1-u))^(plus_yrs-1))/(1-S_recruits*(1-u))
  lefko["P","R"] <- prop_spawners * fecundity * S_eggs
  
  return(lefko)
}

	lefko1 <- create_lefko(S_eggs=0.0000556, S_prerecruits=0.1, S_recruits=0.5, u=0.3, fecundity=300000, plus_yrs=3, prop_spawners=0.5)
	eigen(lefko1)

	lefko0 <- create_lefko(S_eggs=0.0000556, S_prerecruits=0.1, S_recruits=0.5, u=0, fecundity=300000, plus_yrs=3, prop_spawners=0.5)
	eigen(lefko0)


  ## project population forward in time based on inputs
  project_fn <- function(nyears, u_t, S_eggs, S_prerecruits, S_recruits, fecundity, plus_yrs, rate_Mpre=FALSE, rate_Mrec=FALSE){
    stages <- c("P", "R")
    pmat <- matrix(NA, nrow=nyears, ncol=5)
    colnames(pmat) <- c(stages, "lambda", "catch", "exploit")
    
    if(rate_Mpre==FALSE & rate_Mrec==FALSE) lefko0 <- create_lefko(S_eggs=S_eggs, S_prerecruits=S_prerecruits, S_recruits=S_recruits, u=0, fecundity=fecundity, plus_yrs=plus_yrs, prop_spawners=prop_spawners)
    if(rate_Mpre==TRUE & rate_Mrec==FALSE) lefko0 <- create_lefko(S_eggs=S_eggs, S_prerecruits=S_prerecruits[1], S_recruits=S_recruits, u=0, fecundity=fecundity, plus_yrs=plus_yrs, prop_spawners=prop_spawners)
    if(rate_Mpre==FALSE & rate_Mrec==TRUE) lefko0 <- create_lefko(S_eggs=S_eggs, S_prerecruits=S_prerecruits, S_recruits=S_recruits[1], u=0, fecundity=fecundity, plus_yrs=plus_yrs, prop_spawners=prop_spawners)
    if(rate_Mpre==TRUE & rate_Mrec==TRUE) lefko0 <- create_lefko(S_eggs=S_eggs, S_prerecruits=S_prerecruits[1], S_recruits=S_recruits[1], u=0, fecundity=fecundity, plus_yrs=plus_yrs, prop_spawners=prop_spawners)
    e0 <- eigen(lefko0)
    unfishedR <- abs(e0$vectors[,1])[2]
    
    if(rate_Mpre==FALSE & rate_Mrec==FALSE) lefko1 <- create_lefko(S_eggs=S_eggs, S_prerecruits=S_prerecruits, S_recruits=S_recruits, u=u_t[1], fecundity=fecundity, plus_yrs=plus_yrs, prop_spawners=prop_spawners)
    if(rate_Mpre==TRUE & rate_Mrec==FALSE) lefko1 <- create_lefko(S_eggs=S_eggs, S_prerecruits=S_prerecruits[1], S_recruits=S_recruits, u=u_t[1], fecundity=fecundity, plus_yrs=plus_yrs, prop_spawners=prop_spawners)
    if(rate_Mpre==FALSE & rate_Mrec==TRUE) lefko1 <- create_lefko(S_eggs=S_eggs, S_prerecruits=S_prerecruits, S_recruits=S_recruits[1], u=u_t[1], fecundity=fecundity, plus_yrs=plus_yrs, prop_spawners=prop_spawners)
    if(rate_Mpre==TRUE & rate_Mrec==TRUE) lefko1 <- create_lefko(S_eggs=S_eggs, S_prerecruits=S_prerecruits[1], S_recruits=S_recruits[1], u=u_t[1], fecundity=fecundity, plus_yrs=plus_yrs, prop_spawners=prop_spawners)
    e1 <- eigen(lefko1)
    pmat[1,stages] <- abs(e1$vectors[,1])
    pmat[1,"lambda"] <- e1$value[1]
    pmat[1,"catch"] <- pmat[1,"R"]*u_t[1]
    pmat[1,"exploit"] <- u_t[1]
    
    for(i in 2:nyears){	
    
      if(rate_Mpre==FALSE & rate_Mrec==FALSE) lefko <- create_lefko(S_eggs=S_eggs, S_prerecruits=S_prerecruits, S_recruits=S_recruits, u=u_t[i], fecundity=fecundity, plus_yrs=plus_yrs, prop_spawners=prop_spawners)
      if(rate_Mpre==TRUE & rate_Mrec==FALSE) lefko <- create_lefko(S_eggs=S_eggs, S_prerecruits=S_prerecruits[i], S_recruits=S_recruits, u=u_t[i], fecundity=fecundity, plus_yrs=plus_yrs, prop_spawners=prop_spawners)
      if(rate_Mpre==FALSE & rate_Mrec==TRUE) lefko <- create_lefko(S_eggs=S_eggs, S_prerecruits=S_prerecruits, S_recruits=S_recruits[i], u=u_t[i], fecundity=fecundity, plus_yrs=plus_yrs, prop_spawners=prop_spawners)
      if(rate_Mpre==TRUE & rate_Mrec==TRUE) lefko <- create_lefko(S_eggs=S_eggs, S_prerecruits=S_prerecruits[i], S_recruits=S_recruits[i], u=u_t[i], fecundity=fecundity, plus_yrs=plus_yrs, prop_spawners=prop_spawners)
      
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

  base <- project_fn(nyears=20, u_t=rep(0.3,nyears), S_eggs=0.0000278, S_prerecruits=0.1, S_recruits=0.5, fecundity=300000, plus_yrs=3, rate_Mpre=FALSE, rate_Mrec=FALSE)
  inc <- project_fn(nyears=20, u_t=rep(0.7,nyears), S_eggs=0.0000278, S_prerecruits=0.1, S_recruits=0.5, fecundity=300000, plus_yrs=3, rate_Mpre=FALSE, rate_Mrec=FALSE)
  close <- project_fn(nyears=20, u_t=rep(0,nyears), S_eggs=0.0000278, S_prerecruits=0.1, S_recruits=0.5, fecundity=300000, plus_yrs=3, rate_Mpre=FALSE, rate_Mrec=FALSE)

  par(mfrow=c(3,1), mar=c(0,0,0,0), omi=c(1,1,1,1))
  plot(base[,"exploit"], ylim=c(0,1), type="l", lwd=4, xaxt="n", yaxt="n", xaxs="i", yaxs="i", xpd=NA, xlab="", ylab="")
  lines(inc[,"exploit"], col="red", lwd=4, lty=2)
  lines(close[,"exploit"], col="blue", lwd=4, lty=2)
  axis(2, cex.axis=2, las=2, at=seq(0,0.9,by=0.3))
  mtext(side=3, "Harvest rate", font=2, cex=1.5, line=-2)

  plot(base[,"lambda"], ylim=c(0,2), type="l", lwd=4, xaxt="n", yaxt="n", xaxs="i", yaxs="i", xpd=NA, xlab="", ylab="")
  lines(inc[,"lambda"], col="red", lwd=4, lty=2)
  lines(close[,"lambda"], col="blue", lwd=4, lty=2)
  axis(2, cex.axis=2, las=2, at=seq(0.5,1.5,by=0.5))
  mtext(side=3, "Growth rate", font=2, cex=1.5, line=-2)

  plot(relative(base[,"catch"], max(base[,"catch"])), ylim=c(0,3), type="l", lwd=4, xaxt="n", yaxt="n", xaxs="i", yaxs="i", xpd=NA, xlab="", ylab="")
  lines(relative(inc[,"catch"], max(base[,"catch"])), lwd=4, lty=2, col="red")
  lines(relative(close[,"catch"], max(base[,"catch"])), lwd=4, lty=2, col="blue")
  axis(2, cex.axis=2, las=2, at=seq(0,2.5,by=0.5))
  mtext(side=3, "Catch", font=2, cex=1.5, line=-2)
  axis(1, cex.axis=2)

  par(mfrow=c(3,1), mar=c(0,0,0,0), omi=c(1,1,1,1))
  plot(base[,"exploit"], ylim=c(0,1), type="l", lwd=4, xaxt="n", yaxt="n", xaxs="i", yaxs="i", xpd=NA, xlab="", ylab="")
  lines(inc[,"exploit"], col="red", lwd=4, lty=2)
  lines(close[,"exploit"], col="blue", lwd=4, lty=2)
  axis(2, cex.axis=2, las=2, at=seq(0,0.9,by=0.3))
  mtext(side=3, "Harvest rate", font=2, cex=1.5, line=-2)

  plot(base[,"lambda"], ylim=c(0,2), type="l", lwd=4, xaxt="n", yaxt="n", xaxs="i", yaxs="i", xpd=NA, xlab="", ylab="")
  lines(inc[,"lambda"], col="red", lwd=4, lty=2)
  lines(close[,"lambda"], col="blue", lwd=4, lty=2)
  axis(2, cex.axis=2, las=2, at=seq(0.5,1.5,by=0.5))
  mtext(side=3, "Growth rate", font=2, cex=1.5, line=-2)

  plot(relative(base[,"R"], max(base[,"R"])), ylim=c(0,30), type="l", lwd=4, xaxt="n", yaxt="n", xaxs="i", yaxs="i", xpd=NA, xlab="", ylab="")
  lines(relative(inc[,"R"], max(base[,"R"])), lwd=4, lty=2, col="red")
  lines(relative(close[,"R"], max(base[,"R"])), lwd=4, lty=2, col="blue")
  axis(2, cex.axis=2, las=2, at=seq(0,20,by=5))
  mtext(side=3, "Recruits", font=2, cex=1.5, line=-2)
  axis(1, cex.axis=2)



  build_results <- function(scenarios){
    
    Outs <- list()
    u_t <- rep(input$u_equil, input$nyears)
    Outs[["SQ"]] <- project_fn(nyears=input$nyears, u_t=u_t, S_eggs=input$S_eggs, S_prerecruits=input$S_prerecruits, S_recruits=input$S_recruits, fecundity=input$fecundity, plus_yrs=input$plus_yrs, prop_spawners=input$prop_spawners)
    
    scenarios_on <- scenarios[which(scenarios==TRUE)]
    if(length(scenarios_on>1)){
      
      S_prerecruits_val_vec <- S_recruits_val_vec <- NULL
      u_t_val_mat <- NULL
      
      for(i in 1:length(scenarios)){
        if(names(scenarios[i]) %in% names(scenarios_on) == FALSE) next
        if(names(scenarios[i])=="inc_waves" & scenarios[[i]]==TRUE){
          ## scenario-specific rates
          u_t_new=rep(input$u_equil, input$nyears)
          S_eggs_new=input$S_eggs
          S_prerecruits_new=input$S_prerecruits
          S_recruits_new=input$S_recruits
          fecundity_new=input$fecundity
          plus_yrs_new=input$plus_yrs
          prop_spawners_new=input$prop_spawners
          
          val <- ifelse(input$inc_waves_strength=="Low", 1.10, ifelse(input$inc_waves_strength=="High", 1.30, stop("invalid input for change in wave frequency strength")))
          rates <- seq(1, val, length=input$nyears)
          S_prerecruits_new <- S_prerecruits_new*rates
          S_prerecruits_val_vec <- c(S_prerecruits_val_vec, mean(rates))
          Outs[["inc_waves"]] <- project_fn(nyears=input$nyears, u_t=u_t_new, S_eggs=S_eggs_new, S_prerecruits=S_prerecruits_new, S_recruits=S_recruits_new, fecundity=fecundity_new, plus_yrs=plus_yrs_new, prop_spawners=prop_spawners_new, rate_Mpre=TRUE)
        }
        if(names(scenarios[i])=="pollution" & scenarios[[i]]==TRUE){
          ## scenario-specific rates
          u_t_new=rep(input$u_equil, input$nyears)
          S_eggs_new=input$S_eggs
          S_prerecruits_new=input$S_prerecruits
          S_recruits_new=input$S_recruits
          fecundity_new=input$fecundity
          plus_yrs_new=input$plus_yrs
          prop_spawners_new=input$prop_spawners
          
          val <- ifelse(input$pollution_strength=="Low", 1.10, ifelse(input$pollution_strength=="High", 1.30, stop("invalid input for change in wave frequency strength")))
          S_prerecruits_new <- S_prerecruits_new*val
          S_recruits_new <- S_recruits_new*val
          S_prerecruits_val_vec <- c(S_prerecruits_val_vec, val)
          S_recruits_val_vec <- c(S_recruits_val_vec, val)
          Outs[["pollution"]] <- project_fn(nyears=input$nyears, u_t=u_t_new, S_eggs=S_eggs_new, S_prerecruits=S_prerecruits_new, S_recruits=S_recruits_new, fecundity=fecundity_new, plus_yrs=plus_yrs_new, prop_spawners=prop_spawners_new)
        }
        if(names(scenarios[i])=="dec_habitat" & scenarios[[i]]==TRUE){
          ## scenario-specific rates
          u_t_new=rep(input$u_equil, input$nyears)
          S_eggs_new=input$S_eggs
          S_prerecruits_new=input$S_prerecruits
          S_recruits_new=input$S_recruits
          fecundity_new=input$fecundity
          plus_yrs_new=input$plus_yrs
          prop_spawners_new=input$prop_spawners
          
          val <- ifelse(input$dec_habitat_strength=="Low", 1.05, ifelse(input$dec_habitat_strength=="High", 1.20, stop("invalid input for change in wave frequency strength")))
          rates <- seq(1, val, length=input$nyears)
          S_prerecruits_new <- S_prerecruits_new*rates
          S_recruits_new <- S_recruits_new*rates
          S_prerecruits_val_vec <- c(S_prerecruits_val_vec, mean(rates))
          S_recruits_val_vec <- c(S_recruits_val_vec, mean(rates))
          Outs[["dec_habitat"]] <- project_fn(nyears=input$nyears, u_t=u_t_new, S_eggs=S_eggs_new, S_prerecruits=S_prerecruits_new, S_recruits=S_recruits_new, fecundity=fecundity_new, plus_yrs=plus_yrs_new, prop_spawners=prop_spawners_new, rate_Mpre=TRUE, rate_Mrec=TRUE)
        }
        if(names(scenarios[i])=="HABs" & scenarios[[i]]==TRUE){
          ## scenario-specific rates
          u_t_new=rep(input$u_equil, input$nyears)
          S_eggs_new=input$S_eggs
          S_prerecruits_new=input$S_prerecruits
          S_recruits_new=input$S_recruits
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
          Outs[["HABs"]] <- project_fn(nyears=input$nyears, u_t=u_t_new, S_eggs=S_eggs_new, S_prerecruits=S_prerecruits_new, S_recruits=S_recruits_new, fecundity=fecundity_new, plus_yrs=plus_yrs_new, prop_spawners=prop_spawners_new)
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
      
      S_eggs_all=input$S_eggs
      if(length(S_prerecruits_val_vec)==0) S_prerecruits_all <- input$S_prerecruits
      if(length(S_prerecruits_val_vec)>0){ 
        for(i in 1:length(S_prerecruits_val_vec)){
          if(i==1) S_prerecruits_all=input$S_prerecruits*S_prerecruits_val_vec[i]
          if(i>1) S_prerecruits_all <- S_prerecruits_all*S_prerecruits_val_vec[i]
        }
      }
      if(length(S_recruits_val_vec)==0) S_recruits_all <- input$S_recruits
      if(length(S_recruits_val_vec)>0){
        for(i in 1:length(S_recruits_val_vec)){
          if(i==1) S_recruits_all=input$S_recruits*S_recruits_val_vec[i]
          if(i>1) S_recruits_all <- S_recruits_all*S_recruits_val_vec[i]
        }
      }
      fecundity_all=input$fecundity
      plus_yrs_all=input$plus_yrs
      prop_spawners_all=input$prop_spawners
      
      Outs[["total"]] <- project_fn(nyears=input$nyears, u_t=u_t_all, S_eggs=S_eggs_all, S_prerecruits=S_prerecruits_all, S_recruits=S_recruits_all, fecundity=fecundity_all, plus_yrs=plus_yrs_all, prop_spawners=prop_spawners_all)
    }
    return(Outs)
  }

# capacity <- 1  ## metric for beach capacity
# quality <- 1	 ## metric for beach quality

################ EXPLORE POPULATION DYNAMICS #################

nyears <- 20
F0 <- rep(0, nyears)
F_low <- rep(0.3, nyears)
F_med <- rep(0.7, nyears)
F_high <- rep(2, nyears)
F_altlow <- c(rep(c(0.7, 0), (nyears/2)))
F_althigh <- c(rep(c(2, 0), (nyears/2)))

res_list <- list()
res_list$m0 <- project_fn(nyears=nyears, F_t=F0, M_eggs=clam_input$M_eggs, M_prerecruits=clam_input$M_prerecruits, M_recruits=clam_input$M_recruits)

res_list$m1 <- project_fn(nyears=nyears, F_t=F_low, M_eggs=clam_input$M_eggs, M_prerecruits=clam_input$M_prerecruits, M_recruits=clam_input$M_recruits)

res_list$m2 <- project_fn(nyears=nyears, F_t=F_med, M_eggs=clam_input$M_eggs, M_prerecruits=clam_input$M_prerecruits, M_recruits=clam_input$M_recruits)

res_list$m3 <- project_fn(nyears=nyears, F_t=F_high, M_eggs=clam_input$M_eggs, M_prerecruits=clam_input$M_prerecruits, M_recruits=clam_input$M_recruits)

res_list$m4 <- project_fn(nyears=nyears, F_t=F_altlow, M_eggs=clam_input$M_eggs, M_prerecruits=clam_input$M_prerecruits, M_recruits=clam_input$M_recruits)

res_list$m5 <- project_fn(nyears=nyears, F_t=F_althigh, M_eggs=clam_input$M_eggs, M_prerecruits=clam_input$M_prerecruits, M_recruits=clam_input$M_recruits)

##### --------- plot relative population size ------

with(res_list, plot(relative(m0$pmat[,"R"] + m0$pmat[,"P"]), type="l", col=1, lwd=2, ylim=c(0,1.2), xaxs="i", yaxs="i", main="Relative population size", xlab="Year into future", ylab="Relative population size (pre-recruits + recruits)"))
with(res_list, lines(relative(m1$pmat[,"R"] + m1$pmat[,"P"]), col=2, lwd=2))
with(res_list, lines(relative(m2$pmat[,"R"] + m2$pmat[,"P"]), col=3, lwd=2))
with(res_list, lines(relative(m3$pmat[,"R"] + m3$pmat[,"P"]), col=4, lwd=2))
with(res_list, lines(relative(m4$pmat[,"R"] + m4$pmat[,"P"]), col=5, lwd=2))
with(res_list, lines(relative(m5$pmat[,"R"] + m5$pmat[,"P"]), col=6, lwd=2))

##### --------- Plot population growth rate -----

with(res_list, plot(m0$pmat[,"lambda"], type="l", col=1, lwd=2, ylim=c(0,1.2), xaxs="i", yaxs="i", main="Population growth rate", xlab="Year into future", ylab="Population growth rate"))
with(res_list, lines(m1$pmat[,"lambda"], col=2, lwd=2))
with(res_list, lines(m2$pmat[,"lambda"], col=3, lwd=2))
with(res_list, lines(m3$pmat[,"lambda"], col=4, lwd=2))
with(res_list, lines(m4$pmat[,"lambda"], col=5, lwd=2))
with(res_list, lines(m5$pmat[,"lambda"], col=6, lwd=2))

##### --------- Plot expected catch -----

par(mfrow=c(1,2))
with(res_list, plot(m0$pmat[,"catch"], type="l", col=1, lwd=2, ylim=c(-0.02,0.5), xaxs="i", yaxs="i", main="Expected catch over time", xlab="Year into future", ylab="Catch"))
with(res_list, lines(m1$pmat[,"catch"], col=2, lwd=2))
with(res_list, lines(m2$pmat[,"catch"], col=3, lwd=2))
with(res_list, lines(m3$pmat[,"catch"], col=4, lwd=2))
with(res_list, lines(m4$pmat[,"catch"], col=5, lwd=2))
with(res_list, lines(m5$pmat[,"catch"], col=6, lwd=2))

barplot(sapply(1:length(res_list), function(x) sum(res_list[[x]]$pmat[,"catch"])), col=1:6, main="Expected total catch")
