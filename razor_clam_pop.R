#########################################################
## razor clam stage-based population model
#########################################################

#########   DIRECTORIES AND PACKAGES  ###################

rm(list=ls()) ## clear environment
setwd("C:\\Git_Projects\\razor_clam_pop") ## set working directory

################ FUNCTIONS ###############################

create_lefko <- function(M_eggs, M_prerecruits, M_recruits, F, fecundity, plus_yrs, prop_spawners){
	lefko <- matrix(0, nrow=length(stages), ncol=length(stages))
	rownames(lefko) <- colnames(lefko) <- stages
	lefko["R","P"] <- tmat["R","P"] * exp(-M_prerecruits)
	lefko["P","P"] <- tmat["P","P"] * exp(-M_prerecruits)
	lefko["R","R"] <- tmat["R","R"] * exp(-M_recruits - F) * (1-exp(-M_recruits - F)^(plus_yrs - 1))/(1-(exp(-M_recruits - F)^plus_yrs))
	lefko["P","R"] <- fecundity * exp(-M_eggs) * prop_spawners

	return(lefko)
}

catch_fn <- function(abundance, F, M){
	catch <- abundance * F * (1 - exp(-M-F) )/(M + F)
	return(catch)
}

project_fn <- function(nyears, F_t, M_eggs, M_prerecruits, M_recruits){
	pmat <- matrix(NA, nrow=nyears, ncol=4)
	colnames(pmat) <- c(stages, "lambda", "catch")

	lefko1 <- create_lefko(M_eggs=M_eggs, M_prerecruits=M_prerecruits, M_recruits=M_recruits, F=F_t[1], fecundity=clam_input$fecundity, plus_yrs=clam_input$plus_yrs, prop_spawners=clam_input$prop_spawners)
	e1 <- eigen(lefko1)
	pmat[1,stages] <- abs(e1$vectors[,1])
	pmat[1,"lambda"] <- e1$value[1]
	pmat[1,"catch"] <- catch_fn(abundance=pmat[1,"R"], F=F_t[1], M=M_recruits)

	for(i in 2:nyears){	

		lefko <- create_lefko(M_eggs=M_eggs, M_prerecruits=M_prerecruits, M_recruits=M_recruits, F=F_t[i], fecundity=clam_input$fecundity, plus_yrs=clam_input$plus_yrs, prop_spawners=clam_input$prop_spawners)
		
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

relative <- function(x){
	x2 <- x/max(x)
	return(x2)
}

################ LIFE HISTORY PARAMETERS #################

clam_input <- list()

##### --------- Transition probabilities ------------  

stages <- c("P", "R") ## pre-recruits and recruits
tmat <- matrix(0, nrow=length(stages), ncol=length(stages))
rownames(tmat) <- colnames(tmat) <- stages
tmat["P","P"] <- 0.05
tmat["R","P"] <- (1-tmat["P","P"])
tmat["R","R"] <- 1

clam_input$stages <- stages
clam_input$transitions <- tmat

##### --------- Mortality rates -----------------------

clam_input$M_eggs <- 1
clam_input$M_prerecruits <- 0.75
clam_input$M_recruits <- 0.5 ## Reidinger paper on Razor clam growth and natural mortality
clam_input$F_recruits <- 0

##### --------- Years in each stage -----------------------

clam_input$plus_yrs <- 3

##### --------- Other factors ------------------------

clam_input$prop_spawners <- 1 	 ## probability of adults reproducing
clam_input$fecundity <- 3 ## 300,000 eggs estimated min from Seafood Watch report (max was 118 million)

# capacity <- 1  ## metric for beach capacity
# quality <- 1	 ## metric for beach quality

################ EXPLORE POPULATION DYNAMICS #################

nyears <- 20
F0 <- rep(0, nyears)
F_low <- rep(0.2, nyears)
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
