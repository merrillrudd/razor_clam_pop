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
F_alt <- c(rep(c(2, 0), (nyears/2)))

m0 <- project_fn(nyears=nyears, F_t=F0, M_eggs=clam_input$M_eggs, M_prerecruits=clam_input$M_prerecruits, M_recruits=clam_input$M_recruits)

m1 <- project_fn(nyears=nyears, F_t=F_low, M_eggs=clam_input$M_eggs, M_prerecruits=clam_input$M_prerecruits, M_recruits=clam_input$M_recruits)

m2 <- project_fn(nyears=nyears, F_t=F_med, M_eggs=clam_input$M_eggs, M_prerecruits=clam_input$M_prerecruits, M_recruits=clam_input$M_recruits)

m3 <- project_fn(nyears=nyears, F_t=F_high, M_eggs=clam_input$M_eggs, M_prerecruits=clam_input$M_prerecruits, M_recruits=clam_input$M_recruits)

m4 <- project_fn(nyears=nyears, F_t=F_alt, M_eggs=clam_input$M_eggs, M_prerecruits=clam_input$M_prerecruits, M_recruits=clam_input$M_recruits)
