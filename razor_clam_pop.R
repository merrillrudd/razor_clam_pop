#########################################################
## razor clam stage-based population model
#########################################################

#########   DIRECTORIES AND PACKAGES  ###################

rm(list=ls()) ## clear environment
setwd("C:\\Git_Projects\\razor_clam_pop") ## set working directory


################ PARAMETERS ##############################

##### --------- Transition probabilities ------------  

## --- probability of staying the same stage
## larva
LL <- 0 

## set
SS <- 0

## juvenile
JJ <- 0.2

## adults
AA <- 1

## --- probability of transitioning stages
## larva to set
LS <- 1

## set to juvenile
SJ <- 1

## juvenile to adult
JA <- 0.8

## ---- probability of reproducing annually
AL <- 1


##### --------- Natural Survival probabilities -------

## egg survival = function of ocean conditions
survE <- 0.001

## larva survival
survL <- 0.05

## set survival
survS <- 0.1

## juvenile survival
survJ <- 0.2

## adult survival
survA <- 0.3

##### ----------- Harvest rates ----------------------

## harvest rate on juveniles
hrJ <- 0.7

## harvest rate on adults
hrA <- 0.9

##### ----------- Other parameters -------------------

## fecundity (number of eggs produced by a single adult annually)
fec <- 4e6

## beach capacity = relative beach size (could increase above 1 from dredging or decrease from erosion or other factors)
capacity <- 1

## beach quality = relative quality (likely maximum at 1, would decrease below 1 due to driving on beach, runoff, horses, etc.)
quality <- 1



################ TRANSITION MATRIX ##############################

stages <- c("L", "S", "J", "A")
tmat <- matrix(0, nrow=length(stages), ncol=length(stages))
rownames(tmat) <- colnames(tmat) <- stages

#### ----------- from larva ---------------------------
## larva to set = probability of transitioning * larva survival * beach capacity metric * beach quality metric
tmat["S","L"] <- LS * survL * capacity * quality

#### ----------- from set -----------------------------
## set to juvenile = probability of transitioning * set survival * beach quality metric
tmat["J","S"] <- SJ * survS * quality

#### ----------- from juvenile ------------------------
## juvenile to juvenile = probability of staying juvenile * juvenile survival * beach quality metric * probability of not being harvested
tmat["J","J"] <- JJ * survJ * quality * (1 - hrJ)

## juvenile to adult = probability of transitioning * juvenile survival * beach quality metric * probability of not being harvested
tmat["A","J"] <- JA * survJ * quality * (1 - hrJ)

#### ----------- from adult ---------------------------
## adult to adult = probability of staying adult * adult survival * beach quality metric * probability of not being harvested
tmat["A","A"] <- AA * survA * quality * (1 - hrA)

## eggs to larva = probability of an adult reproducing * fecundity * egg survival
tmat["L","A"] <- AL * fec * survE



################ ABUNDANCES OVER TIME ##############################

## number of years to project
nyears <- 300

## initial population size by stage in numbers
initial <- c("L"=fec*survE, "S"=fec*survE*survL, "J"=fec*survE*survL*survS, "A"=fec*survE*survL*survJ)

## population matrix
pmat <- matrix(NA, nrow=nyears, ncol=(length(stages)+1))
colnames(pmat) <- c(stages, "lamda")

## start population at initial values in first year
pmat[1,] <- c(initial, NA)

## loop over years, starting with year 2
for(y in 2:nyears){

	## larva = number of adults last year * number of surviving eggs
	pmat[y, "L"] <- pmat[y-1, "A"] * tmat["L", "A"]

	## sets = number of larva last year * joint probability of being a set this year
	pmat[y, "S"] <- pmat[y-1, "L"] * tmat["S", "L"]

	## juveniles = (number of sets last year * joint probability of being a juv this year if you were a set last year) + (number of juveniles last year * joint probability of being a juvenile this year if you were a juv last year)
	pmat[y, "J"] <- (pmat[y-1, "S"] * tmat["J","S"]) + (pmat[y-1, "J"] * tmat["J", "J"])

	## adults = (number of juveniles last year * joint probability of being an adult this year if you were a juv last year) + (number of adults last year * joint probability of being an adult this year if you were an adult last year)
	pmat[y, "A"] <-  (pmat[y-1, "J"] * tmat["A", "J"]) + (pmat[y-1, "A"] * tmat["A", "A"])

	## population growth rate
	pmat[y, "lamda"] <- sum(pmat[y-1,1:length(stages)])/sum(pmat[y,1:length(stages)])

}

## adjust parameter values to get the equilibrium population growth rate to 1
plot(pmat[,"lamda"], type="l")
abline(h=1, col="red", lwd=2)
