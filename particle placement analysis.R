#' #Introduction

#' Data: particleplacement_icegb.csv

#' Information from 104_datafiles.pdf about variables:   
#' **MODE**: the medium in which the construction was used: spoken vs. written  
#' **REGISTER**: the register in which the construction was used:  
#'     monolog vs. dialog vs. mix vs. printed vs. nonprinted  
#' **SUBREGISTER**: the subregister in which the construction was used  
#' **PARTICLE**: the particle of the transitive phrasal verb  
#' **CONSTRUCTION**: the dependent variable, the choice of construction with a phrasal verb:   
#'     **V_PRT_DO** (e.g., He picked the dirt up) vs. **V_DO_PRT** (e.g., He picked it up)  
#' **TYPE**: the type of DO: a.other vs. b.lexical vs. c.pronoun vs. d.quantifier  
#' **LENGTH**: the length of the DO in words    

#' Question: 

rm(list=ls(all=TRUE)) # clear memory
setwd("~/LING 104/particle placement 2") # sets the working directory, works locally
    # might have to change it for other machines

summary(x <- read.delim("particleplacement_icegb.csv"))

# explore data

# trying a glm (with 3 way interactions between all variables) makes R very sad
# tree is much faster

# do tree

library(tree)
set.seed(42)

attach(x)

# creating a full tree
summary(cart.01 <- tree(CONSTRUCTION ~ MODE+REGISTER+SUBREGISTER+PARTICLE+TYPE+LENGTH))

# looking at the first tree's structure
plot(cart.01)
    text(cart.01, pretty = 0)
    