#' #Introduction

#' Data: particleplacement_icegb.csv  

#' Information from 104_datafiles.pdf about variables:  
#' **CONSTRUCTION**: the dependent variable, the choice of construction with a phrasal verb:   
#' ___ **V_PRT_DO** (e.g., He picked the dirt up) vs. **V_DO_PRT** (e.g., He picked it up)   
#' **MODE**: the medium in which the construction was used: spoken vs. written  
#' **REGISTER**: the register in which the construction was used:  
#' ___ monolog vs. dialog vs. mix vs. printed vs. nonprinted  
#' **SUBREGISTER**: the subregister in which the construction was used  
#' **PARTICLE**: the particle of the transitive phrasal verb  
#' **TYPE**: the type of DO: a.other vs. b.lexical vs. c.pronoun vs. d.quantifier  
#' **LENGTH**: the length of the DO in words    

#' Question: which independent variables, if any, influence the dependent variable's value? 

rm(list=ls(all=TRUE)) # clear memory

# sets the working directory, works locally
# might have to change it for other machines
setwd("~/LING 104/particle placement 2") 

summary(x <- read.delim("particleplacement_icegb.csv"))

#' #Data Exploration  

# for visibility
colors <- c("dark blue", "dark green", "red")

#' #Mode  

barplot(table(x$CONSTRUCTION, x$MODE), 
        ylab = "number", 
        xlab = "spoken v written", 
        beside = TRUE,
        legend = TRUE,
        col = colors[1:2])
# shows that there is a big difference between mode type and construction
# spoken mode tends to have more V_DO_PRT, while written has more V_PRT_DO
# there are also fewer datapoints for the written mode

#' #Register

# barplot looks fine
barplot(table(x$CONSTRUCTION, x$REGISTER), 
        ylab = "number", 
        xlab = "register",
        beside = TRUE,
        legend = TRUE,
        col = colors[1:2])

#' #Subregister

# this is freaky and overwhelming
table(x$CONSTRUCTION, x$SUBREGISTER)

# doesn't look like all register labels are represented
barplot(table(x$CONSTRUCTION, x$SUBREGISTER), 
        ylab = "number", 
        xlab = "subregister",
        legend = TRUE,
        col = colors[1:2])

# not helpful
plot(x$CONSTRUCTION, x$SUBREGISTER, pch = 19)

# sort of helpful? really confusing too
mosaicplot(table(x$SUBREGISTER, x$CONSTRUCTION), shade = TRUE)

#' #Particle

# says very little, will need a different formatting
barplot(table(x$CONSTRUCTION, x$PARTICLE), 
        ylab = "number", 
        legend = TRUE,
        col = colors[1:2])

#' #Type

# again, needs different formatting to see what's going on
barplot(table(x$CONSTRUCTION, x$TYPE), 
        ylab = "number", 
        legend = TRUE,
        col = colors[1:2])


#' #Length

# this is funny, might need another format, but looks fine ish
barplot(table(x$CONSTRUCTION, x$LENGTH), 
        ylab = "number", 
        legend = TRUE,
        col = colors[1:2])

# trying a glm (with 3 way interactions between all variables) makes R very sad
# tree is much faster

library(tree)
set.seed(42)

attach(x)

# creating a full tree
summary(cart.01 <- tree(CONSTRUCTION ~ MODE+REGISTER+SUBREGISTER+PARTICLE+TYPE+LENGTH))

# looking at the first tree's structure
plot(cart.01)
    text(cart.01, pretty = 0)
    