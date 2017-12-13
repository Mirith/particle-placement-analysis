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

# barplot looks almost fine
barplot(table(x$CONSTRUCTION, x$REGISTER), 
        xlab = "number",
        beside = TRUE,
        legend = TRUE,
        horiz = TRUE,
        las = 1,
        col = colors[1:2])

# almost okay
barplot(table(x$CONSTRUCTION, x$REGISTER), 
        legend = TRUE,
        col = colors[1:2])


# Pie Chart with Percentages
# tweak this, this could work
slices <- c(10, 12, 4, 16, 8) 
lbls <- c("US", "UK", "Australia", "Germany", "France")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Pie Chart of Countries")

#' #Subregister

# this is freaky and overwhelming
table(x$CONSTRUCTION, x$SUBREGISTER)

# doesn't look like all register labels are represented
# probably don't need to see all of them anyway
barplot(table(x$CONSTRUCTION, x$SUBREGISTER), 
        ylab = "number", 
        xlab = "subregister",
        legend = TRUE,
        col = colors[1:2])
# there are a few really high spikes which is slightly concerning

#' #Particle

barplot(table(x$CONSTRUCTION, x$PARTICLE), 
        ylab = "number", 
        legend = TRUE,
        col = colors[1:2])
# looks pretty messy
# will need a different format

mosaicplot(table(x$PARTICLE, x$CONSTRUCTION))

library(ggplot2)
# install.packages("ggdendro")
library(ggdendro)

theme_set(theme_bw())

hc <- hclust(dist(table(x$PARTICLE, x$CONSTRUCTION)), "ave")
ggdendrogram(hc, rotate = TRUE, size = 2)
# clear, but interpretable? 

#' #Type

# again, needs different formatting to see what's going on
barplot(table(x$CONSTRUCTION, x$TYPE), 
        ylab = "number", 
        legend = TRUE,
        col = colors[1:2])


#' #Length

# prop.table gives probability compared to entire table, not useful
barplot(table(x$CONSTRUCTION, x$LENGTH), 
        ylab = "percentage", 
        xlab = "length",
        legend = TRUE,
        col = colors[1:2])
# as length increases, the likelihood that a speaker will choose V_DO_PRT over V_PRT_DO increases


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
    