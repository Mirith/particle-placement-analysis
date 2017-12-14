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

################################################################################
# V_PRT_DO v V_DO_PRT -- what do those signify?  Particle and direct object?
# example is unclear

rm(list=ls(all=TRUE)) # clear memory

# sets the working directory, works locally
# might have to change it for other machines
setwd("~/LING 104/particle placement 2") 

summary(x <- read.delim("particleplacement_icegb.csv"))

#' #Data Exploration  

# for visibility
colors <- c("dark blue", "dark green", "red")

#' ##Mode  

barplot(table(x$CONSTRUCTION, x$MODE), 
        ylab = "number", 
        xlab = "spoken v written", 
        beside = TRUE,
        legend = TRUE,
        col = colors[1:2])
# shows that there is a big difference between mode type and construction
# spoken mode tends to have more V_DO_PRT, while written has more V_PRT_DO
# there are also fewer datapoints for the written mode

#' ##Register

# barplot looks fine with adjusted margins
# default par(mar=c(5.1, 4.1, 4.1, 2.1)) 
# bottom, left, top, right

par(mar=c(5.1, 5.5, 4.1, 2.1))

barplot(table(x$CONSTRUCTION, x$REGISTER),
        xlab = "number",
        beside = TRUE,
        legend = TRUE,
        horiz = TRUE,
        las = 1,
        col = colors[1:2])

#' ##Subregister

table(x$SUBREGISTER, x$CONSTRUCTION)
# lots of variance in numbers, hard to see an overall trend

# readjusting margins to fit all labels on 
par(mar=c(5.1, 7.1, 4.1, 2.1))

barplot(table(x$CONSTRUCTION, x$SUBREGISTER), 
        horiz = TRUE,
        las = 1,
        xlab = "number",
        legend = TRUE,
        args.legend = "bottomleft",
        col = colors[1:2])
# there are a few really high spikes which is slightly concerning
# generally, smaller bars are dominated by green (V_PRT_DO) 
# while larger are dominated by blue (V_DO_PRT)

#' ##Particle

# adjusting margin again to fit all labels
par(mar=c(3, 7.1, 0, 2.1))

barplot(table(x$CONSTRUCTION, x$PARTICLE), 
        horiz = TRUE,
        cex.names = .7,
        las = 1,
        legend = TRUE,
        col = colors[1:2])
# kind of difficult to see, but
# there is a huge variance in number of data points for each particle
# overall, bars look to be split fairly evenly though

#' ##Type

table(x$TYPE, x$CONSTRUCTION)
# very big variance in numbers here; 2 to 1097
# overall, numbers are not split evenly within rows

# again, needs different formatting to see what's going on
barplot(table(x$CONSTRUCTION, x$TYPE), 
        ylab = "number", 
        legend = TRUE,
        col = colors[1:2])
# doesn't really help... 
# need some other thing to help visualize   


#' ##Length

barplot(table(x$CONSTRUCTION, x$LENGTH), 
        ylab = "percentage", 
        xlab = "length",
        legend = TRUE,
        col = colors[1:2])
# as length increases, the likelihood that a speaker will choose V_DO_PRT over V_PRT_DO increases

#' ###Overall concerns
#' seems like there are a lot of variables, and a wide range of numbers  
#' ex some types have a total of 7 entries while another has more than 1500  

#' #Making a tree

# trying a glm (with 3 way interactions between all variables) makes R very sad
# tree is much faster

library(tree)
set.seed(42)

attach(x)

# creating a full tree with everything in it
summary(cart.1 <- tree(CONSTRUCTION ~ MODE+REGISTER+SUBREGISTER+PARTICLE+TYPE+LENGTH))

# resetting margins
par(mar=c(4.1, 4.1, 3.1, 2.1)) 

# looking at the first tree's structure
plot(cart.1)
    text(cart.1, pretty = 5)
# labels are really hard to read, but overall looks like length and type are 
# influential predictors based on their relatively high position in the tree

predictions.num <- predict(cart.1)  
predictions.cat <- predict(cart.1, type = "class") 

#' ##Classification accuracy

#' Actual versus prediction
table(CONSTRUCTION, predictions.cat) 

#' **Accuracy**:
(856 + 1092) / length(predictions.cat)
# main diagonal of previous table added up
# accuracy looks pretty good

#' **Precision**:
1092 / (1092 + 336) 

#' **Recall/sensitivity**:
1092 / (1092 + 37) 

#' **F**:
2*((0.7647059*0.9672276)/(0.7647059+0.9672276)) 

#' ##Making training/testing set to apply to data
#' 2321 total data entries, about 75% training, 25% testing
sampler <- sample(rep(c("training", "test"), c(1740, 581)))

# tree from training data
cart.validation.training <- tree(formula(cart.1),              
                                 data=x[sampler=="training",])

# making predictions for test data based on training data
predictions.validation.test <- predict(cart.validation.training, 
                                       newdata=x[sampler=="test",], 
                                       type="class")

# percentage of times the predictions matched the actual data
sum(predictions.validation.test ==     
        CONSTRUCTION[sampler=="test"]) /   
    length(predictions.validation.test) 
# seems good

#' #Pruning (if necessary)

# based on number of misclassifications
pruning <- cv.tree(cart.1, FUN = prune.misclass)

plot(pruning$size, pruning$dev, type="b"); grid()
#' The deviances are lowest for 5 and 6 nodes
#' but 6 is the same tree... so using the tree with 5 nodes

# pruned to 5 terminal nodes
cart.1.pruned <- prune.misclass(cart.1, best=5) 

# plot the new, pruned tree
plot(cart.1.pruned)
text(cart.1.pruned, pretty = 1, all=TRUE) 
# still really messy

# new tree's performance
predictions.cat.pruned <- predict(cart.1.pruned, type="class") 

#' ##Testing new accuracy
table(CONSTRUCTION, predictions.cat.pruned) 

# numbers in the table are actually exactly the same as original tree
# so all calculations will remain the same
# ie:

#' **Accuracy**:
(856 + 1092) / length(predictions.cat)

#' **Precision**:
1092 / (1092 + 336) 

#' **Recall/sensitivity**:
1092 / (1092 + 37) 

#' **F**:
2*((0.7647059*0.9672276)/(0.7647059+0.9672276)) 

#' ##Testing with subsets of data

# same training data as earlier and the pruned tree
cart.validation.training.1 <- tree(formula(cart.1.pruned), 
                                   data=x[sampler=="training",])

# making predictions for test data based on training data
predictions.validation.test.1 <- predict(cart.validation.training.1, 
                                         newdata=x[sampler=="test",], 
                                         type="class")

sum(predictions.validation.test.1 ==     
        CONSTRUCTION[sampler=="test"]) /   
    length(predictions.validation.test.1) 
# also same number 

#' ##Pruning more
#' seemed like 4 nodes also was pretty low
#' let's try that

pruning.2 <- cv.tree(cart.1.pruned, FUN = prune.misclass)

plot(pruning.2$size, pruning.2$dev, type="b"); grid()
#' The deviances are lowest for 4 and 5 nodes
#' but 5 is the same tree... so using the tree with 4 nodes

# pruned to 4 terminal nodes
cart.2.pruned <- prune.misclass(cart.1.pruned, best=4) 

# plot the new, pruned tree
plot(cart.2.pruned)
    text(cart.2.pruned, pretty = 1, all=TRUE) 
# still really messy

# new tree's performance
predictions.cat.pruned.2 <- predict(cart.2.pruned, type="class") 

#' ###Testing new accuracy
table(CONSTRUCTION, predictions.cat.pruned.2) 

# very slightly different numbers
# same calculations as with original tree:

#' **Accuracy**:
(859 + 1082) / length(predictions.cat.pruned.2)
# was 0.8392934
# negligible difference

#' **Precision**:
1082 / (1082 + 333) 
# was 0.7647059
# negligible difference

#' **Recall/sensitivity**:
1082 / (1082 + 47) 
# was 0.9672276
# negligible difference

#' **F**:
2*((0.7646643*0.9583702)/(0.7646643+0.9583702)) 
# was 0.8541259
# negligible difference

#' ###Testing with subsets of data

# same training data as earlier and the pruned tree
cart.validation.training.2 <- tree(formula(cart.2.pruned), 
                                   data=x[sampler=="training",])

# making predictions for test data based on training data
predictions.validation.test.2 <- predict(cart.validation.training.2, 
                                         newdata=x[sampler=="test",], 
                                         type="class")

sum(predictions.validation.test.2 ==     
        CONSTRUCTION[sampler=="test"]) /   
    length(predictions.validation.test.1) 
# was 0.8209983
# no difference

# so pruning even further sacrifices basically nothing,
# but improves tree's simplicity/readability

#' #Conclusion
#' The third tree (cart.2.pruned) pruned to 4 terminal nodes has okay classification accuracy (0.8362775) but very high recall/sensitivity (0.9583702).  Pruning the original tree (cart.1) did not increase prediction accuracy on the training/testing set at all though.  It did not decrease it though, so overall pruning it a little was good in that it decreased the tree's complications and made it easier to interpret.  
#' The tree shows that type is an influential predictor, given that the lines to the next nodes are relatively very long.  
#' It also shows that even with six independent variables, some with many levels, only a few of them really make a difference in predictions.  Type, length, and particle influence prediction choices, and only a subset of those variables at that.  Every other variable/variable level not included in the tree can be assumed to not have enough predictive power to be included.  So things like register and subregister are not influential at all.  