##############################################
# SIENA lab exercise 5
# Networks course in Leuven, March 2018
#
# This is a short script for analysing the co-evolution of the friendship 
# network and the trust network behaviour. We use the RECENS data for this
# exercise.
#
# The script was prepared by Andras Voros based on earlier scripts by
# Christoph Stadtfeld.
#
# Do not forget the STEPS:
# 1. read data from files
# 1.5 calculate network descriptives
# 2. create SIENA objects
# 3. specify SIENA model
# 4. create estimation algorithm
# 5. estimate SIENA model
#
######### Some preparation steps #########
#
# you can clean you workspace
rm(list=ls())
#
# and set working directory
setwd("")
getwd()
#
# load the RSiena library
library("RSiena")
library("igraph")

####################################################
# Step 1: load and recode the data
####################################################

# load the data
# I present the analysis of class 1100, but you should work on
# the classroom assigned to you on the first day.
# first replace classroom IDs below with you own (you can use find-replace or do it by hand)

# read the affective network at t1 and t2
affective_w1 <- as.matrix(read.csv("RECENS_data\\1100_affective_w1.csv", header=TRUE, row.names=1))
affective_w2 <- as.matrix(read.csv("RECENS_data\\1100_affective_w2.csv", header=TRUE, row.names=1))
# read the trust network at t1 and t2
trust_w1 <- as.matrix(read.csv("RECENS_data\\1100_trust_w1.csv", header=TRUE, row.names=1))
trust_w2 <- as.matrix(read.csv("RECENS_data\\1100_trust_w2.csv", header=TRUE, row.names=1))
# read the gender variable
sex <- as.matrix(read.csv("RECENS_data\\1100_sex.csv", header=TRUE, row.names=1))

# recode to friendship at t1
friend_w1 <- affective_w1
friend_w1[friend_w1 %in% c(-2, -1, 0, 1)] <- 0
friend_w1[friend_w1==2] <- 1
# recode to friendship at t2
friend_w2 <- affective_w2
friend_w2[friend_w2 %in% c(-2, -1, 0, 1)] <- 0
friend_w2[friend_w2==2] <- 1


####################################################
# Step 1.5: check some descriptives
####################################################

# Let's check how the networks look like!
g1 <- graph.adjacency(friend_w1)
g2 <- graph.adjacency(friend_w2)
g3 <- graph.adjacency(trust_w1)
g4 <- graph.adjacency(trust_w2)
g1234 <- graph.adjacency(friend_w1 + friend_w2 + trust_w1 + trust_w2)

myLayout <- layout.auto(g1234)

par(mfrow = c(2, 2))
plot(g1,
     vertex.color = ifelse(sex == 2, "red", "darkblue"),
     vertex.shape = ifelse(sex == 2, "square", "circle"),
     edge.color = "black",
     edge.width = 2,
     edge.arrow.size = 0.1,
     vertex.size = 5,
     vertex.label = "",
     layout = myLayout,
     main = "Friendship network wave 1")
plot(g2,
     vertex.color = ifelse(sex == 2, "red", "darkblue"),
     vertex.shape = ifelse(sex == 2, "square", "circle"),
     edge.color = "black",
     edge.width = 2,
     edge.arrow.size = 0.1,
     vertex.size = 5,
     vertex.label = "",
     layout = myLayout,
     main = "Friendship network wave 2")
plot(g3,
     vertex.color = ifelse(sex == 2, "red", "darkblue"),
     vertex.shape = ifelse(sex == 2, "square", "circle"),
     edge.color = "black",
     edge.width = 2,
     edge.arrow.size = 0.1,
     vertex.size = 5,
     vertex.label = "",
     layout = myLayout,
     main = "Trust network wave 1")
plot(g4,
     vertex.color = ifelse(sex == 2, "red", "darkblue"),
     vertex.shape = ifelse(sex == 2, "square", "circle"),
     edge.color = "black",
     edge.width = 2,
     edge.arrow.size = 0.1,
     vertex.size = 5,
     vertex.label = "",
     layout = myLayout,
     main = "Trust network wave 2")
par(mfrow = c(1, 1))

# now a combined view
friendtrust_w1 <- friend_w1 + 2*trust_w1
friendtrust_w2 <- friend_w2 + 2*trust_w2
# now we have a network for each wave in which a friendship tie without a trust
# ties has a value of 1, a trust tie only has a value of 2, and friend&trust ties have
# a value of 3.
ft1 <- graph.adjacency(friendtrust_w1, weighted=TRUE)
ft2 <- graph.adjacency(friendtrust_w2, weighted=TRUE)

par(mfrow = c(1, 2))
plot(ft1,
     vertex.color = ifelse(sex == 2, "red", "darkblue"),
     vertex.shape = ifelse(sex == 2, "square", "circle"),
     edge.color = ifelse(E(ft1)$weight == 1, "black", ifelse(E(ft1)$weight == 2, "grey", "green")),
     edge.width = 2,
     edge.arrow.size = 0.1,
     vertex.size = 5,
     vertex.label = "",
     layout = myLayout,
     main = "Friendship and trust networks wave 1")
plot(ft2,
     vertex.color = ifelse(sex == 2, "red", "darkblue"),
     vertex.shape = ifelse(sex == 2, "square", "circle"),
     edge.color = ifelse(E(ft2)$weight == 1, "black", ifelse(E(ft2)$weight == 2, "grey", "green")),
     edge.width = 2,
     edge.arrow.size = 0.1,
     vertex.size = 5,
     vertex.label = "",
     layout = myLayout,
     main = "Friendship and trust networks wave 2")
par(mfrow = c(1, 1))


# assessing overlap and stability - with numbers

# Define this function
# (by Tom Snijders, it is from one of the example scripts on the Siena website:
# http://www.stats.ox.ac.uk/~snijders/siena/RscriptSienaMultiple.R)
Jaccard <- function(var1,var2){
  # a and b should be 0-1 arrays
  tab <- table(var1,var2)
  print(tab)
  pa <- tab["1","1"] + tab["1","0"]
  pb <- tab["1","1"] + tab["0","1"]
  pab_ind <- pa*pb/sum(tab)
  J_ind <- pab_ind / (pa + pb - pab_ind)
  cat("Expected Jaccard under independence with these marginals =",
      J_ind, "\n")
  cat("Observed Jaccard = ")
  tab["1","1"]/(tab["1","1"] + tab["0","1"] + tab["1","0"])
}

# and make different comparisons
Jaccard(friend_w1, friend_w2)
Jaccard(trust_w1, trust_w2)
Jaccard(friend_w1, trust_w1)
Jaccard(friend_w1, trust_w2)


####################################################
# Step 2: create SIENA objects    
####################################################

# the first dependent network - friendship
friendship.dependent1 <- sienaDependent(array(c(friend_w1, friend_w2), dim=c(31, 31, 2)))
# the second dependent network - trust
trust.dependent2 <- sienaDependent(array(c(trust_w1, trust_w2), dim=c(31, 31, 2)))
# constant covariate - gender
gender.coCovar <- coCovar(sex[,1])

# create siena data object
sienaData <- sienaDataCreate(friendship.dependent1,
                             trust.dependent2,
                             gender.coCovar)

# print initial report to file
print01Report(sienaData, modelname="RECENS_multiplex_report")


####################################################
# Step 3: specify SIENA model
####################################################

sienaEffects <- getEffects(sienaData)

# the default specification for multiplex data
sienaEffects

# let's look at the available effects
effectsDocumentation(sienaEffects)

# first it makes sense to include effects for the two networks separately
sienaEffects <- includeEffects(sienaEffects, name="friendship.dependent1",
                               transTrip, inPop, outPop)
sienaEffects <- includeEffects(sienaEffects, name="trust.dependent2",
                               transTrip, inPop, outPop)
sienaEffects <- includeEffects(sienaEffects, name="friendship.dependent1",
                               egoX, altX, sameX, interaction1="gender.coCovar")
sienaEffects <- includeEffects(sienaEffects, name="trust.dependent2",
                               egoX, altX, sameX, interaction1="gender.coCovar")

# then add the basic dyadic cross-production effect
sienaEffects <- includeEffects(sienaEffects, name="friendship.dependent1",
                               crprod,
                               interaction1="trust.dependent2")
sienaEffects <- includeEffects(sienaEffects, name="trust.dependent2",
                               crprod,
                               interaction1="friendship.dependent1")

# and control for degree correlations between the two networks
# (note: these are square root effects by default- they usually fit better and
#        are more stable)
sienaEffects <- includeEffects(sienaEffects, name="friendship.dependent1",
                               outActIntn,
                               interaction1="trust.dependent2")
sienaEffects <- includeEffects(sienaEffects, name="trust.dependent2",
                               outActIntn,
                               interaction1="friendship.dependent1")

# look at the specification
sienaEffects


####################################################
#####  Step 4: create estimation algorithm     #####
####################################################

sienaAlgorithm <- sienaAlgorithmCreate(projname="RECENS_multiplex_algo")


####################################################
######      Step 5: estimate SIENA model       #####
####################################################

# estimate model
result <- siena07(sienaAlgorithm, data = sienaData, effects = sienaEffects)
result

# rerun estimation if model has not converged
result <- siena07(sienaAlgorithm, data = sienaData, effects = sienaEffects, prevAns=result)
result

# The results:
siena.table(result, type = "html", file = "multiplex_results.html", tstatPrint=T, sig=T, d=2)

# for this classroom, the model is almost reasonable...
# however, the model is also quite large and
# we get some very big standard errors - not enough info in the data?

# in my experience, modelling the degree correlations is the hardest part,
# especially on these small datasets;
# it really depends on the analyzed network which effects work and which don't
# (and which will ruin your entire model).

# Let's discuss if we have some interesting research questions related to the
# co-evolution of friendship and trust in the classrooms that you can explore
# in your class!
# Then, consult with the manual to see if the effects required to answer the
# question are already programmed.
