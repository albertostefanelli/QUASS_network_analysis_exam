#                       ##################################################                         #
#                       ###   STATISTICAL MODELS FOR SOCIAL NETWORKS   ###                         #
#                       ### EXPONENTIAL RANDOM GRAPH MODELS IN statnet ###                         #
#                       ##################################################                         #

###
# This script was originally made for the course on social network analysis
# held in Leuven on 3-5 March 2018.
#
# The script was prepared by Andras Voros, and it uses some visualization code
# by Christoph Stadtfeld.
# (this is the version of 02.03.2018)
###

###
# This exercise demonstrates the basic usage of the statnet package,
# with a focus on running simple ERGMs.
###


### 1. DOWNLOAD THE REQUIRED PACKAGES

# if you do not have the statnet package installed, install it now
install.packages("statnet")
# do not load it just yet

# there are two other useful packages for plotting:
install.packages("latticeExtra")
install.packages("RColorBrewer")
# statnet functions will automatically load them when first needed


### 2. LOAD THE DATA

# For this practical, we will use the data from a classroom of the Children of
# Immigrants study collected by people in Mannheim. Not very excitingly, this
# dataset contains friendship networks and some basic individual variables.
# However, this is excellent for this demonstration.

# first make sure to clear your workspace
rm(list=ls())
# and check whether your working directory is set to where the data are on you computer
getwd()
# an if not, change it
# setwd("PATH TO FOLDER THAT CONTAINS THE DATA SUBFOLDERS")

# load the data files
friendship.t1 <- as.matrix(read.table(file="Mannheim_data/friendship.network.t1.dat"))
friendship.t2 <- as.matrix(read.table(file="Mannheim_data/friendship.network.t2.dat"))
gender <- unlist(read.table(file="Mannheim_data/gender.dat")) # sex
coo <- unlist(read.table(file="Mannheim_data/coo.dat")) # country of origin
nActors <- dim(friendship.t1)[1]

# recode the network - all values larger than 1 should be 1
threshold <- 1
friendship.t1[friendship.t1 >= threshold] <- 1
friendship.t2[friendship.t2 >= threshold] <- 1


### 3. DESCRIBE THE DATA (this part is borrowed from Christoph Stadtfeld)

# let's make some nice plots with the igraph package
library(igraph)

graph1 <- graph.adjacency(friendship.t1)
graph2 <- graph.adjacency(friendship.t2)
graph12 <- graph.adjacency(friendship.t1 + friendship.t2)
myLayout <- layout.fruchterman.reingold(graph12)

# Plotting the networks without attributes
par(mfrow = c(1, 2))
plot(graph1,
     vertex.color = "darkblue",
     edge.color = "black",
     edge.width = 2,
     edge.arrow.size = 0.2,
     vertex.size = 10,
     vertex.label = "",
     layout = myLayout,
     main = "Network wave 1")
plot(graph2,
     vertex.color = "darkblue",
     edge.color = "black",
     edge.width = 2,
     edge.arrow.size = 0.2,
     vertex.size = 10,
     vertex.label = "",
     layout = myLayout,
     main = "Network wave 2")
par(mfrow = c(1, 1))

# Are there groups/clusters in this classroom?
# How stable is the group structure?

# Plotting the networks with gender homophily
par(mfrow = c(1, 2))
plot(graph1,
     vertex.color = ifelse(gender == 1, "pink", "darkblue"),
     vertex.shape = ifelse(gender == 1, "square", "circle"),
     edge.color = "black",
     edge.width = 2,
     edge.arrow.size = 0.2,
     vertex.size = 10,
     vertex.label = NA,
     layout = myLayout,
     main = "Network wave 1")
plot(graph2,
     vertex.color = ifelse(gender == 1, "pink", "darkblue"),
     vertex.shape = ifelse(gender == 1, "square", "circle"),
     edge.color = "black",
     edge.width = 2,
     edge.arrow.size = 0.2,
     vertex.size = 10,
     vertex.label = "",
     layout = myLayout,
     main = "Network wave 2")
par(mfrow = c(1, 1))

# How much do you think sex homohily explains clustering in the class?

# Plotting the networks with ethnic homophily
par(mfrow = c(1, 2))
plot(graph1,
     vertex.color = coo,
     vertex.shape = ifelse(gender == 1, "square", "circle"),
     edge.color = "black",
     edge.width = 2,
     edge.arrow.size = 0.2,
     vertex.size = 10,
     vertex.label = "",
     layout = myLayout,
     main = "Network wave 1")
plot(graph2,
     vertex.color = coo,
     vertex.shape = ifelse(gender == 1, "square", "circle"),
     edge.width = 2,
     edge.color = "black",
     edge.arrow.size = 0.2,
     vertex.size = 10,
     vertex.label = "",
     layout = myLayout,
     main = "Network wave 2")
par(mfrow = c(1, 1))

# Does country of origin seem to be aligned with friendship clusters?

# detach the igraph package
detach(package:igraph)


### 4. LOAD THE statnet PACKAGE

library(statnet)
# note that it loads several packages - statnet is a whole package collection!
# among others, package sna was loaded too
# this is why it was important to detach the igraph package above


### 5. RUN YOUR (POSSIBLY) FIRST ERGM - AN EMPTY MODEL

# we model the structure of the first friendship network

# to make things more simple, we should use objects of class "network"
friend1 <- network(friendship.t1)
# you can add vertex/node/actor attributes to a network object by
# using the %v% operator
friend1 %v% "sex" <- gender
friend1 %v% "coo" <- coo
# what do we get?
class(friend1) # network object
mode(friend1)  # of list mode
friend1        # some more information

# how to run an ERGM? this is the function that does the trick:
?ergm
# it is a pretty complex function, with which you can control everything
# related to the simulation, estimation, etc.

# for now, the most important part is the possible explanatory variables,
# which express local network configurations
??"ergm-terms"
# clicking on the help page 'ergm::ergm-terms', you can see that there are
# dozens of implemented effects, one more complicated than the other...
# (you could also program your own effect - more on this later)

# let's try a model with just a "constant" - a parameter for density
ergm1 <- ergm(friend1~edges)
ergm1
summary(ergm1)
# this is basically a Bernoulli / Erdos-Renyi model (independence!)

# MINITASK: What does the 'edge' estimate mean? How do we transform it
#           to probabilities? What is this probability?

# Congratulations! You just ran an ERGM! Of course, this is not a "real"
# model, so let's try something more useful.


### 6. STRUCTURAL AND COVARIATE EFFECTS

# reciprocity is a universal tendency in friendships - include it in the model
ergm2 <- ergm(friend1~edges+mutual)
# the estimation looks more serious now
# some parts of it are done in C which communicates with R - :-O (shocked face)
# interested in how the algorithm works? check ?ergm

# a useful option, good for debugging bad models:
# use the verbose=TRUE argument to get more feedback about the estimation
ergm2 <- ergm(friend1~edges+mutual, verbose=TRUE)

# now look at the results
summary(ergm2)
# MINITASK: How can you interpret the reciprocity parameter?

# how about clustering?
ergm3 <- ergm(friend1~edges+mutual+gwesp(decay=0, fixed=TRUE))
# this took more time... did the estimation converge?
mcmc.diagnostics(ergm3)
# quite well; in case you see the trace plots going up, down or oscillating,
# you can continue the estimation starting from where you left off
ergm3b <- ergm(friend1~edges+mutual+gwesp(0, fixed=T), control=control.ergm(init=ergm3$coef))
mcmc.diagnostics(ergm3b)
# this is probably better
# watch out: if your model is stuck in a local trap (degeneracy), continuing the estimation
# might make things worse and worse by every run...

# look at the results
summary(ergm3b)
# but what does 'gwesp.fixed.0' mean?


# what are the potential confounding factors causing clustering?
ergm4 <- ergm(friend1~edges+mutual+gwesp(0, fixed=T)+istar(2)+ostar(2)+m2star)
mcmc.diagnostics(ergm4)
summary(ergm4)

ergm5 <- ergm(friend1~edges+mutual+gwesp(0, fixed=T)+istar(2)+ostar(2)+m2star+
                nodematch("sex")+nodeicov("sex")+nodeocov("sex"))
mcmc.diagnostics(ergm5)
summary(ergm5)

ergm6 <- ergm(friend1~edges+mutual+gwesp(0, fixed=T)+istar(2)+ostar(2)+m2star+
                nodematch("sex")+nodeicov("sex")+nodeocov("sex")+nodematch("coo"))
mcmc.diagnostics(ergm6)
summary(ergm6)

# MINITASK: Interpret the results related to sex and ethnic homophily.
#           Is there still a significant tendency for clustering even when taking
#               homophily into account?


### 7. Goodness of Fit

# MINITASK: What do you think "model fit" means in case of ERGMs? What should fit what?

# does adding many covariate effects to the model fit better?
ergm1_gof <- gof(ergm1) # the empty model
ergm4_gof <- gof(ergm4) # the clustering model with degree controls
ergm6_gof <- gof(ergm6) # the homophily model

# print the gof plots to pdf's
pdf("ergm1_gof.pdf")
plot(ergm1_gof)
dev.off()

pdf("ergm4_gof.pdf")
plot(ergm4_gof)
dev.off()

pdf("ergm6_gof.pdf")
plot(ergm6_gof)
dev.off()

# let's inspect the results


### 8. SIMULATING NETWORKS FROM AN ERGM MODEL

# the last topic in this basic intro to the statnet package
# is about simulating random networks from a model that you
# estimated using the ergm function

# it is also very easy (technically speaking)
?simulate # note that this function comes with the stats package - not ergm-specific!
mynets <- simulate(ergm6, 10) # simulates 10 networks from model 6
mynets
# tadaam! you can use these networks to run CUG tests or anything else


### SECTION SUMMARY

###
# We explored some basic functionalities of the statnet package related to fitting
# and testing ERGMs. We covered four important topics: estimating models, checking
# model convergence, chekcing goodness of fit, and simulating random networks based
# on the estimated models.
#
# You may've noted that the statnet package is designed to be user-friendly in the
# sense that the main functions run without specifying many additional arguments.
# However, things can get really complicated when it comes to model specification.
# In my view, this is the trickies part in ERGMs, and the statnet documentation is
# less strong in describing model effects and when and how they should be used.
#
# Luckily, there are many tutorials and helpful sites out there, like:
# http://statnet.org/
# http://statnet.csde.washington.edu/workshops/SUNBELT/EUSN/ergm/ergm_tutorial.html
#
# In addition, there is another, not R-based, implementation of ERGMs, mainly developed
# by people in Melbourne (Garry Robins, Peng Wang) and Manchester (Johan Koskinen). This
# software is called PNet (other version: MPNet). Check out
# http://www.swinburne.edu.au/fbl/research/transformative-innovation/our-research/
#                                             MelNet-social-network-group/
# http://www.swinburne.edu.au/fbl/research/transformative-innovation/our-research/
#                                             MelNet-social-network-group/PNet-software/index.html
#
# Note: the things we did here were mostly implemented in the ergm package. We could've
#       just download and use only this. However, it's never bad to have the rest of
#       the statnet team packages ready in case you ever need them.
###

# We are done with the introductory ERGM in statnet script.
# Please save the Mannheim dataset, we will use it to learn about SAOMs.
save.image("MyMannheimData.RData")
# Clear your workspace.
rm(list=ls())
# Time for an individual exercise!

### END OF SCRIPT - PLEASE OPEN: "practice5.R"