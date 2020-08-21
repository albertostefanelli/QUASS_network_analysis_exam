#                       ##################################################                         #
#                       ###     SOCIAL NETWORK ANALYSIS WITH R IV.     ###                         #
#                       ###  STRUCTURAL EQUIVALENCE AND BLOCKMODELING  ###                         #
#                       ##################################################                         #

###
# This script was originally made for the course on social network analysis
# held in Leuven on 18-20 February 2016.
#
# The script was prepared by Andras Voros.
# (this is the version of 20.02.2018)
###

###
# In this exercise, we are looking at basic functions in the sna package
# for doing simple blockmodeling tasks.
###


### 1. LOAD THE DATA AND THE sna PACKAGE

# let's use the first wave of the s50 dataset

# first make sure to clear your workspace
rm(list=ls())
# and check whether your working directory is set to where the data are on you computer
getwd()
# an if not, change it
# setwd("PATH TO FOLDER THAT CONTAINS THE DATA SUBFOLDERS")

# load the network
friend.w1 <- as.matrix(read.table("s50_data/s50-network1.dat"))

# make sure that igraph is detached and sna is loaded
detach(package:igraph)
library(sna)


### 2. Blockmodeling

# At the heart of any method that aims to classify actors is "equivalence"
# Equivalence expresses how similar actors are in terms of who they are
# connected to.
# There are many equivalence concepts: structural eq., regular eq., stochastic eq., ...

# How to quantify equivalence? There are also many measures
# e.g. Jaccard index, Hamming distance, Simple matching coefficient, correlation coeffs., ...

# here is our network
gplot(friend.w1)

# we cluster the actors based on some measure of similarity
equiv.w1 <- equiv.clust(friend.w1, cluster.method="ward.D2", method="hamming")
# note: this function uses the hclust() function - see ?hclust()

# MINITASK: Do you remember what the Hamming distance is?

# what we got is a cluster tree
plot(equiv.w1)
# like in all clustering examples, you have to decide
# on the number of clusters somehow... hmm... ...
# based on "visual inspection", 9 seems like a good number


# the blockmodel() function basically cuts the tree, but also rearranges
# the actors into their new "blocks"
bm.w1 <- blockmodel(friend.w1, equiv.w1, k=9)

# we can look at the matrix format of the original network
plot.sociomatrix(friend.w1, diaglab=FALSE)
# and of the rearranged one
plot.sociomatrix(bm.w1$blocked.data, diaglab=FALSE)
# the second one looks more "tidy", doesn't it?


# now let's look at the original network (or its largest component) as a graph
# and color its nodes/actors according to their block membership

# how to extract block memberships?
str(bm.w1, 1)
# proably by bm.w3$block.membership
# but watch out - they are in the wrong order!
bm.w1$order.vector

# you can get the block membership in the right order by this line
block.members <- bm.w1$block.membership[order(bm.w1$order.vector)]
# if you are interested why, check ?order and try to figure out
# what x[order(y)] does in general (but do it later please

# so we run gplot by the appropriate vertex color vector
gplot(friend.w1, vertex.col=block.members)
# tadaam! and that's what blockmodeling is good for...


# however, this is a little unfair, because blockmodeling would yield
# something clearer and more interpretable if we applied it to a network
# with a single component

# so, let's focus on the largest component of our network now
cfriend.w1 <- component.largest(friend.w1, connected="weak", result="graph")

# MINITASK: What is "weak" connectedness?

# the main component looks like this
gplot(cfriend.w1)

# let's repeat the blockmodeling exercise
cequiv.w1 <- equiv.clust(cfriend.w1, cluster.method="ward.D2", method="hamming")
# here is the cluster tree
plot(cequiv.w1)
# 6 clusters seems like a good number
cbm.w1 <- blockmodel(cfriend.w1, cequiv.w1, k=6)

# the adjacency matrix of the original network
plot.sociomatrix(cfriend.w1, diaglab=FALSE)
# cleaner block structure
plot.sociomatrix(cbm.w1$blocked.data, diaglab=FALSE)

# extracting block membership
cblock.members <- cbm.w1$block.membership[order(cbm.w1$order.vector)]
# and makin the plot
gplot(cfriend.w1, vertex.col=cblock.members)
# seems a bit more interpretable


### SECTION SUMMARY

###
# You have learnt about the basics of blockmodeling in R. There are a few (2-3) other
# packages and functions to perform more advanced blockmodeling tricks (like the
# package 'blockmodeling'). These have more options readily implemented for
#   1. testing different theoretical block structures,
#   2. better optimization of the clustering solution and
#   3. assessing the fit of an empirical block structure to a theoretical solution.
# For this introduction, the basic sna functions were enough, but you can always
# explore other packages on your own.
###


# Save your s50 data for later
save.image("MyS50Data.RData")


### END OF SCRIPT - PLEASE OPEN: "8_Cliques_and_Communities.R"