#                       ##################################################                         #
#                       ###     SOCIAL NETWORK ANALYSIS WITH R V.      ###                         #
#                       ###          CLIQUES AND COMMUNITIES           ###                         #
#                       ##################################################                         #

###
# This script was originally made for the course on social network analysis
# held in Leuven on 22-24 February 2018.
#
# The script was prepared by Andras Voros.
# (this is the version of 20.02.2018)
###

###
# In this exercise, we look at a few functions in the igraph package
# that help you analyzing clustering, cliques, and communities in networks.
###


### 1. LOAD THE DATA AND THE igraph PACKAGE

# again, we are using the first wave of the s50 dataset

# first make sure to clear your workspace
rm(list=ls())
# and check whether your working directory is set to where the data are on you computer
getwd()
# an if not, change it
# setwd("PATH TO FOLDER THAT CONTAINS THE DATA SUBFOLDERS")

# you saved our modified version of the data in the last script, now load it
load("MyS50Data.RData")

# for this example, we will use igraph functions that are implemented for undirected
# networks only, unfortunately
# so, we make our network to undirected in two steps
friend.w1 <- friend.w1 + t(friend.w1) # check table(friend.w1) to see what happened
friend.w1[friend.w1==2] <- 1

# MINITASK: Why does this recoding result in the same network structure but without
#           the directionality of ties? What do the new values after the first step mean?

# make sure that igraph is detached and sna is loaded
detach(package:sna)
library(igraph)


### 2. IT'S A SMALL WORLD, ISN'T IT?

# The small-world phenomenon: it is widely observed that real-life networks
# (especially large ones) have relatively short average path lengths and high
# clustering (many closed triads); as a consequence, most nodes in the network
# can be reached in only a few steps - it's a small world.

# How small is our analyzed community in this sense?

# Let's put the data into igraph format
friend1 <- graph.adjacency(friend.w1)
friend1 <- as.undirected(friend1)

# is our network connected?
is.connected(friend1)

# how many components of different size are there?
components <- decompose.graph(friend1)
# count the nodes in each component and make a frequency table
table(sapply(components, vcount))
# the main component contains two thirds of the vertices (33 of them)

# look at the network again to confirm the component sizes given by igraph
myLayout <- layout.fruchterman.reingold(friend1)
plot(friend1,
     edge.color = "black",
     edge.width = 1.5,
     edge.arrow.size = 0.25,
     vertex.size = 6,
     vertex.label = "",
     layout=myLayout)
# seems right

# let's save the main component in a separate object
main.component <- components[[1]]
# this is how you get the component memberships
main.component.members <- as.numeric(substr(names(V(main.component)),2,3))


# we focus on the main component - is it a small world?
average.path.length(main.component) # average path length
diameter(main.component) # length of longest path
transitivity(main.component) # ratio of closed to open triads

# MINITASK: What do you think? Is this a "small world"? Is the average path length short?
#           Is clustering high at the same time?


### 3. YET, THERE IS SO MUCH STRUCTURE IN IT: CLIQUES AND CORES...

# regardless of whether we name it a "small-world" network or not, it is clear that
# it has an internal structure - we can explore this

# how many cliques are in the network?
cliques <- cliques(friend1)
length(cliques) # this looks like a long object - why?

# of course, each clique can be a subset of larger cliques...
# maybe counting maximal cliques is more meaningful 
table(sapply(maximal.cliques(friend1), length))

# MINITASK: How many maximal cliques are there in the network? What is the smallest
#           and the largest clique?


# maybe the definition of a clique is too narrow, but we can look at different
# ways for grouping nodes

# for example, by applying the concept of k-cores
# a set of nodes form a k-core in a network if all nodes in the set have at least
# k ties within the set (in other words in the subgraph defined by the nodes)

# we can assign a k for each node in the network
cores <- graph.coreness(friend1)

# and we can plot the network with node colors showing k-cores
plot(friend1,
     edge.color = "black",
     edge.width = 1.5,
     edge.arrow.size = 0.25,
     vertex.size = 6,
     vertex.label = "",
     vertex.color= cores,
     layout=myLayout)

# MINITASK: How would you interpret core membership based on the graph?
#           How is the concept of k-cores similar to or different from structural equivalence?


### 4. ... AND COMMUNITIES OF DIFFERENT KINDS

# a popular way of exploring the structure of networks is by identifying
# "communities" in it
# roughly speaking, community detection algorithms aim to find areas in the network
# which are densely connected within, but relatively loosely connected to other areas;
# this is is a non-trivial optimization task in most interesting cases

# we can easily try, for instance, the fast-greedy community detection algorithm on our data
communities <- fastgreedy.community(friend1)
length(communities) # how many communities were identified by the algorithm?
sizes(communities) # shat are their sizes?
membership(communities) # who belongs to which community?

# let's look at the network with nodes colored according to community membership
plot(communities, friend1,edge.color = "black",
     edge.width = 1.5,
     edge.arrow.size = 0.25,
     vertex.size = 6,
     vertex.label = "",
     layout=myLayout)
# looks nice


### 5. We make a final combined plot to compare the different techniques you learned

par(mfrow=c(2,2))
# the original friendship network
plot(friend1,
     edge.color = "black",
     edge.width = 1.5,
     edge.arrow.size = 0.25,
     vertex.size = 6,
     vertex.label = "",
     vertex.color="skyblue",
     layout=myLayout,
     main="original network")
# the blockmodel (from the previous script)
plot(friend1,
     edge.color = "black",
     edge.width = 1.5,
     edge.arrow.size = 0.25,
     vertex.size = 6,
     vertex.label = "",
     vertex.color= block.members,
     layout=myLayout,
     main="blockmodel")
# the k-cores
plot(friend1,
     edge.color = "black",
     edge.width = 1.5,
     edge.arrow.size = 0.25,
     vertex.size = 6,
     vertex.label = "",
     vertex.color= cores,
     layout=myLayout,
     main="k-cores")
# the fast-greedy communities
plot(communities, friend1,edge.color = "black",
     edge.width = 1.5,
     edge.arrow.size = 0.25,
     vertex.size = 6,
     vertex.label = "",
     layout=myLayout,
     main="fast'n'greedy")
par(mfrow=c(1,1))

### SECTION SUMMARY

###
# Groups and communities can be captured through different measures.
# Since all of this is about classifying nodes in a network in some way,
# the notions of groups and communities abstractly relates to those of 
# social roles and positions - having similar connections, belonging to
# the same circles, being similarly connected to the rest of the community etc.
#
# There are many other methods for detecting positions and communities that we
# could not cover here. Some allow for overlapping communities, which is an exciting
# topic in this area. These furhter approaches you should explore on your own.
###


# Save your s50 data for later
save.image("MyS50Data.RData")


### This is the end.
# We have covered a good deal of things about network analysis in R.
# We WILL continue our exploration of network analysis tools by learning 
# about two very very important multivariate statistical model families 
# for analyzing cross-sectional and longitudinal social networks next time.
###

### END OF SCRIPT - PLEASE TAKE A LONG BREAK