#                       ##################################################                         #
#                       ###     SOCIAL NETWORK ANALYSIS WITH R IV.     ###                         #
#                       ###       TRIAD CENSUS AND RANDOM GRAPHS       ###                         #
#                       ##################################################                         #

###
# This script was originally made for the course on social network analysis with R and
# RSiena held in Budapest on 3-5 December 2014.
#
# The script was prepared by Andras Voros.
# (this is the version of 20.02.2018)
###

###
# We are going to cover three advanced topics: counting different stuctural
# configuration in a network, generating random networks, and the combination
# of the two.
###


### 1. LOAD THE DATA AND THE sna PACKAGE

# we again work on the 160-actor subsample of the Glasgow dataset

# first make sure to clear your workspace
rm(list=ls())
# and check whether your working directory is set to where the data are on you computer
getwd()
# an if not, change it
# setwd("PATH TO FOLDER THAT CONTAINS THE DATA SUBFOLDERS")

# load your version of the Glasgow data
load("MyGlasgowData.RData")

# and make sure that igraph is detached and sna is loaded
detach(package:igraph)
library(sna)


### 2. DYAD, TRIAD, AND CYCLE CENSUS

# in the sna package, there are functions four counting different structural
# configurations in a network, for example: dyads, triads, cycles, paths
# here we try a few of these functions

# count the different dyad types
?dyad.census
(dyad.count <- dyad.census(friendship.1))

# count the different triad types
?triad.census
triad.count <- triad.census(friendship.1)
triad.count
# what do these labels mean? - a short presentation slide

# there is also a function that identifies the type of a dyad
?triad.classify
# for example, what is the triadic relation between the first, fifth, and eigths actor?
triad.classify(friendship.1, tri=c(1,5,8))
# and between the 4th, 7th, and 9th?
triad.classify(friendship.1, tri=c(4,7,8))

# count cycles of different length
?kcycle.census
# it is a more delicate topic than counting dyads or triads - many possibilities
# let's count the cycles up to length 5 (don't go to high or it will take forever)
kcycle.census(friendship.1, maxlen=5)
# oops, apparently the function does not like our absent students,
# so we have to remove them
# we do it in a new object in order not to ruin our original data
temp <- friendship.1[-c(48,136,157),-c(48,136,157)] # excluding rows and columns with the "-" sign
# and now it should work
kcycle.census(temp, maxlen=5, tabulate.by.vertex=FALSE)

# there are other options for counting structural configurations,
# but you have to explore them after class


### 3. GENERATING RANDOM GRAPHS

# it is also possible to simulate random graphs in sna (and in igraph)
# there are functions for several probability models

# here we are going use Bernoulli Random Graphs only

### Bernoulli Random Graphs
# What are they?
# Tie values are drawn from a Bernoulli/binary distribution with success probability p.
# For example, if p=0.5 then there is a fifty-fifty chance that a tie is present in the
# network.
# Key features of Bernoulli Random Graphs:
# Ties are independent: whether a tie is present or not does not alter the probability
# of another tie being present.
# The expected value of the density of the graph is p.
###

# generate a random graph with sna
?rgraph
# we need to set the size of the network (no. of actors) and p
random.graph <- rgraph(50, tprob=0.1)
# now you all have a random graph of 50 actors with tie probability p=0.1
# and they are all a bit different, since they are randomly generated

# let's look at the random network
class(random.graph)
mode(random.graph) # it is simply a numeric matrix

gplot(random.graph) # looks quite homogeneous, maybe too much
gden(random.graph) # density probably close to the expected value (0.1)
hist(degree(random.graph, cmode="indegree"))
hist(degree(random.graph, cmode="outdegree")) # these are a bit different from what we usually see

# random networks may not be realistic from every aspect
# however, they can serve as a good baseline to which real networks can be compared

# finally, you can also simulate several random graphs with the
# same parameters at once
random.stack <- rgraph(50, 100, tprob=0.1)
# you get 100 random networks for 50 actors and p=0.1

class(random.stack)
# they are stored in a 3-dimensional array: a matrix for each 100 graphs, all in one


# 4. TESTING THE TRIAD CENSUS AGAINST RANDOM NETWORKS

# here are the results from the triad census in the friendship network again
triad.count

# how do should we interpret these numbers? are they high? are they low?
# answer: it depends
# for example on the number of actors - many actors means many possible triads
# or on the density of the network - there are many empty triads in a sparse network

# a natural way to interpret the triad census is by comparing the observed number of
# the different triads to the numbers we would see in a SIMILAR NETWORK in which ties
# are RANDOMLY PRESENT or absent

# we have already discussed what RANDOMLY PRESENT means
# but what is a SIMILAR NETWORK?
# networks can be similar or different in many ways, so it is up to you to decide
# what are the important aspects
# the most simple assumption is that two networks are similar if they have the
# same number of actors and the same density

# but can we generate random networks with the same size and density as the friendship network?
# of course we can!
net.size <- nrow(friendship.1)
net.dens <- gden(friendship.1)
# let's simulate 200 similar random networks
random.nets <- rgraph(net.size, 200, net.dens)
# how does the result look like
dim(random.nets)
# it's a 200*160*160 array - this means that the simulated networks are
# identified by the first dimension
gplot(random.nets[1,,]) # this is the first random network
gplot(random.nets[2,,]) # this is the second

# are the densities really distribute around the density of the friendship network?
random.dens <- gden(random.nets)
hist(random.dens)
mean(random.dens)
net.dens
# it seems so. good. it would be even better with more random networks

# to compare the observed triad counts to the simulated ones,
# we need to run the triad census on all 200 random networks
random.triad <- triad.census(random.nets)
# it takes a bit of time...
# what is the result?
class(random.triad) # a matrix
dim(random.triad) # containing count for the 16 triads (columns) in each network (rows)

# excellent! we now only need to meaningfully visualize the results
# for this, we will use the so-called VIOLIN PLOTS
# again, there are several ways to do it in R
# a relatively easy way is to use the vioplot package

# download the vioplot package
install.packages("vioplot") # it downloads some other packages required to use vioplot
# and load it
library(vioplot)

# the plotting function we use has the same name as the package
?vioplot

# to get familiar with this kind of plot, let's look at
# the distribution of transitive triplets (030T) in the simulated networks
vioplot(random.triad[,9],                 # the distribution
        names=colnames(random.triad)[9],  # name of the triad type
        col="transparent",                # let the "violin" be transparent
        ylim=c(0, 75))                    # displayed part of the y axis
# compared to this, where is the number that we actually observed in the friendship networs?
# we add the observed value as a point to the plot
points(1, triad.count[9],                 # x and y coordinates for the added point
       col="red",                         # color of point
       pch=15)                            # shape of point: solid rectangle

# now we plot the distribution of all closed triangles (in a quite primitive way)
vioplot(random.triad[,9], random.triad[,10], random.triad[,12],
        random.triad[,13], random.triad[,14], random.triad[,15], 0,
        names=colnames(random.triad)[c(9,10,12,13,14,15,16)],
        col="transparent")
# and mark the observed numbers in each category
points(1:7,
       triad.count[c(9,10,12,13,14,15,16)],
       col="red",
       type="b",
       pch=15)

# MINITASK: what can you tell about the distribution of closed triads
#           in the friendship newtork?
#           can we see more or less tranitive closure than expected by chance?

### CONGRATULATIONS!
# You have just done a Conditional Uniform Graph Test (perhaps your first).
# You conditioned on network size and density and tested whether some
# triadic configurations are more or less likely to occur in the observed
# friendship network than in a random network. You could have easile done
# the same for testing the number of different dyads, cycles, and so on.
#
# This tehcnique is relatively popular in social network analysis (a famous
# example is the 2004 Bearman-Moody-Stovel paper in AJS on romantic networks
# in schools).
#
# Over size and density, it is possible to compare the characteristics
# of your network with those of random graphs that are similar in other
# respects, e.g. have the same level of reciprocity, similar clique structure.
# However, calculations become quite complicated if the conditions are
# too complex. In this sense, this method is not very flexible. Fortunately,
# some simple conditioning can be done easily with the cugtest function in
# the sna package.
###

# One more question:
# MINITASK: The idea behind QAP was to generate networks which are similar
#           to the observed by permuting rows and columns in its adjacency matrix.
#           Would it make sense to apply a permutation approach instead of simulating
#           random networks in CUG Tests?
#           How would the structural measures of the permuted networks compare with
#           those of the original network?


### SECTION SUMMARY

###
# In this sript on network analysis with the sna and igraph packages
# you have learnt how to count the occurences of different structural
# configurations (e.g. mutual dyads, transitive triads) in your network.
# These can be done easily with the dyad.census, triad.census, kcycle.census,
# etc. functions. To learn more about other "census" functions enter
# ??census in the R console.
#
# In addition, we have disussed how to generate Bernoulli Random Graphs. There
# are many functions in sna and igraph that simulate other types of random
# graphs - google for sna and igraph and have a look at their manuals to
# find out more.
#
# Lastly, we have done a Conditional Uniform Graph Test with our bare hands.
# The CUG Test is a nice intuitive technique for assessing if some observed
# characteristics of a network are only results of chance, given other
# network characteristics are fixed. Random graphs help us to simulate null
# distributions for the chosen measures. We can then compare our observed
# measures with the simulated ones, which offers a good interpretation for
# the counts of different structural configurations.
#
# However, if we want the simulated networks to be similar to
# the observed one in many respects (e.g. size, density, triadic
# closure, degree distributions), the CUG Test becomes very complicated.
# This is unfortunate because more specific conditioning means a
# more specific null hypothesis and more relevant comparisons. At least
# some of the more complicated tricky conditions can be tested simply
# using the cugtest function in the sna package.
###

# Let's do another exercise to practice doing a structure census,
# and also perhaps to learn something general about friendships...

# Save your Glasgow data again, you might want to use it later
save.image("MyGlasgowData.RData")
# And remove all objects
rm(list=ls())


### END OF SCRIPT - PLEASE OPEN: "practice4.R"