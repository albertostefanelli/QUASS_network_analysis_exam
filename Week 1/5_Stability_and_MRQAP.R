#                       ##################################################                         #
#                       ###    SOCIAL NETWORK ANALYSIS WITH R III.     ###                         #
#                       ###           "ON THE EDGE BETWEEN             ###                         #
#                       ###   STATIC AND DYNAMIC NETWORK ANALYSIS"     ###                         #
#                       ##################################################                         #

###
# This script was originally made for the course on social network analysis with R and
# RSiena held in Budapest on 3-5 December 2014.
#
# The script was prepared by Andras Voros.
# (this is the version of 11.02.2018)
###

###
# This section has a very deep title. After getting familiar with the basics of
# the sna and igraph packages, we can move on to something (even) more exciting.
# Here we learn about a few measures and a technique which can be applied to both
# cross-sectional and longitudinal network data. These new bits of knowledge
# will come up here and there in during the next days.
###


### 1. LOAD THE DATA AND THE sna PACKAGE

# we resume working on the 160-actor subsample of the Glasgow dataset

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


### 2. MEASURES OF SIMILARITY AND TEMPORAL STABILITY OF NETWORKS

### Tie-level similarity measures
# Do employees at a workplace prefer to work with colleagues whom they
# perceive as more competent than themselves?
# Do students in second grade hang out with the same classmates they
# used to in first grade?
#
# The first question refers to the similarity or overlap between two
# networks. The second asks about the stability of social relations.
#
# From a technical point of view, these questions are identical:
# to answer them we need to compare two networks.
#
# Comparing macro-structural features of two networks (e.g. density,
# reciprocity rate, degree distribution, clustering) can be misleading,
# because the same macro-level results can mean a lot of different things
# on the micro level - on the level of network ties.
#
# This is why we often calculate similarities/differences between networks
# on the tie level. There are several ways of defining similarity, though,
# so we have several interesting measures to choose from. However, they
# have one general thing in common: they are meaningful only for comparing
# networks defined on the same actor set (e.g. two networks of a single
# group of people). Otherwise, it is not really possible to tell which
# tie should be compared with whic other tie.
###

# The Hamming distance
# perhaps the most simple dissimilarity or distance measure known to mankind
# there is an sna function for calculating the Hamming distance
?hdist # counts the number of ties that are in different states (1-0)
(hamming <- hdist(friendship.1, friendship.2))
# but we should probably divide this raw number by the the maximum possible distance,
# which is when all ties are in different states
(hamming.prop <- hamming/nties(friendship.1)) # distance proportionate to max distance

# The simple matching coefficient
# a similar measure but from the point of view of similarity/stability instead of
# distance/change
(matching <- 1 - hamming.prop)
# it seems like it is a super-stable friendship network
# however, as we saw earlier in the case of grecip, for sparse networks one will always get
# high similarity (or low distance), because most of the ties are absent

# The Jaccard index
# a more useful measure for us: it disregards ties that are absent in both networks
# and only considers these cases
#                network1    network2
#             A:    1           1
#             B:    1           0 
#             C:    0           1
#            ------------------------
# Jaccard index = A / (A + B + C)
#
# funny, but there is no built-in function for the Jaccard index in the sna and
# igraph packages (though you can find it in other, non-network packages)
# luckily, it is easy to calculate it by "hand"
A <- sum((friendship.1 * friendship.2)==1, na.rm=TRUE) # #ties that exist in both networks
BplusC <- sum((friendship.1 + friendship.2)==1, na.rm=TRUE) # #ties that exist in only one network
(jaccard <- A/(A+BplusC))
# this means that only a bit more than one fourth of the ties that
# existed in at least one observation were stable
# so actually, it turns out that the friendship network was quite dynamically evolving

### Closing remarks
# These measures are quite useful for an intuitive description of similarities
# between pairs of networks in multiplex network data, or for assessing
# the stability of dynamic networks.
#
# Our example was about binary (1-0) networks, but the presented indices can
# be directly used on or extended to valued networks.
###


### 3. CORRELATIONS BETWEEN NETWORKS - THE QUADRATIC ASSIGNMENT PROCEDURE (QAP)

# we shall learn about QAP first, from a few slides
# ...
# now you know what QAP is, let's see how to do it in R

# functions that do QAPs for you in the sna package
?netlm
?netlogit

# MRQAP for wave2 friendships on wave2 home distances
(qap1 <- netlogit(friendship.2, distance.2, nullhyp="qap", reps=100))
# don't let yourself be confused by the warnings - everything is fine!

# we can also check the relation between wave2 and wave1 friendships
(qap2 <- netlogit(friendship.2, friendship.1, nullhyp="qap", reps=100))

# what is the effect of same sex on friendship?

# IMPORTANT: variables always have to be in matrix form!
# so first we need to create a same sex matrix for this,
# which contains 1 if two students have the same sex and 0 otherwise
same.sex <- sex.F %*% t(sex.F) # %*% is matrix multiplication (not cell-wise)
# we have a matrix that contains the values 1 (both boys), 4 (both girls), 2 (opposite sex)
# now we only have to recode it
same.sex[same.sex==2] <- 0
same.sex[same.sex==4] <- 1

# run QAP (note how we need to group the x variables in a list)
(qap3 <- netlogit(friendship.2, list(friendship.1, same.sex), nullhyp="qap", reps=100))

# finally, put distance at wave2 back in the model
(qap4 <- netlogit(friendship.2, list(friendship.1, same.sex, distance.2), nullhyp="qap", reps=100))

### Concluding remarks
# QAP is a nice and easy technique, and as you can see it can be applied to both
# cross-sectional and longitudinal data.
#
# However, in the end QAP is based on correlations between cell values, and trying to
# include anything more complicated than the dyad (e.g. triad closure) requires quite
# a bit of tricky data preparation. Not to mention that in many cases we may not wish
# to simply "control for" the interdependences of network ties but also explicitly
# model them. For such studies, different methods are more advisable. You will hear
# about some of them tomorrow.
###


### SECTION SUMMARY

###
# We have discussed two more advanced topics in the analysis of social networks:
# how to compare two networks by different similarity measures to asses their
# level of overlap or temporal stability; how to use MRQAP, a regression technique
# for networks.
###

# It's time for another individual exercise.
# Save your Glasgow data again, it may be useful later
save.image("MyGlasgowData.RData")
# And remove all objects
rm(list=ls())

### END OF SCRIPT - PLEASE OPEN: "practice3.R"