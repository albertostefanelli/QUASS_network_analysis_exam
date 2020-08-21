#                       ##################################################                         #
#                       ###     SOCIAL NETWORK ANALYSIS WITH R II.     ###                         #
#                       ### BASIC DESCRIPTIVES WITH THE igraph PACKAGE ###                         #
#                       ##################################################                         #

###
# This script was originally made for the course on social network analysis with R and
# RSiena held in Budapest on 3-5 December 2014.
#
# The script was prepared by Andras Voros.
# (this is the version of 11.02.2018)
###

###
# We have discussed some basic functions in the sna package which are helpful
# when you need to have a first look on network data. The igraph package has
# some handy functions with which you can calculate other descriptives and make
# nice figures. These form the topic of this session.
###


### 1. LOAD THE DATA

# we continue our work on the 160-actor subsample of the Glasgow dataset

# first make sure to clear your workspace
rm(list=ls())
# and check whether your working directory is set to where the data are on you computer
getwd()
# an if not, change it
# setwd("PATH TO FOLDER THAT CONTAINS THE DATA SUBFOLDERS")

# you saved our modified version of the data two scripts ago,
# now it is time to load it
load("MyGlasgowData.RData")


### 2. LEARN ABOUT THE OBJECTS

# We have already done this, it was only a reminder.


### 3. SWITCHING BETWEEN sna AND igraph

# if you don't have the igraph package installed yet, now is the time to do it
install.packages("igraph")

# there are some functions that you can find in both sna and igraph with the same name
# however, they are not exactly the same in the two packages
# (the two are developed by different people)

# given that you still have the sna package loaded, try loading igraph
library(igraph)
# as you see in the console, this resulted in some functions being
# "masked" from the sna package; in theory, you can still use sna
# (e.g. the gden function), but whenever you call one of the listed
# functions (e.g. degree), the igraph version of the function will
# be run, because igraph was loaded later
# (it is like sliced emmental cheese...)

# this and other things can cause a lot of confusions, so
# the good practice is: detach sna before loading igraph and vice versa
# let's detach both this time, just to be sure
detach(package:sna)
detach(package:igraph)
# now reload igraph
library(igraph)
# and the message about masking doesn't show up


### 4. MEASURING SEGREGATION IN igraph

# there is a nice and easy-to-use segregation measure in igraph
?assortativity # there are three types of assortativity coefficients
# interpretation: positive: homophily , 0: random, negative: heterophily  

# try out what happens when you feed the network matric to the function
assortativity(friendship.1)
# yes, igraph functions often work with special igraph objects only
# this makes life with igraph a bit complicated
# but luckily the package also comes with a function that is able to transform
# adjacency matrices to igraph objects
graph.1 <- graph.adjacency(friendship.1)
graph.2 <- graph.adjacency(friendship.2)
graph.1 # looks strange, but whatever

# finally! let's try out assortativity

# first on age homophily
# unfortunately, the function has some issues with missings at the moment
# there is one missing age, which we now set to the median age
age[is.na(age)] <- median(age, na.rm=TRUE) # note: the argument na.rm=TRUE tells R
                                           # to ignore missings when calculating the median;
                                           # when omitted or set to FALSE, the result is NA
                                           # if there is just one missing value in your data
# tendencies for friendship between students of similar/different age at the two waves
assortativity(graph.1, age)
assortativity(graph.2, age)
# is there age homophily?

# second, we asses whether there is gender homophily in friendship choice
assortativity.nominal(graph.1, sex.F)
assortativity.nominal(graph.2, sex.F)
# is there gender homophily?

# lastly, we can look at "degree homophily"
# that is, do students with similar number of friends tend to be friends with each other?
assortativity.degree(graph.1)
assortativity.degree(graph.2)
# this calls the degree function, calculates degrees, and uses them as node attributes
# (just like gender or age)
# note: if you had loaded sna after igraph, the degree function used by
#       assortativity.degree would have been that from sna,
#       which has different arguments - so horrible things would have occured
# strangely, you cannot specify in assortativity.degree whether you want to use
# in- or outdegree, it only works with total degree at the moment - DOES IT MATTER AT ALL??????


### 5. NETWORK VISUALIZATION WITH THE igraph PACKAGE

# let's make nice plots of the two friendship graphs
# visualization is something that igraph is good at
# (this part of the script is mainly borrowed from Christoph Stadtfeld)

# we already have the two networks as igraph objects
# first, set up a layout that is good for both networks (using the same trick as before)
graph.12 <- graph.adjacency(friendship.1 + friendship.2)
myLayout <- layout.fruchterman.reingold(graph.12)

# then plot the two networks with node colors corresponding to the sex of students
par(mfrow = c(1, 2))
plot(graph.1,
     vertex.color = ifelse(sex.F == 2, "red", "darkblue"),
     vertex.shape = ifelse(sex.F == 2, "square", "circle"),
     edge.color = "black",
     edge.width = 1,
     edge.arrow.size = 0.05,
     vertex.size = 4,
     vertex.label = "",
     layout = myLayout,
     main = "Friendship network - wave 1")
plot(graph.2,
     vertex.color = ifelse(sex.F == 2, "red", "darkblue"),
     vertex.shape = ifelse(sex.F == 2, "square", "circle"),
     edge.color = "black",
     edge.width = 1,
     edge.arrow.size = 0.05,
     vertex.size = 4,
     vertex.label = "",
     layout = myLayout,
     main = "Friendship network - wave 2")
par(mfrow=c(1,1))
# if the plots don't look good on your machine, play around a bit with the plotting arguments

### Differences in plotting between igraph and sna:
# The "plot" function we used is actually the most basic x-y plotting function
# from the package graphics (comes with the base R installation). While sna had
# its own graph plotting function, gplot, igraph takes a different strategy:
# it informs the plot function about what it should do when an "igraph" class
# object is passed on to it.
###

# as another plotting exercise in igraph:
# can we make the node sizes proportional to the indegree of students?
par(mfrow = c(1, 2))
plot(graph.1,
     vertex.color = ifelse(sex.F == 2, "red", "darkblue"),
     vertex.shape = ifelse(sex.F == 2, "square", "circle"),
     edge.color = "black",
     edge.width = 1,
     edge.arrow.size = 0.05,
     vertex.size = indeg.1,
     vertex.label = "",
     layout = myLayout,
     main = "Friendship network - wave 1")
plot(graph.2,
     vertex.color = ifelse(sex.F == 2, "red", "darkblue"),
     vertex.shape = ifelse(sex.F == 2, "square", "circle"),
     edge.color = "black",
     edge.width = 1,
     edge.arrow.size = 0.05,
     vertex.size = indeg.2,
     vertex.label = "",
     layout = myLayout,
     main = "Friendship network - wave 2")
par(mfrow=c(1,1))


### 6. GENDER SEGREGATION IN MORE DETAIL - FRIENDSHIP SELECTION TABLES

# gender homophily was quite strong in both waves, but why is that?
# do girls and boys make equally homophilous friendship choices?

# MINITASK: look at the graph plots again and make a guess.

### Friendship selection tables:
# How can we answer the question above? One way is to look at the
# density of subgraphs (subsets of the network matrix) and create
# a so-called friendship selection table by counting these four
# types of friendship ties:
#     girl -> girl,
#     girl -> boy,
#     boy  -> boy,
#     boy  -> girl.
# Of course, the raw number of ties from girls to girls, boys to girls, etc.
# would not be too informative, because there aren't equally many boys and girls.
# So it is better to calculate the density of each subgraph which represents
# the ratio of existing ties to all possible ties given the number of actors.
###

# we concentrate on the first network
# first, store the four subsets of the network in objects
gg.1 <- friendship.1[sex.F==2, sex.F==2]
gb.1 <- friendship.1[sex.F==2, sex.F==1]
bb.1 <- friendship.1[sex.F==1, sex.F==1]
bg.1 <- friendship.1[sex.F==1, sex.F==2]
# do their dimensions match the number of boys and girls in the network?
table(sex.F)
dim(gg.1)
dim(gb.1)
dim(bb.1)
dim(bg.1)

# now create an empty selection table which we can fill in later
friend.selection <- matrix(NA, 2, 2)
rownames(friend.selection) <- c("girl", "boy")
colnames(friend.selection) <- c("girl", "boy")
# it looks like this
friend.selection # rownames: sex of sender; colnames: sex of receiver

# the igraph density function is quite inflexible, let's switch back to sna
detach(package:igraph)
library(sna)

# fill in the selection table with the subgraph densities
friend.selection[1,1] <- gden(gg.1, diag=FALSE)
friend.selection[1,2] <- gden(gb.1, diag=TRUE)
friend.selection[2,2] <- gden(bb.1, diag=FALSE)
friend.selection[2,1] <- gden(bg.1, diag=TRUE)

# MINITASK: when do we set the "diag" argument to TRUE and when to FALSE?

# here is our selection table
friend.selection
# normalize by average density
(friend.selection.norm <- friend.selection / gden(friendship.1))
friend.selection[1,1]/friend.selection[1,2]
friend.selection[2,2]/friend.selection[2,1]
# MINITASK: interpret the boy-girl selection table
#           whose friendship choices are more homophilous?
#           what is the odds ratio for a girl-girl vs. a girl-boy friendship?
#           what is the odds ratio for a boy-boy vs. a boy-girl friendship?

### Selection table with average degrees:
# Another way to make the number of ties in same and opposite sex dyads
# meaningful is to calculate the average degrees in each category. Then
# the table shows us how many boy friends girls have on average, and so on.
###

# let's make a selection table based on average degrees

# again we start by creating an empty table
friend.selection.degree <- matrix(NA, 2, 2)
rownames(friend.selection.degree) <- c("girl", "boy")
colnames(friend.selection.degree) <- c("girl", "boy")

# fill in the cells
friend.selection.degree[1,1] <- mean(degree(gg.1, diag=FALSE, cmode="outdegree"))
friend.selection.degree[1,2] <- mean(degree(gb.1, diag=TRUE, cmode="outdegree"))
friend.selection.degree[2,2] <- mean(degree(bb.1, diag=FALSE, cmode="outdegree"))
friend.selection.degree[2,1] <- mean(degree(bg.1, diag=TRUE, cmode="outdegree"))

# look at the result
friend.selection.degree
# more readable this way
print(friend.selection.degree, digits=1)
# compare it with the density-based calculations
print(friend.selection.norm, digits=2)

# MINITASK: the average degree in the boy-boy cell is larger than in the girl-girl cell,
#           but when looking at the densities, the girl-girl cell is the larger - why?


### SECTION SUMMARY

###
# This time you have learnt about plotting graphs and analyzing homophily
# using the the igraph packge. The new functions were: assortativity,
# assortativity.nominal, assortativity.degree, and plot (with igraph objects).
# You have also used the gden and degree functions from the sna package for
# more advanced things - to study gender homophily in different directions.
#
# In addition, you now know that some packages may use identically named funcions,
# and that this can cause confusion. Pay attention to this when working with several
# different packages in the future. Apart from this, the concepts of average degree
# and friendship selection table were discussed - these will be very useful later
# in the course.
###

# This is the end of our short tour in the igraph package.
# There are many more functions we have not discussed - google for igraph to learn more.

# We continue with another practical exercise.
# First, save your Glasgow data again, because we are still going to continue work on it.
save.image("MyGlasgowData.RData")
# And remove all objects
rm(list=ls())

### END OF SCRIPT - PLEASE OPEN: "practice2.R"