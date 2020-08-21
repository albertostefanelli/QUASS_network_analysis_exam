#                       ###############################################                            #
#                       ###    SOCIAL NETWORK ANALYSIS WITH R I.    ###                            #
#                       ### BASIC DESCRIPTIVES WITH THE sna PACKAGE ###                            #
#                       ###############################################                            #

###
# This script was originally made for the course on social network analysis with R and
# RSiena held in Budapest on 3-5 December 2014.
#
# The script was prepared by Andras Voros.
# (this is the version of 11.02.2018)
###

###
# Now that everyone is super familiar with the functionalities of R,
# it is time to learn about the options for analyzing social networks
# provided by the most popular network packages in R, sna and igraph.
# While doing so, you will also a few additional things about R in general.
###


### 0. R PACKAGES

### A few words about packages:
# The base R installation does not come with functions for network analysis.
# However, a good thing in R is that it's absolutely open and free, and users
# are allowed to contribute to its functionalities by creating their own
# so-called PACKAGES. A package is a collection of functions, object class
# definitions, and so on which usually build on and complement the basic
# functions and classes in R.
#
# The official list of contributed packages can be found at the CRAN website
# (these are packages that will surely not ruin your are, that are maintained, etc.):
# http://cran.r-project.org/web/packages/available_packages_by_name.html
#
# There are hundreds of contributed packages. So whenever you find that something
# cannot be done with your base R installation, it is useful to check other
# packages - maybe someone has already done the programming for you!
###

# we will use the sna and igraph packages today
# to use a package you need to do two things:
# 1. install it (only once)
# 2. load it (every time you start R)

# you can install most packages from within R (if you have internet connection)
# for example, if the sna package is not yet installed on your computer, run this:



rm(list=ls(all=TRUE))
knitr::opts_chunk$set(fig.pos = 'H')
library(devtools)
# install and load defined list of packages
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE,repos = "http://cran.us.r-project.org")
  sapply(pkg, require, character.only = TRUE)
}

# it is useful to update the packages sometimes - check the update.packages() function


list_of_required_pkg <- c(
#"qualtRics",
"sna"
#"urlshorteneR"
  
)

ipak(list_of_required_pkg)


# now you can load the sna package# from this moment you can use the functions available in the sna package


### 1. LOADING THE DATA - THE .RData FORMAT

# for this section, we use some parts of the Glasgow dataset again,
# now a larger, 160-actor subsample

# first make sure to clear your workspace
rm(list=ls())
# and check whether your working directory is set to where the data are on you computer
getwd()
# an if not, change it
# setwd("PATH TO FOLDER THAT CONTAINS THE DATA SUBFOLDERS")

# the data is in .RData format this time, which is R's own file type,
# so it is really easy to load it
load("Glasgow_data/Glasgow-friendship.RData")
ls() # note: several objects appeared in the workspace

# we are only going to use data from the first two waves, we can remove the last object
rm(friendship.3)

# now load the demographic and geopgraphic datasets
load("Glasgow_data/Glasgow-demographic.RData") # sex, age
load("Glasgow_data/Glasgow-geographic.RData") # distance from school and others
ls()
# from the geographic data, we only need the distances between students' homes at t1 and t2,
# we remove the rest
rm(angle.1, angle.2, angle.3, dist.school, distance.3)


### 2. INSPECTION OF THE DATA OBJECTS

###
# In all cases, before we start analyzing a new dataset, we need to learn about the data.
# This means two things in R:
#
# Step 1: learn about the objects (class, mode, no. of observations, value codes, etc.)
# Step 2: learn about the data (distributions, missings, etc.)
#
# The first step is extremely important becuase we usually work with several objects in R,
# so many things that are self-evident in, say, SPSS are not here (e.g. do object dimensions
# match? are observations in the same order in all objects?).
# The second step is also extremely important because we need to know what is in the objects.
#
# Taking these steps in the beginning can help save a lot of time later.
###

# the first step on the friendship networks
class(friendship.1)
mode(friendship.1)
dim(friendship.1)
rownames(friendship.1)
colnames(friendship.1)
# are there the same students in the first two waves?
all.equal(rownames(friendship.1), rownames(friendship.2)) # note: the order matters for all.equal()
# what kind of values are there in the data?
table(friendship.1)
table(friendship.2) # not friend - best friend - just a friend - weird value

# recode the data: 1. we don't need a distinction between best friends and friends now
#                  2. we set the 10s to 0 (we will learn about the magical "10" later in the course)
friendship.1[friendship.1 == 2] <- 1
friendship.2[friendship.2 == 2] <- 1
friendship.1[friendship.1 == 10] <- 0
friendship.2[friendship.2 == 10] <- 0

table(friendship.1)
table(friendship.2)

# let's have a look at the other variables
table(sex.F) # 2 - girl, 1 - boy
hist(age) # age in years
hist(distance.1) # distance between students' homes in km at t1
hist(distance.2) # the same at t2


### 3. NETWORK VISUALIZATION

# now the real network stuff - let's describe the friendship networks at the two time points!
# from here on we will mostly use functions from the sna package

# how does the network look like - plot it!
?gplot
plot1 <- gplot(friendship.1)
plot2 <- gplot(friendship.2)

# Two things:
# 1. if you run gplot on the same data several times, it will be a little different
#    because the node placement algorithm is not exact but iterative
# 2. related to node placement, it is hard to compare the two plots - what can we do?

# what is saved in plot1 and plot2? you would never guess.
head(plot1) # head() displays the first part of an object
# so, what if...
gplot(friendship.1, coord=plot1) # now it will always look the same
# and
gplot(friendship.2, coord=plot1)
# it looks quite horrible, but here is a little trick:
plot12 <- gplot(friendship.1 + friendship.2)
# and now plot them both with the same node coordinates
gplot(friendship.1, coord=plot12)
gplot(friendship.2, coord=plot12)

# let's pimp our plots: they need a title, and perhaps boys and girls could get different colors
gplot(friendship.1, coord=plot12, main="Friendship network - wave 1", vertex.col=sex.F)
gplot(friendship.2, coord=plot12, main="Friendship network - wave 2", vertex.col=sex.F)

### Graphical parameters:
# gplot, just like other plotting functions in R, uses a GRAPHICS DEVICE
# this has many features that can be changed by equally many GRAPHICAL PARAMETERS
# examples: how large should be the plotting region? what kind of font should be used for
# the title? how big should symbols be? (the default setup is roughly what you see now)
#
# type ?par whenever you are bored to find out about the plotting settings you can adjust
# the help page also suggests good and bad practices for restoring the default settings
# or use google to find out about what the "R community" suggests for dealing with par()
###

# the graphical paramters are set to the default when you restart R,
# so you need to change them before you run gplot, like this:
par(mfrow=c(1,2))
# now plot your graphs again (still looks bad, but we will do better later)
gplot(friendship.1, coord=plot12, main="Friendship network - wave 1", vertex.col=sex.F)
gplot(friendship.2, coord=plot12, main="Friendship network - wave 2", vertex.col=sex.F)
# after you are done, you need to reset the graphical parameter you changed
# or you will get two plots per figure forever (or until you restart the R session)
par(mfrow=c(1,1))

# MINITASK: what can you tell about the friendship network of this school based on the graphs?


### 4. NETWORK DESCRIPTIVES

# some basic functions in the sna package help you to find out things
# that are not plainly visible from the graphs


# a) the density of the networks
gden(friendship.1)
gden(friendship.2)
# NAs are omitted by default - ?gden to see other options

# using the basic packages, the calculation of density would look something like this:
sum(friendship.1==1, na.rm=TRUE)/(sum(friendship.1 %in% c(0,1), na.rm=TRUE) - nrow(friendship.1))
# quite long...
# ...and the results are different because somebody put a "1" and some missings in the diagonal
# gden() takes care of these problems for you - thank you sna!


# b) reciprocity
grecip(friendship.1)
grecip(friendship.2)
# these look suspicious

# MINITASK: find out what is "wrong"
#           what do we need to add to the gplot call to get the "usual" reciprocity rates
?grecip
grecip(friendship.1, measure="dyadic.nonnull")
grecip(friendship.2, measure="dyadic.nonnull")


# c) the degree distribution
?degree
(outdeg.1 <- degree(friendship.1, cmode="outdegree")) # it returns a vector of degrees
indeg.1 <- degree(friendship.1, cmode="indegree")
outdeg.2 <- degree(friendship.2, cmode="outdegree")
indeg.2 <- degree(friendship.2, cmode="indegree")

# normally, we would visualize the distribution of degrees on histograms
?hist
hist(outdeg.1)
hist(indeg.1)
# the outdegree dispersion is low in this case, and the histogram looks bad
# we can fix this manually
hist(outdeg.1, breaks=7)
# to make the two distributions better comparable
hist(outdeg.1, xlim=c(0,12), ylim=c(0,50), breaks=7)
hist(indeg.1, xlim=c(0,12), ylim=c(0,50), breaks=13)
# now plot them both
par(mfrow=c(1,2))
hist(outdeg.1, xlim=c(0,12), ylim=c(0,50), breaks=7)
hist(indeg.1, xlim=c(0,12), ylim=c(0,50), breaks=13)
par(mfrow=c(1,1)) # remember to reset graphical parameters

# finally, let's make a relatively nice plot for in- and outdegrees from both waves
par(mfrow=c(2,2))
hist(outdeg.1, xlim=c(0,12), ylim=c(0,50), breaks=7,
     main="Outdegree distribution in wave 1", xlab="Outdegree", col="blue")
hist(indeg.1, xlim=c(0,12), ylim=c(0,50), breaks=13,
     main="Indegree distribution in wave 1", xlab="Indegree", col="red")
hist(outdeg.2, xlim=c(0,12), ylim=c(0,50), breaks=7,
     main="Outdegree distribution in wave 2", xlab="Outdegree", col="blue")
hist(indeg.2, xlim=c(0,12), ylim=c(0,50), breaks=13,
     main="Indegree distribution in wave 2", xlab="Indegree", col="red")
par(mfrow=c(1,1))


# MINITASK: what can you tell about the friendship network in this school based on
#           the density and reciprocity measures and on the degree distributions?
#           What is the main tendency in activity (outdegree) change?
#           Can we suspect the Matthew effect ("rich get richer") behind the dynamics of indegrees?

### Cheap wisdom about plots:
# Unfortunately, plotting functions in base R are not very strong/sophisticated,
# so it becomes quite a piece of work when you want to do something not basic and
# good-looking, often you have to do some extra programming on your own (e.g. aggregate
# data before plotting). Luckily, however, there are plenty of advanced options over
# the basic plotting functions, e.g. the lattice, ggplot2, igraph packages offer quite
# a few functions (google, download, try them!). But in the end, the price of nice plots
# is that you have to learn 1) a bit of R in general and 2) how to handle advanced plotting
# functions in particular.
###


### SECTION SUMMARY

###
# You have learnt how to do the most basic descriptive analyses on networks in R:
# to plot graphs, calculate the density, reciprocity, and degrees.
# That's a good start! Actually, this is enough for us to do the first individual
# practice session.
#
# In addition, we covered a few additional general topics related to data analysis
# in R, including the use of packages, RData files, basic plotting functions,
# and graphical parameters.
###

# Our first take on the sna package concludes here.
# Before we proceed, it makes sense to clear your workspace to avoid any confusion.
# But first save your objects in an RData file, because we will use this data later.
# This is how to save your entire workspace (into your working directory):
save.image("MyGlasgowData.RData")
# Now it is safe to remove all the objects
rm(list=ls())

### END OF SCRIPT - PLEASE OPEN: "practice1.R"