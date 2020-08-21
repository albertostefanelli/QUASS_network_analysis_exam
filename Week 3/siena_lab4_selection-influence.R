##############################################
# SIENA lab exercise 4
# Networks course in Leuven, March 2018
#
# This is a short script for analysing the co-evolution of the friendship 
# network and smoking behaviour. The data used are from the Glasgow Teenage
# Friends and Lifestyle Study. More information and copyright information at
# http://www.stats.ox.ac.uk/~snijders/siena/Glasgow_data.htm
#
# The script is based a script by Tom Snijders from the SIENA website: 
# http://www.stats.ox.ac.uk/~snijders/siena/ with modifications by 
# Christoph Stadtfeld and Zsofia Boda
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
library(RSiena)
library(igraph)

####################################################
# Step 1: load and recode the data
####################################################

# load the data (you can find it among the course materials,
# or you can download it from the Siena website:
# http://www.stats.ox.ac.uk/~snijders/siena/Glasgow_data.zip)

# here are the friendship networks at the three time points
load("Glasgow_data/Glasgow-friendship.RData")
# here is the data for alcohol, cannabis and tobacco use of the pupils
load("Glasgow_data/Glasgow-substances.RData")

# friendships:
table(friendship.1) # needs recoding
friendship.1[friendship.1 %in% c(1,2)] <- 1
friendship.2[friendship.2 %in% c(1,2)] <- 1
friendship.3[friendship.3 %in% c(1,2)] <- 1

#alcohol
alcohol # three observations, three coloumns. 
table(alcohol)
table(alcohol, useNA="always")
table(alcohol[,1], useNA="always")
table(alcohol[,2], useNA="always")
table(alcohol[,3], useNA="always")
# values are between 1 and 5, and a higher number means a higher frequency
# of alcohol consumption. there are missings, that's ok.
# we can find more information about the data and the coding on the website:
# www.stats.ox.ac.uk/~snijders/siena/ (data sets -> Glasgow data sescription)
tobacco
table(tobacco, useNA="always")
table(tobacco[,1], useNA="always")
table(tobacco[,2], useNA="always")
table(tobacco[,3], useNA="always")
# tobacco use is measured between 1 and 3.

cleanMatrix <- function(m){
  m[m ==10] <- 0
  diag(m) <- 0
  m
}

# Let's check how the networks look like!
g1 <- graph.adjacency(cleanMatrix(friendship.1))
g2 <- graph.adjacency(cleanMatrix(friendship.2))
g3 <- graph.adjacency(cleanMatrix(friendship.3))
g123 <- graph.adjacency((cleanMatrix(friendship.1) + cleanMatrix(friendship.2) 
                         + cleanMatrix(friendship.3))>0)
layout <- layout.auto(g123)

alcohol_v <- alcohol
alcohol_v[is.na(alcohol_v)==T]<-0
tobacco_v <- tobacco
tobacco_v[is.na(tobacco_v)==T]<-0

myplot <- function(g, wave_a, wave_t) plot(g,
       vertex.size = tobacco_v[,wave_t]+4,
       vertex.label = "",
       vertex.color = rgb(alcohol_v[,wave_a], 5-alcohol_v[,wave_a], 
                          0, max = 5),
       vertex.shape = "circle",
       edge.arrow.size = 0.1,
       edge.width = 1.0,
       edge.color = "grey",
       layout = layout,
       edge.curved = 0.1)
# Size of nodes is based on cigarette smoking, color is based on 
# alcohol consumption.

par(mfrow = c(1, 3))

# The three networks:
myplot(g1, wave_a=1, wave_t=1)
myplot(g2, wave_a=2, wave_t=2)
myplot(g3, wave_a=3, wave_t=3)

# Now only the first and the last observation.
# First only the tie changes, then only the behavior changes.
myplot(g1, wave_a=1, wave_t=1)
myplot(g3, wave_a=1, wave_t=1)
myplot(g3, wave_a=3, wave_t=3)

# change of smoking/drinking over time, now using numbers!
apply(tobacco, 2, mean, na.rm=T)
apply(tobacco, 2, sd, na.rm=T)
apply(alcohol, 2, mean, na.rm=T)
apply(alcohol, 2, sd, na.rm=T)

####################################################
# Step 2: create SIENA objects    
####################################################

# The friendship network is the first dependent variable
friendship <- sienaDependent(array(c(friendship.1, friendship.2,
                      friendship.3), dim = c(160, 160, 3)))

# Also smoking is modeled as dependent (behavioral) variable
smokingbeh <- sienaDependent(tobacco, type ="behavior")

# Alcohol drinking in wave 1 is defined as a constant covariate
drink1 <- coCovar(alcohol[,1])

# Define the data set
myCoEvolutionData <- sienaDataCreate(friendship, drink1, smokingbeh)

# print report to check
print01Report(myCoEvolutionData, modelname="Glasgow_coevolution")


####################################################
# Step 3: specify SIENA model
####################################################

myCoEvolutionEff <- getEffects(myCoEvolutionData)
myCoEvolutionEff

effectsDocumentation(myCoEvolutionEff, type="html", display=FALSE, filename="Coev effects")

# Run reports to check that data is properly formated and
# to get some basic descriptives

print01Report(myCoEvolutionData, myCoEvolutionEff, modelname = 's50_3_CoEvinit')

# Define the effects to include in the coevolution model -
# Start with some structural effects.

myCoEvolutionEff <- includeEffects(myCoEvolutionEff, transTrip, cycle3)

# Include a homophily effect for the constant covariate drinking

myCoEvolutionEff <- includeEffects(myCoEvolutionEff, simX, interaction1 = "drink1")

# If we want to parse out whether there is a selection or influence (or both)
# effect for smoking behaviour,
# we need to also include sender, receiver and homophily effects
# of smoking for friendship formation:

myCoEvolutionEff <- includeEffects(myCoEvolutionEff, egoX, altX, simX,
                                   interaction1 = "smokingbeh" )

# For the influence part, i.e. the effect of the network on behaviour,
# we specify the following effects:
# indegree, outdegree and assimilation effects for smoking

myCoEvolutionEff <- includeEffects(myCoEvolutionEff, name = "smokingbeh",
                                   avAlt, indeg, outdeg, interaction1 = "friendship")

# Check what effects you have decided to include:

myCoEvolutionEff

####################################################
#####  Step 4: create estimation algorithm     #####
####################################################

myCoEvAlgorithm <- sienaAlgorithmCreate(projname = 's50CoEv_3')

####################################################
######      Step 5: estimate SIENA model       #####
####################################################

ans <- siena07(myCoEvAlgorithm, data=myCoEvolutionData, effects=myCoEvolutionEff)
ans <- siena07(myCoEvAlgorithm, data=myCoEvolutionData, effects=myCoEvolutionEff,
               prevAns=ans_2)

# THE RESULTS

# now we are cheating a bit to save time, and loading the results from an RData file.
load("ans_lab3.RData")

# To look at the results, type
ans

# or, somewhat more extensive,
summary(ans)

# Note that the "convergence t-ratio" is the t-ratio for convergence checking,
# not the t statistic for testing the significance of this effect.

# We can print the table now!
siena.table(ans, type="html", sig=TRUE)