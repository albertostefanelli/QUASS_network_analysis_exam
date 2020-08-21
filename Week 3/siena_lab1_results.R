##############################################
# SIENA lab exercise 1
# Networks course in Leuven, March 2018
#
# Fitting a SIENA model to one school class of the Mannheim data
#
# Testing structural network effects and
# homophily on 'gender' and 'country of origin'
# in the first classroom
#
# 1. read data from files
# 1.5 calculate network descriptives
# 2. create SIENA objects
# 3. specify SIENA model
# 4. create algorithm
# 5. estimate SIENA model
#
# The data used are from the Children of Immigrants Study,
# (c) MZES Mannheim, Manfred Kalter
# Please do not use and distribute outside the course.
#
# Script by Christoph Stadtfeld
# (minor changes by Andras Voros)
##############################################


# 0. preparations

# if you don't have the RSiena package installed
# install it - from R-Forge! (the version at CRAN is older)
install.packages("RSiena", repos="http://R-Forge.R-project.org")

# loading necessary packages
library(RSiena)
library(igraph)

# set the working directory

setwd("PATH TO FOLDER THE CONTAINS THE DATA SUBFOLDERS")


#############################################
# 1. read data from files and prepare raw data

friendship.t1 <- as.matrix(read.table(file = "Mannheim_data/friendship.network.t1.dat"))
friendship.t2 <- as.matrix(read.table(file = "Mannheim_data/friendship.network.t2.dat"))
gender <- unlist(read.table(file = "Mannheim_data/gender.dat"))
coo <- unlist(read.table(file = "Mannheim_data/coo.dat"))
nActors <- dim(friendship.t1)[1]

# define a network cut-off
threshold <- 1
friendship.t1[friendship.t1 >= threshold] <- 1
friendship.t2[friendship.t2 >= threshold] <- 1


################################################
# 1.5 Decribe the data
# Plotting the networks without attributes

graph1 <- graph.adjacency(friendship.t1)
graph2 <- graph.adjacency(friendship.t2)
graph12 <- graph.adjacency(friendship.t1 + friendship.t2)
myLayout <- layout.fruchterman.reingold(graph12)

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

################################################
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

################################################
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


##########################################
# 2. create internal SIENA objects

# create dependent network variable
friendship.dependent <- sienaDependent(array(c(friendship.t1, friendship.t2),
                                             dim=c(nActors, nActors, 2)))

# create constant actor covariates
coo.coCovar <- coCovar(coo)
gender.coCovar <- coCovar(gender)

mySienaData <- sienaDataCreate(friendship.dependent,
                               coo.coCovar,
                               gender.coCovar)

# print report to check
print01Report(mySienaData,
               modelname="mannheim_network_1")


##############################################
# 3. Specify SIENA model

mySienaEffects <- getEffects(mySienaData)


# network effects
mySienaEffects <- includeEffects(mySienaEffects, transTrip, cycle3)
mySienaEffects <- includeEffects(mySienaEffects, inPop)

# homophily effects and ego alter control
mySienaEffects <- includeEffects(mySienaEffects, egoX, altX, sameX, interaction1="gender.coCovar")
mySienaEffects <- includeEffects(mySienaEffects, egoX, altX, sameX, interaction1="coo.coCovar")

mySienaEffects # check parameters before estimation


##########################################
# 4. Create SIENA algorithm

mySienaAlgorithm <- sienaAlgorithmCreate(projname="mannheim_network_1",
                                         MaxDegree=c(friendship.dependent=5))

##########################################
# 5. Estimate

result <- siena07(mySienaAlgorithm,
                  data=mySienaData,
                  effects=mySienaEffects)

result

siena.table(result, type="html", file="results.html")
