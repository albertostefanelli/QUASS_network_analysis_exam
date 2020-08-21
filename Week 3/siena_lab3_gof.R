##############################################
# SIENA lab exercise 3
# Networks course in Leuven, March 2018
#
# Goodness of fit based on the Mannheim data
#
# 1. read data from files
# 1,5. plot networks
# 2. create SIENA objects
# 3. specify SIENA model
# 4. specify the estimation algorithm
# 5. estimate SIENA model
#
# (c) Christoph Stadtfeld
##############################################

# loading necessary packages
library(RSiena)
library(igraph)

#############################################
# 1. read data from files and prepare raw data

friendship.t1 <- as.matrix(read.table(file = "Mannheim_data/friendship.network.t1.dat"))
friendship.t2 <- as.matrix(read.table(file = "Mannheim_data/friendship.network.t2.dat"))
gender <- unlist(read.table(file = "Mannheim_data/gender.dat"))
coo <- unlist(read.table(file = "Mannheim_data/coo.dat"))
nActors = dim(friendship.t1)[1]

# define a network cut-off
threshold <- 1
friendship.t1[friendship.t1 >= threshold] <- 1
friendship.t2[friendship.t2 >= threshold] <- 1


################################################
# 1.5.a Plotting the networks without attributes

graph1 <- graph.adjacency(friendship.t1)
graph2 <- graph.adjacency(friendship.t2)
graph12 <- graph.adjacency(friendship.t1 + friendship.t2)
myLayout <- layout.fruchterman.reingold(graph12)

par(mfrow = c(1, 2))
plot(graph1,
     vertex.color = "darkblue",
#     vertex.shape = ifelse(gender == 1, "square", "circle"),
     edge.color = "black",
     edge.width = 2,
     edge.arrow.size = 0.1,
     vertex.size = 10,
     vertex.label = "",
     layout = myLayout,
     main = "Network wave 1")
plot(graph2,
     vertex.color = "darkblue",
#     vertex.shape = ifelse(gender == 1, "square", "circle"),
     edge.color = "black",
     edge.width = 2,
     edge.arrow.size = 0.1,
     vertex.size = 10,
     vertex.label = "",
     layout = myLayout,
     main = "Network wave 2")
par(mfrow=c(1, 1))

################################################
# 1,5.b Plotting the networks with gender homophily

par(mfrow = c(1, 2))
plot(graph1,
     vertex.color = ifelse(gender == 1, "pink", "darkblue"),
     vertex.shape = ifelse(gender == 1, "square", "circle"),
     edge.color = "black",
     edge.width = 2,
     edge.arrow.size = 0.1,
     vertex.size = 10,
     vertex.label = NA,
     layout = myLayout,
     main = "Network wave 1")
plot(graph2,
     vertex.color = ifelse(gender == 1, "pink", "darkblue"),
     vertex.shape = ifelse(gender == 1, "square", "circle"),
     edge.color = "black",
     edge.width = 2,
     edge.arrow.size = 0.1,
     vertex.size = 10,
     vertex.label = "",
     layout = myLayout,
     main = "Network wave 2")
par(mfrow=c(1, 1))

################################################
# 1,5.c Plotting the networks with ethnical homophily
par(mfrow = c(1, 2))
plot(graph1,
     vertex.color = coo,
     vertex.shape = ifelse(gender == 1, "square", "circle"),
     edge.color = "black",
     edge.width = 2,
     edge.arrow.size = 0.1,
     vertex.size = 10,
     vertex.label = "",
     layout = myLayout,
     main = "Network wave 1")
plot(graph2,
     vertex.color = coo,
     vertex.shape = ifelse(gender == 1, "square", "circle"),
     edge.width = 2,
     edge.color = "black",
     edge.arrow.size = 0.1,
     vertex.size = 10,
     vertex.label = "",
     layout = myLayout,
     main = "Network wave 2")
par(mfrow=c(1, 1))


##########################################
# 2. create internal SIENA objects

# create dependent network variable
friendship.dependent <- sienaDependent(array (c(friendship.t1, friendship.t2), dim = c(nActors, nActors, 2) ) )

# create constant actor covariates
coo.coCovar <- coCovar(coo)
gender.coCovar <- coCovar(gender)

mySienaData <- sienaDataCreate(friendship.dependent,
                               coo.coCovar,
                               gender.coCovar)

# print report to check
print01Report( mySienaData,
               modelname = 'mannheim_network_1' )


##############################################
# 3. Specify SIENA model

mySienaEffects <- getEffects(mySienaData)


# network effects
mySienaEffects <- includeEffects(mySienaEffects, transTrip, cycle3)
mySienaEffects <- includeEffects(mySienaEffects, inPop)

# homophily effects and ego alter control
mySienaEffects <- includeEffects(mySienaEffects, egoX, altX, sameX, interaction1 = "gender.coCovar")
mySienaEffects <- includeEffects(mySienaEffects, sameX, interaction1 = "coo.coCovar")

mySienaEffects # check parameters before estimation


##########################################
# 4. Specify estimation algorithm

mySienaModel <- sienaAlgorithmCreate(projname = "mannheim_network_1")


##########################################
# 5. Create SIENA model and estimate

result <- siena07(mySienaModel,
                  data = mySienaData,
                  effects = mySienaEffects,
                  returnDeps = TRUE)

result


##########################################
# SIENA GOF

?sienaGOF

gofResult.indegree <- sienaGOF(result, IndegreeDistribution, varName = "friendship.dependent", verbose = TRUE)

gofResult.outdegree <- sienaGOF(result, OutdegreeDistribution, varName = "friendship.dependent", verbose = FALSE)

   # copied from the script on the RSiena website
   GeodesicDistribution <- function (i, data, sims, period, groupName,
                           varName, levls=c(1:5,Inf), cumulative=TRUE, ...) {
     x <- networkExtraction(i, data, sims, period, groupName, varName)
     require(sna)
     a <- sna::geodist(x)$gdist
     if (cumulative)
     {
       gdi <- sapply(levls, function(i){ sum(a<=i) })
     }
	 else
     {
       gdi <- sapply(levls, function(i){ sum(a==i) })
     }
     names(gdi) <- as.character(levls)
     gdi
   }

gofResult.geodesic <- sienaGOF(result, GeodesicDistribution, varName = "friendship.dependent", verbose = FALSE)

   # copied from Siena website GOF example script
   TriadCensus <- function(i, data, sims, wave, groupName, varName, levls=1:16){
       unloadNamespace("igraph") # to avoid package clashes
       require(sna)
       require(network)
       x <- networkExtraction(i, data, sims, wave, groupName, varName)
       tc <- sna::triad.census(x)[1,levls]
       # triad names are transferred automatically
       tc
   }

gofResult.triadcensus <- sienaGOF(result, TriadCensus, varName = "friendship.dependent", verbose = TRUE)


plot(gofResult.indegree)
plot(gofResult.outdegree)
plot(gofResult.geodesic)
plot(gofResult.triadcensus, center = TRUE, scale = TRUE)

##########################################
# Get one of the simulated networks

adjList1 <- result$sims[[1]]$Data1$friendship.dependent$`1`
adjList <- list(list())

#transform to igraph adjacency list
for(i in 1:29){
  adjList[[i]] <- adjList1[adjList1[,1] == i, 2]
}

simGraph <- graph.adjlist(adjList)

plot(simGraph)

##########################################
# Plot real network and one simulated network (the first one)


graph1 <- graph.adjacency(friendship.t2)
graph2 <- simGraph

myLayout <- layout.fruchterman.reingold(graph12)

par(mfrow = c(1, 2))
plot(graph1,
     vertex.color = "darkblue",
#     vertex.shape = ifelse(gender == 1, "square", "circle"),
     edge.color = "black",
     edge.width = 2,
     edge.arrow.size = 0.1,
     vertex.size = 10,
     vertex.label = "",
     layout = myLayout,
     main = "Network wave 2")
plot(graph2,
     vertex.color = "darkblue",
#     vertex.shape = ifelse(gender == 1, "square", "circle"),
     edge.color = "black",
     edge.width = 2,
     edge.arrow.size = 0.1,
     vertex.size = 10,
     vertex.label = "",
     layout = myLayout,
     main = "Simulated network")
par(mfrow = c(1, 1))
