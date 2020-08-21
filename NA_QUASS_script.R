
rm(list=ls(all=TRUE))
library(devtools)
# install and load defined list of packages
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE,repos = "http://cran.us.r-project.org")
  sapply(pkg, require, character.only = TRUE)
}

list_of_required_pkg <- c(
#"qualtRics",
"dplyr",
"igraph",
"ggplot2",
"vioplot"
#"urlshorteneR"
  
)

ipak(list_of_required_pkg)







#### Plotting the frienship networks with drinking and gender homophily ####
par(mfrow = c(1, 2))


par(mfrow = c(1, 1))



t_friend_w1 <- graph.adjacency(trust_w1)
t_friend_w2 <- graph.adjacency(trust_w2)
t_friend_w1_w2 <- graph.adjacency(trust_w1 + trust_w2)
myLayout <- layout.kamada.kawai(t_friend_w1_w2)

t_in_w1 <- sna::degree(trust_w1, cmode="indegree")
t_in_w2 <- sna::degree(trust_w2, cmode="indegree")
sna::betweenness(trust_w1)    #Compute betweenness scores

density_t1_t <- sna::gden(trust_w1)
density_t2_t <- sna::gden(trust_w2)

#Conceptually, high-betweenness vertices lie on a large number of non-redundant shortest paths between other vertices; they can thus be thought of as ``bridges'' or ``boundary spanners.''


1-gden(trust_w2)/gden(trust_w1)


#### Plotting the frienship networks with drinking and gender homophily ####
par(mfrow = c(1, 2))


par(mfrow = c(1, 1))

, "Desnity Trust"


# table_descr[9,1] <- density_t1_t 
# table_descr[9,2] <- density_t2_t


## LOCAL LEVEL ##


dyadic_reciprocity_t1 <- sna::grecip(friendship_w1, measure="dyadic.nonnull")
dyadic_reciprocity_t2 <- sna::grecip(friendship_w2, measure="dyadic.nonnull")
1-sna::grecip(friendship_w2, measure="dyadic.nonnull")/sna::grecip(friendship_w1, measure="dyadic.nonnull")

## jaccard index 
#it disregards ties that are absent in both networks
# and only considers these cases

A <- sum((friendship_w1 * friendship_w2)==1, na.rm=TRUE) # #ties that exist in both networks
BplusC <- sum((friendship_w1 + friendship_w2)==1, na.rm=TRUE) # #ties that exist in only one network
(jaccard <- A/(A+BplusC))

## CENSUS dyadic 

dyad_count_w1 <- dyad.census(g_friend_w1)
dyad_count_w2 <- dyad.census(g_friend_w2)


### GENDER SEGRAGATION ####

graph_t1 <- graph.adjacency(friendship_w1)
graph_t2 <- graph.adjacency(friendship_w2)

assortativity.nominal(graph_t1, sex)
assortativity.nominal(graph_t2, sex)
assortativity.nominal(graph_t1, drink_w1)
assortativity.nominal(graph_t2, drink_w2)

## WAVE 1 

# we concentrate on the first network
# first, store the four subsets of the network in objects
gg.1 <- friendship_w1[sex==2, sex==2]
gb.1 <- friendship_w1[sex==2, sex==1]
bb.1 <- friendship_w1[sex==1, sex==1]
bg.1 <- friendship_w1[sex==1, sex==2]

# now create an empty selection table which we can fill in later
friend.selection <- matrix(NA, 2, 2)
rownames(friend.selection) <- c("girl", "boy")
colnames(friend.selection) <- c("girl", "boy")
# it looks like this
friend.selection # rownames: sex of sender; colnames: sex of receiver

# the igraph density function is quite inflexible, let's switch back to sna

# fill in the selection table with the subgraph densities
friend.selection[1,1] <- sna::gden(gg.1, diag=FALSE)
friend.selection[1,2] <- sna::gden(gb.1, diag=TRUE)
friend.selection[2,2] <- sna::gden(bb.1, diag=FALSE)
friend.selection[2,1] <- sna::gden(bg.1, diag=TRUE)

(friend.selection.norm_1 <- friend.selection / sna::gden(friendship_w1))
friend.selection.norm_2
#girl-girl vs. a girl-boy
OR_gg_vs_gb_t1 <- friend.selection[1,1]/friend.selection[1,2]
#  boy-boy X boy-girl 
OR_bb_vs_gb_t1 <- friend.selection[2,2]/friend.selection[2,1]


## WAVE 2 

# we concentrate on the first network
# first, store the four subsets of the network in objects
gg.1 <- friendship_w2[sex==2, sex==2]
gb.1 <- friendship_w2[sex==2, sex==1]
bb.1 <- friendship_w2[sex==1, sex==1]
bg.1 <- friendship_w2[sex==1, sex==2]
# do their dimensions match the number of boys and girls in the network?
table(sex)
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

# fill in the selection table with the subgraph densities
friend.selection[1,1] <- sna::gden(gg.1, diag=FALSE)
friend.selection[1,2] <- sna::gden(gb.1, diag=TRUE)
friend.selection[2,2] <- sna::gden(bb.1, diag=FALSE)
friend.selection[2,1] <- sna::gden(bg.1, diag=TRUE)

# here is our selection table
friend.selection
# normalize by average density
(friend.selection.norm_2 <- friend.selection / sna::gden(friendship_w2))

#girl-girl vs. a girl-boy
OR_gg_vs_gb_t2 <- friend.selection[1,1]/friend.selection[1,2]
#  boy-boy X boy-girl 
OR_bb_vs_gb_t2 <- friend.selection[2,2]/friend.selection[2,1]

### DRINKING - WAVE 1 ###

# first, store the four subsets of the network in objects
dd.1 <- friendship_w1[drink[,1]>1, drink[,1]>1]
dn.1 <- friendship_w1[drink[,1]>1, drink[,1]==1]
nn.1 <- friendship_w1[drink[,1]==1, drink[,1]==1]
nd.1 <- friendship_w1[drink[,1]==1, drink[,1]>1]

# now create an empty selection table which we can fill in later
drink.selection <- matrix(NA, 2, 2)
rownames(drink.selection) <- c("drinker", "non-drinker")
colnames(drink.selection) <- c("drinker", "non-drinker")
# it looks like this
drink.selection 

# the igraph density function is quite inflexible, let's switch back to sna

# fill in the selection table with the subgraph densities
drink.selection[1,1] <- sna::gden(dd.1, diag=FALSE)
drink.selection[1,2] <- sna::gden(dn.1, diag=TRUE)
drink.selection[2,2] <- sna::gden(nn.1, diag=FALSE)
drink.selection[2,1] <- sna::gden(nd.1, diag=TRUE)

(drink.selection_norm_1 <- drink.selection / sna::gden(friendship_w1))



#drinker-drinker vs. drinker-non-drinker a 
OR_dd_vs_dn_t1 <- drink.selection[1,1]/drink.selection[1,2]
#  non-drinker-non-drinker X non-drinker-drinker 
OR_bb_vs_gb_t1 <- drink.selection[2,2]/drink.selection[2,1]


## DRINKING - WAVE 2 ##

# we concentrate on the first network
# first, store the four subsets of the network in objects
dd.1 <- friendship_w2[drink[,1]>1, drink[,1]>1]
dn.1 <- friendship_w2[drink[,1]>1, drink[,1]==1]
nn.1 <- friendship_w2[drink[,1]==1, drink[,1]==1]
nd.1 <- friendship_w2[drink[,1]==1, drink[,1]>1]


# now create an empty selection table which we can fill in later
drink.selection <- matrix(NA, 2, 2)
rownames(drink.selection) <- c("drinker", "non-drinker")
colnames(drink.selection) <- c("drinker", "non-drinker")

# the igraph density function is quite inflexible, let's switch back to sna

# fill in the selection table with the subgraph densities
drink.selection[1,1] <- sna::gden(dd.1, diag=FALSE)
drink.selection[1,2] <- sna::gden(dn.1, diag=TRUE)
drink.selection[2,2] <- sna::gden(nn.1, diag=FALSE)
drink.selection[2,1] <- sna::gden(nd.1, diag=TRUE)

(drink.selection_norm_2 <- drink.selection / sna::gden(friendship_w2))

#drinker-drinker vs. drinker-non-drinker 
OR_dd_vs_dn_t2 <- drink.selection[1,1]/drink.selection[1,2]
#  non-drinker-non-drinker X non-drinker-drinker 
OR_bb_vs_gb_t2 <- drink.selection[2,2]/drink.selection[2,1]



#### CUG test ####

library()

triad.count_fr1 <- sna::triad.census(friendship_w1)
triad.count_fr2 <- sna::triad.census(friendship_w2)



par(mfrow = c(2, 2))




## WAVE 2 ##



par(mfrow = c(1, 1))




## WAVE 1 ##

par(mfrow=c(2,2))

plot(g_friend_w1,
vertex.color = ifelse(drink_w1 == 1, "#fa8072",
ifelse(drink_w1 == 2, "#cd5c5c", 
  ifelse(drink_w1 == 3, "#dc143c",
    ifelse(drink_w1 == 4, "#ff0000", "black")))), 
vertex.shape = ifelse(sex == 1, "square", "circle"),
vertex.size = friendship_in_w1*1.5, 
edge.color = "black",
edge.width = 1,
edge.arrow.size = 0.40,
layout = myLayout ,
main = "Friendship Network - wave 1", 
label = names ,
label.dist = 1, 
label.cex = 1,
vertex.label.color= "grey28", 
label.font = 4)
plot(fast_gready_1, friendship_w1_undirect ,
  edge.color = "black",
  edge.width = 1, 
  edge.arrow.size = 0.25,
  vertex.size = 15, 
  layout = myLayout ,
  main = "Fast ’n Greedy",
  vertex.shape = ifelse(sex == 1, "square", "circle"),
  label = names ,
  label.dist = 1,
  label.cex = 1,
  vertex.label.color= "grey28",
  label.font = 4)
plot(rand_walk_1, friendship_w1_undirect ,
  edge.color = "black",
  edge.width = 1,
  edge.arrow.size = 0.25,
  vertex.size = 15,
  layout=myLayout ,
  main = "Random Walk",
  vertex.shape = ifelse(sex == 1, "square", "circle"),
  label = names ,
  label.dist = 1,
  label.cex = 1,
  vertex.label.color= "grey28",
  label.font = 4)
plot(eigenvector_1, friendship_w1_undirect ,
  edge.color = "black",
  edge.width = 1,
  edge.arrow.size = 0.25,
  vertex.size = 15,
  layout=myLayout ,
  main = "Leading Eigenvector",
  vertex.shape = ifelse(sex == 1, "square", "circle"),
  label = names ,
  label.dist = 1,
  label.cex = 1,
  vertex.label.color= "grey28",
  label.font = 4)

## WAVE 2 ##

friendship_w2_undirect <- friendship_w2 %>% graph.adjacency() %>% 
  as.undirected()

fast_gready_2 <- fastgreedy.community(friendship_w2_undirect) 
rand_walk_2 <- walktrap.community(friendship_w2_undirect)
eigenvector_2 <- leading.eigenvector.community(friendship_w2_undirect) 
#edge_1 <- edge.betweenness.community(friendship_w1_undirect)

par(mfrow=c(2,2))

plot(g_friend_w2,
vertex.color = ifelse(drink_w2 == 1, "#fa8072",
ifelse(drink_w2 == 2, "#cd5c5c", 
  ifelse(drink_w2 == 3, "#dc143c",
    ifelse(drink_w2 == 4, "#ff0000", "black")))), 
vertex.shape = ifelse(sex == 1, "square", "circle"),
vertex.size = friendship_in_w2*1.5, 
edge.color = "black",
edge.width = 0.5,
edge.arrow.size = 0.40,
layout = myLayout ,
main = "Original Network", 
label = names ,
label.dist = 1, 
label.cex = 1,
vertex.label.color= "grey28", 
label.font = 4)
plot(fast_gready_2, friendship_w2_undirect ,
  edge.color = "black",
  edge.width = 0.5, 
  edge.arrow.size = 0.25,
  vertex.size = 15, 
  layout = myLayout ,
  main = "Fast ’n Greedy",
  vertex.shape = ifelse(sex == 1, "square", "circle"),
  label = names ,
  label.dist = 1,
  label.cex = 1,
  vertex.label.color= "grey28",
  label.font = 4)
plot(rand_walk_2, friendship_w2_undirect ,
  edge.color = "black",
  edge.width = 0.5,
  edge.arrow.size = 0.25,
  vertex.size = 15,
  layout=myLayout ,
  main = "Random Walk",
  vertex.shape = ifelse(sex == 1, "square", "circle"),
  label = names ,
  label.dist = 1,
  label.cex = 1,
  vertex.label.color= "grey28",
  label.font = 4)
plot(eigenvector_2, friendship_w2_undirect ,
  edge.color = "black",
  edge.width = 0.5,
  edge.arrow.size = 0.25,
  vertex.size = 15,
  layout=myLayout ,
  main = "Leading Eigenvector",
  vertex.shape = ifelse(sex == 1, "square", "circle"),
  label = names ,
  label.dist = 1,
  label.cex = 1,
  vertex.label.color= "grey28",
  label.font = 4)


## MODELLING ERGMs





predict(ergm_4_w2)

install.packages("statnet")

plogis(coef(ergm_4_w1)[["edges"]])
plogis(coef(ergm_4_w2)[["edges"]])
coef(ergm_4_w1)[["mutual"]]
coef(ergm_4_w2)[["mutual"]]

plogis(coef(ergm_4_w1)[["edges"]] + coef(ergm_4_w1)[["mutual"]]) 
plogis(coef(ergm_4_w2)[["edges"]] + coef(ergm_4_w2)[["mutual"]]) 

plogis(coef(ergm_4_w2)[["edges"]] + (1*coef(ergm_4_w2)[["gwesp.fixed.0"]])) 
plogis(coef(ergm_4_w2)[["edges"]] + (2*coef(ergm_4_w2)[["gwesp.fixed.0"]]))


plogis(coef(ergm_4_w1)[["edges"]] + (1*coef(ergm_4_w1)[["nodematch.sex"]]))
plogis(coef(ergm_4_w2)[["edges"]] + (1*coef(ergm_4_w2)[["nodematch.sex"]]))


exp(coef(ergm_4_w1)[["edges"]] + (1*coef(ergm_4_w1)[["nodematch.sex"]]))/exp(coef(ergm_4_w1)[["edges"]])
exp(coef(ergm_4_w2)[["edges"]] + (1*coef(ergm_4_w2)[["nodematch.sex"]]))/exp(coef(ergm_4_w1)[["edges"]])


plogis(coef(ergm_4_w2)[["nodeicov.sex"]])
plogis(coef(ergm_4_w1)[["nodeicov.sex"]])

plogis(coef(ergm_4_w1)[["nodeocov.sex"]])
plogis(coef(ergm_4_w2)[["nodeocov.sex"]])


## SIENA ##


library(RSiena)
fr1_SienaEffects <- NA
nActors <- dim(friendship_w1)[1]

# create dependent network variable
friend.dependent <- sienaDependent(array(c(friendship_w1, friendship_w2),
  dim=c(nActors , nActors , 2)))

sex.coCovar <- coCovar(sex[,1])

#also drinking is treated as dependent variable
drinking <- sienaDependent(drink, type ="behavior")

fr1_SienaData <- sienaDataCreate(friend.dependent,
  drinking,
  sex.coCovar)

# print report to check
print01Report(fr1_SienaData , modelname="friend_network_1")

fr1_SienaEffects <- getEffects(fr1_SienaData)

# network effects
fr1_SienaEffects <- includeEffects(fr1_SienaEffects , transTrip , cycle3) #transitive triplets , 3cycles
fr1_SienaEffects <- includeEffects(fr1_SienaEffects , inPop, outPop) #indegree popularity
# homophily effects and ego alter control
fr1_SienaEffects <- includeEffects(fr1_SienaEffects, egoX, altX, sameX, 
  interaction1="sex.coCovar")

# chekc the included effect 
fr1_SienaEffects

fr1_SienaAlgorithm <- sienaAlgorithmCreate(projname="friend_network_1")

# Estimate the model 
set.seed(667)

fr1_result <- siena07(fr1_SienaAlgorithm,
data = fr1_SienaData,
effects = fr1_SienaEffects,
returnDeps = T,
useCluster=TRUE, 
nbrNodes =2)

## NOT NEEDED. the model converge properly ###
# fr2_result <- siena07(fr1_SienaAlgorithm,
# data = fr1_SienaData,
# effects = fr1_SienaEffects ,
# returnDeps = T,
# prevAns=fr1_result,
# useCluster=TRUE, 
# verbose=F,
# nbrNodes =2)

# fr3_result <- siena07(fr1_SienaAlgorithm,
# data = fr1_SienaData,
# effects = fr1_SienaEffects ,
# returnDeps = T,
# prevAns=fr2_result,
# useCluster=TRUE, 
# verbose=F,
# nbrNodes =2)

fr1_SienaEffects <- includeEffects(fr1_SienaEffects, egoX, altX, simX, 
  interaction1="drinking")

fr4_result <- siena07(fr1_SienaAlgorithm,
data = fr1_SienaData,
effects = fr1_SienaEffects ,
returnDeps = T,
prevAns=fr3_result,
useCluster=TRUE, 
verbose=F,
nbrNodes =2)

fr1_SienaEffects <- includeEffects(fr1_SienaEffects,
    name = "drinking",
    avSim,
    interaction1 = "friend.dependent")

fr5_result <- siena07(fr1_SienaAlgorithm,
data = fr1_SienaData,
effects = fr1_SienaEffects ,
returnDeps = T,
prevAns=fr4_result,
useCluster=TRUE, 
verbose=F,
nbrNodes =2)


fr6_result <- siena07(fr1_SienaAlgorithm,
data = fr1_SienaData,
effects = fr1_SienaEffects ,
returnDeps = T,
prevAns=fr5_result,
useCluster=TRUE, 
verbose=F,
nbrNodes =2)


names <- fr4_result$effects$functionName
names_short <- fr4_result$effects$effectName
coef <- fr4_result$theta
se   <- sqrt(diag(fr4_result$covtheta)) # also fr3_result$se
t_conv <- fr4_result$tconv
t_ratio <- coef/se
signficant <- ifelse(abs(t_ratio)>2,"YES","NO")

cbind(names,names_short,coef,se,t_ratio,signficant,t_conv)

## SIENA MULTIPLEX ##
set.seed(47)
# the first dependent network - friendship
friendship.dependent1 <- sienaDependent(array(c(friendship_w1, friendship_w2), dim=c(nActors, nActors, 2))
)
# the second dependent network - trust
trust.dependent2 <- sienaDependent(array(c(trust_w1, trust_w2), dim=c(nActors, nActors, 2))) # constant covariate - gender
gender.coCovar <- coCovar(sex[,1])
# create siena data object
sienaData <- sienaDataCreate(friendship.dependent1 ,
trust.dependent2 , gender.coCovar)
# print initial report to file
print01Report(sienaData , modelname="RECENS_multiplex_report")
sienaEffects <- getEffects(sienaData)

sienaAlgorithm <- sienaAlgorithmCreate(projname="RECENS_3100_multiplex_algo")

frienship_trust_result_1 <- siena07(sienaAlgorithm,
  data = sienaData ,
  effects = sienaEffects,
  returnDeps = T,
  useCluster=TRUE, 
  verbose=FALSE,
  batch=TRUE,
  silent=TRUE,
  nbrNodes =2)

# first it makes sense to include effects for the two networks separately
sienaEffects <- includeEffects(sienaEffects , name="friendship.dependent1", transTrip, inPop, outPop)
sienaEffects <- includeEffects(sienaEffects , name="trust.dependent2", transTrip, inPop, outPop)

frienship_trust_result_2 <- siena07(sienaAlgorithm,
  data = sienaData ,
  effects = sienaEffects,
  returnDeps = T,
  prevAns=frienship_trust_result_1,
  useCluster=TRUE, 
  verbose=FALSE,
  batch=TRUE,
  silent=TRUE,
  nbrNodes =2)


sienaEffects <- includeEffects(sienaEffects,
  name="friendship.dependent1",
  egoX, altX, sameX, 
  include=T,
  interaction1="gender.coCovar")

sienaEffects <- includeEffects(sienaEffects,
  name="trust.dependent2",
  egoX, altX, sameX,
  include=T,
  interaction1="gender.coCovar")


frienship_trust_result_3 <- siena07(sienaAlgorithm,
  data = sienaData ,
  effects = sienaEffects,
  returnDeps = T,
  prevAns=frienship_trust_result_2,
  useCluster=TRUE, 
  verbose=FALSE,
  batch=TRUE,
  silent=TRUE,
  nbrNodes =2)


frienship_trust_result_4 <- siena07(sienaAlgorithm,
  data = sienaData ,
  effects = sienaEffects,
  returnDeps = T,
  prevAns=frienship_trust_result_3,
  useCluster=TRUE, 
  verbose=FALSE,
  batch=TRUE,
  silent=TRUE,
  nbrNodes =2)


# then add the basic dyadic cross-production effect

# sienaEffects <- includeEffects(sienaEffects,
#   name="friendship.dependent1",
#   crprod,
#   interaction1="trust.dependent2") 

## FRIENDSHIP ON TRUST
sienaEffects <- includeEffects(sienaEffects,
  name="trust.dependent2",
  crprod,
  interaction1="friendship.dependent1")


frienship_trust_result_5 <- siena07(sienaAlgorithm,
  data = sienaData ,
  effects = sienaEffects,
  returnDeps = T,
  prevAns=frienship_trust_result_4,
  useCluster=TRUE, 
  verbose=FALSE,
  batch=TRUE,
  silent=TRUE,
  nbrNodes =2)


sienaEffects <- includeEffects(sienaEffects,
  name="trust.dependent2",
  crprodRecip,
  include=T,
  interaction1="friendship.dependent1")

frienship_trust_result_6 <- siena07(sienaAlgorithm,
  data = sienaData ,
  effects = sienaEffects,
  returnDeps = T,
  prevAns=frienship_trust_result_5,
  useCluster=TRUE, 
  verbose=FALSE,
  batch=TRUE,
  silent=TRUE,
  nbrNodes =2)


sienaEffects <- includeEffects(sienaEffects,
  name="trust.dependent2",
  crprodMutual,
  include=FALSE,
  interaction1="friendship.dependent1")


frienship_trust_result_7 <- siena07(sienaAlgorithm,
  data = sienaData ,
  effects = sienaEffects,
  returnDeps = T,
  prevAns=frienship_trust_result_6,
  useCluster=TRUE, 
  verbose=FALSE,
  batch=TRUE,
  silent=TRUE,
  nbrNodes =2)



names <- frienship_trust_result_7$effects$functionName
names_short <- frienship_trust_result_7$effects$effectName
coef <- frienship_trust_result_7$theta
se   <- sqrt(diag(frienship_trust_result_7$covtheta)) # also fr3_result$se
t_conv <- frienship_trust_result_7$tconv
t_ratio <- coef/se
signficant <- ifelse(abs(t_ratio)>2,"YES","NO")

siena_multiplex_results cbind(names,names_short,coef,se,t_ratio,signficant,t_conv)



siena_multiplex_results <- as.data.frame(siena_multiplex_results)
siena_multiplex_results$names_final <- as.character(siena_multiplex_results$names)
siena_multiplex_results$names_final[6:16] <- array(unlist(sapply(as.character(siena_multiplex_results$names_short),simpleCap)))[(6:16)]

siena_multiplex_results %>% select(names_final, coef,se,t_ratio,t_conv) -> selected_table_uniplex

names(siena_multiplex_results) <- c("Effects", "Estimate", "Standard Error", "t-value", "Convergence t-ratio")


siena_multiplex_results <- siena_multiplex_results %>% mutate_at(vars(2,3,4,5), list(as.character)) %>%
    mutate_at(vars(2,3,4,5), list(as.numeric)) %>%
    mutate_at(vars(2,3,4,5), round,2)


### ENDS HERE ####


# Overview of the exam structure 

# # Q1.A Main characteristics of the network between T1 and T2 [MACRO-CHARATERISTICS]

# 1. MACRO-LEVEL: Comparison classroom at T1 VS T2
#    - H: Network at T1 is less dense, connceted and with a lower degree centrality,compared to the network at T2 because we expect more cohesion, trust, and social capital present at T2. 
#    - How dense is the friendship network between T1 and T2 ? gden()
#    - How many of the dyads are reciprocated between T1 and T2 ? grecip()
#    - Degree distribution: Random networks versus scale-free power-law networks. How do the in- and outdegree distributions look like in the classroom at T1 and T2? 
#    - Degree centrality: Does the he Matthew effect ("rich get richer") exist in relation to the activity (outdegree) change? 
#    - Triad Census between T1 VS T2 
# 2. MICRO-LEVEL: dimilarities/differences between networks on the tie level
#    - Network stability: Jaccard index between the networks at wave 1 and 2.
#    - MRQAP model: friendship at wave 2 ON friendship at wave 1


# # TRUST ON FRIENDSHIP 
# sienaEffects <- includeEffects(sienaEffects,
#   name="friendship.dependent1",
#   crprod,
#   interaction1="trust.dependent2") 


# frienship_trust_result_6 <- siena07(sienaAlgorithm,
#   data = sienaData ,
#   effects = sienaEffects,
#   returnDeps = T,
#   prevAns=frienship_trust_result_5,
#   useCluster=TRUE, 
#   nbrNodes = 2)


# # Q1.B. What are the relations between gender, drinking, and friendship in wave 1? How do these change by wave 2?

# ## GENDER Segregation
# - H: Students will be likely to segregate by gender dynamics
# - Proportion of girls
# - Proportion of boys 
# - Gender homophony in friendship choice (assortativity) T1 VS T2
# - Selection table T1 VS T2
#    - Girl -> boy relative subgraph density 
#    - Boy -> girl relative supgraph  density
#    - Odds ratio for a girl-girl vs. a girl-boy friendship?
# - MRQAP model: friendship at wave 2 ON friendship at wave 1 + gender

# ## Drinking Segregation
# - H: Students will be likely to segregate by drinking behaviour
# - Proportion of drinkers
# - Proportion of non-drinkers
# - Drinking homophily in friendship choice (assortativity) T1 VS T2
# - Selection table T1 VS T2
#    - Drinker -> non-drinker relative subgraph density 
#    - Non-drinker -> Drinker relative supgraph  density
#    - Odds ratio for a drinker-drinker vs. a drinker-non-drinker friendship
# - MRQAP model: friendship at wave 2 ON friendship at wave 1 + drinking wave 1 


# # Q2. Which micro patterns are important in explaining the structure of the two friendship networks (separately)?

# 1. Descriptive: 
#    - Triad Census for each actor, and for the network as a whole 
#    - Equivalence: Transitivity (i.e. "Adjacency") and Conditional Uniform Graph Test
# 2. Inferential with a step-wise approach  
#    - H: Gender homophony is stronger than age and drinking homophiliy. 
#    - ERGMs: Bernoulli Model: Friendship W1/W2 ON edges 
#    - ERGMs: Mutuality: Friendship W1/W2 ON edges + mutuality + clustering + confunders 
#    - ERGMs: Homophily: Friendship W1/W2 ON edges + mutuality + clustering + confunders + age + sex + drinking


# # 3. Which micro mechanisms shape the evolution of the friendship network in your classroom? (SIENA) 
# 1. H: Drinking increase the ability of students to make friends since it acts as a social lubricant
#   - Test whether drinking helps one make friends (egoX)
# 2. H: Alcohol use increase popularity because it function as a social payoffs to adopt a deviant behaviour 
#   - Test whether drinking enhances (or reduces) popularity (altX)
# 3. H: Students will select friends based on drinking levels since alcohol usage function a reward from conforming to peers' behaviour (Assimilation effect)
#   - Test whether actors select friends based on similar drinking level  (simX)



# # 4. State one hypothesis about other micro mechanisms which you could test with the learnt methods. (SIENA) 

# **RQ:** How do drinking/trust relationship affect drinking prevalence ?

# During the past three decades, the concept of social capital has received considerable attention both from academic and non-academic world. Recently, Kawachi et al. brought to the attention of the general public the implication that social capital have in term of public health. Specifically, many scholars have started to explore the relationship between social capital and alcohol usage. Despite the increasing number of publications on the relationship between various indicators of social capital and drinking behaviour, results are mixed and often lacks of inferential power. Most of the studies in the literature are based on on cross-sectional data and, thus, struggle to casually identify the effect of low or high social capital on alcohol usage. 

# In this exercise, I aim at testing if (a) drinking behaviour is driven by higher levels of social capital and (b) if being isolated lead to a higher drinking propensity. In line with the literature on the topic, I operazionalize social capital as self-reported trust between peers. The RECENS longitudinal high-school class nuber 3100 will be modelled using Simulation Investigation for Empirical Network Analysis approach (SIENA).

# 1. H1A: Higher level of social captiral (trust) increase the propensity to drinking since it functions as a forme of social belongning and identity
# - Test whether drinking is influcend by the fact that other people you trust engage in the same behaviour (avSim)
# 2. H2B: Low trust may result in different antisocial ways of coping with stress, peer evaluation, and conformity, which are common during adolescence. 
# - Test whether being not able to establish trust ties leads to drinking (isolate)





