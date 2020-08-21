#                       ##############################################                             #
#                       ###         INDIVIDUAL PRACTICE 2          ###                             #
#                       ###       ASSESSING GENDER HOMOPHILY       ###                             #
#                       ##############################################                             #

###
# This is the second practice script originally prepared for the course on social network analysis
# with R and RSiena held in Budapest on 3-5 December 2014.
#
# The script was prepared by Andras Voros.
# (this is the version of 11.02.2018)
###

### OVERVIEW
# You will work on your RECENS classroom again. The structure of the exercise
# is similar to that of the previous one, but now the focus is on practicing
# the igraph functions we learned about. And also, there is less help given,
# but feel free to use the earlier scripts. Let's see the instructions!
###

### INSTRUCTIONS
# 1.  Load your RData file saved at the end of the first practice session.
# 2.  Study gender segregation in the classroom. Is it present? Is it strong?
# 3.  Who prefers same-sex friends? Make a selection table based on density measures.
# 4.  Record your results in the given google spreadsheet.
# 5.  Optional: visualize the friendship network using igraph and the plot() function.
#
# NOTE: Always use detach() before switching between sna and igraph.
###


### START OF INDIVIDUAL PRACTICE

# don't forget to double check your working directory and set it if necessary


# 1. Load the data

# it is probably called "MyRecensData.RData"



# 2. Study gender segregation

# first you need to load igraph (detach sna!)


# put the network into an igraph graph object
graph_w1 <- graph.adjacency(friendship_w1)

# choose the appropriate assortativity function for gender and use it



# 3. Friendship selection table by gender

# try to replicate what we did in the end of the last script
# switch to the sna package (detach igraph!)

# it started by creating an empty selection table, like this:
friend.selection <- matrix(NA, 2, 2)
rownames(friend.selection) <- c("girl", "boy")
colnames(friend.selection) <- c("girl", "boy")

# then you need to define the subgraphs and calculate their densities


# normalize the results by the overall density
friend.selection.norm <- friend.selection / gden(friendship_w1)



# 4. Record your results in the google spreadsheet

# we do some between-classroom comparisons again

# there is a google spreadsheet here:
# https://docs.google.com/spreadsheets/d/1PucVX6lXEK0G1nwhTaYD4oisITsumvvst4bhKnG3-nI/edit?usp=sharing

# open it and add these results to the row corresponding to your classroom:
# 1. the proportion of girls in the classroom as given by
gender_comp[2] / class_size # you should have these objects from the previous exercise
# 2. the assortativity coefficient from step 2
# 3. the value of friend.selection.norm[1,2] (girl -> boy relative subgraph density)
# 4. the value of friend.selection.norm[2,1] (boy -> girl relative subgraph density)

# Thank you.


# 5. Optional: Visualize the friendship network with igraph and plot()

# if you are ready before everyone else...



# Before closing the script, take the opportunity to save your workspace,
# because we will use this data later.
save.image("MyRecensData.RData")
# and remove all of the objects
rm(list=ls())

### END OF SECOND INDIVIDUAL PRACTICE - PLEASE OPEN: "5_Stability_and_MRQAP.R"