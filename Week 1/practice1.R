#                       ##############################################                             #
#                       ###         INDIVIDUAL PRACTICE 1          ###                             #
#                       ###       BASIC NETWORK DESCRIPTIVES       ###                             #
#                       ##############################################                             #

###
# This is the first practice script originally prepared for the course on social network analysis
# with R and RSiena held in Budapest on 3-5 December 2014.
#
# The script was prepared by Andras Voros.
# (this is the version of 11.02.2018)
###

### OVERVIEW
# The aim of the exercise is to practice the basic R and sna skills we have talked
# through so far. It is not a test, so take your time and concentrate on how each
# function works. Whether you use the earlier srcipts or not is your choice. If
# you are stuck somewhere, or something doesn't work, just ask questions!
# But it will not be difficult.
#
# DATA. In all the individual assigments in this course, you will work on data from a
# a single classroom in the RECENS high-school study. This practice focuses on
# the descriptive analysis of the friendship network from the first data collection
# (October 2010, after the start of 9th grade). You should have received a piece of
# paper by now, telling you which classroom is "yours" to work on. Everybody
# in the course studies a different class, which gives us the opportunity to
# compare results and gain an overall impression of the population of studied groups.
# Moreover, classrooms are "sticky": you will work on the same dataset in the
# subsequent practice sessions, so all the time spent on looking at descriptives
# might pay off when you apply more complicated models.
#
# HOW TO PROCEED. Below you can find the list of instructions for the exercise.
# After that comes a partially written script - you need to fill in the blanks,
# but there will be enough help that you won't get lost. This is all. Go on.
###


### INSTRUCTIONS
# 1.  Read the data from the classroom which has been assigned to you.
#     The following files will be necessary for this exercise:
#              the affective network from wave 1 (XXXX_affective_w1.csv)
#              sex                               (XXXX_sex.csv)
# 2.  Learn about the objects.
#              How do they look like?
#              Do all objects store the same number of observations?
#              How are the variables coded?
# 3.  Recode the affective network to a friendship network.
#              Values of 2 should be coded to 1, all other non-missing values
#              should be coded to 0.
# 4.  Get familiar with the data.
#              How large is the classroom? How many boys and girls are in the class?
#              How many of the students were absent (marked by full rows of NAs in the network)?
#              How dense is the friendship network? How do the degree distributions look like?
# 4b. Using the sna package, plot the friendship network with node colors representing the
#     sex of the students. Save the coordinates from the plotting.
# 5.  Save some of your results objects in an RData file as indicated below,
#     and send them to me in an e-mail.
# X.  If you are quick and still have some time, plot the friendship network
#     with node colors based on gender and node sizes proportional to indegree.
###


### START OF INDIVIDUAL PRACTICE

# check your working directory and change it if it's not set to where the data is!
getwd()


# 1. Read data

# the data files from the RECENS study are in csv format
# there are convenient functions in R for reading/writing .csv files
?read.csv

# now read the data (insert the ID of your class in the filename)
affective_w1 <- as.matrix(read.csv("RECENS_data/YOURCLASS_affective_w1.csv",
                                   header=TRUE, row.names=1, sep=","))
sex <- as.matrix(read.csv("RECENS_data/YOURCLASS_sex.csv",
                          header=TRUE, row.names=1, sep=","))


# 2. Explore the objects

# here is some space for you to do it:




# these are the value labels for the affective network:
#                   -2: enemy, -1: dislike, 0: neutral, 1: like, 2: friend


# 3. Recode the affective network to a 1-0 friendship matrix

# we need a matrix that contains the value 1 where the affective network is 2 and 0 everywhere else
# there are several ways to do it, this is one (unfinished) example:
friendship_w1 <- affective_w1 # first put the original network into a friend object
# now change the values in the new object (you need to fill in the "???")
friendship_w1["???" %in% c("???")] <- "???"
friendship_w1["???" == "???"] <- "???"

# check your solution: if you did it right, the following lines should return TRUE
all.equal(which(friendship_w1==1), which(affective_w1==2))
all.equal(which(friendship_w1==0), which(affective_w1 %in% c(-2, -1, 0, 1)))


# 4. Explore the data

# here are some questions for guidance (you can do more)
# it's a good idea to put the answers into objects - you will have them later


# How many students are in the classroom?
# (as many as rows in your data objects)
class_size <- "???"

# How many boys and girls?
# (if only you could make a table with the frequencies and put it in an object...)
gender_comp <- "???"

# How much missing data is in the friendship network?
# (earlier we did this by combining two functios: is.na and sum)
# you only need to count the number of NAs in the matrix
friend_miss_w1 <- "???"
# run the next line to divide it by the number of possible ties
# and get the proportion of missing values
friend_miss_w1 <- friend_miss_w1 / ( nrow(friendship_w1) * (ncol(friendship_w1) - 1) )
# (advanced users: check diag(friendship_w1) and think about how we could correct the result)

# How many students were absent at the time of the data collection?
# (these are the people whose rows in the friendship matrix only contains NAs)
# this is actually a bit tricky, so if you had enough of these exercises, just
# count the NA rows in friendship_w1 by "visual inspection" (i.e. look at it)
absent_w1 <- "???"
# if you want R to count these students for you, you need to cleverly combine
# these bits: is.na(friendship_w1),
#             rowSums(),
#             a logical statement involving these and ncol(friendship_w1),
#             sum().
# You might find an easier solution, though...


# for the following questions, load the package sna if its not yet loaded
library(sna)

# How dense is the friendship network?
friend_dens_w1 <- "???"

# How many of the dyads are reciprocated?
# (remember to specify the arguments correctly, look at ?grecip if you forgot how to)
friend_rec_w1 <- "???"

# How do the in- and outdegree distributions look like in the classroom?
# (plot them based on the previous scipt)
# first count the degrees
friend_ind_w1 <- "???" # indegrees
friend_outd_w1 <- "???" # outdegrees
# then plot them using histograms (they don't have to look nice and don't have to be
# combined in a single figure; but they can be)
hist("???")
hist("???")


# 4b. Plot the friendship network
# color nodes by gender and save coordinates
friend_plot_w1 <- "???"
"???"


# 5. Save and send objects

# we will shortly do between-classroom comparisons based on everyone's work

# I don't have your results, so you have to send them to me
# to make it easier, please create the following objects that contain the ID of your class
friend_">>CLASS<<" <- friendship_w1
dens_">>CLASS<<" <- friend_dens_w1
recip_">>CLASS<<" <- friend_rec_w1
coords_">>CLASS<<" <- friend_plot_w1

# save these objects, but these only in an RData file (insert class ID)
save(friend_">>CLASS<<", dens_">>CLASS<<", recip_">>CLASS<<", coords_">>CLASS<<", file=">>CLASS<<.RData")

# now please send the saved file to my e-mail address (which I tell you if you don't know it)


# Finally, save your workspace, because we will use the dataset in later practices.
save.image("MyRecensData.RData")
# and remove all of the objects
rm(list=ls())


# in case you are done and everyone else is still working:
# X. Think about the extra question (or anything else related to network description)



###
# Once everybody is finished and sent me their results, we discuss and compare them.
# Note that while completing this assigment you have written a script that
# does basic descriptives on network data for you - it can be used straigthaway
# or adapted easily for other datasets.
###

### END OF FIRST INDIVIDUAL PRACTICE - PLEASE OPEN: "4_igraph_Intro.R"