#                       ##############################################                             #
#                       ###         INDIVIDUAL PRACTICE 3          ###                             #
#                       ###   DYNAMIC NETWORK ANALYSIS WITH QAP    ###                             #
#                       ##############################################                             #

###
# This is the third practice script originally prepared for the course on social network analysis
# with R and RSiena held in Budapest on 3-5 December 2014.
#
# The script was prepared by Andras Voros.
# (this is the version of 11.02.2018)
###

### OVERVIEW
# You will work on your RECENS classroom again. The structure of the exercise
# is similar to that of the previous one, but now the focus is on the Jaccard
# index and MRQAP. But feel free to consult the earlier scripts.
###

### INSTRUCTIONS
# 1.  Load your dataset from the RData file saved last time.
# 2.  Read the affective network from wave 2, and recode it to a friendship network.
# 3.  Calculate the Jaccard index between the networks at wave 1 and 2.
# 4.  Build up an MRQAP model in which the dependent network is friendship
#     at wave 2, and the explanatory variables are
#                                     first only friendship at wave 1,
#                                     then also same gender.
# 5.  Record your results in the given google spreadsheet.
# 6.  Find out how to include variables in the model that capture
#                                     whether girls send more nominations than boys,
#                                     whether girls receive more nominations than boys.
###


### START OF INDIVIDUAL PRACTICE

# don't forget to double check your working directory and set it if necessary


# 1. Load the data

# it is probably called "MyRecensData.RData"



# 2. Read the friendship network from wave 2 and recode it

# the file is here: /RECENS_data/XXXX_affective_w2.csv (XXXX is the ID of your class)
# remember the beginning of the first practice? you read a similar csv file back then.
affective_w2 <- as.matrix("???")

# also, you have recoded the first wave network - just do the same here



# 3. Calculate the Jaccard index between the two waves

# if you need help: we did it in three lines, around line 100 in the last script



# 4. Build an MRQAP model of friendship_w2 in two steps

# the classrooms are much smaller than the Glasgow dataset, so you can set
# reps=500 or 1000 in the netlogit function - it will not take very long time

qap1 <- "friendship_w2 regressed on friendship_w1"

# you need to create a same sex matrix
# help is around line 140 in the last script - the variable coding is the same, so it is easy
same.sex <- "???"

# run the second model
qap2 <- "friendship_w2 regressed on list(friendship_w1, same.sex)"

# interpret the results


# 5. Enter some of these results into a google spreadsheet

# there is a google spreadsheet here:
# https://docs.google.com/spreadsheets/d/1hEf1YR_tFBAD8DK68A0XIHvcWgnKjfpJ_mf2uMJIsss/edit?usp=sharing

# open it and add these results to the row corresponding to your classroom:
# 1. the Jaccard index between friendship_w1 and friendship_w2.
# 2. the MRQAP coefficient of friendship_w1 from the first MRQAP model (qap1)
# for the latter, you can simply look at the results table,
# but it is also possible to extract the coefficient only from the results object
# look at the structure of the results object:
str(qap1)
# it has 21 parts, each storing different pieces of information about the results
# the first is "$coefficients" which lists the two estimated parameters
qap1$coefficients
# the first is the intercept, so you need the second
qap1$coefficients[2]
# note: there are some objects, e.g. data frames or different results objects,
#       which can be subsetted by using the "$" sign followed by the name of the subset
#       (but this does not work for vectors or matrices)

# Thanks for sharing your results. You can continue the exercise.


# 6. Include sex sender and receiver effects in the last model

# this is not very easy
# it is enough if you think about how the two variables should look like:
#     the one that distinguishes girl senders from boy senders
#     the one that distinguishes girl receivers from boy receivers
# then we will discuss the solution in the group
# but if you are really experienced, you can go ahead, create the objects,
# and run the extended model with four variables (friendship_w1, same.sex,
# sender.sex, receiver.sex)




# Before closing the script, take the opportunity to save your workspace,
# because we will use this data later.
save.image("MyRecensData.RData")
# and remove all of the objects
rm(list=ls())

### END OF THIRD INDIVIDUAL PRACTICE - PLEASE OPEN: "6_TriadCensus_and_RandomGraphs.R"