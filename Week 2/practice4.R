#                       ##############################################                             #
#                       ###         INDIVIDUAL PRACTICE 4          ###                             #
#                       ###         DYAD AND TRIAD CENSUS          ###                             #
#                       ##############################################                             #

###
# This practice script was originally prepared for the course on social network analysis
# held in Leuven on 18-20 February 2016.
#
# The script was prepared by Andras Voros.
# (this is the version of 20.02.2018)
###

### OVERVIEW
# You will work on your RECENS classroom again. In this exercise, you will
# have to assess the distribution of dyadic and triadic structures in the
# friendship network of wave 2.
###

### INSTRUCTIONS
# 1.  Load your dataset from the RData file saved earlier.
# 2.  Run a dyad census on the friendship network of wave 2 and save the results in an object.
# 3.  Run a triad census on the friendship network of wave 2 and save the results in an object.
# 4.  Generate 100 random networks with the same size (number of actors) and density as your
#     friendship network of wave 2.
# 5.  Plot the distribution of dyads and triads in your friendship network against those of
#     the simulated networks.
# X.  In case you are done with everything, load the wave 2 trust network of your classroom,
#     repeat the exercise and try to identify the main structural similarities and differences
#     between friendship and trust networks.
###


### START OF INDIVIDUAL PRACTICE

# don't forget to double check your working directory and set it if necessary


# 1. Load the data

# it is probably called "MyRecensData.RData"



# 2. Run a dyad census on the friendship network from wave 2

friend2_dyad <- "???" # with the right function, it's a one-liner



# 3. Run a triad census on the friendship network from wave 2

friend2_triad <- "???" # it's also pretty easy



# 4. Generate 100 random networks wit the same size and density as the friendship network

# you need to get the size and the density
friend2_size <- "???"
friend2_dens <- "???"
# and use them as parameters when you generate the networks
simfriend <- "???"



# 5. Plot the dyad and triad distributions in a violinplot

# first you need to repeat steps 2 and 3 for the simulated networks.
simfriend_dyad
simfriend_triad

# use the vioplot function for plotting (don't forget to load the package if needed)

# in case of dyads, put all three configurations (0, ->, <->) on the plot
# for triads, first plot all of them, but if it looks ugly, concentrate on the closed
# triangles only (see around line 200 of the last script for help)



# X. Repeat the exercise for the trust network of the 2nd wave

# the file is here: /RECENS_data/XXXX_trust_w2.csv (XXXX is the ID of your class)
trust_w2 <- "???"

# note: you don't need to recode this network, the entries are only
#       1s (trust) and 0s (no trust)

# if you are done with this part, compare the plots with those you got from the
# friendship networks. How are they similar and different?


# Before closing the script, save your workspace as always
save.image("MyRecensData.RData")
# and remove all of the objects
rm(list=ls())

### END OF FOURTH INDIVIDUAL PRACTICE - PLEASE OPEN: "7_Blockmodeling.R"