#                       ##############################################                             #
#                       ###         INDIVIDUAL PRACTICE 5          ###                             #
#                       ###            ERGMS IN statnet            ###                             #
#                       ##############################################                             #

###
# This practice script was originally prepared for the course on social network analysis
# held in Leuven on 3-5 March 2016.
#
# The script was prepared by Andras Voros.
# (this is the version of 02.03.2016)
###

### OVERVIEW
# You will work on your RECENS classroom again. Your task is to build ERGMs
# that model clustering and homophily by gender. Then you need to assess the GOF, too.
###

### INSTRUCTIONS
# 1.  Load your dataset from the RData file saved earlier.
# 2.  Build up an ERGM for friendship w1 step by step including the following effects
#                       edges
#                       mutuality
#                       clustering (ttriple, ctriple)
#                       sender, receiver, and homophily effects for gender
# 3. Repeat step 2 for friendship w2.
# 4. Assess the convergence of the full models.
# 5. Generate GOF plots for the full models and check if they fit reasonably.
# 6. As earlier, save the main results in an RData file and send them to me.

### START OF INDIVIDUAL PRACTICE

# don't forget to double check your working directory and set it if necessary


# 1. Load the data

# it is probably called "MyRecensData.RData"



# 2. Build an ERGM for wave 1 friendship

# use the effects from the previous example script
# it's good practice to add effects one by one or in small groups



# 3. Build an ERGM for wave 2 friendship

# the same as step 2



# 4. Do the models converge?



# 5. Genearte goodness of fit plots

# you can use RStudio's graphical device or print them to pdf, jpg, ...


# 6. Send me your results

# the full models (those that contain the most variables) from both waves are necessary
# it is important that the object names look like this:
cXXXX_ergm1 <- "full model results from wave 1"
cXXXX_ergm2 <- "full model results from wave 2"
# substitute XXXX with the ID of your RECENS classroom, as usual

# also write your classroom's ID instead ot XXXX here too!
save(cXXXX_ergm1, cXXXX_ergm2, file="XXXX_ERGM.RData")

# Thank you!


# You are done with this exercise.
# Before closing the script, save your workspace as always
save.image("MyRecensData.RData")
# and remove all of the objects.
rm(list=ls())

### END OF FIFTH INDIVIDUAL PRACTICE - THE NEXT TOPIC IS RSIENA!