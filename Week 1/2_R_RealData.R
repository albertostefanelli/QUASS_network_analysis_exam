#                          ############################################                            #
#                          ###  A VERY BRIEF INTRODUCTION TO R II.  ###                            #
#                          ###        WORKING WITH REAL DATA        ###                            #
#                          ############################################                            #

###
# This script was originally prepared for the second introductory part of the course on social
# network analysis in R and RSiena held in Budapest on 3-5 December 2014.
#
# The script was prepared by Andras Voros; it partly builds on and significantly extends
# Per Block's similar purpose script.
# (this is the version of 11.02.2018)
###

###
# We have discussed technicalities of storing data in objects in R, but you never enter
# an actual dataset by hand (it would be tedious and silly). The data we work with is
# almost always prerecorded in certain files on our computer (or perhaps the internet).
# So, you have to learn how to read and edit external files in R. Having loaded the data,
# there are some standard operations that you usually have to perform before an analysis.
# We cover the basics of these topics in the present script.
###


### 1. SETTING THE WORKING DIRECTORY

# it is possible to set up a WORKING DIRECTORY in an R session
# if no path (e.g. "C:\blabla\") is given before a file name,
# R will look for the file in the current working directory
# so, it is useful to set the wd to where your data and/or script files are -
# no need to type folder names anymore!

getwd() # what is the current working directory?

# let's set the working directory to where the downloaded files are for day 1
setwd("C:/YOUR FOLDER HERE/Week 1/") # note: folder location between quotation marks
# we use slash (/) - this works on both Macs and other machines as well
# you can also use double backslash (\\) (WHY double?? look at ?Quotes at home)
getwd() # here we are

# we can have a look at the files and folders in our current working directory
list.files()


### 2. IMPORTING DATA FROM EXTERNAL FILES TO THE WORKSPACE

# there are many different data formats out there, most of which can be imported into R somehow
# at the end of the script you can find information about where to look for help if you don't know
# how to open a specific type of file
# data often comes in .dat format, and in this course we will work with .RData and .csv files
# these three file types are all very easy to read and write from R

# now we import some data from .dat files

### The Glasgow dataset
# The datasets used for the illustration of methods throughout the course are subsamples of the
# Teenage Friends and Lifestyle Atudy conducted in Scotland between 1995 and 1997. The
# datasets contain information on friendships, other dyadic variables (e.g. distance between
# homes), and individual attributes (e.g. sex, drinking) of students from 3 waves.
# For the first example, we use a 50-actor excerpt of the dataset (aka "the s50 dataset"),
# and later we will switch to a richer subsample a whole student cohort comprising of
# 160 students from a single school.
#
# Further information about the data and other variables can be downloaded from the
# RSiena website: http://www.stats.ox.ac.uk/~snijders/siena/Glasgow_data.htm
###

# drinking should be a vector of values for each student
drink <- read.table("s50_data/s50-alcohol.dat")
# friendship is a matrix
friend.w1 <- as.matrix(read.table("s50_data/s50-network1.dat"))
# note the combination of two functions: read.table() returns a data frame
?read.table
# which we immediately convert into an object of class matrix
# by the as.matrix() function (for convenience)

# let's look at some attributes of the data objects
class(friend.w1)
dim(friend.w1)
# ok, it's a matrix, but is it numeric?
mode(friend.w1)

# how about the drinking object?
class(drink) # read.table without as.matrix returns a data frame
mode(drink) # which has a special mode
# let's turn it into a matrix
drink <- as.matrix(drink)
class(drink)
mode(drink) # better

dim(drink) # but why does it have three columns?
head(drink)
# we have drinking data from three time points,
# but we only need one at the moment - what to do now?


### 3. MANIPULATING THE DATA - SUBSETTING OBJECTS

# we would only like to use the first column of the drinking matrix

# it is possible to refer to cells, rows, or columns of an object
drink[1,2] # cell in the first row and second column
drink[6,] # the sixth row
drink[,1] # the first column

# we put the first column of object "drink" into a separate object
drink.w1 <- drink[,1]
drink.w1
class(drink.w1) # it's still numeric, but now a vector and not a matrix

# we can refer to elements of a vector in the same way as to cells of a matrix
drink.w1[3] # but vectors are 1-dimensional objects, of course
drink.w1[1:5]
drink.w1[c(1,3,5,7,9)]
# the latter also works for matrices
friend.w1[1:5, c(1,3,5,7,9)]

# using this, it is easy to change data entries
drink.w1[3] <- 9
friend.w1[4:5, 6] <- 4
friend.w1[, 9] <- 5
friend.w1[c(1,3,5,7,9), 2:3] <- 6


### 4. RECODING THE DATA - LOGICAL STATEMENTS AND SUBSETTING

# referring to certain elements in a vector or matrix is very important
# when you want to recode your data,
# but you don't have to change every value one by one
# LOGICAL STATEMENTS help to identify the location of values in the data
# that you might want to change

# a logical statement in R looks like these
3>4
3<=4

# now, which cells of the friendship matrix have a value of 5?
friend.w1==5 # note the double equal sign
# which entries are equal to or larger than 4?
friend.w1>=4
# which entries are either 4 or 6
friend.w1 %in% c(4,6) # == is not good in such cases

# using logical statements, we can recode unwanted values in our objects,
# even if we don't know where they are
friend.w1[friend.w1==5] <- 99
friend.w1[friend.w1 %in% c(4,6)] <- 999
# Why do you have to write friend.w1 twice? because you need to specify the object you want to
# change and the positions where you would like to change it. The first line can be read as:
# "R, change the cells of friend.w1 which equal to 5 to 99.", or a longer sentence for the second:
# "R, change the cells of friend.w1 which are in the positions where friend.w1 is 4 or 6 to 999."

# the longer sentence shows that this procedure is more general: the first friend.w1 could be replaced
# by another object's name
# for example, create an empty matrix and change its cells to 1 where friend.w1 is 99
newMatrix <- matrix(0, 50, 50)
newMatrix[friend.w1==99] <- 1


### 5. MISSING VALUES

# the last topic to cover is that of missing values
# which have a special value of NA in R

# let's set the values of 9, 99, 999 to NA in our objects
drink.w1[drink.w1==9] <- NA
friend.w1[friend.w1 %in% c(99,999)] <- NA
drink.w1
friend.w1

# you can also check by a logical statement where the NAs are in your data
# but whatch out, because NA
# is not a value cells can be equal to
drink.w1==NA # WRONG
# is not simply a character string
drink.w1=="NA" # WRONG
# but it is a unique value recognized by R
drink.w1 %in% NA # RIGHT
# it is perhaps even easier this way:
is.na(drink.w1) # also RIGHT

# how many NAs are in the friendship network?
(missings <- sum(is.na(friend.w1)))
# WHYY?? the result of is.na(friend.w1) is an object of logical mode (has TRUE/FALSE values)
# the function sum() works only for numeric mode objects,
# therefore it tries to force (coerce)our logical object to be numeric
# TRUE and FALSE values turn into 1 and 0 when coerced to numeric mode

# and how many friendship ties are there in the network?
sum(friend.w1)
# for most functions you need to tell what to do if they encounter NAs
# e.g. sum just does what normal addition (the "+" operator) would do, but saves you time and space
# if sum encounters one NA the final result will be NA
# to simply skip NAs in the data, do this:
(ones <- sum(friend.w1, na.rm=TRUE))

# MINITASK: How do you count the number of zeros in the friendship matrix?
zeros <- "???"
zeros

# we can now calculate the proportion of NAs in the data
missings / (zeros + ones)


### 6. HANDLING DIFFERENT FILE FORMATS AND OTHER THINGS

# you can import and export data from and to several types of files, other than .dat

# of the different file formats, we will get acquainted with importing data
# from .RData and .csv files later in this course
# but you can check the functions load() and read.csv()/write.csv()
# if you are too impatient to wait

# R also supports importing data from files created by popular statistical packages
# to use these special functions, you need the package called "foreign"
# if you don't have it, download it, then load it by the command library(foreign)
# two of the many options:
# read.spss() - reads SPSS data files
# read.data() - reads stata data files
# be careful to specify the right arguments!

# files created by software for network analysis can also be read in R
# for example, to read Pajek data files use library(network) and the function read.paj()


# talking about network analysis, two more things related to handling network data in R -
# a script for transformation between adjacency matrix and edgelist formats:
# http://www.stats.ox.ac.uk/~snijders/siena/Rscript01DataFormat.R
# subsetting data based on individual attributes (e.g. ties of girls):
# http://www.stats.ox.ac.uk/~snijders/siena/Rscript01DataFormat.R


### SECTION SUMMARY

###
# With the help of this script, you have familiarized yourselves with the following:
# creating objects in R from external files (.dat; later we will also use .RData and .csv)
# accessing specfic parts (subsets) of your objects (vectors, matrices(
# modifying and correcting your data using logical statements
# missing values (NAs) in R
# further options for importing data from external files
#
# note: we did not try out how exporting data from R to a file works, but if you need to do so,
#       it is really easy using the functions mentioned in the last part
###

# We are done with the basics, you can clear your workspace if you would like to.
# Now we move on to do some actual network analyses in R!

### END OF SCRIPT - PLEASE OPEN: "3_sna_Intro.R"