#                           ###########################################                            #
#                           ###  A VERY BRIEF INTRODUCTION TO R I.  ###                            #
#                           ###               THE BASICS            ###                            #
#                           ###########################################                            #

###
# This script was originally prepared for the first introductory part of the course on social
# network analysis in R and RSiena held in Budapest on 3-5 December 2014.
#
# The script was prepared by Andras Voros; it partly builds on and significantly extends
# Per Block's similar purpose script.
# (this is the version of 11.02.2018)
###

###
# This course is not about programming in R. Yet, if you intend to analyze networks in R,
# you have to be familiar with the language and the environment to a certain extent. We
# now briefly cover some of the basics, so that nobody is left behind. If you know a lot
# about R, you might still want to follow this part, because we will cover certain key
# concepts to quite some depth.
#
# Let's start at the beginning!
###


### 0. HOW TO USE R

# The R installation comes with a default user interface in which you can program R.
# Outside of the classic GUI, there are a few alternatives, most noteworthy
# RStudio (what I use) and NppToR with Notepad++.

# Once you started a GUI, there are two main ways to tell R what it should do:
# you can enter commands directly in the CONSOLE
# or send them to the console from a SCRIPT (like the one you are reading right now)


### 1. BASIC OPERATIONS


# for example, you can copy the following line to the console and hit Enter/Return,
# or you can simply run it from this script
# let's try it both ways
print("Hi!")
# it's the same

# print() is a FUNCTION - it tells R to print whatever we entered within the brackets
# in general, this is how functions look like in R: FUNNAME(arg1, arg2, ...)
# note: R is case sensitive, so Print("Hello!") doesn't work (capital P)

print("Oh, hello there!" # if the command you entered is not complete, R will ask for more input,
)                        # as shown by the "+" sign in the console

# you can do simple calculations in the console or from the script
1+1
101*36
sqrt(9)
sum(4, 3, 9.1, 5) # there are spaces between numbers
mean(c(5,10,15)) # there aren't any - spaces don't matter


### 2. CREATING OBJECTS - ASSIGNMENT

# note the [1] at the beginning of the return line in the console
# the results are not simply numbers or text, they are actually objects (vectors) of length 1
# but since we didn't "save" them somehow, they pop up in the console and then they are gone

# instead of just displaying results in the console, you can store them in the WORKSPACE by using
# the "<-" assign operator
a <- 2
b <- mean(c(5, 10, 15))
# now you have two objects in the workspace called "a" and "b"
# you can check what they contain simply:
a
b

### Some rules for naming your objects:
# 1. object names cannot start with numbers or other funny characters (e.g. @ or _)
# 2. but you can use numbers (0-9), dots (.), underscores (_) within or at the end of object names
# 3. there are some reserved words which you cannot give to objects
#    (type ?Reserved in the console to find out more about them)
###

# assigment works in the same way with text
(hey <- "Howdy?") # by putting an assignnent in brackets,
                   # what gets stored in the object is also displayed for you in the console

### The console vs. scripts:
# in general, it is advisable to use scripts instead of the console
# this is especially true if you change something in the data you are working with
# scripts do not only make your work easier, but more importantly reproducible!
###

# we already have three objects in the workspace
# "a", "b", and "hey" are all vectors of length one, yet they are different somehow...
mode(a)
mode(b)
mode(hey)
# aha! the first two are numeric vectors, the third is a character vector - of course...

### The 'mode' of objects:
# mode is a so-called ATTRIBUTE of EVERY object in R
# mode tells functions about the structure of an object -
# what kind of things are stored in it (numbers?, text?, etc.) -
# so functions can determine whether they can do something with the object you pass on to them
# (ADVICE: if you get an error, check if the mode of the object(s)
#          is what the function you are trying to use expects!)
# the modes in R you most often see (so-called 'atomic' modes): character, logical, numeric, complex
#
# when you create and sometimes when you modify an object, mode is automatically assigned to it by R
# it is often also possible to manually "force" (COERCE) an object to be of a certain mode
# but this may not always be smooth - think about it: how would you transform "Hello!" to a number?
# understanding the fun with modes and classes is important for a strong basic knowledge of R,
# so we will return to this topic in a moment when we learn about classes
###


### 3. CREATING VECTORS - FURTHER OPTIONS

# there are several useful ways for creating vectors in R

# function c() stands for "concatenate" (~ combine into a single object)
(c <- c(1, 5, 6, 7, 8, 11, 5, 27, 9, 410))
# sequence of numbers can be simply assigned by using the colon
(toten <- 1:10)
# or the seq() function
(toten.byhalfs <- seq(1, 10, by=0.5))
# note that the result of the latter is a vector of length 19
length(toten.byhalfs)
# (just like mode, length is an attribute of objects)


### 4. CREATING MATRICES

# matrices are two-dimensional objects (rows, columns)

# this is how you create a matrix that contains 0s only
nullmat <- matrix(0, nrow=4, ncol=4)
# the "nrow=" and "ncol=" can be omitted IF they are exactly the 2nd and 3rd arguments
# like here:
nullmat2 <- matrix(0, 4, 4)
nullmat
nullmat2

mat <- matrix(1:4, 6, 6)
mat # if there are fewer input values (4) than cells (36) specified, the values are recycled
    # to fill the all the cells in the matrix
    # (note: cells are filled by column, unless the function is otherwise instructed)

# what is the mode of object "mat"?
mode(mat)
# but then how can we see if "mat" is a matrix, vector, or something else?
class(mat)

### The 'class' of objects:
# class is also a very important attribute of objects in R
# it determines what happens to objects when they are passed on to functions,
# class is an attribute of most objects, but not all of them (see the case of vectors below)
# some common classes are: (vector), matrix, array, data frame, list
###

# unfortunately, class can easily be confused with mode, because in case of vectors
class(c) # class and
mode(c) # mode
# are the same.

# WHY?? vectors are so basic objects in R that they don't have an own class called "vector"
# they are kind of the "default guys"
# because of this, R just copies the mode attribute of vectors to their class attribute
# (in general, if an object does not have a class defined, its class attribute will be its mode)

### So, what is the difference between class and mode, and why are they important?
# in the end, it's simple:
# mode tells functions whether the type of data stored in an object are appropriate
# class tells functions how exactly they should do things with an object
# for example:
# it is not meaningful to execute this: "hel" + "lo!" - R knows, as their mode is character
# it is meaningful to add a vector to a vector or a vector to a matrix (if they are numeric),
# but how it should be done exactly (by row? by column?) is determined by their class
###

# now we (hopefully) better understand what mode is good for!
# to see the risks involved in changing the mode of an object (or coercion),
# run these lines:
(dummy <- matrix(1:4, 6, 6))
mode(dummy)
mode(dummy) <- "character"
dummy # characters are in quotes
mode(dummy) <- "numeric"
dummy # back to normal
mode(dummy) <- "logical"
dummy # 1 is TRUE, 0 is FALSE
mode(dummy) <- "character"
dummy # TRUE and FALSE as text
mode(dummy) <- "numeric"
dummy # oops
# in some cases it is useful to change the mode of objects, e.g. when R treats numbers as string
# however, certain values are meaningful for one node but not for others (e.g. TRUE, "Hello!", 9)
# in such cases NAs may be introduced - more on this later

# you can also change the class of objects (e.g. data frame -> matrix), but be careful!

### About other attributes of objects
# objects can have many other attributes, depending on their class and mode
# e.g.: length, dim, names, rownames, ...
# some of these will be empty until you specify them (e.g. names, rownames),
# some are automatically specified by R when you create or modify an object (e.g. length, dim)
###


### 5. OPERATIONS ON OBJECTS

# operations work on objects in the same way they do on numbers
a+b
a*b
mean(c)

# if you add a number to a vector, it will be added to all of its elements, cell by cell
b
c
b+c
# WHY?? remember how the 6x6 matrix was filled up with the numbers 1, 2, 3, 4?
# This is the same: the shorter object is recycled and added to each element of the longer object

# you can also create objects that store the result of operations in other objects
product <- a*b
product

something <- b*sd(c)
something
# but what is sd(c)? let's look it up by asking R about the function sd
?sd # nice, so sd(c) is the standard deviation of the elements in c

# it is also possible to replace existing objects
nullmat2 <- mat
nullmat2
nullmat2 <- nullmat
nullmat2

# finally, if you don't need some objects, you can remove them from the workspace
rm(product)
product # it's gone...
# you can remove several objects at a time
rm(nullmat, nullmat2)
# or if you really had enough, you can simply clear your entire workspace
# let's do this now
rm(list=ls()) # and everything is gone
# WHY?? if you are curious why the command for clearing the workspace looks like as it does,
#       try to find it out by typing ?rm and then ls() in the console (not right now, please)


### 6. GETTING HELP WITH YOUR PROBLEMS

# when using R, you "get stuck" very often - this is normal
# perhaps the most important skill to master is finding answers to your questions

# getting help in R:
# ? - if you remember a function's name, but don't know how to use it
?sd
# ?? - if you don't remember a function's name, but have some good keywords in mind
??standarddeviation

# getting help outside of R:
# google it! - R communities (e.g. stack overflow) usually help
# e.g. google "how to sort columns of matrix by first row in R" (don't forget the "in R" part)
# important: try to refine your question - find appropriate terms, expressions

# R tutorials - only a few examples:
#        http://cran.r-project.org/doc/manuals/R-intro.pdf (the R manual by the R Core Team)
#        http://www.statmethods.net/
#        http://www.burns-stat.com/pages/Tutor/hints_R_begin.html
#        http://data.princeton.edu/R/gettingStarted.html
#        http://www.ats.ucla.edu/stat/R/sk/


### SECTION SUMMARY

###
# Now you know some of the basics of R:
# how to create, change, remove objects
# how to do basic operations on objects (especially vectors and matrices)
# how access and edit important attributes of objects (especially mode and class(
# how to find help when you run into problems
#
# However, there are a few other things you need to be familiar with if you want to
# work with "real data" - these skills are covered in the following script.
###

### END OF SCRIPT - PLEASE OPEN: "2_R_RealData.R"