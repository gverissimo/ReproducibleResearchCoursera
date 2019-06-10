

# clear terminal
ctrl-L

# load swirl
library(swirl)

# load class
install_from_swirl("Getting and Cleaning Data")

# clear workspace
ls()
rm(list=ls())

# launch swirl
swirl()


# You can exit swirl and return to the R prompt (>) at any time by pressing the
# Esc key. If you are already at the prompt, type bye() to exit and save your
# progress. When you exit properly, you'll see a short message letting you know
# you've done so.
#
# When you are at the R prompt (>):
# -- Typing skip() allows you to skip the current question.
# -- Typing play() lets you experiment with R on your own; swirl will ignore what you do...
#    - UNTIL you type nxt() which will regain swirl's attention.
# -- Typing bye() causes swirl to exit. Your progress will be saved.
# -- Typing main() returns you to swirl's main menu.
# -- Typing info() displays these options again.


# follow the menus and select the R Programming course when given the option. For the first part of this course you should complete the following lessons:

# Basic Building Blocks
# Workspace and Files
# Sequences of Numbers
# Vectors
# Missing Values
# Subsetting Vectors
# Matrices and Data Frames
# 

# If you need help...
# * Visit the Frequently Asked Questions (FAQ) page at https://github.com/swirldev/swirl/wiki/Coursera-FAQ
# to see if you can answer your own question immediately.
# 
# * Search the Discussion Forums this course.
# 
# * If you still can't find an answer to your question, then create a new thread under the swirl Programming Assignment sub-forum and provide the following information:
# -- A descriptive title
# -- Any input/output from the console (copy & paste) or a screenshot
# -- The output from sessionInfo()
# 
# Good luck and have fun!
# 
# For more information on swirl, visit http://swirlstats.com/ 
# 


# How to submit
# Copy the token below and run the submission script included in the assignment download. 
# When prompted:
# 
# * use your email address gvca80-coursera@yahoo.com
# 
# * and tokens:
# -- swirl Week3/Lesson 1 (swirl# 15)  : CaseStudy               :  eJa7BE0ixxFWHNBf
#
# Your submission token is unique to you and shou0ld not be shared with anyone. 
# You may submit as many times as you like.
#
# swirl has play() and nxt() functions. 
# -- typing play() allows you to leave swirl temporarily so you can try different R commands 
# -- typing nxt() when you’re done playing brings you back to swirl and you can resume your lesson.)
#
# create a random mix of NA and samples from a normal distribution 
# by creating 1000 samples of each and then taking 100 random samples from each:
#   y <- rnorm(1000) #vector of 1000 samples drawn from normal distribution
#   z <- rep(NA, 1000) #vector of 1000 NA
#   my_data <- sample(c(y,z),100) # 100 random samples drown from union(y,z)

##*******************************
library(swirl)
install_from_swirl("Exploratory Data Analysis")
swirl()


##*******************************




