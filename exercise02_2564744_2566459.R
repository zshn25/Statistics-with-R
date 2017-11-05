###############
### Cleaning Data
###############

# Please do the "Cleaning Data with R" exercise that was assigned in dataCamp.
# We recommend that you take notes during the DataCamp tutorial, so you're able to use the commands 
# you learned there in the exercise below.
# This week, the exercise will be about getting data into a good enough shape to start analysing. 
# Next week, there will be a tutorial on running t tests for this data.

# download the data file "digsym.csv" from the moodle and save it in your working directory. 
# The read in the data into a variable called "data".
data <- read.csv("digsym.csv")

# Load the libraries languageR, stringr, dplyr and tidyr.
library(languageR)
library(stringr)
library("dplyr")
library(tidyr)

# how many rows, how many columns does that data have?
dim(data)

# take a look at the structure of the data frame using "glimpse"
glimpse(data)

# view the first 20 rows, view the last 20 rows
head(data, 20)
tail(data, 20)

# Is there any missing data in any of the columns?
any(is.na(data))

# get rid of the row number column
data$X <- NULL

# put the Sub_Age column second
data <- subset(data, select = c(ExperimentName, Sub_Age, Group:File))

# replace the values of the "ExperimentName" column with something shorter, more legible
data$ExperimentName <- str_replace(data$ExperimentName, "Digit Symbol - Kopie", "DSK")

# keep only experimental trials (encoded as "Trial:2" in List), get rid of practice trials 
# (encoded as "Trial:1"). When you do this, assign the subset of the data to a variable "data2", 
# then assign data2 to data and finally remove data2.
data2 <- data$List
data2 <- str_replace(data2, "Trial:1", "NA")
data$List <- data2
data2 <- NULL

# separate Sub_Age column to two columns, "Subject" and "Age", using the function "separate"
data <- separate(data, Sub_Age, c("Subject", "Age"))

# make subject a factor
data$Subject <- as.factor(data$Subject)


# extract experimental condition from the "File" column:
# the stimulus that people saw - did it correspond to a wrong or right digit-symbol combination?

# 1) using str_pad to make values in the File column 8 chars long, by putting 0 on the end  (i.e., 
# same number of characters, such that "1_right" should be replaced by "1_right0" etc)
data$File <- str_pad(data$File, width = 8, side = "right", pad = "0")

# create a new column ("condition" (levels:right, wrong)) by extracting "right"/"wrong" using substr
data$condition <- substr(data$File, 3, 7)

# get rid of obsolete File column
data$File <- NULL

# missing values, outliers:

# do we have any NAs in the data, and if so, how many and where are they?
sum(is.na(data))

# create an "accuracy" column using if-statement
# if actual response (StimulDS1.RESP) is the same as the correct response (StimulDS1.CRESP), put 
# in value 1, otherwise put 0
#if(data$StimulDS1.RESP == data$StimulDS1.CRESP) {
#  data$accuracy <- 1
#} else {
#  data$accuracy <- 0
#}

data$accuracy <- as.numeric(data$StimulDS1.RESP == data$StimulDS1.CRESP)

# how many wrong answers do we have in total?
length(data$accuracy) - sum(data$accuracy)

# whats the percentage of wrong responses?
(length(data$accuracy) - sum(data$accuracy)) / length(data$accuracy)

# create correct_RT column


# create boxplot of correct_RT - any outliers?

# create histogram of correct_RT with bins set to 50


# describe the two plots - any tails? any suspiciously large values?

# view summary of correct_RT


# we got one apparent outlier: 13850

# There is a single very far outlier. Remove it.


# dealing with the tail of the distribution: 
# outlier removal
# Now, remove all correct_RT which are more than 2.5. SD away from the grand mean



# create new "correct_RT_2.5sd" column in data which prints NA if an RT value is below/above the cutoff


# take a look at the outlier observations
# any subjects who performed especially poorly?


# how many RT outliers in total?


# plot a histogram and boxplot of the correct_RT_2.5sd columns again - nice and clean eh?

# Next, we'd like to take a look at the avrg accuracy per subject
# Using the "cast" function from the library "reshape", create a new data.frame which shows the 
# average accuracy per subject. Rename column which lists the average accuracy as "avrg_accuracy".

# sort in ascending order or plot the average accuracies per subject.


# would you exclude any subjects, based on their avrg_accuracy performance?

# Congrats! Your data are now ready for analysis. Please save the data frame you created into a new 
# file "digsym_clean.csv".