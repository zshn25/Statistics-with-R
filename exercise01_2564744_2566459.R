### Stats with R Exercise sheet 1 

## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:59 on Monday, October 30. Write the code below the questions. 
## If you need to provide a written answer, comment this out using a hashtag (#). 
## Submit your homework via moodle.
## You are allowed to work together in group up to three students, but everybody 
## needs to submit the group version of the homework via moodle.


## Please write below your (and your teammates) name, matriculation number. 
## Name: Alyona Morozova
## Matriculation number: 2564744

## Name: Zeeshan Khan Suri
## Matriculation number: 2566459

## Change the name of the file by adding your matriculation numbers
## (exercise01_firstID_secondID_thirdID.R)



## Many of the things on this exercise sheet have not been discussed in class. 
## The answers will therefore not be on  the slides. You are expected to find 
## the answers using the help function in R, in the textbooks and online. If you 
## get stuck on these exercises, remember: Google is your friend.
## If you have any questions, you can ask these during the tutorial, or use the 
## moodle discussion board for the course.

###############
### Exercise 1: Getting started
###############
## a) Look at your current working directory.

getwd()

## b) Get help with this function.

? getwd()

## c) Change your working directory to another directory.

# setwd("C:/UdS")


###############
### Exercise 2: Participants' age & boxplots
###############
## In this exercise, we will deal with data from a package.

## a) Install the package "languageR" and load it.

install.packages("languageR")
library(languageR)

## b) Specifically, we will deal with the dataset 'dutchSpeakersDistMeta'. 
##    This dataset should be available to you once you've loaded languageR.
##    The dataset contains information on the speakers included in the Spoken 
##    Dutch Corpus. Inspect 'dutchSpeakersDistMeta'. Look at the head, tail, 
##    and summary. What do head and tail show you?

head(dutchSpeakersDistMeta)
tail(dutchSpeakersDistMeta)
summary(dutchSpeakersDistMeta)

# Head() shows first elements of a data set, 
# Tail() prints out the last observations of a data set. 

## c) Each line in this file provides information on a single speaker. How many 
##    speakers are included in this dataset? In other words, use a function to 
##    retrieve the number of rows for this dataset.

nrow(dutchSpeakersDistMeta)

## d) Let's say we're interested in the age of the speakers included in the 
##    corpus, to see whether males and females are distributed equally. 
##    Create a boxplot for Sex and AgeYear.

boxplot(dutchSpeakersDistMeta$AgeYear ~ dutchSpeakersDistMeta$Sex, xlab = "Age Year", ylab = "Sex")

## e) Does it seem as if either of the two groups has more variability in age?

#Female group has more variability in age than Male group as inferred from the box-plot


## f) Do you see any outliers in either of the two groups?

#Yes, as the box-plot shows, Male group has 2 outliers which lie outside the whiskers


## g) Now calculate the mean and standard deviation of the AgeYear per group.
male <- split(dutchSpeakersDistMeta, dutchSpeakersDistMeta$Sex)$male
female <- split(dutchSpeakersDistMeta, dutchSpeakersDistMeta$Sex)$female

mean(male$AgeYear, na.rm = TRUE)
sd(male$AgeYear, na.rm = TRUE)

mean(female$AgeYear, na.rm = TRUE)
sd(female$AgeYear, na.rm = TRUE)

## h) What do the whiskers of a boxplot mean?
# Whiskers are the most extreme points inside 1.5 times inter-quartile range

###############
### Exercise 3: Children's stories & dataframes
###############
# A researcher is interested in the way children tell stories. More specifically,
# she wants to know how often children use 'and then'. She asks 25 children to
# tell her a story, and counts the number of times they use 'and then'.
# The data follow:

# 18 15 22 19 18 17 18 20 17 12 16 16 17 21 25 18 20 21 20 20 15 18 17 19 20 

## a) What measurement scale is this data? Is it discrete or continuous? Explain
##    in one sentence why? (remember, comment out written answers)

# This data is discrete, as there can't be anything in between two values. For example, 21 and 22 - are the amount of times
# that children used the required phrase, no number in between is possible.

## b) In the next questions (c-e), you will create a dataframe of this data, 
##    which will also include participant IDs.
##    Why is a dataframe better suited to store this data than a matrix?

#Data frame may consist of different types of data, while matrices include only same type of data. 
# As we are working with categorical variables and integers, it's preferable to use dataframe.


## c) First create a vector with participant IDs. Your vector should be named 
##    'pps', and your participants should be labeled from 1 to 25

pps <- c(1:25)

## d) Next, create a vector containing all the observations. Name this vector 'obs'.
obs <- c(18, 15, 22, 19, 18, 17, 18, 20, 17, 12, 16, 16, 17, 21, 25, 18, 20, 21, 20, 20, 15, 18, 17, 19, 20)

## e) Create a dataframe for this data. Assign this to 'stories'. 
stories <- data.frame(pps, obs)

## f) Take a look at the summary of your dataframe, and at the classes of your 
##    columns. What class is the variable 'pps'?
summary(stories)
class(pps)

#Variable 'pps' is integer.

## g) Change the class of 'pps' to factor. Why is factor a better class for this
##    variable?

pps <- as.factor(obs)

## h) Plot a histogram (using hist()) for these data. Set the number of breaks 
##    to 8.

hist(obs, breaks = 8)

## i) Create a kernel density plot using density().
plot(density(obs))

## j) What is the difference between a histogram and a kernel density plot?
# A histogram counts how many samples fall into each predefined area (result is discrete)
# A kernel density plot uses Euclidean distances to known samples in order to assign probabilities (result is continuous)

## k) Overlay the histogram with the kernel density plot 
##    (hint: the area under the curve should be equal for overlaying the graphs 
##    correctly.)

x <- stories$obs 
hist(x, breaks = 8, prob=TRUE, col="green", xlab="number of repetions", main = "Overlay of histogram and kernel density plot")
lines(density(x), col="red")


###############
### Exercise 4: Normal distributions
###############
## In this exercise, we will plot normal distributions.

## a) First, use seq() (?seq) to select the x-values to plot the range for
##    (will become the x-axis in the plot).
##    Get R to generate the range from -5 to 5, by 0.1. Assign this to the 
##    variable x.

x <- seq(-5, 5, 0.1)

## b) Now we need to obtain the y-values of the plot (the density). We do this 
##    using the density function for the normal distribution. 
##    Use "help(dnorm)" to find out about the standard functions for the normal 
##    distribution.
y <- dnorm(x)

## c) Now use plot() to plot the normal distribution for z values of "x". 
plot(x, y)

## d) The plot now has a relatively short y-range, and it contains circles 
##    instead of a line. 
##    Using plot(), specify the y axis to range from 0 to 0.8, and plot a line 
##    instead of the circles.
## plot(x, y>=0 & y<=0.8, type = "l")
plot(x, y, type = "l", ylim = c(0, 0.8))

## e) We want to have a vertical line to represent the mean of our distribution.
##    'abline()' can do this for us. Look up help for abline(). 
##    Use abline() to create the vertical line. Specify the median of x using
##    the argument 'v'.
##    In order to get a dashed line, set the argument 'lty' to 2.
abline(v = median(x), lty = 2)

## f) Take a look at the beaver1 dataset. (You can see it by typing "beaver1".) 
##    Then select only the temperature part and store it in a variable "b1temp".
b1temp <- beaver1$temp

## g) Calculate the mean and standard deviation of this dataset and plot a normal
##    distribution with these parameters.
mean.b1temp <- mean(b1temp)
sd.b1temp <- sd(b1temp)
plot(b1temp, dnorm(b1temp, mean.b1temp, sd.b1temp))

## h) We observe two tempareatures (36.91 and 38.13). What's the likelihood that
##    these temperatures respectively come from the normal distribution from g)?
#sum(b1temp == 36.91 | b1temp == 38.13) / length(b1temp)

# We need to find the likelihood of these values coming from a normal distribution
pnorm(36.91, mean.b1temp, sd.b1temp, lower.tail = FALSE,) * 2
pnorm(38.13, mean.b1temp, sd.b1temp, lower.tail = FALSE,) * 2

# Gives the likelihood of the two temperatures

## i) Use the random sampling function in R to generate 20 random samples from
##    the normal distribution from g), and draw a histrogram based on this sample.
##    Repeat 5 times. What do you observe?
hist(sample(b1temp, 20))
hist(sample(b1temp, 20))
hist(sample(b1temp, 20))
hist(sample(b1temp, 20))
hist(sample(b1temp, 20))
hist(rnorm(b1temp, mean.b1temp, sd.b1temp))
# Each time, a new distribution is seen. This is because of the random sampling of the data