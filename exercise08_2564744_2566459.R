### Stats with R Exercise sheet 8

##########################
#Week9: Checking Assumptions underlying ANOVA and linear regression
##########################


## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:59 on Monday, December 18. Write the code below the questions. 
## If you need to provide a written answer, comment this out using a hashtag (#). 
## Submit your homework via moodle.
## You are allowed to work together in group up to three students, but everybody 
## needs to submit the group version of the homework via moodle.


## Please write below your (and your teammates) name, matriculation number. 
## Name: Alyona Morozova, Zeeshan Khan Suri
## Matriculation number: 2564744, 2566459


## Change the name of the file by adding your matriculation numbers
## (exercise0N_firstID_secondID_thirdID.R)

###############################################################################
###############################################################################

library(boot)
library(reshape2)
library(ggplot2)
library(reshape)
library(tidyr)
library(lsr)
library(dplyr)
library(stats)
library(car)

########
### Please, use ggplot to make plots in all exercises below!
########

# a) Read in the data kidiq.txt and take a look at the data summary.
#    It contains information about the mum's iq and their child's iq.
#    mom_hs indicates whether the mother has a high school degree
#    1= high school education, 0= no high school degree.

setwd("C:/UdS/Statistics with R/")
kidiq <- read.table("kidiq.txt")
who(TRUE)
summary(kidiq)

# b) Plot kid_score against mom_iq in a scatter plot, and add a regression line 
#    (kid_score should be the response variable and mom_iq the predictor 
#    variable) to the plot. 
#    Name the plot and the axis in sensible ways.

ggplot(kidiq, aes(x = mom_iq, y = kid_score)) + geom_point() + geom_smooth(method="lm", se = F) + labs(x = "Mother's IQ", y = "Kid's IQ", title = "Dependence of kid's IQ from mother's IQ")

# c) Calculate a simple regression model for kid_score with mom_hs as a 
#    predictor and interpret the results.

lm1 <- lm(kid_score ~ mom_hs, kidiq)
summary(lm1)

#As mom_hs variable may be only 0 or 1, we may state from the model that presence of a high school degree adds in average 11.77 points to a kid's IQ, 
#so in average IQ of a kid, whos mom doesn't have a high school degree will be 77.55,
#wile for the kid with HS degree mom - 89.32

# d) Next, fit a regression model with two predictors: mom_hs and mom_iq. 
#    Interpret the model and compare to the previous model.

lm2 <- lm(kid_score ~ mom_hs + mom_iq, kidiq)
summary(lm2)

#This model shows that mom's IQ plays an important role in kid's IQ, as it explains 56,38% of kid's IQ,
#so basic kid's IQ is lower - 25.7315 for kid's of mom's without HS degree and with 0 IQ
#mother's HS degree adds only 5.9501 points to kid's IQ
#while mum's own IQ influences kid's quite much - it "adds" more than half of mother's IQ to kid's IQ value

# e) Now plot a model where both predictors are shown. Do this by plotting 
#    data points for mothers with high school degree==1 in one color and those 
#    without one in another color. Then also fit two separate regression lines such 
#    that these lines reflect the model results.
#	 HINT: One solution in ggplot is to calculate fitted values of the regression model 
#    and then plot them along with the original data points:
#    pred = data.frame(mom_iq=kidiq$mom_iq, mom_hs=kidiq$mom_hs, kid_score_pred=fitted(your_model))

pred = data.frame(mom_iq=kidiq$mom_iq, mom_hs=kidiq$mom_hs, kid_score_pred=fitted(lm2))

ggplot(kidiq) +
  geom_point(aes(mom_iq, kid_score, col = mom_hs)) +
  geom_point(data = pred, aes(mom_iq, kid_score_pred, col = mom_hs), size = 2) + labs(x = "Mother's IQ", y = "Kid's IQ", col = "Mother's HS degree", title = "Dependence of kid's IQ from mother's IQ and HS grade: linear regression model without interaction")


# f) Next, we will proceed to a model including an interaction between mom_hs
#    and mom_iq. Interpret your results.

lm3 <- lm(kid_score ~ mom_iq * mom_hs, kidiq)
summary(lm3)

#there is a crossover interaction between two factors: mom's IQ and her HS degree. 
#It means, that main factors are less important, than interaction

# g) Next, let's plot the results of this model.

interact = data.frame(mom_iq=kidiq$mom_iq, mom_hs=kidiq$mom_hs, kid_score_pred=fitted(lm3))
ggplot(kidiq) +
  geom_point(aes(mom_iq, kid_score, col = mom_hs)) +
  geom_point(data = interact, aes(mom_iq, kid_score_pred, col = mom_hs), size = 2) + labs(x = "Mother's IQ", y = "Kid's IQ", col = "Mother's HS degree", title = "Dependence of kid's IQ from mother's and HS grade: linear regression model with interaction")

# h) Next, let's explore the "predict.lm" function. Please first generate
#    a new dataframe with one datapoint (a mother with high school degree
#    and iq of 100). Then, use the predict function to predict the child's iq. 
#    Please specify the predict function to also give you the 0.95 confidence 
#    interval.

new_data <- data.frame("mom_hs" = c(1), "mom_iq" = c(100))
lm4 <- predict.lm(lm2, new_data, interval = "confidence", level = 0.95, se.fit = T)
lm4

#according to prediction of our model if mom has IQ=100 and HS degree, her kid will have IQ nearly 88

# i) Meaning of confidence intervals for regression line.
#    Let's go back to the exercise b) and plot again the data points with the 
#    regression line. By default, there should also be displayed the borders of 
#    the confidence interval. What is the meaning of the confidence interval?

ggplot(kidiq, aes(x = mom_iq, y = kid_score)) + geom_point() + geom_smooth(method="lm") + labs(x = "Mother's IQ", y = "Kid's IQ", title = "Dependence of kid's IQ from mother's IQ")

#confidence interval area indicates the range in which regression line may be located, 
#if we would like to repeat the experiment over, for example on another sample of the same data

# j) Finally, do model checking on your model with the interaction, i.e. inspect 
#    the standard model plots provided by R, and interpret what you see.

par(mfrow = c(2, 2))
plot(lm3)

#First (top left) graph checks for the homogeneity of the variance 
#Second (top right) graph checks for the normal distribution of the residuals: in our case they line on a line, so are normally distributed
#Third (bottom left) graph checks heterogeneity of variance
#Forth (bottom right) graph allow detecting points that have a too big impact on the regression coefficient

#we see that overall assumptions are fulfilled, there is homogeneity of variance and residuals are distributed normally