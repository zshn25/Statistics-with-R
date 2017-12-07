### Stats with R Exercise sheet 7

##########################
#Week 8: ANOVA
##########################


## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:59 on Monday, December 11. Write the code below the questions. 
## If you need to provide a written answer, comment this out using a hashtag (#). 
## Submit your homework via moodle.
## You are allowed to work together in group up to three students, but everybody 
## needs to submit the group version of the homework via moodle.


## Please write below your (and your teammates) name, matriculation number. 
## Name: Alyona Morozova, Zeeshan Khan Suri
## Matriculation number: 2564744, 2566459

## Change the name of the file by adding your matriculation numbers
## (exercise0N_firstID_secondID_thirdID.R)

###########################################################################################



#######################
### PART 1: Preparation
#######################

library(boot)
library(ggplot2)
library(reshape)

# This time we will be working with the "amis" data frame (package 'boot') that has 
# 8437 rows and 4 columns.

# In a study into the effect that warning signs have on speeding patterns, Cambridgeshire
# County Council considered 14 pairs of locations. The locations were paired to account 
# for factors such as traffic volume and type of road. One site in each pair had a sign 
# erected warning of the dangers of speeding and asking drivers to slow down. No action 
# was taken at the second site. Three sets of measurements were taken at each site. 
# Each set of measurements was nominally of the speeds of 100 cars but not all sites 
# have exactly 100 measurements. These speed measurements were taken before the erection 
# of the sign, shortly after the erection of the sign, and again after the sign had been 
# in place for some time.

# For the further reference please use ?amis. It may take some time to understand the dataset. 
?amis
# Load the dataset and briefly inspect it. 
# Feel free to make some plots, calculate some statistics in order to understand the data.
head(amis)
str(amis)
# All our columns have numeric type. Convert the categorial columns to factors.
names <- names(amis)
names <- names[2:4]
amis[names] <- lapply(amis[names], factor)

levels(amis$period) <- c("before", "immediately", "after")

# Build a boxplot for the distribution of `speed` for each of `period` values 
# (before, immediately after and after some time). Build 2 plots side by side
# depending on the `warning` variable.
# (for all plots here and below please use ggplot)
ggplot(amis, aes(x = period, y = speed)) +
  geom_boxplot() + 
  facet_grid(. ~ warning)

# What can you conclude according this plot? What can you say about people behaviour in
# different periods: before, immediately after and after some time?
# It can be concluded that the average speed increases after the sign. Immediately after the sign was put,
# it decreases a bit and then increased more than before

# What are your ideas about why the data with warning==2 (which correspond to the
# measurements in different times on sites where no sign was erected) was collected?



#######################
### PART 2: 1-way ANOVA
#######################
# For 1-way ANOVA we will be working with the subset of `amis` where the 
# warning sign was erected, which corresponds to warning==1.


# First, let's check the ANOVA assumptions 
# 1.) Independence assumption is violated, because we have repeated measurements from 
# the same location (column 'pair'), which introduces dependencies. So you need to 
# average the speed over each `pair` and `period` (using the function cast as in ex5). 
# But please note that this
# is not the best way to do it. In a few lectures we will learn about Linear Mixed
# Models, that are designed to handle the situations, where the assumption of 
# independence is violated due to multiple measurements per same subject (remember ex5) or 
# per item (this exercise).


# Please, note that we will be working only with this new data frame in this section.
# Again, build a boxplot of the average speed depending on period


# How do you think if there is a difference between periods?


# 2.) Normality of residuals
# (you need to figure out the best way to do it and give a detailed justified answer)


# 3.) Homogeneity of variance of residuals
# (you need to figure out the best way to do it and give a detailed justified answer)



# Now we are ready to perform 1-way ANOVA: please use the function aov() on the speed
# depending on the period,
# report p-value and interpret the result in details


# Please do a pairwise t-test with pairwise.t.test()

# Report pair-wise p-values and interpret the result in details


# Try to use no adjustment for pairwise testing and then Bonferroni correction.
# Does the result change?




#######################
### PART 3: 2-way ANOVA
#######################
# Now we want to analyze the influence of 2 categorial variables (period and warning) on the speed.
# So let's turn back to our initial dataset amis (not its subset with warning==1)
# First, we need to again average the speed over each `pair`, `warning` and `period`.


# Calculate the mean for each of 6 pairs of `period` and `warning`


# Do you think there is a significant difference in some of the groups?


# Now apply 2-way ANOVA: please use the function aov() on the speed depending on the period and warning
# report p-value and interpret the result in details


# What do you conclude about the behaviour of drivers based on the 2-way ANOVA?



