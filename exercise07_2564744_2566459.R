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
library(reshape2)
library(ggplot2)
library(reshape)
library(tidyr)
library(lsr)
library(dplyr)
library(stats)
library(car)

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

data<-boot::amis

who(TRUE) #shows current structure of data
head(data)
str(data)

# All our columns have numeric type. Convert the categorial columns to factors.
names <- names(data)
names <- names[2:4]
data[names] <- lapply(data[names], factor)

levels(data$period) <- c("before", "immediately", "after")
levels(data$warning) <- c("sign", "no sign")

# Build a boxplot for the distribution of `speed` for each of `period` values 
# (before, immediately after and after some time). Build 2 plots side by side
# depending on the `warning` variable.
# (for all plots here and below please use ggplot)
ggplot(data, aes(x = period, y = speed)) +
  geom_boxplot() + 
  facet_grid(~ warning)

# What can you conclude according this plot? What can you say about people behaviour in
# different periods: before, immediately after and after some time?

## It can be concluded that the average speed increases after the sign. Immediately after the sign was put,
## it decreases a bit and then increased more than before

# What are your ideas about why the data with warning==2 (which correspond to the
# measurements in different times on sites where no sign was erected) was collected?

## The data with no signs was collected in order to compare it with the data collected after the sign was erected.
## This comparision will show if there was a real difference between these two variables.


#######################
### PART 2: 1-way ANOVA
#######################
# For 1-way ANOVA we will be working with the subset of `amis` where the 
# warning sign was erected, which corresponds to warning==1.

data_warn <- subset(data, warning == "sign")

# First, let's check the ANOVA assumptions 
# 1.) Independence assumption is violated, because we have repeated measurements from 
# the same location (column 'pair'), which introduces dependencies. So you need to 
# average the speed over each `pair` and `period` (using the function cast as in ex5). 
# But please note that this
# is not the best way to do it. In a few lectures we will learn about Linear Mixed
# Models, that are designed to handle the situations, where the assumption of 
# independence is violated due to multiple measurements per same subject (remember ex5) or 
# per item (this exercise).

data_warn_av <- dcast(data_warn, period + pair ~ 'avg_speed', mean, na.rm = T, value.var = "speed")
head(data_warn_av)

# Please, note that we will be working only with this new data frame in this section.
# Again, build a boxplot of the average speed depending on period

ggplot(data_warn_av, aes(x = period, y = avg_speed)) +
  geom_boxplot()

# How do you think if there is a difference between periods?

## According to our new boxplot there is a difference between periods and it is visible more clear. 
## The lowest average speed corresponds to the situation when a reading was taken shortly after erection of the sign  

model <- lm(avg_speed ~ period + pair, data = data_warn_av)

# 2.) Normality of residuals
# (you need to figure out the best way to do it and give a detailed justified answer)

hist(residuals(model))

##doesn't look completely like a bell curve

boxplot(residuals(model))

##boxplot is non-symmetric, which probably tell us of non-normality of residuals

shapiro.test(residuals(model))

## but according to shapiro test though we cannot reject the hypothesis of normality of residuals, as p-value is relatively high

qqnorm(residuals(model))
qqline(residuals(model))

##qq plot also looks quite "normal"

# 3.) Homogeneity of variance of residuals
# (you need to figure out the best way to do it and give a detailed justified answer)

##we may use Bartlett test or Figner-Killeen test or Levene test

bartlett.test(avg_speed ~ period, data = data_warn_av)
fligner.test(avg_speed ~ period, data = data_warn_av)
leveneTest(data_warn_av$avg_speed, data_warn_av$period) #requires "car" library

##according to p-value that equals nearly 0.8 we cannot reject hypothesis of homogeneity of variance of residuals

# Now we are ready to perform 1-way ANOVA: please use the function aov() on the speed
# depending on the period,
# report p-value and interpret the result in details
anova_1_way <- aov(data_warn_av$avg_speed ~ data_warn_av$period)
summary(anova_1_way)

# Please do a pairwise t-test with pairwise.t.test()
pairwise.t.test(data_warn_av$avg_speed, data_warn_av$period)

# Report pair-wise p-values and interpret the result in details
## F-value = 0.986, p-value = 0.382
## F value is quite high, which means that variance between groups is way higher than variance witheen groups, that's how it should be
## but high p-value tells of statistical insignificance


# Try to use no adjustment for pairwise testing and then Bonferroni correction.
# Does the result change?
pairwise.t.test(data_warn_av$avg_speed, data_warn_av$period, p.adjust = "none")
pairwise.t.test(data_warn_av$avg_speed, data_warn_av$period, p.adjust = "bonferroni")

## results changed in a predictable way, as Bonferroni is the most conservative method, while no adjustment, oppositely,
## is the least conservative. 


#######################

### PART 3: 2-way ANOVA

#######################

# Now we want to analyze the influence of 2 categorial variables (period and warning) on the speed.

# So let's turn back to our initial dataset amis (not its subset with warning==1)

# First, we need to again average the speed over each `pair`, `warning` and `period`.

data.2way <- dcast(data, period + pair + warning ~ 'avg_speed', mean, na.rm = T, value.var = "speed")



# Calculate the mean for each of 6 pairs of `period` and `warning`

daa <- dcast(data.2way, period  + warning ~ 'mean', mean, na.rm = T, value.var = "avg_speed")



# Do you think there is a significant difference in some of the groups?

# It looks like there is very little differene in the groups. The most different groups are immediately after sign

# before sign.



# Now apply 2-way ANOVA: please use the function aov() on the speed depending on the period and warning

# report p-value and interpret the result in details



# We should use + between the variables because there is no interaction between them since before sign, the variable

# perio doesn't make sense.

aov2way <- aov(daa$mean ~ daa$period + daa$warning)

summary(aov2way)



# p-value with respect to period variable is 0.2444 which is not significant which means it doesn't depend when

# the data is taken before/immediately/after the sign, it doesn't have significant affect on driver's speed



# p-value with respect to warning variable is 0.0402 which is < 0.05 which means that the effect of presence of

# warning sign makes significant difference



# What do you conclude about the behaviour of drivers based on the 2-way ANOVA?

# Based on 2-way ANOVA, we can conclude that the warning variable i.e. the presence of sign makes a significant

# difference in drivers speed while the period variable i.e. when the data was taken before/immediately/after

# the sign doesn't make a significant difference which is intuitive.


