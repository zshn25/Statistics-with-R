### Stats with R Exercise sheet 6

##########################
#Week 7: Correlation and Regression
##########################


## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:59 on Monday, December 4. Write the code below the questions. 
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
###########################################################################################


library(reshape)
library(reshape2)
library(languageR)
library(ggplot2)
library(tidyr)
library(lsr)
library(dplyr)
library(cowplot)

#######################
### PART 1: Correlation
#######################

########
### Please, use ggplot to make plots in all exercises below!
########

# Get some data - access the ratings data set in languageR and name it "data".
# Subjective frequency ratings and their length averaged over subjects, for 81 concrete English nouns.
data<-languageR::ratings
who(TRUE)

# Take a look at the data frame.
str(data)
glimpse(data)

# Let's say you're interested in whether there is a linear relationship between the word frequency of 
# the 81 nouns and their length.
# Take look at the relationship between the frequency and word length data by means a of a scatterplot 
# (from ggplot library).
ggplot(data, aes(x = Frequency, y = Length, col = Length)) + geom_point()

# Judging from the graphs, do you think that word frequency and word length are in any way correlated 
# with one another?

#Seems that the longer is the word, the rare it appears, so there is an inverse relationship between frequency and length

# Compute the Pearson correlation coefficient between the two variables by means of cor().
# Tell R to only include complete pairs of observations.
# As a reminder: Pearson coefficient denotes the covariance of the two variable divided by the product 
# of their variance. It is scaled between 1 (for a perfect positive correlation) to -1 (for a perfect 
# negative correlation).
cor(data$Frequency, data$Length, c("complete.obs"))

# Does the correlation coefficient suggest a small, medium or large effect?
# What about the direction of the effect?

#The correlation coefficient suggests medium (~0.428) effect
#The coefficient is negative and below -1, which means that there is a perfect negative correlation

# Note that we have a large number of tied ranks in word length data (since there are multiple words 
# with the length of, e.g. 5).
# Thus, we might draw more accurate conclusions by setting the method to Kendall's tau instead of 
# Pearson (which is the default).
# How do you interpret the difference between these 2 correlation coefficients?
cor(data$Frequency, data$Length, method = "kendall", c("complete.obs"))

#Here the absolute value is smaller (~0.316), which still suggests moderate effect though. This coefficient is more resistant to tied data.  

# What about significance? Use the more user-friendly cor.test!
cor.test(data$Frequency, data$Length)

# Take a look at the output and describe what's in there.
# What do you conclude?

#We may conclude that our result is significant; estimated coefficient lies inside the confidence interval

# Finally, we can also calculate Spearman's rank correlation for the same data.
cor(data$Frequency, data$Length, method = "spearman", c("complete.obs"))


###################################################################################################


#######################
### PART 2: Regression
#######################

# Fit a linear regression model to the data frame for the variables frequency (outcome variable) 
# and Length (predictor variable).
# General form: "modelname <- lm(outcome ~ predictor, data = dataFrame, na.action = an action)"

frlen <- lm(Frequency ~ Length, data, na.action = NULL)
coef(frlen)

# How do you interpret the output? Is the relationship between the two variables positive or negative?
# Plot the data points and the regression line.
ggplot(data, aes(x = Frequency, y = Length, col = Length)) + geom_jitter() + geom_smooth(method="lm", se = F)

# Run the plotting command again and have R display the actual words that belong to each point. 
# (Don't worry about readability of overlapping words.)
ggplot(data, aes(x = Frequency, y = Length, col= Length, label = Word)) + geom_point(alpha=0.5) + geom_smooth(method="lm", se = F) + geom_text(aes(label=Word),size = 3, hjust=0.5, vjust=1.5)


###################################################################################################


# Try this again for another example:
# Let's go back to our digsym data set.
# Set your wd and load the data frame digsym_clean.csv
setwd("C:/UdS/Statistics with R/")
df <- read.csv("digsym_clean.csv")
head(df)

# Suppose you want to predict reaction times in the digit symbol task by people's age.
# Fit a linear regression model to the data frame for the variables correct_RT_2.5sd (outcome variable) 
# and Age (predictor variable).
# General form: "modelname <- lm(outcome ~ predictor, data = dataFrame, na.action = an action)"

react <- lm(correct_RT_2.5sd ~ Age, data = df, na.action = na.exclude)
coef(react)

# Let's cast the data to compute an RT mean for each subject, so that we have only one Age 
# observation by Subject.
# In case you're wondering why we still have to do this - like the t-test, linear regression assumes 
# independence of observations.
# In other words, one row should correspond to one subject or item only.

dfcast <- dcast(df, Subject + Age ~ 'Mean_RT', mean, na.rm = T, value.var = 'StimulDS1.RT')
head(dfcast)

# Fit the regression model.
fit_react <- lm(Mean_RT ~ Age, dfcast, na.action = na.exclude)
coef(fit_react)

# Let's go over the output - what's in there?
# How do you interpret the output?

#According to our model mean react time tends to increase with age of participant. One year increase of age gives us nearly 23 seconds increase in mean response time 

# Again plot the data points and the regression line. 

plot_init <- ggplot(dfcast, aes(x = Age, y = Mean_RT)) + geom_jitter() + geom_smooth(method="lm", se = F)

# Plot a histogram and qq-plot of the residuals. Does their distribution look like the normal distribution?
ggplot(fit_react, aes(.resid)) + geom_histogram(binwidth = 50)

qqp<-ggplot(fit_react, aes(qqnorm(.stdresid)[[1]], .stdresid))+geom_point(na.rm = TRUE)
qqp<-qqp+geom_abline(aes(qqline(.stdresid)))+xlab("Theoretical Quantiles")+ylab("Standardized Residuals")
qqp<-qqp + ggtitle("Normal Q-Q")
qqp

#The distribution doesn't look like normal dstribution

# Plot Cooks distance which estimates the residuals (i.e. distance between actual values and the 
# regression line) for individual data points in the model.

cooks<-ggplot(fit_react, aes(seq_along(.cooksd), .cooksd))+geom_bar(stat="identity", position="identity")
cooks<-cooks+xlab("Obs. Number")+ylab("Cook's distance")
cooks<-cooks+ggtitle("Cook's distance")
cooks

# It actually looks like we have 1 influential observation in there that has potential to distort 
# (and pull up) our regression line.
# The last observation (row 37) in cast yielded a Cooks D is very high (greater than 0.6).
# In other words, the of the entire regression function would change by more than 0.6 when this 
# particular case would be deleted.

# What is the problem with observation 37?
# Run the plotting command again and have R display the subjects that belong to each point.

ggplot(dfcast, aes(x = Age, y = Mean_RT, col = Age, label = Subject)) + geom_jitter() + geom_smooth(method="lm", se = F) + geom_text(aes(label=Subject), hjust=0.5, vjust=1.5)

# Make a subset of "cast" by excluding this subject and name it cast2.

cast2 <- subset(dfcast, Subject!= 37)

# Fit the model again, using cast2, and take a good look at the output.

fit_cast2 <- lm(Mean_RT ~ Age, cast2, na.action = na.exclude)
coef(fit_cast2)

# What's different about the output?
# How does that change your interpretation of whether age is predictive of RTs?

#Now when we got rid of one of outlier, our data better fits linear regression model, but still the result is quite unconvincing, as linear regression fits poorly into our data. Slope got a bit bigger

# Plot the regression line again - notice the difference in slope in comparison to our earlier model fit?
plot_clean <- ggplot(fit_cast2, aes(x = Age, y = Mean_RT)) + geom_jitter() + geom_smooth(method="lm", se = F)


# Display the two plots side by side to better see what's going on.
plot_grid(plot_init, plot_clean, labels = "AUTO")

# Compute the proportion of variance in RT that can be accounted for by Age.
# In other words: Compute R Squared.
# Refer to Navarro (Chapter on regression) if you have trouble doing this.

R.squared <- summary(fit_cast2)$r.squared
R.squared

# How do you interpret this number?

# It means that predictor (Age) explains only 15% of the variance in the outcome (Mean_RT)
