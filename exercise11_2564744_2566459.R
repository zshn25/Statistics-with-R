##########################
#Week 12: Model Families and Logistic Regression
##########################


## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:59 on Monday, January 15. Write the code below the questions. 
## If you need to provide a written answer, comment this out using a hashtag (#). 
## Submit your homework via moodle.
## You are allowed to work together in group up to three students, but everybody 
## needs to submit the group version of the homework via moodle.


## Please write below your (and your teammates) name, matriculation number. 
## Name: Alyona Morozova, Zeeshan Khan Suri
## Matriculation number: 2564744, 2566459

## Change the name of the file by adding your matriculation numbers
## (exercise0N_firstID_secondID_thirdID.R)

######################################################################################################################


####
#Part 1
####
# The folder speed.dating
# contains data from an experiment on a few hundred students that randomly assigned
# each participant to 10 short dates with participants of the opposite sex
# (Fisman et al., 2006). For each date, each person recorded several subjective
# numerical ratings of the other person (attractiveness, compatibility, and some 
# other characteristics) and also wrote down whether he or she would like to meet
# the other person again. Label and rij1, . . . , rij6 as person iâs numerical ratings of person j 
# on the dimensions of attractiveness, compatibility, and so forth.

library(lme4)
library(lattice)
library(Matrix)
library(ggplot2)
library(plyr)

library(boot)
library(reshape2)
library(reshape)
library(tidyr)
library(lsr)
library(dplyr)
library(plotly)

setwd("D:/Studies_WS_17-18/Statistics with R")
sd <- data.frame(read.csv("Speed Dating Data.csv"))

glimpse(sd)

#(1) Fit a classical logistic regression predicting Pr(yij = 1) given person i's 
#    ratings of person j. For ratings, use the features attr, sinc, intel, fun; see the documentation for what exactly these
#    abbreviations stand for.
#    Also, please plot the data in order to inspect it, and discuss the importance of attractiveness, compatibility, and so 
#    forth in this predictive model.
mod <- glm(dec ~ attr, data = data.frame(sd), family = 'binomial')

summary(mod)

ggplot(sd, aes(x = attr, y = dec, color = iid)) + 
  geom_point() + geom_jitter(width = 0, height = 0.05, alpha = .5)
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = F)


#(2) Expand this model to allow varying intercepts for the persons making the
#    evaluation; that is, some people are more likely than others to want to meet
#    someone again. Discuss the fitted model.
mod <- glm(dec ~ attr + intel+ fun + shar + factor(iid),
               data = data.frame(sd))

summary(mod)
## Many of these people making evluations do not have statistically significant effect on the model but some do.

#(3) Expand further to allow varying intercepts for the persons being rated. Discuss
#    the fitted model.
mod <- glm(dec ~ attr + intel+ fun + shar + factor(pid),
           data = data.frame(sd))

summary(mod)
##Same as before but, the AIC in the previous model was lower. So, that is better than this model.

#(4) Now fit some models that allow the coefficients for attractiveness, compatibility, and the 
#    other attributes to vary by person.  Fit a multilevel model, allowing the intercept and the 
#    coefficients for the 6 ratings to vary by the rater i. (Hint: The model will not converge when you 
#    include many predictors as random slopes; see with how many predictors you can get the model to converge;
#    and try out some of the tricks we have seen to see whether they affect convergence for this dataset.)

mod <- glmer(dec ~ attr + intel+ fun + shar + sinc + amb +
               (1+attr|iid) +
               data = data.frame(sd), family = 'binomial',
             glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000)))

p <- plot_ly(data = sd, z = ~dec, x = ~attr, y = ~intel, opacity = 0.6) %>%
  add_markers() 
# draw the plane
p %>%
  add_surface(x = ~x, y = ~y, z = ~dec, showscale = FALSE)

## We increase the number of iterations to 10000 and use a better optimizer bobyqa in order to converge.
## sinc and amb don't seem to have significant difference on the model.

#(5) compare the output for the different models that you calculated - did the model design affect your conclusions?
## We see that the last model is better than the other because of the lower AIC. We can also remove sinc and amb to
## improve more

####
#Part 2
####

# In this example, num_awards is the outcome variable and indicates the number of awards earned by students at
# a high school in a year, math is a continuous predictor variable and represents students' scores on their 
# math final exam, and prog is a categorical predictor variable with three levels indicating the type of program 
# in which the students were enrolled. It is coded as 1 = "General", 2 = "Academic" and 3 = "Vocational". 
# Let's start with loading the data and looking at some descriptive statistics.

p = read.csv("poisson_sim.csv", sep=";")

p <- within (p, {
  prog <- factor(prog, levels=1:3, labels=c("General", "Academic", "Vocational"))
  id <- factor(id)  
})

summary(p)

#(6) Plot the data to see whether program type and math final exam score seem to affect the number of awards.

ggplot(data = p, aes(x = math, y = num_awards, col = prog)) + geom_point()

##graph shows us that people with the highest amounts of awards are all from academic degrees
##higher math exam results also belong to mostly academics, except for 1 person from vocational program

#(7) Run a generalized linear model to test for significance of effects.

mod1 <- glm(num_awards ~ math + prog, data = p)
drop1(mod1,test="Chisq")

gmod1 <- glmer(num_awards ~ math + prog, family=poisson, data=p)
anova(mod1, gmod1, test="Chisq")

#(8) Do model comparisons to find out whether the predictors significantly improve model fit.
mod2 <- glm(num_awards ~ math, data = p)
drop1(mod2,test="Chisq")

mod3 <- glm(num_awards ~ prog, data = p)
drop1(mod3,test="Chisq")

##both predictors are significant

mod4 <- glm(num_awards ~ math + prog + math*prog, data = p)
anova(mod1, mod4, test="Chisq")

#including interaction significantly improves our model

#(9) Compare to a model that uses a gaussian distribution (normal lm model) for this data.
nlm <- lm(num_awards ~ math + prog, data = p)
anova(gmod1, nlm, test="Chisq")

## AIC and BIC is lower for gmod1 than for nlm
##it means that model that uses poisson distribution better fits our data than standard one
