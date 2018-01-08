# homework: play around with the simulation code. The code simulates how a dataset may be generated. The advantage over using a real data set is that we know exactly how the data was generated, and can observe whether the model manages to correctly identify the original model structure and coefficients.

# IMPORTANT! run each model simulation code several times to see how stable the results are -- this is necessary because we are sampling the data randomly, so it could be that we sometimes get more or less "lucky" draws.

library(lme4)
library(car)

n <- 20000 # number of observations to be simulated
predA <- rnorm(n, 100, 20)
predB <- rnorm (n, 60, 30)
interact <- 0.002*(predA*predB) 
error <- rnorm (n, 0, 30)

resp <- 25 + predA + 1.2*predB - interact + error

d <- data.frame(predA, predB, resp)
# WRITE DOWN WHAT VALUES YOU WOULD HOPE FOR THE MODEL TO ESTIMATE
# IN THE IDEAL CASE
# intercept= 25
# predA= 1
# predB= 1.2
# predA:predB = -0.002

#m1 <- lm(resp~ predA + predB - predA*predB, data=d)
m1<- lm(resp~predA*predB, data=d)
# Ignore the warning message about rescaling for now, we'll get to that below.
summary(m1)  
# Can the model recover the original model structure and estimate correct coefficients for the predictors?
## The model cannot recover the original model structure but predict it by estimating the coeffecients of the
## predictors. If the estimation is close to ideal, then the model structute is close to the original one

# What happens if you change the number of subjects?
## More number of subjects leads to better representration of the population and thus more similar to ideal coefficients

# What happens if you change the variance of the error term?
## If we increase/decrease the error, the standard error of the intercept increases/decreases.

# What happens if you change the effect sizes?
## Increasing/Decreasing the effect size will reduce/increase interaction between the predictors.
## Thus, decreasing/increasing the interact variable.

# Next, we want to observe the effect of scaling the predictors. 

# by hand, we could do: normpredA <- (predA - mean(predA)) / sd(predA)
# this is the same as calling "normpredA <- scale(predA)"
# we can do this for the whole data frame:
nd <- as.data.frame(scale(d))

 
sm1<- lm(resp~predA*predB, data=nd)
summary(sm1)
vif(m1)
vif(sm1)

# Are the predictors currently correlated? What does the vif value mean?
## Yes, since vif is >5 for predA and > 25 for predB, they are highly correlated.
## But, after scaling them, we can see that the vif value is ~1, indicating that scaling reduces the correlation.
## Variation inflation factor tells us if the predictors are colinear, i.e. if they interact with each other.

# - check whether normalization also has a large effect when there is no interaction present in the model

sm2<- lm(resp~predA+predB, data=nd)
m2<- lm(resp~predA+predB, data=d)
summary(sm2)
summary(m2)
vif(m2)
vif(sm2)
## If there is no interaction present in the model, the predictors are not colinear. So, vif is already very low and
## further scaling the data doesn't effect it.

# Try out what happens if there was originally no interaction in the data.
resp1 <- 25 + predA + 1.2*predB + error
sm3<- lm(resp1~predA+predB, data=nd)
m3<- lm(resp1~predA+predB, data=d)
summary(sm3)
summary(m3)
vif(m3)
vif(sm3)
## If there was originally no interaction, the vif values are naturally very low indicating no interaction. Scaling 
## has no effect.

# next, we want to calculate interpretable estimates

names(sm1)
coef(sm1)

denormPredA <- coef(sm1)[2] *sd(d$resp)/ sd(d$predA) 
#denormPredA <- coef(sm1)[2] * sd(d$predA) + mean(predA)
 denormPredA

# Explain in your own words, why the denormalization for predictor A works this way.
## To denormalize, we just need to multiply the normalized predictor with the sd of the whole model. Then, we divide
## with the respective sd of the predictor since we need the coeffecient only.

denormPredB <- coef(sm1)[3] * sd(d$resp)/ sd(d$predB)
 denormPredB
# expected: 1.2


 denormIntercept<-coef(sm1)[1] * sd(d$resp)+mean(d$resp)-
 (denormPredA*mean(d$predA) + denormPredB* mean(d$predB))
 denormIntercept
# expected: 25

denormInteract <- coefficients(sm1)[4] / (sd(d$predA)*sd(d$predB)) * sd(d$resp)
denormInteract




# next, we create correlated variables 
n <- 20000 # number of observations to be simulated
predA <- rnorm(n, 100, 20)
predB<- rnorm (n, 60, 30)
predC <- -1* predA + rnorm(n,0,10)
error <- rnorm (n, 0, 30)
respcor <- 25 + predA + 3* predB+ 2*predC - (0.02*(predA*predC)) + error
d2<-data.frame(predA, predB, predC, respcor)
summary(lm(respcor ~ predA * predC + predB, data=d2))

sd2 <-as.data.frame(scale(d2))
summary(lm(respcor ~ predA * predC + predB, data=sd2))

# What do you observe regarding the results from the models? Do the models obtain the same or different results with / without normalization?

# Denormalize the coefficients.

 
# finally, we will generate repeated measures!
# For this, we will use the dataframe d; for simplicity of interpretation, we will do no normalization here.
n<-400
predA <- rnorm(n, 100, 20)
predB <- rnorm (n, 60, 30)
error <- rnorm (n, 0, 30)
subjno <- 20 #number of subjects; 
itemno<- 20 # number of items;
subj<-as.factor(rep(1:subjno,itemno))
item<-as.factor(rep(1:itemno, each=subjno))
lmerd <- data.frame(predA, predB, resp, subj, item)
# basic data frame done; now on to by subject and by item random effects:
subjid<-as.factor(1:subjno)
subjint<- rnorm(subjno, 0, 8)
subjeffdf<-data.frame(subjid, subjint)
itemid<-as.factor(1:itemno)
itemint<-rnorm(itemno, 0, 4)
itemeffdf<-data.frame(itemid, itemint)
newd <- merge(lmerd, itemeffdf, by.x="item", by.y="itemid")
newd<- merge(newd, subjeffdf, by.x="subj", by.y = "subjid")
lmerd <- newd
# add by-subject and by-item effects here!
lmerd$respr <- 25+newd$subjint + newd$itemint + newd$predA + 1.2*newd$predB + error

m0<-lm(respr ~ predA + predB , data=lmerd)
summary(m0)
m1<-lmer(respr ~ predA + predB + (1|subj) + (1| item), data=lmerd)
summary(m1)

lmerd$resp <- 25 + newd$predA + 1.2*newd$predB + error
m2<-lmer(resp ~ predA + predB + (1|subj) + (1| item), data=lmerd)
summary(m2)

#explain the difference between models m0 m1 and m2


# play around with the size of the by item and by subject effects (here: intercepts only)


# generate the data such that subjects differ in terms of how much predictor A affects them.



# then build a mixed effects model that includes a random slope for subjects.




