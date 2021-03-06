### Stats with R Exercise sheet 4

##########################
#Week5: Tests for Categorial Data
##########################

## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:59 on Monday, November 20. Write the code below the questions. 
## If you need to provide a written answer, comment this out using a hashtag (#). 
## Submit your homework via moodle.
## You are allowed to work together in group up to three students, but everybody 
## needs to submit the group version of the homework via moodle.


## Please write below your (and your teammates) name, matriculation number. 
## Name: Alyona Morozova, Zeeshan Khan Suri
## Matriculation number: 2564744, 2566459

## Change the name of the file by adding your matriculation numbers
## (exercise0N_firstID_secondID_thirdID.R)

#################################################################################
#################################################################################

##########
##Exercise 1. Binomial distribution
##########
## Suppose there are 12 multiple choice questions in a quiz. 
## Each question has 5 possible answers, and only one of them is correct. 

## a) Please calculate the probability of getting exactly 4 answers right 
##    if you answer by chance. Calculate this using the dbinom() function.
dbinom(4 , size = 12, prob = 1/5)

## b) Next please calculate the probability of answering 4 or less questions 
##    correctly by chance. 
sum(dbinom(0:4 , size = 12, prob = 1/5))

##########
##Exercise 2. Chi-square test
##########
## a) Consider the dataset dutchSpeakersDistMeta from our first tutorial again. 
##    Load the package (languageR) and look at the summary of the variables, 
##    as well as their classes. Which variables are factors?
check.install <-function(package) {
  if(is.element(package, rownames(installed.packages())) == FALSE) {install.packages(package)}
  library(package, character.only = TRUE)
}

check.install("languageR")
check.install("ggplot2")

summary(dutchSpeakersDistMeta)
lapply(dutchSpeakersDistMeta, class)
# "Speaker", "Sex", "AgeGroup", "ConversationType", "EduLevel" are factors

## b) We want to find out whether there is a difference between males and females 
##    with respect to the age groups they are in.
##	  First use the function 'table()' to get the counts and create 
##    a contingency table of AgeGroup by Sex.
age <- table(dutchSpeakersDistMeta$Sex, dutchSpeakersDistMeta$AgeGroup)

##    Visualize your data with a single bar plot (use ggplot) that represents the counts with respect to each age group 
##	  and each sex.
# ggplot(con.table) + geom_bar(stat="identity")
ggplot(dutchSpeakersDistMeta, aes(x = AgeGroup, fill = Sex)) + 
  geom_bar(position = "dodge")

## c) Inspect the table 'age' you created. Does it look like there could be a significant 
##    difference between the sexes?
head(age)

# It seems that in each age group, except for age25to34 and age35to44, there are significantly more females than males

## d) We are going to calculate whether there's a difference between males and females 
##    regarding their age group
##    using the function chisq.test. Look at the help of this function. Then use the 
##    function to calculate whether there's a difference in our table 'age'. 
##    Is there a significant difference in age group?
?chisq.test()
chisq <- sum((age[1,] - age[2,])^2/age[2,]) #manually counted chi-square
chisq.test(age)

# We can reject the null hypothesis that there is no significant difference in age group since p-value = 0.5124

## e) What are the degrees of freedom for our data? How are they derived?

# Degrees of freedom is number of categories - 1. Since we have 5 categories age18to24, age25to34,
# age35to44, age45to55 age56up as columns and 2 categories as rows, dof is (2-1)*(5-1) = 4

##########
##Exercise 3. Binomial versus chi-square
##########
##    In this exercise, we'll do significance tests for a paper on therapeutic touch 
##    (google it if you want to know what that is...) that was published in the Journal 
##    of the American Medical Association (Rosa et al., 1996).
##    The experimenters investigated whether therapeutic touch is real by using the 
##    following method:
##    21 practitioners of therapeutic touch were blindfolded. The experimenter 
##    placed her hand over one of their hands. If therapeutic touch is a real 
##    phenomenon, the principles behind it suggest that the participant should 
##    be able to identify which of their hands is below the experimenter's hand. 
##    There were a total of 280 trials, of which the therapeutic touch therapists 
##    correctly indicated when a hand was placed over one of their hands 123 times.

## a) What is the null hypothesis, i.e. how often would we expect the participants to 
##    be correct by chance (in raw number and in percentage)?

# Null hypothesis is that therapeutic touch doesn't work and the participants were not able to identify
# which of their hands is below the experimenter's hand.
# By chance, we expect them to get half of the trail right. i.e. 140/280 trials

## b) Using a chisquare test, what do you conclude about whether therapeutic touch 
##    works? 
#tt <- matrix(c(123, 157, 140, 140), ncol = 2, byrow = TRUE)
#colnames(tt) <- c("Correct", "Incorrect")
#rownames(tt) <- c("Observed", "Expected")
#tt <- as.table(tt)
#tt

#chisq.test(tt)

#You should have used simply chisq.test(c(123, 157)). If you supply expected counts as well, chisq.test() will calculate them again, thinking that 140 and 140 are actually observed values, which is not true.
chisq.test(c(123, 154))

#we cannot reject the null hypothesis because p>0.05. The obtained frequencies differ significantly from expected ones


## c) Now calculate significance using the binomial test as we used it in exercise 1.
# sum(dbinom(123 , size = 280, prob = 1/2))
pbinom(123, size = 280, prob = 0.5) * 2  #we need to multiply by 2, since our H0 is that the observed results are random, so we need to calculate binomial test, such that we take into account differences in both directions (both tails).

#we reject the hypothesis the null hypothesis, as the probability of our data belonging to binomial distribution is very small

## d) The results from these two tests are slightly different. Which test do you think 
##    is better for our data, and why?

#Chisquare test checks generally if our sample belongs to some distribution.
#So, probably, in case of binomial data it is better to use specific binomial test.

##########
##Exercise 4.
##########
## Describe a situation where you would choose McNemar's test over the ChiSquare test. 
## What would be the problem of using the normal ChiSquare test in a case where 
## McNemar's test would be more appropriate?

#For a ChiSquare test we assume that unpaired observations are unpaired from each other,
#while McNemar's test is appropriate when independency not necessarily helb between the pairs
