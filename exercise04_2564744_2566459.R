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
age
# Females are usually more than males except for the AgeGroup age35to44 where males are double than females

## d) We are going to calculate whether there's a difference between males and females 
##    regarding their age group
##    using the function chisq.test. Look at the help of this function. Then use the 
##    function to calculate whether there's a difference in our table 'age'. 
##    Is there a significant difference in age group?
?chisq.test
chisq.test(age)
# We cannot reject the null hypothesis that there is no significant difference in age group since p-value is nearly 0.5

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

## b) Using a chisquare test, what do you conclude about whether therapeutic touch 
##    works? 

## c) Now calculate significance using the binomial test as we used it in exercise 1.

## d) The results from these two tests are slightly different. Which test do you think 
##    is better for our data, and why?


##########
##Exercise 4.
##########
## Describe a situation where you would choose McNemar's test over the ChiSquare test. 
## What would be the problem of using the normal ChiSquare test in a case where 
## McNemar's test would be more appropriate?

