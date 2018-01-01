### Stats with R Exercise sheet 9

##########################
#Week 10: Linear Mixed Effects Models
##########################


## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:59 on Monday, January 01. Write the code below the questions. 
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

library(lme4)
library(lattice)
library(Matrix)
library(ggplot2)

# Read in the data file from gender.Rdata, sem.Rdata or relclause.Rdata.
# You can choose the one you'd like best based on the description of the items below, 
# and explore the analysis for that dataset. Afterwards, you can adapt your analysis 
# for the other datasets (that should be considerably less work.)

# The files contain data from an experiment where people were reading sentences, 
# and pressed the space bar to see the next word. The duration for which a word was 
# viewed before pressing the space bar again is the reading time of the word, and is 
# stored in the file as "WORD_TIME". The experiment had 24 items (given as "ITEM_ID") 
# and 24 subjects (given as "PARTICIPANT"). The order in which the different sentences 
# were presented in the experiment is given in the variable "itemOrder". 

# For each of the files, the sentences that were shown had a different property. 

# Sentences in the sem.Rdata experiment had a semantic violation, i.e. a word that 
# didn't fit in with the previous words in terms of its meaning. The experiment 
# contained two versions of each item, which were identical to one another except 
# for the one sentence containing a semantic violation, while the other one was 
# semantically correct. These conditions are named "SG" for "semantically good" 
# and "SB" for "semantically bad".

# Semantic materials (the experiment is in German, English translation given 
# for those who don't speak German')

# Christina schießt / raucht eine Zigarette nach der Arbeit. 
# "Christina is shooting / smoking a cigarette after work."

# The crticial word here is "Zigarette", as this would be very surprising in the 
# context of the verb "schießt", but not in the context of the verb "smoke". 
# Reading times are comparable because the critical word "Zigarette" is identical 
# in both conditions.

# Syntactic items:
# Simone hatte eine(n) schreckliche(n) Traum und keine Lust zum Weiterschlafen. 
# "Simone had a[masc/fem] horrible[masc/fem] dreammasc and didn't feel like sleeping 
# any longer."

# Here, there are again two conditions, one using correct grammatical gender on 
# "einen schrecklichen" vs. the other one using incorrect grammatical gender 
# "eine schreckliche". The critical word is "Traum" (it's either consisten or 
# inconsistent with the marking on the determiner and adjective)

# Relative clause items:
# Die Nachbarin, [die_sg nom/acc einige_pl nom/acc der Mieter auf Schadensersatz  
# verklagt hat_sg/ haben_pl]RC, traf sich gestern mit Angelika. 
# "The neighbor, [whom some of the tenants sued for damages / who sued some of  the
# tenants for damages]RC, met Angelika yesterday."

# When reading such a sentence, people will usually interpret the relative pronoun 
# die as the subject of the relative clause and the following noun phrase 
# "einige der Mieter" as the object. This interpretation is compatible with 
# the embedded singular-marked (sg) verb hat at the end of the relative clause. 
# Encountering the verb haben, which has plural marking (pl), leads to processing 
# difficulty: in order to make sense of the relative clause, readers need to 
# reinterpret the relative pronoun die as the object of the relative clause 
# and the following noun phrase "einige der Mieter" as its subject. 
# (Note that the sentences are all grammatical, as the relative pronoun and 
# following NPs are chosen such that they are ambiguous between nominative (nom)
# and accusative (acc) case marking.)

# The number of the word in a sentence is given in column "SEMWDINDEX". 
# 0 designates the word where the semantic violation happens (in the SB condition; 
# in the SG condition, it's the corresponding word). We call this word the 
# "critical word" or "critical region". -1 is the word before that, -2 is 
# two words before that word, and 2 is two words after that critical word. 
# "EXPWORD" shows the words. We expect longer reading times for the violation 
# at word 0 or the word after that (word 1) (if people press the button quickly 
# before thinking properly).

#######################################################################################
#######################################################################################

# a) Take a look at the data.
gender.data <- read.table("gender.Rdata")
# relclause.data <- read.table("relclause.Rdata")
# sem.data <- read.table("sem.Rdata")
head(gender.data)
# b) Plot it (use ggplot for this task and all the tasks below).
#    (You can provide any plots we have seen so far to interpret the data.
#    For example, you can study the difference between the subjects (participants) 
#    in terms of responce time or the difference between items (sentences) in 
#    terms of response time).
ggplot(data = gender.data, aes(x = PARTICIPANT, y = WORD_TIME, color = ITEM_ID)) + geom_point()

#    Below you also find the plot for the dataset 'sleepstudy' from the package 'lme4'.
#    The figure shows relationships between days without sleeping and reaction 
#    time for each participant (subject) separately.

summary(sleepstudy)
print(xyplot(Reaction ~ Days | Subject, sleepstudy, aspect = "xy",
             layout = c(9,2), type = c("g", "p", "r"),
             index.cond = function(x,y) coef(lm(y ~ x))[1],
             xlab = "Days of sleep deprivation",
             ylab = "Average reaction time (ms)"))

#    Your task is also to figure out how to adapt this plot for our data. What do you 
#    conclude regarding the reading sentences experiment?
print(xyplot(PARTICIPANT ~ WORD_TIME | ITEM_ID, gender.data, aspect = "xy",
             layout = c(8,3), type = c("g", "p", "r"),
             index.cond = function(x,y) coef(lm(y ~ x))[1],
             xlab = "Days of sleep deprivation",
             ylab = "Average reaction time (ms)"))

# c) Decide whether you want to exclude any data points (provide not only the code,
#    but also a detailed (!) explanation);

# d) Try to make a plot where for each word, the average reading 
#    (collapsing across items and subjects) is shown; in this plot all violations 
#    are at point 0. Of course, you should not collapse the semantically good vs. 
#    bad condition.

# e) Experiment with calculating a linear mixed effects model for this study, 
#    and draw the appropriate conclusions (give a detailed explanation 
#    for each model).

#    Let's get back to the dataset 'sleepstudy'. The following plot shows 
#    subject-specific interception and slope. Adapt this plot for our study 
#    and make conclusions.

model = lmer(Reaction ~ Days + (Days|Subject), sleepstudy)
print(dotplot(ranef(model,condVar=TRUE),  scales = list(x = list(relation = 'free')))[["Subject"]])