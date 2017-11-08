###############
### Cleaning Data
###############

# Please do the "Cleaning Data with R" exercise that was assigned in dataCamp.
# We recommend that you take notes during the DataCamp tutorial, so you're able to use the commands 
# you learned there in the exercise below.
# This week, the exercise will be about getting data into a good enough shape to start analysing. 
# Next week, there will be a tutorial on running t tests for this data.

# download the data file "digsym.csv" from the moodle and save it in your working directory. 
# The read in the data into a variable called "data".
data <- read.csv("digsym.csv")

# Load the libraries languageR, stringr, dplyr and tidyr.
check.install <-function(package) {
  if(is.element(package, rownames(installed.packages())) == FALSE) {install.packages(package)}
  library(package, character.only = TRUE)
}

check.install("languageR")
check.install("stringr")
check.install("dplyr")
check.install("tidyr")

# how many rows, how many columns does that data have?
# dim(data)
nrow(data)

# take a look at the structure of the data frame using "glimpse"
glimpse(data)

# view the first 20 rows, view the last 20 rows
head(data, 20)
tail(data, 20)

# Is there any missing data in any of the columns?
any(is.na(data))
sapply(X = data, function(col) {sum(is.na(col))})

# get rid of the row number column
data$X <- NULL

# put the Sub_Age column second
data <- subset(data, select = c(ExperimentName, Sub_Age, Group:File))
#better method
data <- select(data, ExperimentName, Sub_Age, Group:File)

# replace the values of the "ExperimentName" column with something shorter, more legible
data$ExperimentName <- str_replace(data$ExperimentName, "Digit Symbol - Kopie", "DSK")

# keep only experimental trials (encoded as "Trial:2" in List), get rid of practice trials 
# (encoded as "Trial:1"). When you do this, assign the subset of the data to a variable "data2", 
# then assign data2 to data and finally remove data2.
data2 <- data$List
data2 <- str_replace(data2, "Trial:1", "NA")
# better way
data2 <- subset(x = data, List == "Trial:2")
data <- data2
rm(data2)

# separate Sub_Age column to two columns, "Subject" and "Age", using the function "separate"
data <- separate(data, Sub_Age, c("Subject", "Age"))

# make subject a factor
data$Subject <- as.factor(data$Subject)
data$Age <- as.integer((data$Age))

# extract experimental condition from the "File" column:
# the stimulus that people saw - did it correspond to a wrong or right digit-symbol combination?

# 1) using str_pad to make values in the File column 8 chars long, by putting 0 on the end  (i.e., 
# same number of characters, such that "1_right" should be replaced by "1_right0" etc)
data$File <- str_pad(data$File, width = 8, side = "right", pad = "0")

# create a new column ("condition" (levels:right, wrong)) by extracting "right"/"wrong" using substr
data$condition <- substr(data$File, start = 3, stop = 7)

# get rid of obsolete File column
rm(data$File)

# missing values, outliers:

# do we have any NAs in the data, and if so, how many and where are they?
sum(is.na(data))

# create an "accuracy" column using if-statement
# if actual response (StimulDS1.RESP) is the same as the correct response (StimulDS1.CRESP), put 
# in value 1, otherwise put 0
data$accuracy <- ifelse(data$StimulDS1.RESP == data$StimulDS1.CRESP, 1, 0)

data$accuracy <- as.numeric(data$StimulDS1.RESP == data$StimulDS1.CRESP)

# how many wrong answers do we have in total?
length(data$accuracy) - sum(data$accuracy)
# better way
sum(data$accuracy == 0)

# whats the percentage of wrong responses?
sum(data$accuracy == 0) / nrow(data$accuracy) * 100

# create correct_RT column
data$correct_RT <- ifelse(data$accuracy, data$StimulDS1.RT, NA)

# create boxplot of correct_RT - any outliers?
boxplot(data$correct_RT)
# A lot of outliers

# create histogram of correct_RT with bins set to 50
hist(data$correct_RT, breaks = 50)

# describe the two plots - any tails? any suspiciously large values?

# view summary of correct_RT
summary(data$correct_RT)

# we got one apparent outlier: 13850
data$correct_RT[data$correct_RT == 13852] <- NA

# There is a single very far outlier. Remove it.


# dealing with the tail of the distribution: 
# outlier removal
# Now, remove all correct_RT which are more than 2.5. SD away from the grand mean
m <- mean(data$correct_RT, na.rm = TRUE)
d <- sd(data$correct_RT, na.rm = TRUE)
data$correct_RT_2.5sd <- data$correct_RT
#data$correct_RT_2.5sd[data$correct_RT > 2.5 * d + m] <- NA
#data$correct_RT[data$correct_RT < mean(data$correct_RT) - 2.5 * sd(data$correct_RT)] <- NA
data$correct_RT_2.5sd <- ifelse(data$correct_RT > m + 2.5 * d, NA, data$correct_RT)
data$correct_RT_2.5sd <- ifelse(data$correct_RT < m - 2.5 * d, NA, data$correct_RT)

# create new "correct_RT_2.5sd" column in data which prints NA if an RT value is below/above the cutoff
boxplot(data$correct_RT_2.5sd)
hist(data$correct_RT_2.5sd)
# take a look at the outlier observations
# any subjects who performed especially poorly?


# how many RT outliers in total?


# plot a histogram and boxplot of the correct_RT_2.5sd columns again - nice and clean eh?

# Next, we'd like to take a look at the avrg accuracy per subject
# Using the "cast" function from the library "reshape", create a new data.frame which shows the 
# average accuracy per subject. Rename column which lists the average accuracy as "avrg_accuracy".

# sort in ascending order or plot the average accuracies per subject.


# would you exclude any subjects, based on their avrg_accuracy performance?

# Congrats! Your data are now ready for analysis. Please save the data frame you created into a new 
# file "digsym_clean.csv".