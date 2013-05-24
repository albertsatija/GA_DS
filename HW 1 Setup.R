# this code should run the KNN and the NB test automatically
# unfortunately I ran into a problem with the KNN function.  
# The code works independently (i.e. if I just run the code inside the function in the console )
# but I think my 'avgerr.rates' parameter is being dropped before it reaches my plot function when I use the 
# function call .. 


# load required libraries
library(class)
library(ggplot2)
library(e1071)

	


# reads the initial table 
	german.table <- read.table('/Users/albertsatija/data_science_class_examples/homework/homework_1.txt', sep = "", h=T)

# assign columns names.  1 is good credit, 2 is bad 
	 names(german.table) <- c(
	 	'checkaccount',
	 	'duration',
	 	'rateofdi',
	 	'purpose',
	 	'savings',
	 	'employmentlength',
	 	'sex',
	 	'residence',
	 	'liable',
	 	'age',
	 	'otherinstallment',
	 	'numofcredits',	
	 	'property',
	 	'telephone',
	 	'creditamt',
	 	'jobtype',
	 	'housing',
	 	'credithistory',
	 	'guarantors',
	 	'foreign',
	 	'flag1', 
	 	'flag2', 
	 	'flag3', 
	 	'flag4', 
	 	'classification'
	 	 )

# create a linear model (RSE of 0.4, R2 of .25)
	fit <- lm(classification ~ ., data = german.table)
	summary(fit)

# the variables that seem to fit best are checkaccount, duration, savings.  Just exploring these a bit further
# the coefficients for checkaccount and savings are both negative which makes sense as the more money/savings, the
# the higher the likehlihood you would think of being a good credit.  duration has a positive coefficient which 
# suggests that longer the loan outstanding, the worse the credit.  also makes sense
# this table for savings shows for example that much greater % of people with high savings (5) are good credits (1)


	xtabs(~classification + savings, data=german.table)


# remove variables that are not significant to enhance my fit but didn't help (RSE of 0.4, R2 of .25)
# doesn't make sense to run linear regression because this should be a logistical regression (binary outcome.. 
	fit1 <- update(fit, .~. -purpose -residence -age -property -flag1 -flag2 -flag3 -flag4)
	summary(fit1)	

# passing in the source for my knn file
	setwd ('~/GA_DS')
	source('HW 1 KNN.R')

# calling in my function 	
	knnvalidation()

# passing in the source for my naive Bayes file.  
	source('HW 1 NB.R')

# calling my naivebayes function
	nbtest()
