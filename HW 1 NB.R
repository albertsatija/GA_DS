# This file runs the naive bayes function to decide on the credit score 
# result is that it has an error rate of 26% which is lower than my KNN test (27.5%)
# 



#library(class)
#library(ggplot2)
#library(e1071)
	

## Runs the naiveBayes Test
nbtest <- function() {
	# reads the initial table 
	x <- read.table('/Users/albertsatija/data_science_class_examples/homework/homework_1.txt', sep = "", h=T)
	
	classifier <- naiveBayes(x[,1:24], as.factor(x[,25]))
	table(predict(classifier, x[,-25]), as.factor(x[,25]), dnn=list('predicted','actual'))
}

# result is that it has an error rate of 26% which is lower than my KNN test (28.5%)





## second try - didn't work when I use the actualy column nmames.. 

#	x <- read.table('/Users/albertsatija/data_science_class_examples/homework/homework_1.txt', sep = "", h=T)
#	names(x) <- c('checkaccount','duration','credithistory', 'purpose','creditamt', 'savings', 'employmentlength', 'rateofdi', 'sex', 'guarantors', 'residence', 'property','age', 'otherinstallment', 'housing', 'numofcredits', 'jobtype','liable', 'telephone', 'foreign', 'flag1', 'flag2', 'flag3', 'flag4', 'classification')
#	classifier <- naiveBayes(-x$classification, as.factor(x$classification))
#	table(predict(classifier, -x$classification), as.factor(x$classification), dnn=list('predicted','actual'))