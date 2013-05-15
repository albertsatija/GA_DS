
# basically we create a historgram of NH4, create a regression of two varialbes P04, oP04). 
# Then we use a knnimputation function to fill out the NA in our entire data frame
# We then use this new filled in dataframe to create a regression model for a1 (on all the other variables)


install.packages('DMwR') # downloaded datamining with R
library(DMwR)

data(algae) # creating a data frame
summary(algae) # summarizing the data

hist(algae$mxPH, prob = T) #creating a histogram of the column mxPH

plot(algae$NH4, xlab = '') #plotting the NH4 colmumn
abline(h = mean(algae$NH4, na.rm = T), lty = 1)   #adding a line for mean
abline(h = mean(algae$NH4, na.rm = T) + sd(algae$NH4, na.rm = T), lty = 2) # adding a line for 1SD
abline(h = median(algae$NH4, na.rm = T), lty = 3)   # adding a line for meiduam

lm(PO4 ~ oPO4, data = algae) # regression of oPO4 for P04
clean.algae <- knnImputation(algae, k = 10) # fills in NA values by using KNN function 

lm.a1 <- lm(a1 ~ ., data = clean.algae[, 1:12]) # regression of all variables on a1 using the new cleaned data frame
summary(lm.a1)
anova(lm.a1) # creates a variance table 

lm2.a1 <- update(lm.a1, . ~ . - season) # this time we remove the season variable
summary(lm2.a1)
anova(lm.a1, lm2.a1)

final.lm <- step(lm.a1) # step algorithim.  I dont know what that does 
