library(tidyverse)
library(ROCR)

'
 Building a logistic regression model using the default of credit card clients 
 Data Set from UCI Machine Learning Repository. 
 
 Know how to use glm() to build logistic regression
 
 Know how to get ROC and AUC based on predicted probability
 
'

credit_data <- read_csv('C:/Users/ltfou/Desktop/MSBA/MSBA 635 - Data Analytics II/R/Credit_default.csv') 

credit_data <- rename(credit_data, DEFAULT = default.payment.next.month)

colnames(credit_data)

view(credit_data)


## See statistics--------------------------------------------------------------
mean(credit_data$DEFAULT)      #how many people are default in the sample
str(credit_data)               #see variable types
summary(credit_data)           #summary stats


## Categorical data - convert to factor----------------------------------------
credit_data$SEX <- as.factor(credit_data$SEX)
credit_data$EDUCATION <- as.factor(credit_data$EDUCATION)
credit_data$MARRIAGE <- as.factor(credit_data$MARRIAGE)
str(credit_data)


## Partition the data----------------------------------------------------------
index <- sample(nrow(credit_data), nrow(credit_data) * 0.80) #80% training

credit_train = credit_data[index,]
credit_test = credit_data[-index,]


## Train or Fit Model on Training Data-----------------------------------------

'
 Logistic Regression In this lab, glm() is the main function used to build 
 logistic regression model because it is a member of generalized linear model.
 In glm(), family specifies the distribution of your response variable.

 Note: you will need to change family depending on if you have a classification
 or regression problem. For logistic regression, use family=binomial(logit). 
 For linear regression, use family=gaussian.
'

credit_glm0 <- glm(DEFAULT~., family = binomial(logit), data = credit_train)

summary(credit_glm0) # returns coefficient estimates

#Get the predicted probabilities for the test data to use in our ROC curve 
#function later
pred_glm0_test <- predict(credit_glm0, newdata = credit_test, type = 'response')

## Evaluate Model Performance (ROC/AUC)----------------------------------------
'
 In order to show give an overall measure of goodness of classification, using 
 the Receiver Operating Characteristic (ROC) curve is one way. Rather than use 
 an overall misclassification rate, it employs two measures - true positive rate
 (TPR) and false positive rate (FPR).

 To create an ROC curve and AUC statistic, use the Package ROCR

'

pred <- prediction(pred_glm0_test, credit_test$DEFAULT)

perf <- performance(pred, 'tpr', 'fpr')

plot(perf, colorize = TRUE)

unlist(slot(performance(pred, 'auc'), 'y.values'))


