library(tidyverse)
library(rpart)       #?rpart
library(rpart.plot)  #?rpart.plot
library(ROCR)

'
 Use rpart() to fit regression and classification trees.

 Know how to interpret a tree.

 Use predict() for prediction, and how to assess the performance.

 Know how to use Cp plot/table to prune a large tree.
'

churn_data <- read_csv("C:/Users/ltfou/Desktop/MSBA/MSBA 635 - Data Analytics II/R/churn_dataset.csv")

head(churn_data)
str(churn_data)

##Partition the data-----------------------------------------------------------
set.seed(9)
index <- sample(nrow(churn_data), nrow(churn_data) * 0.70)
churn_train = churn_data[index,]
churn_test = churn_data[-index,]


##Train/Fit Classification Tree on the Train Data-------------------------------
churn_rpart <- rpart(formula = Churn~., data = churn_train, method = 'class',
                     parms = list(split = 'information')) #method = "class" is required if the response is not declared as factors

churn_rpart
prp(churn_rpart, extra = 1)


'
 plotcp() function gives the relationship between 10-fold cross-validation error
 in the training set and size of tree. We choose the cp value that is below the
 horizontal line which is 1SE above the minimum of the curve, which is cp =0.03
'
##Prune to Avoid Overfitting---------------------------------------------------
plotcp(churn_rpart)

prune_tree <- prune(churn_rpart, cp = 0.03)

prp(prune_tree, extra = 1) #plot prune tree


'
 For a binary classification problem, as you learned in logistic regression 
 there are 2 types of predictions. One is the predicted class of response 
 (0 or 1), and the second type is the probability of response being 1. We 
 use an additional argument type="class" or type="prob" to get these
'
##Get Predictions using the testing set data-----------------------------------
churn_test_prob <- predict(prune_tree, churn_test, type = 'prob')

#Use the second column for predicted probability of Yes
head(churn_test_prob)


'
 To get ROC curve, we get the predicted probability of Y being 1 from the 
 fitted tree. The additional cp parameter controls the complexity of tree. 
 Here we change it from its default 0.01 to a smaller value to grow a more 
 complex tree than just the root node (if you use the default the tree you 
 get will tell you to clasify everything as 0)
'
##Evaluate Model Performance---------------------------------------------------

#How Good is the Classifier using ROC Curve and AUC
churn_test_prob<- predict(prune_tree, churn_test, type="prob")
pred = prediction(churn_test_prob[,2], churn_test$Churn)
perf = performance(pred, "tpr", "fpr")
plot(perf, colorize=TRUE)

slot(performance(pred, "auc"), "y.values")[[1]] #AUC

