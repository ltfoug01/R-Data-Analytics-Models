library(tidyverse)
library(rpart)       #?rpart
library(rpart.plot)  #?rpart.plot

housing <- read_csv("C:/Users/ltfou/Desktop/MSBA/MSBA 635 - Data Analytics II/R/Housing(1).csv")
set.seed(9)

##Partition the data-----------------------------------------------------------
sample_index <- sample(nrow(housing), nrow(housing) * 0.90)
housing_train <- housing[sample_index,]
housing_test <- housing[-sample_index,]


##Train/Fit Regression Tree on Train Data--------------------------------------
housing_rpart <- rpart(formula = medv~ ., data = housing_train) #formula specifies response/predictor vars
                       
housing_rpart

prp(housing_rpart, digits = 4, extra = 1) #plots the decision tree using prp func.

'
 In rpart(), the cp(complexity parameter) argument is one of the parameters 
 that are used to control the complexity of the tree. The help document for 
 rpart tells you "Any split that does not decrease the overall lack of fit 
 by a factor of cp is not attempted". For a regression tree, the overall 
 R-square must increase by cp at each step. Basically, the smaller the 
 cp value, the larger (complex) tree rpart will attempt to fit. 
 The default value for cp is 0.01.
'
##Pruning to Avoid Overfit-----------------------------------------------------
housing_largetree <- rpart(formula = medv ~ ., data = housing_train, cp = 0.001)
prp(housing_largetree)

#plotcp() function gives the relationship between 10-fold cross-validation error in the training set and size of tree
plotcp(housing_largetree)

'
You can observe from the above graph that the cross-validation error (x-val) 
does not always go down when the tree becomes more complex. The analogy is 
when you add more variables in a regression model, its ability to predict 
future observations does not necessarily increase. A good choice of cp for 
pruning is often the leftmost value for which the mean lies below the 
horizontal line. This horizontal line is 1SE above the minimum error, 
thus where we want the smallest tree below this line. In the Boston housing 
example, you may conclude that having a tree mode with more than 10 splits is 
not helpful.

To get the best tree, we use the cp value that is the leftmost value for which 
the mean lies below the horizontal line. That is cp=0.008.
'
prune_tree <- prune(housing_largetree, cp = 0.008)
prp(prune_tree, digits = 4, extra = 1)

##Get Predictions--------------------------------------------------------------
housing_test_predtree = predict(prune_tree, housing_test)


##Evaluate Model Performance---------------------------------------------------
ASE_tree <- mean((housing_test_predtree - housing_test$medv)^2)

ASE_tree

#compare model's out-of-sample performance with linear regression model with all variables
housing.regression = lm(medv~., data = housing_train)
housing_test_pred_reg = predict(housing.regression, housing_test)
reg_ASE=mean((housing_test_pred_reg - housing_test$medv)^2)
reg_ASE

#lower ASE better










