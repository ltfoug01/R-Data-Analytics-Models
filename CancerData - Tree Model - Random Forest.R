'For this problem, we use the Wisconsin Breast cancer data set where the goal is
 to predict if a mass is malignant or benign. Features are computed from a 
 digitized image of a fine needle aspirate (FNA) of a breast mass.'

library(caret)            #Classification And REgression Trainin - ?caret
library(tidyverse)

cancer_data <- read_csv("C:/Users/ltfou/Desktop/MSBA/MSBA 635 - Data Analytics II/R/cancer_data.csv")

view(cancer_data)

##Partition the data-----------------------------------------------------------
set.seed(99)

#index <- sample(nrow(cancer_data), nrow(cancer_data) * 0.80)

index <- createDataPartition(cancer_data$Outcome, p = .8, list = FALSE) #partition
bc_train <- cancer_data[index,]    #train set
bc_test <- cancer_data[-index,]    #test set


##Fit the Model----------------------------------------------------------------
'
 We use cross-validation to choose the tuning parameters to minimize overfitting.
 In caret, to implement cross-validation in the training data, we use the 
 trainControl method for cross-validation.
'
fitControl <- trainControl(method = "repeatedcv", 
                           number = 5, 
                           verboseIter = FALSE)

library(randomForest)
library(e1071)

model_rf <- train(Outcome ~ .,            #response var - outcome
                  data = bc_train,        #data - train set
                  method = "rf",          #predictive algorithm - rand. forest
                  trControl =fitControl,  #5 Fold Cross Validation 
                  verbose = 0)            #limited output

model_rf
plot(model_rf)


'
 Here importance refers to the total amount that the RSS is decreased due to 
 splits with a given predictor, averaged over all trees for regression trees. 
 For classification trees, it is the total amount that the Gini index is 
 decreased by splits over a given predictor, averaged over all trees.
'
plot(varImp(model_rf,scale=F))  #see the important predictor variables


#Set up tuning parameter selection and CV for the training set
# define a grid of parameter options to try
rf_grid <- expand.grid(mtry = c(1, 3,6,9))
rf
plot(model_rf_tune)_grid

# Train model with preprocessing & cv
model_rf_tune <- train(Outcome ~ .,
                       data = bc_train,
                       method = "rf",
                       trControl =fitControl,
                       tuneGrid = rf_grid,     #provide a grid of parameters
                       verbose = FALSE) 



##Get Predictions using Testing Set Data---------------------------------------
bc_prob<- predict(model_rf, bc_test, type="prob")

head(bc_prob)    #we want yes(2nd col);heart attack


##Evaluate Model Performance---------------------------------------------------
library(ROCR)

#churn_test_prob<- predict(prune_tree, churn_test, type="prob")
pred = prediction(bc_prob[,2], bc_test$Outcome)
perf = performance(pred, "tpr", "fpr")

plot(perf, colorize=TRUE)                          #ROC Curve

slot(performance(pred, "auc"), "y.values")[[1]]    #AUC





