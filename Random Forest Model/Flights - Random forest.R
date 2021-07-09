library(caret)            #Classification And REgression Trainin - ?caret
library(tidyverse)


##Partition the data-----------------------------------------------------------
set.seed(98)

flight_train <- read_csv("Flights_train.csv")    #train set
flight_test <- read_csv("Flights_test.csv")    #test set


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

model_rf <- train(Arr_Delay ~ .,            #response var - outcome
                  data = flight_train,      #data - train set
                  method = "rf",            #predictive algorithm - rand. forest
                  trControl = fitControl,   #5 Fold Cross Validation 
                  verbose = FALSE)          #limited output

model_rf
plot(model_rf)


'
 Here importance refers to the total amount that the RSS is decreased due to 
 splits with a given predictor, averaged over all trees for regression trees. 
 For classification trees, it is the total amount that the Gini index is 
 decreased by splits over a given predictor, averaged over all trees.
'
plot(varImp(model_rf, scale = F))  #see the important predictor variables


#Set up tuning parameter selection and CV for the training set
# define a grid of parameter options to try
rf_grid <- expand.grid(mtry = c(1, 3,6,9))
rf_grid

# Train model with preprocessing & cv
model_rf_tune <- train(Arr_Delay ~ .,
                       data = flight_train,
                       method = "rf",          #predictive algorithm - rand. forest
                       trControl = fitControl,
                       tuneGrid = rf_grid,     #provide a grid of parameters
                       verbose = FALSE) 

plot(model_rf_tune)


##Get Predictions using Testing Set Data---------------------------------------
bc_prob<- predict(model_rf, bc_test)

head(bc_prob)    #we want yes(2nd col);heart attack


##Evaluate Model Performance---------------------------------------------------
library(ROCR)

#churn_test_prob<- predict(prune_tree, churn_test, type="prob")
ASE <- mean((bc_prob - bc_test$Arr_Delay) ^2)    #Compute ASE on test set.

ASE

pred = prediction(bc_prob[,2], bc_test$Arr_Delay)
perf = performance(pred, "tpr", "fpr")

plot(perf, colorize = TRUE)                        #ROC Curve

slot(performance(pred, "auc"), "y.values")[[1]]    #AUC





