library(caret)

housing_data <- read.csv("C:/Users/ltfou/Desktop/MSBA/MSBA 635 - Data Analytics II/R/Housing(1).csv",
                         header = T)

view(housing_data)

##Partition the data-----------------------------------------------------------
index <- createDataPartition(housing_data$medv, p = 0.8, list = FALSE)

h_train <- housing_data[index,]  #train set
h_test <- housing_data[-index,]  #test set


##Fit the Model----------------------------------------------------------------
'
 We use cross-validation to choose the tuning parameters to minimize overfitting.
 In caret, to implement cross-validation in the training data, we use the 
 trainControl method for cross-validation.
'
fitControl <- trainControl(method = "repeatedcv", 
                           number = 5, 
                           verboseIter = FALSE)

library(xgboost) #?xgboost

model_gbm <- train(medv ~ .,                 #response var - medv
                   data = h_train,           #data - train set
                   method = "xgbTree",       #predictive algorithm - XG Boost
                   trControl = fitControl,   #5 Fold Cross Validation 
                   verbose = FALSE)           

plot(model_gbm)

plot(varImp(model_gbm,scale=F)) #see the important predictor variables


##Tuning Parameters & Cross Validation-----------------------------------------

#Set up tuning parameter selection and CV for the training set
xgb_grid <- expand.grid(
  nrounds = c(50,200),
  eta = c(0.025, 0.05, 0.1),
  max_depth = c(2, 3, 4, 5),
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1)


# Train model with preprocessing & cv
model_rf_tune <- train(medv~.,data = h_train,
                       method = "xgbTree",
                       trControl = fitControl,
                       tuneGrid = xgb_grid,     # provide a grid of parameters
                       verbose = FALSE)

plot(model_rf_tune)


##Get predictions using Testing Set Data---------------------------------------
h_prob <- predict(model_gbm, h_test)

head(h_prob)


##Evaluate Model Performance---------------------------------------------------
ASE <- mean((h_prob - h_test$medv) ^2)    #Compute ASE on test set.

ASE





