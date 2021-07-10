##Problem Description----------------------------------------------------------
'
 Covers Principal Components Analysis for regression (also known as PCR). This 
 technique uses PCA which can reduce the dimension of predictor variables before
 fitting supervised models. Therefore, note that PCR is a two step process, and 
 in the first step, the PCA does not consider any aspects of the response when 
 it selects the components. Instead, it seeks to reduce the variability present 
 throughout the predictor space.

 PCA can also be used to visualize trends in the data for datasets with a large 
 number of variables.

 In this problem, we wish to predict a baseball players Salary on the basis of 
 various statistics associated with performance in the previous year. Here we 
 use principal component analysis for regression to first reduce the number of 
 predictor variables in data to two or three principal components with minimal 
 loss of information
'
library(caret)
library(tidyverse)

Hitters_data <- read.csv("Hitters.csv", header = T)

names(Hitters_data)


##Partition the Data-----------------------------------------------------------
set.seed(99)

index <- createDataPartition(Hitters_data$Salary, p = 0.8, list = FALSE)

Hitters_train <- Hitters_data[index,]
Hitters_test <- Hitters_data[-index,]


##Fit the Model----------------------------------------------------------------

#Set up tuning parameters using cross validation to minimize overfitting
fit_control <- trainControl(method = 'cv',
                            number = 5)

library(pls)

'
 Anytime we want to fit a model using train we tell it which model to fit by 
 providing a formula for the first argument with the response first then ~ then 
 the features. Then we need to provide a method (we specify "rf" to implement 
 randomForest). Make sure your categorical variables are either factor or 
 character.
'

# define a grid of parameter options to try - 19 variables
pcr_grid <- expand.grid(ncomp = 1:(ncol(Hitters_train)-1))

pcr_grid

model_pcr <- train(Salary ~ .,
                   data = Hitters_train,               #train set
                   method = "pcr",                     #principal comp. regression
                   preProcess = c("center", "scale"),  #center and scale variables 
                   trControl = fit_control,            #set up cross-validation
                   tuneGrid = pcr_grid)                #provide a grid of number of Principal components to try

model_pcr

summary(model_pcr)     #Summarize the final model - show principal components

plot(model_pcr)

plot(varImp(model_pcr)) #variable importance


##Get Predictions using Test Set Data------------------------------------------
Hitters <- predict(model_pcr, Hitters_test) #predict on the testing sample

head(Hitters)


##Evaluate Model Performance---------------------------------------------------
ASE <- mean((Hitters- Hitters_test$Salary) ^2) #ASE

ASE

