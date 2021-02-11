#CreditScoreExam - Question 16
#Luke Fougerousse
library(caret)
library(randomForest)
library(e1071)
library(ROCR)

credit_score <- read.csv('CreditScoreExam.csv')
head(credit_score)

set.seed(99)

##Partition the Data-----------------------------------------------------------
index <- createDataPartition(credit_score$CreditScore, p = 0.7, list = FALSE)
credit_train <- credit_score[index,]
credit_test <- credit_score[-index,]


##Fit Model--------------------------------------------------------------------

#CV and tuning parameters
fit_control <- trainControl(method = "cv",  
                            number = 5, 
                            verboseIter = FALSE) 

#train rf w/ cv
credit_model_rf <- train(CreditScore ~ .,         #response var - CreditScore
                         data = credit_train,     #data - train set
                         method = "rf",           #predictive algorithm - rand. forest
                         trControl = fit_control, #5 Fold Cross Validation 
                         verbose = 0)             #limited output

credit_model_rf

plot(credit_model_rf) #plot credit model

plot(varImp(credit_model_rf, scale = F)) #important predictor variables



#Get Test Set Predictions------------------------------------------------------
credit_prob <- predict(credit_model_rf, credit_test)

head(credit_prob)

ASE <- mean((credit_prob - credit_test$CreditScore)^2) #Compute ASE on test set.

ASE



