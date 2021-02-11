library(caret)

credit_data <- read.csv('Credit_default.csv',
                        header = T)

##Preprocessing----------------------------------------------------------------
library(dplyr)

credit_data<- rename(credit_data, default=default.payment.next.month)

str(credit_data)

credit_data$SEX<- as.factor(credit_data$SEX)
credit_data$EDUCATION<- as.factor(credit_data$EDUCATION)
credit_data$MARRIAGE<- as.factor(credit_data$MARRIAGE)

##Impute Missing Predictor Variables-------------------------------------------
install.packages("Rtools")
library(imputeMissings)

credit_data<- imputeMissings::impute(credit_data, method = "median/mode")
sum(is.na(credit_data))

#convert to dummy variables for character variables
credit_data<-model.matrix(~.,data=credit_data)
credit_data<-data.frame(credit_data[,-1]) #take out intercept column


##Partition the data-----------------------------------------------------------
set.seed(99)
index <- createDataPartition(credit_data$default, p = .6,list = FALSE)
credit_train <- credit_data[index,]
credit_test <- credit_data[-index,]


##Fit the Model----------------------------------------------------------------
library(glmnet)

#Set up tuning parameter selection and CV for the training set
fitControl <- trainControl(method = "cv", 
                           number = 5, 
                           verboseIter = FALSE)

LASSOgrid = expand.grid(alpha=1,
                        lambda=seq(0, 1, by = 0.01))


modelFit <- train(as.factor(default)~., data = credit_train , 
                  method = "glmnet", 
                  trControl = fitControl,
                  tuneGrid = LASSOgrid)
plot(modelFit)


##Get Predictions using Testing Set Data---------------------------------------
credit_prob<- predict(modelFit, credit_test, type="prob")


##Evaluate Model Performance---------------------------------------------------
library(ROCR)

pred = prediction(credit_prob[,2], credit_test$default)
perf = performance(pred, "tpr", "fpr")

plot(perf, colorize=TRUE)

slot(performance(pred, "auc"), "y.values")[[1]]




