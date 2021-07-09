library(tidyverse)
library(corrplot)
library(ggplot2)
library(stargazer)
'
How to perform linear regression and other plotting methods in R.
Based on historical Boston housing data.
'

housing <- read_csv("C:/Users/ltfou/Desktop/MSBA/MSBA 635 - Data Analytics II/R/Housing(1).csv")


dim(housing)      #no. of rows and cols

names(housing)    #col names

str(housing)      #variable types

summary(housing)  #summary stats


## BOXPLOTS------------------------------------------------------------
boxplot(housing)         #boxplots all predictor variables

boxplot(housing[,1:3])   #boxplot(dataframe[row,column])

boxplot(housing[,c(1,2,4,6,7,8,9,11,12,13)])

boxplot(housing$medv~housing$chas, ylab = 'Median house price',
        xlab = 'Near the river (N/Y)')   #plot by group


## SCATTER PLOT MATRIX-------------------------------------------------
pairs(housing)                #all variables

pairs(housing[,c(1,6,13)])    #specified vars

#check correlation between variables
corrplot(cor(housing), method = "number", type = "upper", diag = FALSE)

#scatter plot
housing %>%
  gather(key, val, -medv) %>%
  ggplot(aes(x = val, y = medv)) +
  geom_point() +
  stat_smooth(method = "lm", se = TRUE, col = "blue") +
  facet_wrap(~key, scales = "free") +
  theme_gray() +
  ggtitle("Scatter Plot of Dependent Variables vs Median Value (medv)") 


## Data Partitioning---------------------------------------------------

#randomly sample the row numvers of 90% data
sample_index <- sample(nrow(housing), nrow(housing) * 0.90) 

#create the training partition (90% of data)
Housing_train <- housing[sample_index,]

#create the testing partition (10% of data)
Housing_test <- housing[-sample_index,]


model_1 <- lm(medv~crim+zn+chas+nox+rm+dis+rad+tax+
                ptratio+lstat+age, data = Housing_train) #includes all vars

model_1 <- lm(medv~., data=Housing_train) #also includes all vars

summary(model_1) #returns coefficients & other stats
stargazer(model_1, type = 'text')

#new model with indus and age removed becasue of insignificance
model_2 <- lm(medv ~ . -indus -age, data = Housing_train)
summary(model_2)

## Model Assessment----------------------------------------------------
'
 Model Evaluation on the test data (also known as out of sample error).
 To evaluate how the model performs on future data, we use the function 
 predict() to get the predicted values from the test set.
'

#pi is a vector that contains predicted values for test set.
pi <- predict(object = model_1, Housing_test) #or predict(model_1 ,Housing_test)
pi2 <- predict(object = model_2, Housing_test)

#Average Squared Error (ASE) also known as 
#Mean squared eeror(MSE): average of the squared differences 
#between the predicted and actual values
mean((pi - Housing_test$medv)^2)
mean((pi2 - Housing_test$medv)^2)

predict(model_1)
predict((model_2))

